// Shared by the gates: local and cluster exec (ssh through the bastion's
// mux), a slot of 48 minis (the live ones), a pool that hands jobs to free
// nodes, and the verdict line.
//
// $BEND_HOC, a bastion user (or user@host), is the way in for a machine
// without the `cluster` aliases or a key the minis trust: each session goes
// to the bastion and on through its own wrapper, `sudo hoc-ssh <index>`,
// which holds the fleet key, so no personal key lives on the Macs. Only
// the hop differs: the slots, the pool and the scripts are the same.

import * as child from "node:child_process";
import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as path from "node:path";

// Types
// =====

export type Exec = { out: string; err: string; code: number };

export type Job = (node: number) => Promise<void>;

// Constants
// =========

export const GATE = process.argv.includes("--gate");

export const ROOT = path.join(import.meta.dirname, "..");

// the site repo (bendlang/bend-lang.com): the installer, the launcher, the
// hub and release.ts live there, beside this checkout or at $SITE_REPO
export const SITE = process.env.SITE_REPO ?? path.join(ROOT, "..", "bend-lang.com");

export const BUN = "/usr/local/bun/bin/bun";

const HOC = process.env.BEND_HOC ?? "";

const SLOTS = { dir: "/tmp/bend-cluster-slots", count: 4, size: 48, base: 2 };

const STALE = 20 * 60 * 1000;

const MUX = ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8",
  "-o", "ControlMaster=auto", "-o", "ControlPath=/tmp/bend-cluster-mux",
  "-o", "ControlPersist=600"];

const SSH = ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8",
  "-o", "ProxyCommand=ssh " + MUX.join(" ") + " -W %h:%p cluster"];

const HOC_HUB = ["-p", "22022", HOC.includes("@") ? HOC : HOC
  + "@52.67.125.18"];

const DROP = new RegExp("Connection reset|closed by remote host"
  + "|Broken pipe|kex_exchange_identification|mux_client|timed out");

let held = "";

const staged = new Map<string, Promise<Exec>>();

const opened = new Map<number, Promise<Exec>>();

// Exec
// ====

export function exec(bin: string, args: string[], input?: Buffer | string,
  timeout = 600_000, env?: Record<string, string>, cwd = ROOT): Promise<Exec> {
  return new Promise((resolve) => {
    const kid = child.spawn(bin, args, { cwd, env: { ...process.env,
      ...env }, stdio: ["pipe", "pipe", "pipe"] });
    const outs: Buffer[] = [];
    const errs: Buffer[] = [];
    const bomb = setTimeout(() => kid.kill("SIGKILL"), timeout);
    kid.stdout.on("data", (d: Buffer) => outs.push(d));
    kid.stderr.on("data", (d: Buffer) => errs.push(d));
    kid.stdin.on("error", () => {});
    kid.on("error", (e) => {
      clearTimeout(bomb);
      resolve({ out: "", err: String(e), code: 255 });
    });
    kid.on("close", (code) => {
      clearTimeout(bomb);
      resolve({ out: Buffer.concat(outs).toString(),
        err: Buffer.concat(errs).toString(), code: code ?? 1 });
    });
    kid.stdin.end(input);
  });
}

export function node_name(node: number): string {
  return "cluster-" + node.toString(16).padStart(2, "0");
}

// A session the transport dropped (or that timed out in the banner
// exchange: the shared bastion path stalled, not the node) is retried
// twice; a node the bastion cannot reach (channel refused) fails at once.
export async function ssh(node: number, script: string,
  input?: Buffer | string, timeout?: number): Promise<Exec> {
  for (let hop = 0; ; hop += 1) {
    const got = HOC === ""
      ? await exec("ssh", [...SSH, node_name(node), script], input, timeout)
      : await hoc_ssh(node, script, input, timeout);
    if (hop >= 2 || got.code !== 255 || !DROP.test(got.err)) {
      return got;
    }
    await sleep(500 + Math.random() * 1500);
  }
}

// The hoc way sends each input up to the bastion once, and every session
// reads it there: 48 shards of one pack cost one upload, not 48.
async function hoc_ssh(node: number, script: string,
  input?: Buffer | string, timeout?: number): Promise<Exec> {
  const open = await hoc_open(node);
  if (open.code !== 0) {
    return open;
  }
  let from = "";
  if (input !== undefined) {
    const sum = crypto.createHash("sha1").update(input).digest("hex");
    const up = staged.get(sum) ?? exec("ssh", [...hoc_mux(node), ...HOC_HUB,
      "d=$HOME/.bend-gate; mkdir -p $d && { find $d -type f -mmin +60"
      + " -delete 2>/dev/null; cat > $d/" + sum + ".$$; } && mv $d/" + sum
      + ".$$ $d/" + sum], input);
    staged.set(sum, up);
    const got = await up;
    if (got.code !== 0) {
      if (staged.get(sum) === up) {
        staged.delete(sum);
      }
      return got;
    }
    from = " < $HOME/.bend-gate/" + sum;
  }
  return exec("ssh", [...hoc_mux(node), ...HOC_HUB, "sudo -n"
    + " /usr/local/bin/hoc-ssh " + String(node) + " '"
    + script.replaceAll("'", "'\\''") + "'" + from], undefined, timeout);
}

// The bastion opens at most 10 sessions on one connection, so the hoc way
// gives each 10 nodes in a row their own mux: a node runs one session at a
// time, so no mux carries more than 10, whatever gates run side by side.
function hoc_mux(node: number): string[] {
  return MUX.map((o) => o.startsWith("ControlPath=") ? "ControlPath=/tmp/"
    + "bend-hoc-nodes-" + String(Math.floor(node / 10)) : o);
}

// One session opens a node's mux before the rest use it: sessions that
// start together on a closed mux would each dial the bastion on its own,
// and past its startup limit it drops them.
function hoc_open(node: number): Promise<Exec> {
  const mux = Math.floor(node / 10);
  const got: Promise<Exec> = opened.get(mux) ?? exec("ssh",
    [...hoc_mux(node), ...HOC_HUB, "true"]).then((g) => {
    if (g.code !== 0 && opened.get(mux) === got) {
      opened.delete(mux);
    }
    return g;
  });
  opened.set(mux, got);
  return got;
}

function sleep(ms: number): Promise<void> {
  return new Promise((wake) => setTimeout(wake, ms));
}

// Node
// ====

// Locks a slot of 48 minis, after one session to the bastion opens the
// mux the rest share. A bastion that refuses that session (the agent lost
// id_rsa at a reboot, the host is down) fails the gate at once with its
// reason; without this every node's first session dies as "Connection
// closed by UNKNOWN" and the pool runs dry. A dead node is found by its
// first job (node_pool requeues the job and drops the node), not by a
// probe.
export async function node_lock(): Promise<number[]> {
  const nodes = slot_lock();
  const gots = await Promise.all(HOC === ""
    ? [exec("ssh", [...MUX, "cluster", "true"])]
    : nodes.map(hoc_open));
  const got = gots.find((g) => g.code !== 0);
  if (got !== undefined) {
    throw new Error("the bastion refused the mux session (is id_rsa in the"
      + " agent? ssh-add ~/.ssh/id_rsa): "
      + (got.err.trim().split("\n").pop() ?? ""));
  }
  return nodes;
}

// Packs what a node needs to build the programs under dir, as one tar.gz:
// the compiler, Base and the effect kit (not docs or pack)
// beside dir, under its own name. A gate packs once and sends the same
// bytes to every node: packing takes 0.35 s of the main thread, so a pack
// per shard held the last of 48 launches back by 17 s.
export function pack(dir: string): Buffer {
  const tmp = fs.mkdtempSync("/tmp/bend-pack-");
  fs.cpSync(path.join(ROOT, "bend2"), path.join(tmp, "bend2"), {
    recursive: true, filter: (p) => fs.statSync(p).isDirectory()
      ? !/\/(docs|pack)$/.test(p) : /\.(ts|bend|c|js)$/.test(p) });
  fs.cpSync(dir, path.join(tmp, path.basename(dir)), { recursive: true });
  const tar = child.spawnSync("tar", ["-czf", "-", "-C", tmp, "."],
    { maxBuffer: 1 << 28 });
  fs.rmSync(tmp, { recursive: true, force: true });
  return tar.stdout;
}

function slot_lock(): number[] {
  fs.mkdirSync(SLOTS.dir, { recursive: true });
  for (let slot = 0; slot < SLOTS.count; slot += 1) {
    const dir = path.join(SLOTS.dir, "slot" + String(slot));
    const file = path.join(dir, "lock.json");
    try {
      const lock = JSON.parse(fs.readFileSync(file, "utf8")) as
        { pid: number; time: number };
      let dead = Date.now() - lock.time > STALE;
      try {
        process.kill(lock.pid, 0);
      } catch {
        dead = true;
      }
      if (dead) {
        fs.rmSync(dir, { recursive: true, force: true });
      }
    } catch {}
    try {
      fs.mkdirSync(dir);
      fs.writeFileSync(file, JSON.stringify({ pid: process.pid,
        time: Date.now() }));
      held = dir;
      process.on("exit", node_free);
      const first = SLOTS.base + SLOTS.size * slot;
      return Array.from({ length: SLOTS.size }, (_, i) => first + i);
    } catch {}
  }
  process.stderr.write("cluster out of capacity: every slot is busy\n");
  process.exit(2);
}

export function node_free(): void {
  if (held !== "") {
    fs.rmSync(held, { recursive: true, force: true });
    held = "";
  }
}

// A job that throws "node" (its cause: the session's last error line) goes
// back to the queue and its node leaves the pool; a node with nothing to do
// waits while others still run, since their jobs may come back.
export async function node_pool(nodes: number[], jobs: Job[]): Promise<void> {
  const queue = [...jobs];
  let busy = 0;
  let why = "";
  await Promise.all(nodes.map(async (node) => {
    for (;;) {
      const job = queue.shift();
      if (job === undefined) {
        if (busy === 0) {
          return;
        }
        await sleep(200);
        continue;
      }
      busy += 1;
      try {
        await job(node);
        busy -= 1;
      } catch (e) {
        busy -= 1;
        queue.unshift(job);
        if (!(e instanceof Error && e.message === "node")) {
          throw e;
        }
        why = String(e.cause ?? "").trim().split("\n").pop() ?? "";
        return;
      }
    }
  }));
  if (queue.length > 0) {
    throw new Error("the cluster ran out of live nodes: " + why);
  }
}

// Verdict
// =======

export function verdict(pass: number, total: number): never {
  console.log("PASS: " + String(pass) + " / " + String(total));
  process.exit(pass === total ? 0 : 1);
}
