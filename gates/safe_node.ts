// safe.ts's node side: runs `bend <f> --verdict` on each file named on
// stdin, PAR at a time, each capped at CAP s, and prints one JSON array
// of Got. On a mismatch (bend2 checks, the kernel does not), diagnostic
// says why: the defs `-o` leaves out of scope, or else the kernel's error
// on the translation, with the failing call if live. ms times the verdict
// only; a failed diagnostic never changes it, and an infrastructure error
// is never a proof rejection.
import * as child from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

export type Got = {
  f: string; code: number | null; ms: number; out: string; timeout: boolean;
  signal?: string; error?: string;
  diagnostic?: string; diagnostic_error?: string; diagnostic_ms?: number;
};

// Classification consumes explicit process outcomes, never elapsed diagnostics.
export function judge(g: Got): [string, string] {
  const out = g.out.trim();
  if (g.timeout) {
    return ["t", "timeout"];
  }
  if (g.error || g.code === null || g.code < 0) {
    const why = (g.error ?? out).replace(/\s+/g, " ").slice(0, 300);
    return ["e", "infrastructure: " + why];
  }
  if (g.code === 0 && out === "ALL PROOFS CHECK") {
    return [" ", "agree"];
  }
  if (/^Error: \d+ defs? rel(y|ies) on unsafe or foreign code/m.test(out)) {
    return ["u", "unsafe: " + [...out.matchAll(/^- (\S+)$/gm)].map((m) => m[1]).slice(0, 3).join(" ")];
  }
  if (!out.includes("Sorry - ")) {
    return [" ", "bend2 rejects: " + out.split("\n").slice(1, 3).join(" ").slice(0, 80)];
  }
  const tt = g.diagnostic ?? out;
  if (tt.startsWith("out of scope")) {
    return ["-", "out of scope: " + [...tt.matchAll(/^- \S+: (.*)$/gm)].map((m) => m[1]).slice(0, 2).join(" | ").slice(0, 160)];
  }
  // the kernel's first line is SOME PROOFS FAIL; why starts at In <def>:
  return ["!", tt.split("\n").slice(1, 3).join(" | ").slice(0, 300)];
}

async function run(bin: string, args: string[], cap: number): Promise<Omit<Got, "f">> {
  const g: Omit<Got, "f"> = { code: null, ms: 0, out: "", timeout: false };
  const t0 = Date.now();
  const { promise, resolve } = Promise.withResolvers<void>();
  const kid = child.spawn(bin, args, { env: process.env, detached: true });
  kid.stdout.on("data", (d) => { g.out += d; });
  kid.stderr.on("data", (d) => { g.out += d; });
  kid.on("error", (e) => { g.error = e.message; });
  const bomb = setTimeout(() => {
    if (kid.pid !== undefined) {
      try { process.kill(-kid.pid, "SIGKILL"); g.timeout = true; }
      catch (e) { g.error = String(e); }
    }
  }, cap);
  kid.on("close", (code, signal) => {
    clearTimeout(bomb);
    g.code = code;
    g.ms = Date.now() - t0;
    if (signal) {
      g.signal = signal;
      if (!g.timeout) {
        g.error = "child terminated by " + signal;
      }
    }
    resolve();
  });
  await promise;
  return g;
}

async function one(f: string, cap: number): Promise<Got> {
  const g: Got = { f, ...await run(process.execPath, ["bend2/main.ts", f, "--verdict"], cap) };
  if (g.out.includes("Error: BendTT infrastructure:")) {
    g.error = g.out.trim();
  }
  if (!g.timeout && !g.error && g.out.includes("Sorry - ")) {
    const t1 = Date.now();
    let dir: string | undefined;
    try {
      dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-safe-"));
      const tt = path.join(dir, "diagnostic.bendtt");
      const emitted = await run(process.execPath, ["bend2/main.ts", f, "-o", tt], 20000);
      if (emitted.error || emitted.timeout || emitted.code !== 0) {
        throw new Error("elaboration diagnostic: " + (emitted.error ?? (emitted.out.trim()
          || "exit " + emitted.code + ", signal " + emitted.signal)));
      }
      if (emitted.out.trim() !== "") {
        g.diagnostic = "out of scope\n" + emitted.out.trim();
      } else {
        const kernel = await run(process.env.BENDTT ?? "", [tt], 20000);
        if (kernel.error || kernel.timeout || kernel.code === null
          || (kernel.code !== 0 && !kernel.out.startsWith("SOME PROOFS FAIL"))) {
          throw new Error("kernel diagnostic: " + (kernel.error ?? (kernel.out.trim()
            || "exit " + kernel.code + ", signal " + kernel.signal)));
        }
        g.diagnostic = kernel.out.trim();
        const m = /^In (\S+):\naffine live code/m.exec(kernel.out);
        if (m !== null) {
          const detail = await run(process.execPath, ["gates/safe_diag.ts", tt, m[1]], 20000);
          if (detail.error || detail.timeout || detail.code !== 0) {
            throw new Error("descent diagnostic: " + (detail.error ?? (detail.out.trim()
              || "exit " + detail.code + ", signal " + detail.signal)));
          }
          g.diagnostic += "\n" + detail.out.trim().slice(0, 300);
        }
      }
    } catch (e) {
      g.diagnostic_error = String(e);
    } finally {
      if (dir !== undefined) {
        fs.rmSync(dir, { recursive: true, force: true });
      }
      g.diagnostic_ms = Date.now() - t1;
    }
  }
  return g;
}

// The required suite is local and builds in a fresh HOME, never trusting $BENDTT.
// The larger stdin survey keeps its descriptive contract.
async function required(): Promise<void> {
  const home = process.env.HOME;
  const override = process.env.BENDTT;
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-verdict-gate-"));
  process.env.HOME = dir;
  delete process.env.BENDTT;
  const expect = (g: Got, want: string): void => {
    const [mark, why] = judge(g);
    const got = g.diagnostic_error ? "infrastructure"
      : mark === "t" ? "timeout"
      : mark === "e" || g.signal ? "infrastructure"
      : g.code === 0 && why === "agree" ? "accepted"
      : g.code === 1 && mark === "-" ? "out of scope"
      : g.code === 1 && g.out.startsWith("SOME PROOFS FAIL") && mark === " "
        ? g.f.endsWith(".bendtt") ? "kernel rejection" : "source rejection"
      : g.code === 1 && mark === "!" && g.diagnostic?.startsWith("SOME PROOFS FAIL")
        ? "kernel rejection" : "infrastructure";
    if (got !== want) {
      throw new Error(g.f + ": expected " + want + ", got " + got + "\n"
        + (g.diagnostic_error ?? g.error ?? g.diagnostic ?? g.out));
    }
    if (got === "out of scope") {
      const omitted = [...(g.diagnostic ?? "").matchAll(/^- (\S+):/gm)].map(m => m[1]);
      if (omitted.length !== 1 || omitted[0] !== "reflexive") {
        throw new Error(g.f + ": unexpected omitted definitions: " + omitted.join(", "));
      }
    }
    console.log(g.f + ": " + got + " (" + g.ms + " ms)");
  };
  try {
    const first = "tests/proof/cong_lambda_argument.bend";
    expect(await one(first, 300000), "accepted");
    const cache = path.join(dir, ".bend", "bendtt");
    const builds = fs.readdirSync(cache);
    if (builds.length !== 1) {
      throw new Error("BendTT infrastructure: expected one fresh source-keyed kernel build");
    }
    const kernel = path.join(cache, builds[0], "bendtt");
    process.env.BENDTT = kernel;
    for (const [f, want] of [
      ["tests/proof/group_dead_forward.bend", "accepted"],
      ["tests/proof/rewrite_motive_evidence.bend", "accepted"],
      ["tests/proof/model_no_base.bend", "out of scope"],
    ]) {
      expect(await one(f, 30000), want);
    }
    const rejected = path.join(dir, "rejected.bend");
    fs.writeFileSync(rejected, "import Base\nlaw main:\n  {0 == 1 : U32}\ndef main(): {==}\n");
    expect(await one(rejected, 30000), "source rejection");
    for (const [name, text, want] of [
      ["unit", "unit : <()> = ()\n", "accepted"],
      ["empty", "empty : <> = ()\n", "kernel rejection"],
    ]) {
      const f = path.join(dir, name + ".bendtt");
      fs.writeFileSync(f, text);
      expect({ f, ...await run(kernel, [f], 30000) }, want);
    }
    const broken = path.join(dir, "broken-kernel");
    fs.writeFileSync(broken, "#!/bin/sh\nexit 23\n", { mode: 0o700 });
    process.env.BENDTT = broken;
    expect(await one(first, 30000), "infrastructure");
    expect({ f: "expired deadline", ...await run(process.execPath,
      ["-e", "setInterval(() => {}, 1000)"], 1) }, "timeout");
    console.log("PASS required kernel verdicts");
  } finally {
    if (home === undefined) { delete process.env.HOME; } else { process.env.HOME = home; }
    if (override === undefined) { delete process.env.BENDTT; } else { process.env.BENDTT = override; }
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

if (import.meta.main) {
  if (process.argv[2] === "--required") {
    await required();
    process.exit(0);
  }
  const files = fs.readFileSync(0, "utf8").split("\n").filter((l) => l !== "");
  const cap = Number(process.env.CAP ?? 30) * 1000;
  const res: Got[] = [];
  let next = 0;
  await Promise.all(Array.from({ length: Number(process.env.PAR ?? 8) }, async () => {
    while (next < files.length) {
      res.push(await one(files[next++], cap));
    }
  }));
  process.stdout.write(JSON.stringify(res));
}
