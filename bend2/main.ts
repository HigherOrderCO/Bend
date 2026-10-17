#!/usr/bin/env bun
// Run, this file is the CLI. Imported, it is the loader that makes `import
// Game from "./x.bend"` work: a bun plugin (preload it in bunfig.toml, list
// it under [serve.static] plugins, or hand it to Bun.build) and a node hook
// (node --import). A .bend module exports every filled, non-base, non-IO
// def, wrapped so a JS caller passes the live arguments, in one call or
// curried, and gets a plain value back: a constructor is {$: "Name", field:
// value, ...}, a closure is a function, Nat is BigInt, Bool, String and U32
// are native. A page bundles through Bun.build with the loader on, since
// the bun build CLI takes no plugins.

import * as child from "node:child_process";
import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as mod from "node:module";
import * as os from "node:os";
import * as path from "node:path";
import * as url from "node:url";
import * as thr from "node:worker_threads";

import type { BunPlugin } from "bun";

import * as Bend from "./bend.ts";
import * as Comp from "./comp.ts";

// Main
// ====

// Constants
// =========

const VERSION = "2.0.25";

const HELP = `Bend ${VERSION}: check, run, build and publish Bend programs.

usage:
  bend <file.bend> [args]       check the file, then run main with args
  bend <file.bend> -o <out>     build a binary; <out>.c emits C, <out>.js JS
  bend <file.bend> --check-only check the file and its imports; run nothing
  bend <file.bend> --publish    publish the file and its imports to the hub
  bend <page.html> -o <dir>     bundle a page that imports .bend files
  bend base [--types|<name>]    print Base, its types, or a name and subnames
  bend guide                    print the Bend guide
  bend update                   install the latest bend (curl | sh, shown first)
  bend version                  print the version

Read the guide (\`bend guide\`) before writing Bend code.
`;

const BASE = Bend.BASE_BEND;

const GUIDE = path.join(Bend.BEND_DIR, "..", "guide");

const ORIGIN = process.env.BEND_ORIGIN ?? "https://bend-lang.com";

// the daily version check's cache: when it last asked, and the answer
const CHECK = path.join(os.homedir(), ".bend", "check.json");

const DAY = 86400000;

// A package's proof of work is a nonce whose sha256(hash + " " + nonce)
// opens (its top 53 bits) with a number under 2^53 / work, where work is
// POW hashes (two seconds of an M4 Max's sixteen cores) per 256 KiB of
// package, and no less. Every core mines; the hub checks it with one hash.
const POW = 140000000;

const POW_JS = `
const crypto = require("node:crypto");
const { parentPort, workerData: { pre, lim, from, step } }
  = require("node:worker_threads");
for (let n = from;; n += step) {
  const h = crypto.hash("sha256", pre + n, "buffer");
  if ((h[0] * 16777216 + (h[1] << 16) + (h[2] << 8) + h[3]) * 2097152
    + ((h[4] * 16777216 + (h[5] << 16) + (h[6] << 8) + h[7]) >>> 11) < lim) {
    parentPort.postMessage(n);
    break;
  }
}`;

const PLUGIN: BunPlugin = {
  name: "bend",
  setup(build) {
    build.onLoad({ filter: /\.bend$/ }, async (args) =>
      ({ contents: await load_js(args.path), loader: "js" }));
  },
};

// CLI
// ===

// cli runs the command, then (not after --version or update) the daily
// version check, so the check never delays the command's own work.
async function cli(): Promise<void> {
  const args = process.argv.slice(2);
  if (args[0] === "version" && args.length === 1) {
    return cli_say(1, "bend " + VERSION + "\n");
  }
  if (args[0] === "update" && args.length === 1) {
    return cli_update();
  }
  if (args[0] === "guide" && args.length <= 2) {
    cli_guide(args[1] ?? "guide");
  } else if (args[0] === "base" && args.length <= 2) {
    cli_base(args[1]);
  } else {
    await cli_file(args);
  }
  await check();
}

// cli_guide prints guide/<NAME>.md: the guide, or a named extra.
function cli_guide(name: string): void {
  const file = path.join(GUIDE, name.toUpperCase() + ".md");
  if (!fs.existsSync(file)) {
    cli_fail("no guide named " + name);
  }
  cli_say(1, fs.readFileSync(file, "utf8"));
}

// cli_update runs the installer again: the one way bend changes. The
// command prints first, so the user can run it alone.
function cli_update(): void {
  const cmd = "curl -fsSL " + ORIGIN + "/install.sh | sh";
  cli_say(2, cmd + "\n");
  process.exitCode = child.spawnSync("sh", ["-c", cmd],
    { stdio: "inherit" }).status ?? 1;
}

// check is the whole telemetry: once a day, a GET of /check?v=&os=&arch=
// (nothing else: no id, no command, no timing) whose answer {ver, notice}
// is cached in CHECK; a cached ver newer than this one prints one line on
// stderr, and the notice. The cache is stamped before the request, so a
// day has one request whatever happens to it; BEND_NO_TELEMETRY=1 skips
// everything; the check never fails the command.
async function check(): Promise<void> {
  if (process.env.BEND_NO_TELEMETRY) {
    return;
  }
  let last = { t: 0, ver: VERSION, notice: "" };
  try {
    last = { ...last, ...JSON.parse(fs.readFileSync(CHECK, "utf8")) };
  } catch {}
  try {
    if (Date.now() - last.t > DAY) {
      last.t = Date.now();
      fs.mkdirSync(path.dirname(CHECK), { recursive: true });
      fs.writeFileSync(CHECK, JSON.stringify(last) + "\n");
      const res = await fetch(ORIGIN + "/check?v=" + VERSION + "&os="
        + process.platform + "&arch=" + process.arch, { headers: { "User-Agent":
        "bend/" + VERSION }, signal: AbortSignal.timeout(3000) });
      const got = await res.json() as { ver?: unknown; notice?: unknown };
      last.ver = typeof got.ver === "string" ? got.ver : VERSION;
      last.notice = typeof got.notice === "string" ? got.notice : "";
      fs.writeFileSync(CHECK, JSON.stringify(last) + "\n");
    }
  } catch {}
  if (ver_newer(last.ver)) {
    cli_say(2, "bend " + last.ver + " is available: run bend update\n"
      + (last.notice === "" ? "" : last.notice.replace(/[\x00-\x1f\x7f]/g, "")
      .slice(0, 200) + "\n"));
  }
}

function ver_newer(ver: string): boolean {
  const a = ver.split(".").map(Number);
  const b = VERSION.split(".").map(Number);
  return a.length === 3 && a.every(Number.isInteger)
    && (a[0] - b[0] || a[1] - b[1] || a[2] - b[2]) > 0;
}

// cli_file checks, runs, builds, publishes or bundles a file
async function cli_file(args: string[]): Promise<void> {
  const outs: string[] = [];
  const argv: string[] = [];
  let file: string | undefined;
  let only = false;
  let checkup = false;
  let publish = false;
  for (let i = 0; i < args.length; i += 1) {
    const a = args[i];
    if (a === "--help" || a === "-h") {
      return cli_say(1, HELP);
    } else if (a === "--check-only") {
      only = true;
    } else if (a === "--checkup") {
      checkup = true;
    } else if (a === "--publish") {
      publish = true;
    } else if (a === "-o") {
      i += 1;
      outs.push(args[i] ?? cli_fail("-o needs an output file"));
    } else if (a === "--") {
      argv.push(...args.splice(i + 1));
    } else if (a.startsWith("-")) {
      cli_fail("unknown option " + a);
    } else if (file !== undefined) {
      argv.push(a);
    } else {
      file = a;
    }
  }
  if (file === undefined) {
    cli_say(1, HELP);
    process.exit(1);
  }
  if (file.endsWith(".html")) {
    if (outs.length !== 1 || only || checkup || publish) {
      cli_fail("a page bundles with -o <dir>");
    }
    return cli_bundle(file, outs[0]);
  }
  if (publish && (outs.length !== 0 || only || checkup)) {
    cli_fail("--publish takes no other option");
  }
  if (only && (outs.length !== 0 || checkup)) {
    cli_fail("--check-only takes no other option");
  }
  if (argv.length !== 0 && (outs.length !== 0 || only || checkup || publish)) {
    cli_fail("arguments go to a run: bend <file.bend> [args]");
  }
  if (checkup && outs.length !== 0) {
    cli_fail("--checkup takes no -o: a binary holds one main, so build each"
      + " import alone");
  }
  try {
    if (publish) {
      return await cli_publish(file);
    }
    if (checkup) {
      return await cli_checkup(file);
    }
    if (only) {
      return cli_report(...await book_read(file), 1);
    }
    const seen = new Map<string, string | null>();
    const [book, n0] = await book_read(file, undefined, seen);
    if (outs.length !== 0 || book_main(book) !== null) {
      cli_report(book, n0, 2);
    }
    if (outs.length === 0) {
      process.exitCode = book_run(book, n0, argv);
      return;
    }
    const ins = new Set([...seen.keys(), ...Object.values(book.tlds).flatMap((t) =>
      t.$ === "Def" && t.i !== undefined ? t.i.map(path_real) : [])]);
    for (const out of outs) {
      const at = path_real(out);
      if (ins.has(at) || (fs.existsSync(at) && fs.statSync(at).isDirectory())) {
        cli_fail("-o " + out + " is a file the program reads, or a directory");
      }
      cli_emit(book, out);
    }
  } catch (e) {
    cli_say(2, book_err(e) + "\n");
    process.exitCode = 1;
  }
}

// cli_checkup checks and runs each import of the file alone (Base read
// once, seeded into every module that imports it); one that fails fails it.
async function cli_checkup(file: string): Promise<void> {
  const [base] = await book_read(BASE);
  let bad = false;
  for (const raw of fs.readFileSync(file, "utf8").split("\n")) {
    const m = /^import\s+(\S+)\s+as\s+[A-Za-z_][A-Za-z0-9_]*\s*$/
      .exec(raw.trim());
    if (m === null) {
      continue;
    }
    const at = m[1].startsWith("/") ? m[1]
      : path.join(path.dirname(file), m[1]);
    cli_say(1, "--- " + m[1] + " ---\n");
    let code = 1;
    try {
      const own = /^import Base$/m.test(fs.readFileSync(at, "utf8"));
      code = book_run(...await book_read(at, own ? base : undefined), []);
    } catch (e) {
      cli_say(2, book_err(e) + "\n");
    }
    if (code !== 0) {
      cli_say(1, "exit " + String(code) + "\n");
      bad = true;
    }
  }
  if (bad) {
    process.exit(1);
  }
}

function path_real(p: string): string {
  return fs.existsSync(p) ? fs.realpathSync(p) : path.resolve(p);
}

function cli_emit(book: Bend.Book, out: string): void {
  if (/\.c?js$/.test(out)) {
    fs.writeFileSync(out, Comp.js_book(book));
  } else if (out.endsWith(".c")) {
    fs.writeFileSync(out, Comp.compile_book(book));
  } else {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-"));
    const c   = path.join(dir, path.basename(out) + ".c");
    fs.writeFileSync(c, Comp.compile_book(book));
    try {
      cli_build(out, c);
    } finally {
      fs.rmSync(dir, { recursive: true, force: true });
    }
  }
}

// cc_find is the first of $CC, clang and every clang-NN on PATH (newest
// first) that is new enough: clang 14 for a CPU build, and for a GPU build
// clang 19 (Apple clang 17, which ships LLVM 19), whose #embed
// carries the device program.
function cc_find(gpu: boolean): string {
  function dir_list(dir: string): string[] {
    try {
      return fs.readdirSync(dir);
    } catch {
      return [];
    }
  }
  const dirs = (process.env.PATH ?? "").split(path.delimiter);
  const nums = [...new Set(dirs.flatMap(dir_list).filter((f) =>
    /^clang-\d+$/.test(f)))].sort((a, b) => Number(b.slice(6)) - Number(a.slice(6)));
  const olds: string[] = [];
  const ccs  = [...(process.env.CC ? [process.env.CC] : []), "clang", ...nums];
  for (const cc of ccs) {
    const out = child.spawnSync(cc, ["--version"], { encoding: "utf8" }).stdout ?? "";
    const m   = /^(Apple )?(?:\w+ )?clang version (\d+)/m.exec(out);
    const need = gpu ? (m?.[1] === undefined ? 19 : 17) : 14;
    if (m !== null && Number(m[2]) >= need) {
      return cc;
    }
    olds.push(m !== null ? "clang " + m[2] + " as " + cc
      : out ? cc + ", which is not clang" : "no " + cc);
  }
  throw "Error: bend needs clang " + (gpu ? "19 (Apple clang 17)" : "14")
    + " or newer to build " + (gpu ? "a GPU program" : "binaries") + " (found "
    + olds.join(", ") + "); on Debian/Ubuntu: curl -fsSL"
    + " https://apt.llvm.org/llvm.sh | sudo bash -s 19; on macOS: xcode-select"
    + " --install";
}

// cli_build builds the C file at `file` into the binary `bin`. A `!` program
// builds with the GPU lane and writes its GPU program too (on Linux only with
// CUDA at $CUDA_HOME, else at /usr/local/cuda, its libraries in lib64 or, as
// nix lays them, lib; else the ! runs on the cores). On macOS a program with
// a framework (#import: a window, audio) builds as Objective-C; on Linux it
// links the X11 and ALSA libraries it includes.
function cli_build(bin: string, file: string): void {
  const c     = fs.readFileSync(file, "utf8");
  const mac   = process.platform === "darwin";
  const cuda  = process.env.CUDA_HOME || "/usr/local/cuda";
  const bangs = !/^#define BANGS\s+0$/m.test(c)
    && (mac || fs.existsSync(cuda + "/include/nvrtc.h"));
  const cc    = cc_find(bangs);
  const objc  = mac && (bangs || /^#import /m.test(c))
    ? ["-x", "objective-c", "-fobjc-arc", "-fmodules"] : [];
  const libs  = [["X11", "X11"], ["alsa", "asound"]].flatMap(([h, l]) =>
    !mac && c.includes("#include <" + h + "/") ? ["-l" + l] : []);
  const cpu = [...objc, "-std=c11", "-O3", file, "-lpthread", "-lm",
    ...libs, "-o", path.resolve(bin)];
  const gpu = mac ? ["-DBEND_METAL=1", ...cpu]
    : ["-DBEND_CUDA=1", "-I" + cuda + "/include", "-L" + cuda + "/lib64",
      "-L" + cuda + "/lib", ...cpu, "-lcuda", "-lnvrtc"];
  const steps: [string, string[]][] = bangs
    ? [[cc, gpu], [path.resolve(bin), ["--gpu-build"]]] : [[cc, cpu]];
  for (const [cmd, args] of steps) {
    if (child.spawnSync(cmd, args, { stdio: "inherit" }).status !== 0) {
      throw "Error: " + path.basename(cmd) + " failed to build " + bin;
    }
  }
}

// cli_base prints the base library; with --types, its type declarations
// (every `type`, and every law whose result is a kind); with a name, the
// blocks declaring it or a name under it (its law, its def, its @unsafe).
function cli_base(what?: string): void {
  const src = fs.readFileSync(BASE, "utf8");
  if (what === undefined) {
    return cli_say(1, src);
  }
  const want: string[] = [];
  for (const text of src.split(/\n(?=type |law |def |@)/)) {
    const m = /^(type|law|def) ([^\s(<:]+)/m.exec(text);
    if (m === null) {
      continue;
    }
    const s = text.replace(/(\n(#[^\n]*)?)+$/, "");
    const last = s.slice(s.lastIndexOf("\n") + 1);
    const ok = what === "--types"
      ? m[1] === "type" || (m[1] === "law" && /^ *(Type|Data|Kind\(.*\))$/.test(last))
      : m[2] === what || m[2].startsWith(what + ".");
    if (ok) {
      want.push(s);
    }
  }
  if (want.length === 0) {
    cli_fail("Base has no " + what);
  }
  cli_say(1, want.join("\n\n") + "\n");
}

async function cli_bundle(page: string, dir: string): Promise<void> {
  const out = await Bun.build({
    entrypoints: [page],
    outdir: dir,
    target: "browser",
    minify: true,
    plugins: [PLUGIN],
  });
  for (const a of out.outputs) {
    cli_say(1, a.path + " (" + (a.size / 1024).toFixed(1) + "kb)\n");
  }
}

// Publish
// =======

// cli_publish checks the file, then posts what the loader read (no TODO
// left) to the hub with its proof of work, and prints the import line.
async function cli_publish(file: string): Promise<void> {
  const seen = new Map<string, string | null>();
  const [book, n0] = await book_read(file, undefined, seen);
  cli_report(book, n0, 2);
  const files = pkg_files(file, book, seen);
  const entry = Object.keys(files)[0];
  const name  = path.basename(entry, ".bend");
  if (name === "") {
    cli_fail("a published file needs a name before .bend");
  }
  const paths = Object.keys(files).sort();
  const bytes = paths.reduce((n, p) => n + Buffer.byteLength(files[p]), 0);
  const hash  = "0x" + sha256(paths.map((p) => sha256(files[p]) + " " + p
    + "\n").join("")).slice(0, 32);
  cli_say(2, "publishing " + String(paths.length) + " files, "
    + String(bytes) + " bytes, as " + hash + " (mining its proof of work)\n");
  const nonce = await pow_mine(hash, bytes);
  const res = await fetch(Bend.BEND_HUB, { method: "POST",
    body: JSON.stringify({ files, nonce }) });
  const got = (await res.text()).trim();
  if (!res.ok || got !== hash) {
    throw "Error: " + Bend.BEND_HUB + " answered: " + got;
  }
  cli_say(1, hash + "\nimport " + hash + "/" + entry + " as "
    + name[0].toUpperCase() + name.slice(1) + "\n");
}

// pkg_files is the package the loader read for this file, the entry first:
// every .bend file at its namespace (the entry at its name), every foreign
// .c or .js file at its path from the entry's directory; base and the
// store's packages stay out. A path that climbs above the entry's directory
// takes the entry's ancestor directories along, as many as the deepest climb.
function pkg_files(file: string, book: Bend.Book,
  seen: Map<string, string | null>): Record<string, string> {
  const dir  = file.slice(0, file.lastIndexOf("/") + 1);
  const raws = [...[...seen].flatMap(([real, ns]): [string, string][] =>
    real === BASE || ns === null || ns.startsWith("0x") ? []
      : [[ns === "" ? path.basename(file) : ns + ".bend", real]]),
  ...Object.entries(book.tlds).flatMap(([k, tld]): [string, string][] =>
    tld.$ !== "Def" || tld.i === undefined || tld.b === true
      || k.startsWith("0x") ? [] : tld.i.map((f) =>
      [f.startsWith(dir) ? f.slice(dir.length) : f, f]))];
  const ups = raws.map(([p]) => path.posix.normalize(p).split("/")
    .filter((s) => s === "..").length);
  const anc = fs.realpathSync(path.dirname(file)).split("/")
    .slice(-Math.max(0, ...ups) || Infinity);
  const files: Record<string, string> = {};
  for (const [raw, real] of raws) {
    const p = path.posix.join(...anc, raw);
    if (p.startsWith("/") || p.startsWith("..")) {
      throw "Error: " + real + " cannot be published (an absolute import,"
        + " or a climb above the file system)";
    }
    files[p] = fs.readFileSync(real, "utf8");
  }
  return files;
}

function sha256(text: string): string {
  return crypto.createHash("sha256").update(text).digest("hex");
}

async function pow_mine(hash: string, bytes: number): Promise<number> {
  const step = os.availableParallelism();
  const lim  = 2 ** 53 / (POW * Math.max(1, bytes / 262144));
  const ws   = Array.from({ length: step }, (_, k) => new thr.Worker(POW_JS,
    { eval: true, workerData: { pre: hash + " ", lim, from: k, step } }));
  const n = await new Promise<number>((res) =>
    ws.forEach((w) => w.on("message", res)));
  ws.forEach((w) => w.terminate());
  return n;
}

// Report
// ======

// cli_report prints the verdict of a check on stdout, or a note before a
// run, an emit or a publish on stderr (silent then when nothing relies on
// a promise): the file's own claims (book.order from n0, the loader's
// mark) that are @unsafe or foreign, or whose type, body or constructor
// fields name a def that relies on one. A foreign def is a promise like
// @unsafe is: the checker reads its type, never its code. If the book
// holds one, a walk from the claims collects who names whom, then the
// promises flood back along those edges.
function cli_report(book: Bend.Book, n0: number, fd: number): void {
  const own  = [...new Set(book.order.slice(n0))];
  const bad  = new Set(Object.keys(book.tlds).filter((k) => {
    const t = book.tlds[k] as Bend.Def;
    return t.u === true || (t.i !== undefined && t.b !== true);
  }));
  const uses: Record<string, string[]> = Object.create(null);
  const seen = new Set<string>();
  const queued = new Set<string>(bad.size === 0 ? [] : own);
  for (const q = bad.size === 0 ? [] : own.slice(); q.length > 0;) {
    const k = q.pop() as string;
    const t = book.tlds[k];
    if (t !== undefined && !seen.has(k)) {
      seen.add(k);
      const rs = new Set<string>();
      for (const c of t.$ === "ADT" ? t.c : [t]) {
        term_refs_higher(c.T, rs);
      }
      term_refs(t.$ === "Def" ? t.e : undefined, rs);
      for (const r of rs) {
        (uses[r] ??= []).push(k);
        if (!seen.has(r) && !queued.has(r)) {
          queued.add(r);
          q.push(r);
        }
      }
    }
  }
  for (const k of bad) {
    uses[k]?.forEach((j) => bad.add(j));
  }
  const list = own.filter((k) => bad.has(k));
  if (list.length > 0) {
    cli_say(fd, `All terms check, but ${list.length} def${list.length === 1
      ? " relies" : "s rely"} on unsafe or foreign code:\n`
      + list.map((k) => "- " + k + "\n").join(""));
  } else if (fd === 1) {
    cli_say(1, "All terms check.\n");
  }
}

// term_refs_higher walks HTerm directly and opens its binder closures with
// the same fresh variables used by term_lower. It collects names without
// constructing a lowered copy of the full type tree.
function term_refs_higher(tm: Bend.HTerm, out: Set<string>, depth = 0): void {
  const t = Bend.term_force(tm);
  switch (t.$) {
    case "Var": {
      const v = t.v;
      if (v !== undefined && v.$ !== "PVar" && v.$ !== "PCtr") {
        term_refs_higher(v as Bend.HTerm, out, depth);
      }
      return;
    }
    case "Ref": {
      out.add(t.k);
      return;
    }
    case "Sub": {
      const v = t.v;
      if (v.$ !== "PVar" && v.$ !== "PCtr") {
        term_refs_higher(v as Bend.HTerm, out, depth);
      }
      term_refs_higher(t.f, out, depth);
      return;
    }
    case "Let": {
      for (const v of t.v) term_refs_higher(v, out, depth);
      const xs = t.k.map((k, j) => Bend.Var<Bend.HBody>(k, depth + j));
      term_refs_higher(t.f(xs), out, depth + t.k.length);
      return;
    }
    case "Typ": {
      term_refs_higher(t.g, out, depth);
      return;
    }
    case "Qnt":
    case "Qua":
    case "Lit":
    case "Efq":
    case "Rfl":
    case "Hol": {
      return;
    }
    case "Min": {
      term_refs_higher(t.a, out, depth);
      term_refs_higher(t.b, out, depth);
      return;
    }
    case "All": {
      term_refs_higher(t.A, out, depth);
      term_refs_higher(t.B(Bend.Var<Bend.HBody>(t.k, depth)), out, depth + 1);
      return;
    }
    case "Lam": {
      term_refs_higher(t.f(Bend.Var<Bend.HBody>(t.k, depth)), out, depth + 1);
      return;
    }
    case "App": {
      term_refs_higher(t.f, out, depth);
      term_refs_higher(t.x, out, depth);
      return;
    }
    case "ADT": {
      out.add(t.k);
      for (const x of t.x) term_refs_higher(x, out, depth);
      return;
    }
    case "Ctr": {
      for (const x of t.x) term_refs_higher(x, out, depth);
      return;
    }
    case "Mat": {
      term_refs_higher(t.h, out, depth);
      term_refs_higher(t.m, out, depth);
      return;
    }
    case "Eql": {
      term_refs_higher(t.a, out, depth);
      term_refs_higher(t.b, out, depth);
      term_refs_higher(t.T, out, depth);
      return;
    }
    case "Rwt": {
      term_refs_higher(t.e, out, depth);
      term_refs_higher(t.p, out, depth);
      term_refs_higher(t.f, out, depth);
      return;
    }
    case "Ann": {
      term_refs_higher(t.x, out, depth);
      term_refs_higher(t.T, out, depth);
      return;
    }
  }
}

// term_refs adds to out the names a term (a span skipped) refers to.
function term_refs(tm: unknown, out: Set<string>): void {
  if (typeof tm === "object" && tm !== null) {
    const { $, k } = tm as { $?: string; k?: string };
    if (($ === "Ref" || $ === "ADT") && k !== undefined) {
      out.add(k);
    }
    const o = tm as Record<string, unknown>;
    for (const f in o) {
      if (f !== "s" && Object.prototype.hasOwnProperty.call(o, f)) {
        term_refs(o[f], out);
      }
    }
  }
}

function cli_say(fd: number, text: string): void {
  try {
    fs.writeSync(fd, text);
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code !== "EPIPE") {
      throw e;
    }
    process.exit(0);
  }
}

function cli_fail(msg: string): never {
  cli_say(2, "bend: " + msg + " (see bend --help)\n");
  process.exit(1);
}

// Book
// ====

async function book_read(file: string, base?: Bend.Book,
  seen = new Map<string, string | null>()): Promise<[Bend.Book, number]> {
  const book = base === undefined ? Bend.book_nil() : book_seed(base);
  if (base !== undefined) {
    seen.set(BASE, "");
  }
  const n0 = await Bend.book_load(book, file, "", seen);
  const laws = path.join(path.dirname(file), "LAWS.bend");
  if (path.basename(file) === "PROOF.bend" && fs.existsSync(laws)
    && !seen.has(fs.realpathSync(laws))) {
    cli_fail("PROOF.bend must import ./LAWS.bend");
  }
  Bend.book_valid(book, base?.order.length ?? 0);
  Comp.book_owned(book, Comp.SYNTH);
  const hols = book.hols + book.open;
  if (hols > 0) {
    throw "Error: " + String(hols) + " TODO" + (hols === 1 ? "" : "s")
      + " found.\nThe code is incomplete, and not a valid proof yet.";
  }
  return [book, n0];
}

function book_seed(base: Bend.Book): Bend.Book {
  const book = Bend.book_nil();
  for (const k of Object.keys(base.tlds)) {
    book.tlds[k] = { ...base.tlds[k] };
  }
  Object.assign(book.ctrs, base.ctrs);
  for (const k of Object.keys(base.tmps)) {
    book.tmps[k] = { ...base.tmps[k] };
  }
  book.order.push(...base.order);
  return book;
}

function book_main(book: Bend.Book): Bend.Def | null {
  const main = book.tlds["main"];
  return main === undefined || main.$ !== "Def"
    || (main.v === null && main.i === undefined) ? null : main;
}

function book_run(book: Bend.Book, n0: number, argv: string[]): number {
  const main = book_main(book);
  if (main === null) {
    cli_report(book, n0, 1);
    return 0;
  }
  if (Comp.io_type(book) !== null) {
    return Comp.io_run(book, argv);
  }
  const snf = Bend.term_snf(book, main.v as Bend.HTerm);
  cli_say(1, Bend.term_show(Bend.term_lower(snf)) + "\n");
  return 0;
}

function book_err(e: unknown): string {
  const err = e as Bend.Err;
  if (e instanceof RangeError) {
    return "Error: the machine stack overflowed (a deep recursion, or a"
      + " literal too large to expand)";
  }
  return err?.$ === "Err" ? Bend.err_show(err) : String(e);
}

// Load
// ====

async function load_js(path: string): Promise<string> {
  try {
    const [book, n0] = await book_read(path);
    cli_report(book, n0, 2);
    const outs = [...new Set(book.order)].filter((k) => {
      const tld = book.tlds[k];
      return tld.$ === "Def" && tld.v !== null && tld.b !== true
        && tld.x === 0 && tld.i === undefined
        && Comp.io_base(book, tld.T) === null;
    });
    return Comp.js_lib(book, outs, outs);
  } catch (e) {
    throw new Error(book_err(e));
  }
}

export async function load(u: string, context: unknown,
  next: (u: string, context: unknown) => unknown): Promise<unknown> {
  return u.endsWith(".bend")
    ? { format: "module", shortCircuit: true,
      source: await load_js(url.fileURLToPath(u)) }
    : next(u, context);
}

export default PLUGIN;

if (import.meta.main) {
  if (typeof Bun === "undefined") {
    cli_say(2, "bend runs on Bun: curl -fsSL https://bend-lang.com/install.sh"
      + " | sh\n");
    process.exit(1);
  }
  await cli();
  process.exit();
} else if (typeof Bun !== "undefined") {
  Bun.plugin(PLUGIN);
} else if (thr.isMainThread) {
  mod.register(import.meta.url);
}
