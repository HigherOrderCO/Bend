#!/usr/bin/env bun
// Run, this file is the CLI. Imported, it is the loader that makes `import
// Game from "./x.bend"` work: a bun plugin (preload it in bunfig.toml, list
// it under [serve.static] plugins, or hand it to Bun.build) and a node hook
// (node --import). A .bend module exports every filled, non-base, non-IO
// def, wrapped so a JS caller passes the live arguments, in one call or
// curried, and gets a plain value back: a constructor is {$: "Name", field:
// value, ...}, a closure is a function, Nat is a number, Bool, String and U32
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
import * as Safe from "./safe.ts";

// Main
// ====

// Constants
// =========

const VERSION = "2.0.39";

// the commands, one row each: [usage, what it does]; bend guide stays last
const USAGE = [
  ["bend <file.bend> [args]", "check the file, then run main with args"],
  ["bend <file.bend> -o <out>", "build a binary, or C, JS, .mjs or BendTT by extension"],
  ["bend <file.bend> --check-only", "check the file and its imports; run nothing"],
  ["bend <file.bend> --verdict", "check it, then recheck it with the proven kernel"],
  ["bend <file.bend> --publish [<name>@<version>]", "publish the file and its imports; a name needs login"],
  ["bend link <name>@<version> 0x<hash>", "name a package already on the hub"],
  ["bend login", "log in to BendAI for --publish <name>@…"],
  ["bend <page.html> -o <dir>", "bundle a page that imports .bend files"],
  ["bend base [--types|<name>]", "print Base, its types, or a name and subnames"],
  ["bend update", "install the latest bend (curl | sh, shown first)"],
  ["bend version", "print the version"],
  ["bend guide", "print the Bend guide"],
];

const USE_W = Math.max(...USAGE.map(([use]) => use.length));

const HELP = `Bend ${VERSION}: check, run, build and publish Bend programs.

usage:
${USAGE.map(([use, say]) => `  ${use.padEnd(USE_W)}  ${say}`).join("\n")}

Read the guide (\`bend guide\`) before writing Bend code.
`;

const BASE = Bend.BASE_BEND;

const GUIDE = path.join(Bend.BEND_DIR, "..", "guide");

const ORIGIN = process.env.BEND_ORIGIN ?? "https://bend-lang.com";

// the BendAI key `bend login` wrote: {key, login}, mode 0600
const BENDAI = path.join(os.homedir(), ".bend", "bendai.json");

// where bend 2.0.35 and before wrote that key, while BendAI was named
// Bender; hub_check moves it to BENDAI
const BENDAI_OLD = path.join(os.homedir(), ".bend", "bender.json");

// the daily version check's cache: when it last asked, and the answer
const CHECK = path.join(os.homedir(), ".bend", "check.json");

const DAY = 86400000;

// the verdict on a book: PASS when every def outside Base is a valid proof
// (see cli_verdict), else FAIL and why
const PASS = "ALL PROOFS CHECK";

const FAIL = "SOME PROOFS FAIL";

const HINT = "Use --verdict for mathematical validity.";

const MISMATCH = "Sorry - this is a mismatch between the TypeScript implementation,"
  + " and the formalized BendTT kernel. Your proofs may or may not be correct, and"
  + " we cannot validate them yet. This will be addressed in a future update."
  + " Meanwhile, feel free to open an issue to report this bug.";

// BendHub's terms; s18.4 makes MIT-0 the default license
const TERMS = "https://bend-lang.com/bendai/terms#s18";

// the hub's SPDX line rule (hubdb.ts)
const SPDX_RE = /^\s*SPDX-License-Identifier:\s*([A-Za-z0-9.+\-() ]{1,80}?)\s*$/;

// A package's proof of work is a nonce whose sha256(hash + " " + nonce)
// opens (its top 53 bits) with a number under 2^53 / work, where work is
// the hub's pow hashes (GET /pow.json; two seconds of an M4 Max's sixteen
// cores) per 256 KiB of package, and no less. Every core mines; the hub
// checks it with one hash.
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
  if (args[0] === "login" || args[0] === "link") {
    if (args.length !== (args[0] === "link" ? 3 : 1)) {
      cli_fail(args[0] === "link" ? "link takes <name>@<version> and 0x<hash>"
        : args[0] + " takes no argument");
    }
    try {
      await (args[0] === "login" ? cli_login() : cli_link(args[1], args[2]));
    } catch (e) {
      cli_say(2, book_err(e) + "\n");
      process.exitCode = 1;
    }
    return;
  }
  if (args[0] === "guide" && args.length <= 2) {
    cli_guide(args[1] ?? "guide");
  } else if (args[0] === "base" && args.length <= 2) {
    cli_base(args[1]);
  } else {
    await cli_file(args);
  }
  await check().catch(() => {});
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
  const cmd = "s=$(curl -fsSL " + ORIGIN
    + "/install.sh) && printf '%s\\n' \"$s\" | sh";
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
        + process.platform + "&arch=" + process.arch,
      { signal: AbortSignal.timeout(3000) });
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

// ua_fetch tags every request to the hub or bend-lang.com with bend/<VERSION>
function ua_fetch(): void {
  const raw = globalThis.fetch;
  globalThis.fetch = Object.assign((u: string | URL | Request, o: RequestInit = {}) => {
    const to = u instanceof Request ? u.url : String(u);
    return !to.startsWith(Bend.BEND_HUB) && !to.startsWith(ORIGIN) ? raw(u, o)
      : raw(u, { ...o, headers: { ...Object.fromEntries(new Headers(o.headers
        ?? (u instanceof Request ? u.headers : undefined))),
        "user-agent": "bend/" + VERSION } });
  }, raw);
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
  let verdict = false;
  let checkup = false;
  let publish = false;
  let named: string | undefined;
  for (let i = 0; i < args.length; i += 1) {
    const a = args[i];
    if (a === "--help" || a === "-h") {
      return cli_say(1, HELP);
    } else if (a === "--check-only") {
      only = true;
    } else if (a === "--verdict") {
      verdict = true;
    } else if (a === "--checkup") {
      checkup = true;
    } else if (a === "--publish") {
      publish = true;
      if (args[i + 1]?.includes("@")) {
        i += 1;
        named = args[i];
        named_parts(named);
      }
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
  if (file.endsWith(".html") && (outs.length !== 1 || only || verdict
    || checkup || publish || argv.length !== 0)) {
    cli_fail("a page bundles with -o <dir>");
  }
  if (publish && (outs.length !== 0 || only || verdict || checkup)) {
    cli_fail("--publish takes no other option");
  }
  if ((only || verdict) && (outs.length !== 0 || checkup || (only && verdict))) {
    cli_fail((verdict ? "--verdict" : "--check-only") + " takes no other option");
  }
  // args go to a plain run only: any option (all start with -) refuses them
  if (argv.length !== 0 && args.some((a) => a.startsWith("-") && a !== "--")) {
    cli_fail("arguments go to a run: bend <file.bend> [args]");
  }
  if (checkup && outs.length !== 0) {
    cli_fail("--checkup takes no -o: a binary holds one main, so build each"
      + " import alone");
  }
  try {
    if (file.endsWith(".html")) {
      return await cli_bundle(file, outs[0]);
    }
    if (publish) {
      return await cli_publish(file, named);
    }
    if (checkup) {
      return await cli_checkup(file);
    }
    const seen = new Map<string, string | null>();
    const book = await book_read(file, undefined, seen);
    if (only || verdict) {
      process.exitCode = cli_verdict(book, verdict);
      return;
    }
    if (outs.length === 0) {
      process.exitCode = book_run(book, [file, ...argv]);
      return;
    }
    const ins = new Set([...seen.keys(), ...Object.values(book.tlds).flatMap((t) =>
      t.$ === "Def" && t.i !== undefined ? t.i.map(path_real) : [])]);
    for (const out of outs) {
      if (output_conflicts(out, ins)) {
        cli_fail("-o " + out + " is a file the program reads, or a directory");
      }
      // A binary's GPU build also writes <binary>.gpu.
      if (!/\.(mjs|c?js|c|bendtt)$/.test(out)
        && output_conflicts(out + ".gpu", ins)) {
        cli_fail("GPU output " + out + ".gpu is a file the program reads, or a"
          + " directory");
      }
    }
    for (const out of outs) {
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
  const base = await book_read(BASE);
  let bad = false;
  for (const raw of fs.readFileSync(file, "utf8").split("\n")) {
    const line = raw.trim();
    if (line === "" || line.startsWith("#")) {
      continue;
    }
    if (!/^import(\s|$)/.test(line)) {
      break;
    }
    const m = /^import\s+(\S+)\s+as\s+[A-Za-z_][A-Za-z0-9_]*\s*(?:#.*)?$/
      .exec(line);
    if (m === null) {
      continue;
    }
    const at = m[1].startsWith("/") ? m[1]
      : path.join(path.dirname(file), m[1]);
    cli_say(1, "--- " + m[1] + " ---\n");
    let code = 1;
    try {
      const own = /^(?:[\t\r ]*(?:(?:#|import )[^\n]*)?\n)*[\t ]*import Base\s/
        .test(fs.readFileSync(at, "utf8"));
      code = book_run(await book_read(at, own ? base : undefined), [at]);
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

// same_file compares device and inode, so links to one file agree.
function same_file(a: string, b: string): boolean {
  if (!fs.existsSync(a) || !fs.existsSync(b)) {
    return false;
  }
  const x = fs.statSync(a, { bigint: true });
  const y = fs.statSync(b, { bigint: true });
  return x.dev === y.dev && x.ino === y.ino;
}

function output_conflicts(out: string, ins: Set<string>): boolean {
  const at = path_real(out);
  return ins.has(at) || (fs.existsSync(at) && (fs.statSync(at).isDirectory()
    || [...ins].some((input) => same_file(at, input))));
}

function cli_emit(book: Bend.Book, out: string): void {
  if (out.endsWith(".mjs")) {
    fs.writeFileSync(out, Comp.js_lib(book, true));
  } else if (/\.c?js$/.test(out)) {
    fs.writeFileSync(out, Comp.js_book(book));
  } else if (out.endsWith(".c")) {
    fs.writeFileSync(out, Comp.compile_book(book));
  } else if (out.endsWith(".bendtt")) {
    const oos = Safe.safe_emit(book, out);
    if (oos.length !== 0) {
      cli_say(2, "BendTT: out of scope, so not in " + out + ":\n" + oos.join(""));
    }
  } else {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-"));
    try {
      const c = path.join(dir, path.basename(out) + ".c");
      fs.writeFileSync(c, Comp.compile_book(book));
      Bun.gc(true);
      cli_build(out, c);
    } finally {
      fs.rmSync(dir, { recursive: true, force: true });
    }
  }
}

// cc_find is the first of $CC, clang and every clang-NN on PATH (newest
// first) that is new enough: clang 14 for a CPU build, and for a GPU build
// clang 19 (Apple clang 17, which ships LLVM 19), whose #embed
// carries the device program. A $CC that is not one of these is skipped with
// a warning, which names the compiler bend builds with instead.
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
  const want = gpu ? "19 (Apple clang 17)" : "14";
  const env  = process.env.CC;
  const ccs  = [...new Set([...(env ? [env] : []), "clang", ...nums])];
  for (const cc of ccs) {
    const [got, out] = Safe.run_read(cc, ["--version"]);
    const m   = /^(Apple )?(?:\w+ )?clang version (\d+)/m.exec(out);
    const need = gpu ? (m?.[1] === undefined ? 19 : 17) : 14;
    if (m !== null && Number(m[2]) >= need) {
      if (env && cc !== env) {
        process.stderr.write("warning: $CC is not a clang " + want + " or newer (found "
          + olds[0] + "), so bend builds with " + cc + "\n");
      }
      return cc;
    }
    const err = (got.error as NodeJS.ErrnoException | undefined)?.code;
    olds.push(m !== null ? "clang " + m[2] + " as " + cc
      : err === "ENOENT" ? "no " + cc + (/\s/.test(cc) ? " ($CC names one program, without flags)" : "")
      : err ? cc + ", which does not run (" + err + ")"
      : got.status !== 0 ? cc + ", which fails --version"
      : out ? cc + ", which is not clang" : cc + ", which prints no version");
  }
  throw "Error: bend needs clang " + want
    + " or newer to build " + (gpu ? "a GPU program" : "binaries") + " (found "
    + olds.join(", ") + "); on Debian/Ubuntu: curl -fsSL"
    + " https://apt.llvm.org/llvm.sh | sudo bash -s 19; on macOS: xcode-select"
    + " --install";
}

// cli_build builds the C file at `file` into the binary `bin`. A `!` program
// builds with the GPU lane and writes its GPU program too (on Linux only with
// CUDA at the first of $CUDA_HOME, $CUDA_PATH, /usr/local/cuda and /opt/cuda
// (Arch's) that holds nvrtc.h, its libraries in lib64 or, as nix lays them,
// lib; else the ! runs on the cores). On macOS a program with a framework
// (#import: a window, audio) builds as Objective-C; on Linux it links the X11
// (plus libdl, for dlopen before glibc 2.34) and ALSA libraries it includes.
// The GPU program's build runs without the user's $BENDRTS.
function cli_build(bin: string, file: string): void {
  const c     = fs.readFileSync(file, "utf8");
  const mac   = process.platform === "darwin";
  const cuda  = [process.env.CUDA_HOME, process.env.CUDA_PATH,
    "/usr/local/cuda", "/opt/cuda"]
    .find(d => d && fs.existsSync(d + "/include/nvrtc.h")) ?? "";
  const bangs = !/^#define BANGS\s+0$/m.test(c) && (mac || cuda !== "");
  const cc    = cc_find(bangs);
  const objc  = mac && (bangs || /^#import /m.test(c))
    ? ["-x", "objective-c", "-fobjc-arc", "-fmodules"] : [];
  const libs  = [["X11", "X11", "dl"], ["alsa", "asound"]].flatMap(([h, ...ls]) =>
    !mac && c.includes("#include <" + h + "/") ? ls.map((l) => "-l" + l) : []);
  const cpu = [...objc, "-std=c11", "-O3", file, "-lpthread", "-lm",
    ...libs, "-o", path.resolve(bin)];
  const gpu = mac ? ["-DBEND_METAL=1", ...cpu]
    : ["-DBEND_CUDA=1", "-I" + cuda + "/include", "-L" + cuda + "/lib64",
      "-L" + cuda + "/lib", ...cpu, "-lcuda", "-lnvrtc"];
  const steps: [string, string[]][] = bangs
    ? [[cc, gpu], [path.resolve(bin), ["+RTS", "--gpu-build"]]] : [[cc, cpu]];
  for (const [cmd, args] of steps) {
    if (child.spawnSync(cmd, args, { stdio: "inherit",
      env: { ...process.env, BENDRTS: "" } }).status !== 0) {
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
  let tag = "";
  for (const part of src.split(/\n(?=type |law |def |@)/)) {
    const text = tag + part;
    tag = part.startsWith("@") ? text + "\n" : "";
    const m = /^(type|law|def) ([^\s(<:?]+)/m.exec(text);
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

// An inline module script; an empty svg <script/> can't have content.
// HTMLRewriter keeps character references in attributes. Only ASCII
// references and HTML whitespace can turn a type into "module"; NBSP cannot.
function is_inline_module(el: HTMLRewriterTypes.Element): boolean {
  const type = el.getAttribute("type")?.replace(
    /&#(?:[xX]([\da-fA-F]+)|(\d+));?|&(Tab|NewLine);/g, (t, hex, dec, ws) => {
      if (ws) return ws === "Tab" ? "\t" : "\n";
      const n = parseInt(hex ?? dec, hex === undefined ? 10 : 16);
      return n < 128 ? String.fromCharCode(n) : t;
    });
  return el.canHaveContent && !el.hasAttribute("src")
    && type?.replace(/^[\t\n\f\r ]+|[\t\n\f\r ]+$/g, "").toLowerCase() === "module";
}

// cli_bundle builds a page. Bun skips inline module scripts (#1400), so the
// inline plugin moves each to a virtual file beside its page, padded so an
// error points at the page's own line and column.
// Virtual names must not shadow real files; only their diagnostics map to a page.
async function cli_bundle(page: string, dir: string): Promise<void> {
  type Inline = { id: string; page: string; text: string };
  const code = new Map<string, Inline>();
  const inline: BunPlugin = {
    name: "bend inline",
    setup(build) {
      build.onResolve({ filter: /\.bend-inline-\d+\.js$/ }, (a) => {
        const id = path.resolve(a.resolveDir, a.path);
        return code.has(id) ? { path: id, namespace: "inline" } : undefined;
      });
      build.onLoad({ filter: /.*/, namespace: "inline" }, (a) =>
        ({ contents: code.get(a.path)!.text, loader: "js" }));
      build.onLoad({ filter: /\.html$/ }, async (a) => {
        const src = await Bun.file(a.path).text();
        // HTMLRewriter gives no offsets, so a first pass marks where each
        // script's text starts, with a mark the page doesn't hold.
        let mark = "BEND_INLINE_";
        while (src.includes(mark)) {
          mark += "_";
        }
        const marked = new HTMLRewriter().on("script", {
          element(el) {
            if (is_inline_module(el)) {
              el.prepend(mark);
            }
          },
        }).transform(src);
        const mods: Inline[] = [];
        let mod: Inline | null = null;
        let serial = 0;
        const html = new HTMLRewriter().on("script", {
          element(el) {
            mod = null;
            if (is_inline_module(el)) {
              let name: string;
              let id: string;
              do {
                name = "." + path.basename(a.path) + ".bend-inline-" + serial++ + ".js";
                id = path.join(path.dirname(a.path), name);
              } while (fs.existsSync(id));
              mod = { id, page: a.path, text: "" };
              mods.push(mod);
              el.setAttribute("src", "./" + name);
            }
          },
          text(t) {
            if (mod) {
              mod.text += t.text;
              t.remove();
            }
          },
        }).transform(src);
        let end = 0;
        let line = 0;
        for (const [i, m] of mods.entries()) {
          const skip = i * mark.length;
          const at = marked.indexOf(mark, end + skip) - skip;
          line += src.slice(end, at).split("\n").length - 1;
          const col = at - src.lastIndexOf("\n", at - 1) - 1;
          m.text = "\n".repeat(line) + " ".repeat(col) + m.text;
          code.set(m.id, m);
          end = at;
        }
        return { contents: html, loader: "html" };
      });
    },
  };
  try {
    // Only the page keeps its name in the bundle (chunks and assets are hashed),
    // so it is the input an -o can overwrite: refuse that, through links too.
    const entry = path_real(page);
    if (same_file(path.join(dir, path.basename(entry)), entry)) {
      cli_fail("-o " + dir + " would overwrite the page");
    }
    const out = await Bun.build({
      entrypoints: [entry],
      outdir: dir,
      target: "browser",
      minify: true,
      plugins: [inline, PLUGIN],
    });
    for (const a of out.outputs) {
      cli_say(1, a.path + " (" + (a.size / 1024).toFixed(1) + "kb)\n");
    }
  } catch (e) {
    let message = book_err(e);
    for (const [id, mod] of code) {
      message = message.replaceAll(id + ":", mod.page + ":");
    }
    throw message;
  }
}

// Publish
// =======

// cli_publish checks the file, then posts what the loader read (no TODO
// left) to the hub with its proof of work, and prints the import line.
// First it prints the terms and the license the hub will show.
async function cli_publish(file: string, named?: string): Promise<void> {
  const seen = new Map<string, string | null>();
  const book = await book_read(file, undefined, seen);
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
  const lic   = paths.filter((p) => path.posix.basename(p) === "LICENSE")
    .sort((a, b) => a.split("/").length - b.split("/").length)[0];
  const spdx  = lic === undefined ? undefined : files[lic].split("\n").slice(0, 5)
    .map((l) => SPDX_RE.exec(l)?.[1]).find((id) => /[A-Za-z]/.test(id ?? ""))
    ?.replace(/\s+/g, " ");
  cli_say(2, "Publishing to BendHub: public and permanent, under " + TERMS
    + "\nLicense: " + (lic === undefined ? "MIT-0, the default (no LICENSE file): "
    + TERMS + ".4\nwarning: no file is named exactly LICENSE, so the package is"
    + " MIT-0; to license it otherwise, put the license in a file named LICENSE"
    + " beside " + entry : spdx === undefined ? "see " + lic : spdx + " (" + lic + ")") + "\n");
  const auth  = named === undefined ? null : await hub_check(named);
  cli_say(2, "publishing " + String(paths.length) + " files, "
    + String(bytes) + " bytes, as " + hash + " (mining its proof of work)\n");
  const nonce = await pow_mine(hash, bytes);
  const res = await fetch(Bend.BEND_HUB, { method: "POST",
    headers: auth === null ? {} : { authorization: "Bearer " + auth.key },
    body: JSON.stringify({ files, nonce }) });
  if (res.status === 401) {
    key_dead();
  }
  const got = (await res.text()).trim();
  if (!res.ok || got !== hash) {
    throw "Error: " + Bend.BEND_HUB + " answered: " + got;
  }
  cli_say(1, hash + "\n");
  if (auth !== null) {
    await hub_name(auth, hash).catch((e: unknown) => {
      throw String(e) + "\n" + hash + " is published but not named: bend link " + auth.named + " " + hash;
    });
    cli_say(1, "published " + auth.named + "\n");
  }
  cli_say(1, "import " + (auth === null ? hash : auth.named) + "/" + entry + " as "
    + name[0].toUpperCase() + name.slice(1) + "\n");
}

// cli_link names a package already on the hub
async function cli_link(named: string, hash: string): Promise<void> {
  if (!/^0x[0-9a-f]{32}$/.test(hash)) {
    cli_fail("link takes the package's hash: 0x and 32 hex digits");
  }
  await hub_name(await hub_check(named), hash);
  cli_say(1, "linked " + named + " to " + hash + "\n");
}

// named_parts splits a <name>@<version>, or fails
function named_parts(named: string): [string, string] {
  const m = Bend.NAMED.exec(named);
  return m === null ? cli_fail("a package is named <name>@<version>: a-z, 0-9 and -,"
    + " 1 to 64 characters, at four numbers like 1.0.0.0") : [m[1], m[2]];
}

// hub_check reads the key (a login when there is none), then asks the
// hub's /publish-check whose the name is and whether the version goes up
async function hub_check(named: string): Promise<{ named: string; key: string; free: boolean }> {
  const [name, version] = named_parts(named);
  let key = "";
  if (!fs.existsSync(BENDAI) && fs.existsSync(BENDAI_OLD)) {
    fs.renameSync(BENDAI_OLD, BENDAI);
  }
  try {
    key = String((JSON.parse(fs.readFileSync(BENDAI, "utf8")) as { key?: string }).key ?? "");
  } catch {}
  if (key === "") {
    key = await cli_login();
  }
  const got = await hub_ask("/publish-check?name=" + name + "&version=" + version, key);
  if ((got.name !== "yours" && got.name !== "free") || got.version_ok !== true) {
    throw "Error: " + String(got.reason);
  }
  return { named, key, free: got.name === "free" };
}

// hub_name registers a free name and links name@version to a hash
async function hub_name(auth: { named: string; key: string; free: boolean }, hash: string): Promise<void> {
  const [name, version] = named_parts(auth.named);
  if (auth.free) {
    await hub_ask("/register", auth.key, { name });
    cli_say(2, "registered " + name + "\n");
  }
  await hub_ask("/link", auth.key, { name, version, hash });
}

// hub_ask sends the key with a GET, or a POST of body, and answers the
// hub's JSON; unreachable, a refused key or a refused request ends the run
async function hub_ask(route: string, key: string, body?: unknown): Promise<Record<string, unknown>> {
  const res = await fetch(Bend.BEND_HUB + route, { method: body === undefined ? "GET" : "POST",
    headers: { authorization: "Bearer " + key, "content-type": "application/json" },
    body: JSON.stringify(body) }).catch(() => null);
  if (res === null) {
    throw "Error: " + Bend.BEND_HUB + " could not be reached";
  }
  if (res.status === 401) {
    key_dead();
  }
  const got = await res.json().catch(() => null) as Record<string, unknown> | null;
  if (!res.ok || got === null) {
    throw "Error: " + Bend.BEND_HUB + route + " answered: "
      + (typeof got?.reason === "string" ? got.reason : String(res.status));
  }
  return got;
}

// key_dead forgets a key the hub refused, so the next run logs in
function key_dead(): never {
  fs.rmSync(BENDAI, { force: true });
  throw "Error: " + Bend.BEND_HUB + " does not know this login: run bend login";
}

// cli_login starts BendAI's CLI login, opens its page and polls until the
// browser authorized a key (SPEC.md 6.14 of bend-lang.com)
async function cli_login(): Promise<string> {
  const st = await fetch(ORIGIN + "/bendai/cli/start", { method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ machine: os.hostname() }) }).then((r) => r.json()).catch(() => null) as
    { poll_secret?: string; verify_url?: string; expires_at?: string; interval_ms?: number } | null;
  if (st === null || typeof st.poll_secret !== "string" || typeof st.verify_url !== "string") {
    throw "Error: " + ORIGIN + " did not start a login";
  }
  cli_say(2, "log in at " + st.verify_url + "\n");
  try {
    Bun.spawn([process.platform === "darwin" ? "open" : "xdg-open", st.verify_url], { stdout: "ignore", stderr: "ignore" });
  } catch {}
  const until = Date.parse(st.expires_at ?? "") || Date.now() + 600000;
  while (Date.now() < until) {
    await new Promise((r) => setTimeout(r, Math.max(1000, st.interval_ms ?? 2000)));
    const got = await fetch(ORIGIN + "/bendai/cli/poll", { method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ poll_secret: st.poll_secret }) }).then((r) => r.json()).catch(() => null) as
      { status?: string; key?: string; login?: string } | null;
    if (got?.status === "authorized" && typeof got.key === "string") {
      fs.mkdirSync(path.dirname(BENDAI), { recursive: true });
      fs.writeFileSync(BENDAI, JSON.stringify({ key: got.key, login: got.login ?? "" }) + "\n", { mode: 0o600 });
      cli_say(2, "logged in as " + String(got.login ?? "") + "\n");
      return got.key;
    }
    if (got?.status === "expired") {
      break;
    }
  }
  throw "Error: the login was not authorized in time: run bend login again";
}

// pkg_files is the package the loader read for this file, the entry first:
// every .bend file at its namespace (the entry at its name), every foreign
// .c or .js file at its path from the entry's directory; base and the
// store's packages stay out. The entry's directory is the one the loader
// roots namespaces at, so an entry linked from another directory, or one
// at the file system's root, is refused.
// A LICENSE beside a published file goes along; a license/ directory, in
// any case, is refused (it clashes with LICENSE on a case-blind disk).
function pkg_files(file: string, book: Bend.Book,
  seen: Map<string, string | null>): Record<string, string> {
  const ent = fs.realpathSync(file);
  const dir = path.join(path.dirname(ent), "/");
  if (dir !== path.join(fs.realpathSync(path.dirname(file)), "/")) {
    throw "Error: " + file + " cannot be published: it links to " + ent
      + ", in another directory; publish that file";
  }
  if (dir === "/") {
    throw "Error: " + file + " cannot be published: it is at the file"
      + " system's root; move it into a directory";
  }
  const raws = [...[...seen].flatMap(([real, ns]): [string, string][] =>
    real === BASE || ns === null || ns.startsWith("0x") ? []
      : [[ns === "" ? path.basename(file) : ns + ".bend", real]]),
  ...Object.entries(book.tlds).flatMap(([k, tld]): [string, string][] =>
    tld.$ !== "Def" || tld.i === undefined || tld.b === true
      || k.startsWith("0x") ? [] : tld.i.map((f) =>
      [path.posix.normalize(f.slice(dir.length)), f]))];
  // .bend files come first, so a foreign file's .bend, its directory a
  // prefix of the file's path, is checked inside dir before its path is cut
  const files: Record<string, string> = {};
  for (const [p, src] of raws) {
    if (p.split("/").slice(0, -1).some((s) => s.toLowerCase() === "license")) {
      throw "Error: " + src + " cannot be published: it is in a directory"
        + " named license, which clashes with a LICENSE file; rename it";
    }
    files[p] = pkg_read(p, src, dir, "move it");
    if (fs.readdirSync(path.dirname(src)).includes("LICENSE")) {
      const lic = path.posix.join(path.posix.dirname(p), "LICENSE");
      files[lic] = pkg_read(lic, path.join(path.dirname(src), "LICENSE"), dir,
        "it is the LICENSE beside " + src + "; copy it");
    }
  }
  return files;
}

// pkg_read is the text of src, published at p: p and src's real path stay
// inside dir, the entry's directory. p is the import's own path, normalized
// as text, since an importer resolves it inside the package that way; one
// that climbs out and back in names a file the package lacks. A leading
// byte order mark is dropped: the hub stores the text as sent, and an
// importer's fetch drops the mark, so a file sent with one would never
// match its hash.
function pkg_read(p: string, src: string, dir: string, fix: string): string {
  const real = fs.realpathSync(src);
  if (!real.startsWith(dir)) {
    throw "Error: " + src + " cannot be published: it is outside " + dir
      + " (the entry's directory); " + fix + " inside";
  }
  if (p.startsWith("../") || p.startsWith("/")) {
    throw "Error: " + src + " cannot be published: its import climbs out of"
      + " the package (" + p + "); import it by a path that stays inside "
      + dir;
  }
  return fs.readFileSync(real, "utf8").replace(/^\uFEFF/, "");
}

function sha256(text: string): string {
  return crypto.createHash("sha256").update(text).digest("hex");
}

async function pow_mine(hash: string, bytes: number): Promise<number> {
  const pow  = Number((await hub_ask("/pow.json", "")).pow);
  const step = os.availableParallelism();
  const lim  = 2 ** 53 / (pow * Math.max(1, bytes / 262144));
  const ws   = Array.from({ length: step }, (_, k) => new thr.Worker(POW_JS,
    { eval: true, workerData: { pre: hash + " ", lim, from: k, step } }));
  const n = await new Promise<number>((res) =>
    ws.forEach((w) => w.on("message", res)));
  ws.forEach((w) => w.terminate());
  return n;
}

// Verdict
// =======

// cli_verdict prints the verdict on a book bend2 checked: PASS when no
// def outside Base relies on unsafe or foreign code and, with the kernel,
// when BendTT checks every def too; else FAIL and why. A kernel failure is
// a mismatch: bend2 accepted what the kernel rejects; it shows the why.
// When none of those is a law or a def of the file, no law or proof uses
// them: they come in through imports, and a hint names them.
function cli_verdict(book: Bend.Book, kernel: boolean): number {
  const bad = book_promises(book);
  const n   = new Set(bad.flatMap(([, ks]) => ks)).size;
  if (n !== 0) {
    const own = bad.some(([, ks]) => ks.some((k) =>
      !k.includes(":") || (book.tlds[k] as Bend.Def).l === true));
    cli_say(2, FAIL + "\nError: " + String(n) + " def" + (n === 1 ? " relies"
      : "s rely") + " on unsafe or foreign code:\n"
      + bad.map(([on, ks]) => "defs that depend on " + on + ": "
        + ks.map(Bend.name_key).join(", ") + "\n").join("")
      + (own ? "" : "Hint: no law or proof uses these defs. Move them to a"
        + " file this proof does not import, and the proofs will check: "
        + [...new Set(bad.flatMap(([, ks]) => ks))].map(Bend.name_key)
          .join(", ") + "\n"));
    return 1;
  }
  const why = kernel ? Safe.safe_check(book) : null;
  if (why !== null) {
    cli_say(2, FAIL + "\n" + why + "\n" + MISMATCH + "\n");
    return 1;
  }
  cli_say(1, PASS + "\n" + (kernel ? "" : HINT + "\n"));
  return 0;
}

// book_promises lists, per promise (@unsafe, foreign effects), the defs
// outside Base (laws and types too) that are one, or whose type, body,
// datatype kind, parameters or constructor fields name a def that relies
// on one. A foreign def is a promise like @unsafe is: the checker reads
// its type, never its code. If the book holds a promise, a walk from the
// defs outside Base collects who names whom, then each promise floods
// back along those edges.
function book_promises(book: Bend.Book): Array<[string, string[]]> {
  const own  = [...new Set(book.order)].filter((k) => book.tlds[k].b !== true);
  const keys = Object.keys(book.tlds);
  const roots: Array<[string, string[]]> = [
    ["@unsafe", keys.filter((k) => (book.tlds[k] as Bend.Def).u === true)],
    ["foreign effects", keys.filter((k) => {
      const t = book.tlds[k] as Bend.Def;
      return t.u !== true && t.i !== undefined && t.b !== true;
    })],
  ];
  if (roots.every(([, ks]) => ks.length === 0)) {
    return [];
  }
  const uses: Record<string, string[]> = Object.create(null);
  const reach = new Set(own);
  for (const k of reach) {
    const t = book.tlds[k];
    if (t !== undefined) {
      const rs = new Set<string>();
      for (const c of t.$ === "ADT" ? [t, ...t.c] : [t]) {
        term_refs(Bend.term_lower(c.T), rs);
      }
      if (t.$ === "Def" && t.e !== undefined) {
        term_refs(t.e, rs);
      }
      for (const r of rs) {
        (uses[r] ??= []).push(k);
        reach.add(r);
      }
    }
  }
  return roots.flatMap(([on, ks]) => {
    const bad = new Set(ks);
    for (const k of bad) {
      uses[k]?.forEach((j) => bad.add(j));
    }
    const out = own.filter((k) => bad.has(k));
    return out.length === 0 ? [] : [[on, out]];
  });
}

// term_refs adds to out the names a term (a span skipped) refers to.
function term_refs(t: object, out: Set<string>): void {
  const seen = new Set([t]);
  for (const todo = [t]; todo.length > 0;) {
    const x = todo.pop() as Record<string, unknown>;
    if ((x.$ === "Ref" || x.$ === "ADT") && typeof x.k === "string") {
      out.add(x.k);
    }
    for (const f in x) {
      const v = x[f];
      if (f !== "s" && typeof v === "object" && v !== null && !seen.has(v)) {
        seen.add(v);
        todo.push(v);
      }
    }
  }
}

// cli_say writes text to fd, and drops it if the reader has left (EPIPE).
function cli_say(fd: number, text: string): void {
  const data = Buffer.from(text);
  for (let at = 0; at < data.length;) {
    try {
      const n = fs.writeSync(fd, data, at, data.length - at);
      if (n === 0) {
        throw new Error("bend: a write made no progress");
      }
      at += n;
    } catch (e) {
      const code = (e as NodeJS.ErrnoException).code;
      if (code === "EPIPE") {
        return;
      }
      if (code !== "EAGAIN" && code !== "EINTR") {
        throw e;
      }
    }
  }
}

function cli_fail(msg: string): never {
  cli_say(2, "bend: " + msg + " (see bend --help)\n");
  process.exit(1);
}

// Book
// ====

async function book_read(file: string, base?: Bend.Book,
  seen = new Map<string, string | null>()): Promise<Bend.Book> {
  const book = base === undefined ? Bend.book_nil() : book_seed(base);
  if (base !== undefined) {
    seen.set(BASE, "");
  }
  try {
    await Bend.book_load(book, file, "", seen);
    const laws = path.join(path.dirname(file), "LAWS.bend");
    if (path.basename(file) === "PROOF.bend" && fs.existsSync(laws)
      && !seen.has(fs.realpathSync(laws))) {
      throw "Error: PROOF.bend must import ./LAWS.bend";
    }
    Bend.book_valid(book, base?.order.length ?? 0);
    if (book.hols > 0) {
      throw "Error: " + String(book.hols) + " TODO" + (book.hols === 1 ? "" : "s")
        + " found.\nThe code is incomplete, and not a valid proof yet.";
    }
  } catch (e) {
    throw new Check_Fail(e);
  }
  return book;
}

// a failed check: bend2's reason, which book_err prints under FAIL
class Check_Fail {
  constructor(readonly why: unknown) {}
}

function book_seed(base: Bend.Book): Bend.Book {
  const book = Bend.book_nil();
  for (const k of Object.keys(base.tlds)) {
    book.tlds[k] = { ...base.tlds[k] };
  }
  Object.assign(book.ctrs, base.ctrs);
  for (const k of Object.keys(base.tmps)) {
    book.tmps[k] = new Map(base.tmps[k]);
  }
  book.order.push(...base.order);
  return book;
}

function book_main(book: Bend.Book): Bend.Def | null {
  const main = book.tlds["main"];
  return main === undefined || main.$ !== "Def"
    || (main.v === null && main.i === undefined) ? null : main;
}

function book_run(book: Bend.Book, argv: string[]): number {
  const main = book_main(book);
  if (main === null) {
    return cli_verdict(book, false);
  }
  if (Comp.io_type(book) !== null) {
    return Comp.io_run(book, argv);
  }
  const snf = Bend.term_snf(book, main.v as Bend.HTerm);
  cli_say(1, Bend.term_show(Bend.term_lower(snf)) + "\n");
  return 0;
}

function book_err(e: unknown): string {
  if (e instanceof Check_Fail) {
    return FAIL + "\n" + book_err(e.why);
  }
  if (e instanceof AggregateError) {
    return e.errors.map((m) => Bun.inspect(m, { colors: false })).join("\n")
      || String(e);
  }
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
    return Comp.js_lib(await book_read(path), true);
  } catch (e) {
    throw new Error(book_err(e));
  }
}

export async function load(u: string, context: unknown,
  next: (u: string, context: unknown) => unknown): Promise<unknown> {
  const file = u.startsWith("file:") ? url.fileURLToPath(u) : null;
  return file !== null && file.endsWith(".bend")
    ? { format: "module", shortCircuit: true,
      source: await load_js(file) }
    : next(u, context);
}

export default PLUGIN;

if (import.meta.main) {
  if (typeof Bun === "undefined") {
    cli_say(2, "bend runs on Bun: curl -fsSL https://bend-lang.com/install.sh"
      + " | sh\n");
    process.exit(1);
  }
  ua_fetch();
  await cli();
  process.exit();
} else if (typeof Bun !== "undefined") {
  Bun.plugin(PLUGIN);
} else if (thr.isMainThread || thr.isInternalThread === false) {
  mod.register(import.meta.url);
}
