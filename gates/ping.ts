#!/usr/bin/env bun
// The installer, the compiled bend, its daily check and the hub, on this
// machine: release.ts --dry (the site repo at lib.SITE) builds this host's
// target into a temp DL_DIR (the archive, install.sh, bend.rb, latest.json);
// a hub.ts on a random localhost port, at HUB_POW=1 (bend mines against
// the hub's pow, so a publish costs one hash), logs to a temp file; a
// Bun.serve plays Caddy and GitHub in front of it (/install.sh with the
// GitHub URL turned into this origin and the https-only flags dropped,
// since this origin is plain http; the archive under /dl; the store under
// /0x<hash>; /check, /ping and /pow.json to the hub); then install.sh runs
// in a temp HOME over the old launcher's layout. Checks:
// bashka (SKIP without it) calls the script green; the install replaces the
// launcher with the executable, drops app/, current, id, last, rep and bad,
// cleans its temp dir, writes no shell rc, names the version, the PATH line
// and the daily check in its card (a second install, bin on PATH, omits the
// PATH line); bend --help prints the help, and its check
// logs one line {v, os, arch, ip} with no id and no cmd; a second run and
// bend --version log nothing; BEND_NO_TELEMETRY=1 asks nothing and writes
// no cache; a newer release with a notice prints one line and the notice
// (control characters stripped) on stderr, stdout and the exit code being
// the command's own; a dead origin costs one run under four seconds; bend
// update runs the installer again; guide, base and a program run through
// the executable; run in a project, it preloads none of its bunfig.toml
// and reads none of its .env (its BEND_LIB would name a package and swap in
// the project's copy); a tampered sha256 installs nothing; a Windows or a MIPS
// uname is refused in one line; a 2.0.0-2.0.7 launcher's ping and its
// latest.json fallback name the version, no sha256 and the move notice; the
// formula carries the sum; --publish ships LICENSE files, names the license
// as the hub does, sends no leading byte order mark (a package published
// from a file with one imports), refuses a License/ directory, and every
// request carries User-Agent: bend/<ver>. SKIP when the site repo is not at
// lib.SITE.

import * as child from "node:child_process";
import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import * as lib from "./_lib";

// Constants
// =========

if (!fs.existsSync(path.join(lib.SITE, "release", "release.ts"))) {
  console.log("SKIP the site repo is not at " + lib.SITE + " (set SITE_REPO)");
  process.exit(0);
}

const PORT   = 20000 + Math.floor(Math.random() * 40000);
const ORIGIN = "http://localhost:" + String(PORT);
const HUB    = "http://localhost:" + String(PORT + 1);
const TMP    = fs.mkdtempSync(path.join(os.tmpdir(), "bend-ping-"));
const HOME   = path.join(TMP, "home");
const BEND   = path.join(HOME, ".bend");
const BIN    = path.join(BEND, "bin", "bend");
const DL     = path.join(TMP, "dl");
const LOG    = path.join(TMP, "check.jsonl");
const TARGET = process.platform + "-" + process.arch;
const SAID   = "Once a day, bend asks bend-lang.com";
const MOVED  = "Bend's installer changed";
const PATHS  = "/usr/bin:/bin";
const TERMS  = "Publishing to BendHub: public and permanent, under"
  + " https://bend-lang.com/bendai/terms#s18\n";

// what the Caddy stand-in saw: "<method> <path> <user-agent>"
const seen: string[] = [];

const fails: string[] = [];
let total = 0;

// Run
// ===

function run(bin: string, args: string[], env: Record<string, string> = {},
  input?: string): Promise<lib.Exec> {
  return lib.exec(bin, args, input, 25_000, { HOME, PATH: PATHS,
    BEND_ORIGIN: ORIGIN, ...env }, TMP);
}

function bend(args: string[], env: Record<string, string> = {}):
  Promise<lib.Exec> {
  return run(BIN, args, env);
}

// The reader has exited before bend starts: no race with its first write.
function bend_closed(args: string[]): Promise<lib.Exec> {
  return run("bash", ["-c",
    'exec 3> >(true); wait "$!"; exec "$@" >&3 2>&3',
    "--", BIN, ...args], { BEND_NO_TELEMETRY: "1" });
}

// the card without its colors
function plain(out: string): string {
  return out.replace(/\x1b\[[0-9;]*m/g, "");
}

function install(env: Record<string, string> = {}): Promise<lib.Exec> {
  return run("sh", ["-c", "curl -fsSL " + ORIGIN + "/install.sh | sh"], env);
}

function check(what: string, ok: boolean): void {
  total += 1;
  if (!ok) {
    fails.push(what);
  }
}

function release(ver: string, notice = ""): void {
  fs.writeFileSync(path.join(DL, "latest.json"),
    JSON.stringify({ ver, notice }));
}

function logs(): Record<string, unknown>[] {
  try {
    return fs.readFileSync(LOG, "utf8").trim().split("\n")
      .map((l) => JSON.parse(l) as Record<string, unknown>);
  } catch {
    return [];
  }
}

// pkg_hash is the hash --publish gives these files
function pkg_hash(files: Record<string, string>): string {
  const sha = (t: string) => crypto.createHash("sha256").update(t).digest("hex");
  return "0x" + sha(Object.keys(files).sort().map((p) => sha(files[p]) + " "
    + p + "\n").join("")).slice(0, 32);
}

// publish writes files under TMP/pub/<dir> and publishes lic_<dir>.bend
function publish(dir: string, files: Record<string, string>):
  Promise<lib.Exec> {
  for (const [f, text] of Object.entries(files)) {
    fs.mkdirSync(path.dirname(path.join(TMP, "pub", dir, f)),
      { recursive: true });
    fs.writeFileSync(path.join(TMP, "pub", dir, f), text);
  }
  return bend([path.join(TMP, "pub", dir, "lic_" + dir + ".bend"),
    "--publish"], { BEND_HUB: ORIGIN });
}

function fresh(): void {
  fs.rmSync(path.join(BEND, "check.json"), { force: true });
}

async function hub_wait(): Promise<void> {
  for (let i = 0; i < 50; i += 1) {
    try {
      await fetch(HUB + "/index.json");
      return;
    } catch {
      await new Promise((wake) => setTimeout(wake, 100));
    }
  }
  throw new Error("hub.ts did not come up on " + HUB);
}

// Server
// ======

let script = "";

const caddy = Bun.serve({
  port: PORT,
  fetch(req) {
    const url = new URL(req.url);
    const at  = url.pathname;
    seen.push(req.method + " " + at + " " + (req.headers.get("user-agent")
      ?? ""));
    if (at === "/install.sh") {
      return new Response(script);
    }
    if (at.startsWith("/dl/") || /^\/0x[0-9a-f]{32}\//.test(at)) {
      const file = at.startsWith("/dl/") ? path.join(DL, path.basename(at))
        : path.join(TMP, "store", path.posix.normalize(at));
      return fs.existsSync(file) ? new Response(Bun.file(file))
        : new Response(null, { status: 404 });
    }
    return fetch(HUB + at + url.search, { method: req.method,
      headers: req.headers, body: req.body });
  },
});

// Main
// ====

const hub = child.spawn(process.execPath, [path.join(lib.SITE, "apps", "hub",
  "hub.ts")], { stdio: "ignore", env: { ...process.env, HUB_PORT:
  String(PORT + 1), HUB_POW: "1", HUB_STORE: path.join(TMP, "store"),
  CHECK_LOG: LOG, DL_DIR: DL } });
try {
  await hub_wait();
  const rel = await lib.exec(process.execPath, [path.join(lib.SITE, "release",
    "release.ts"), "--dry", TARGET], undefined, 25_000, { DL_DIR: DL,
    BEND_REPO: lib.ROOT });
  const ver = (JSON.parse(fs.readFileSync(path.join(DL, "latest.json"),
    "utf8")) as { ver: string }).ver;
  const tgz = "bend-" + ver + "-" + TARGET + ".tar.gz";
  check("release.ts --dry " + TARGET + ": " + rel.err, rel.code === 0
    && rel.out.includes(tgz) && fs.existsSync(path.join(DL, "bend.rb")));
  const sum = /^SHA_[A-Z0-9_]+="([0-9a-f]{64})"$/m.exec(
    fs.readFileSync(path.join(DL, "install.sh"), "utf8"))?.[1] ?? "";
  check("bend.rb carries the version and the sum",
    fs.readFileSync(path.join(DL, "bend.rb"), "utf8").includes('sha256 "' + sum)
    && fs.readFileSync(path.join(DL, "bend.rb"), "utf8").includes(ver));
  const orig = fs.readFileSync(path.join(DL, "install.sh"), "utf8");
  script = orig.replace("https://github.com/$REPO/releases/download/v$VER",
    ORIGIN + "/dl").replace("--proto '=https' --tlsv1.2 ", "");
  const bashka = Bun.which("bashka");
  if (bashka === null) {
    console.log("SKIP bashka is not installed (cargo build in its checkout,"
      + " then put it on PATH)");
  } else {
    const vet = await lib.exec(bashka, ["--check", "--non-interactive"], orig,
      25_000, { NO_COLOR: "1" });
    check("bashka calls install.sh green: " + vet.err.split("\n").pop(),
      vet.code === 0 && vet.err.includes("GREEN"));
  }
  for (const f of ["id", "last", "rep", "bad"]) {
    fs.mkdirSync(BEND, { recursive: true });
    fs.writeFileSync(path.join(BEND, f), "old\n");
  }
  fs.mkdirSync(path.join(BEND, "app", "2.0.7", "x"), { recursive: true });
  fs.symlinkSync("app/2.0.7/x", path.join(BEND, "current"));
  fs.mkdirSync(path.join(BEND, "bin"));
  fs.writeFileSync(BIN, "#!/bin/sh\necho launcher\n", { mode: 0o755 });
  const ins = await install();
  check("install.sh over the old layout: " + ins.err, ins.code === 0);
  const vers = await bend(["version"]);
  check("bin/bend is the executable", vers.code === 0
    && vers.out === "bend " + ver + "\n"
    && fs.statSync(BIN).size > 1_000_000);
  check("the old layout is gone", ["app", "current", "id", "last", "rep",
    "bad"].every((f) => !fs.existsSync(path.join(BEND, f)))
    && !fs.readdirSync(BEND).some((f) => f.startsWith("tmp.")));
  check("no shell rc is written", !fs.readdirSync(HOME).some((f) =>
    f !== ".bend"));
  check("the card names the version, the PATH line and the daily check",
    plain(ins.out).includes("Bend \u2588  " + ver) && ins.out.includes("Bend developer")
    && ins.out.includes("export PATH=\"")
    && ins.out.includes(SAID));
  const again = await install({ PATH: path.dirname(BIN) + ":" + PATHS });
  check("a second install, bin on PATH, omits the PATH line", again.code === 0
    && !again.out.includes("PATH=")
    && fs.statSync(BIN).size > 1_000_000);
  check("bend version logs nothing", logs().length === 0
    && !fs.existsSync(path.join(BEND, "check.json")));
  const help = await bend(["--help"]);
  const line = logs().pop() ?? {};
  check("bend --help prints the help", help.code === 0
    && help.out.includes("usage:"));
  check("the check logs {v, os, arch, ip} and nothing else",
    logs().length === 1 && line.v === ver && line.os === process.platform
    && line.arch === process.arch && typeof line.ip === "string"
    && Object.keys(line).sort().join() === "arch,ip,os,t,v");
  await bend(["--help"]);
  check("a second run logs nothing", logs().length === 1);
  fresh();
  const mute = await bend(["--help"], { BEND_NO_TELEMETRY: "1" });
  check("BEND_NO_TELEMETRY=1 asks nothing and writes no cache",
    mute.code === 0 && logs().length === 1
    && !fs.existsSync(path.join(BEND, "check.json")));
  release("99.0.0", "hello\u001b\nworld");
  fresh();
  fs.writeFileSync(path.join(TMP, "bad.bend"),
    "import Base\ndef main() -> Nat:\n  True{}\n");
  await bend(["guide"]);
  const bad = await bend([path.join(TMP, "bad.bend")]);
  check("a newer release prints its line and the notice on stderr, the"
    + " command's stdout and exit code untouched", bad.code === 1
    && bad.out === "" && bad.err.includes("bend 99.0.0 is available: run"
    + " bend update\nhelloworld\n"));
  release(ver);
  fresh();
  const t0 = Date.now();
  const dead = await bend(["--help"], { BEND_ORIGIN: "http://127.0.0.1:1" });
  check("a dead origin costs one run under four seconds", dead.code === 0
    && dead.out.includes("usage:") && Date.now() - t0 < 4000);
  const was = fs.statSync(BIN).ino;
  const upd = await bend(["update"]);
  check("bend update runs the installer again: " + upd.err, upd.code === 0
    && upd.err.startsWith("curl -fsSL " + ORIGIN + "/install.sh | sh\n")
    && plain(upd.out).includes("Bend \u2588  " + ver) && fs.statSync(BIN).ino !== was);
  const guide = await bend(["guide"]);
  const base  = await bend(["base", "Map"]);
  fs.writeFileSync(path.join(TMP, "sum.bend"),
    "import Base\ndef main() -> Nat:\n  (2n + 3n : Nat)\n");
  const sum5 = await bend([path.join(TMP, "sum.bend")]);
  check("guide, base and a program run through the executable",
    guide.out.startsWith("# Bend") && base.out.startsWith("type Map")
    && sum5.code === 0 && sum5.out === "5n\n");
  fs.writeFileSync(path.join(TMP, "page.html"),
    "<script type=\"module\" src=\"./page.ts\"></script>\n");
  fs.writeFileSync(path.join(TMP, "page.ts"), "import \"./none.ts\";\n");
  const page = await bend([path.join(TMP, "page.html"), "-o",
    path.join(TMP, "dist")]);
  check("a page that fails to bundle names the import and where, and exits 1"
    + " with no crash banner: " + page.err, page.code === 1
    && page.err.includes("Could not resolve: \"./none.ts\"")
    && page.err.includes("page.ts:1:") && !page.err.includes("Bun v"));
  const bad_file = path.join(TMP, "bad.bend");
  const sum_file = path.join(TMP, "sum.bend");
  const unsafe_file = path.join(TMP, "unsafe.bend");
  const checkup = path.join(TMP, "checkup.bend");
  const good_checkup = path.join(TMP, "good_checkup.bend");
  const verdict_tmp = path.join(TMP, "verdict-tmp");
  fs.mkdirSync(verdict_tmp);
  const no_lean = await bend([sum_file, "--verdict"],
    { BENDTT: "", TMPDIR: verdict_tmp, BEND_NO_TELEMETRY: "1" });
  check("a failed kernel build leaves no private BendTT input",
    no_lean.code === 1 && no_lean.err.includes("--verdict needs Lean")
    && fs.readdirSync(verdict_tmp).every((name) => !name.startsWith("bendtt-")));
  fs.writeFileSync(unsafe_file,
    "import Base\n@unsafe\ndef main() -> Nat:\n  0n\n");
  fs.writeFileSync(checkup,
    "import ./sum.bend as Good\nimport ./bad.bend as Bad\n");
  fs.writeFileSync(good_checkup, "import ./sum.bend as Good\n");
  for (const args of [[bad_file], [bad_file, "--check-only"],
    [unsafe_file, "--verdict"], ["--unknown"], [checkup, "--checkup"]]) {
    const got = await bend_closed(args);
    check("a closed reader keeps failure: " + args.join(" "), got.code === 1);
  }
  for (const args of [[sum_file], [sum_file, "--check-only"],
    ["--help"], [good_checkup, "--checkup"]]) {
    const got = await bend_closed(args);
    check("a closed reader keeps success: " + args.join(" "), got.code === 0);
  }
  const two  = "import Base\ndef two() -> Nat:\n  2n\n";
  const use  = (at: string) => "import Base\nimport ./" + at
    + " as T\ndef main() -> Nat:\n  T.two\n";
  const lics = { "lic_spdx.bend": use("sub/two.bend"), "sub/two.bend": two,
    "LICENSE": "SPDX-License-Identifier: MIT\n",
    "sub/LICENSE": "SPDX-License-Identifier: Apache-2.0\n" };
  const spdx = await publish("spdx", lics);
  const hash = pkg_hash(lics);
  const got  = await fetch(ORIGIN + "/" + hash + "/sub/LICENSE");
  check("a LICENSE beside each published file goes along, in the hash: "
    + spdx.err, spdx.code === 0 && spdx.out.startsWith(hash + "\n")
    && got.ok && await got.text() === lics["sub/LICENSE"]);
  check("the notice names the terms and the shallowest LICENSE's SPDX id",
    spdx.err.includes(TERMS + "License: MIT (LICENSE)\n"));
  for (const flags of [["--verdict", "--publish"], ["--publish", "--verdict"]]) {
    const count = seen.length;
    const run = await bend([path.join(TMP, "sum.bend"), ...flags],
      { BEND_HUB: ORIGIN, BEND_NO_TELEMETRY: "1" });
    check(flags.join(" ") + " is refused before publishing: " + run.err,
      run.code === 1 && run.out === "" && run.err === "bend: --publish"
      + " takes no other option (see bend --help)\n" && seen.length === count);
  }
  const ids: [string, string][] = [
    ["SPDX-License-Identifier: MIT\r\n", "MIT (LICENSE)"],
    ["SPDX-License-Identifier: (MIT  OR Apache-2.0)\n",
      "(MIT OR Apache-2.0) (LICENSE)"],
    ["# SPDX-License-Identifier: MIT\n", "see LICENSE"],
    ["// SPDX-License-Identifier: GPL-2.0-or-later\n", "see LICENSE"],
    ["SPDX-License-Identifier: MIT <see below>\n", "see LICENSE"],
    ["SPDX-License-Identifier: ()\n", "see LICENSE"],
    ["1\n2\n3\n4\n5\nSPDX-License-Identifier: MIT\n", "see LICENSE"]];
  for (const [k, [text, want]] of ids.entries()) {
    const pkg = { ["lic_id" + String(k) + ".bend"]: use("two.bend"),
      "two.bend": two, "LICENSE": text };
    const run = await publish("id" + String(k), pkg);
    const hub = await (await fetch(ORIGIN + "/package/" + pkg_hash(pkg)
      + ".json")).json() as { license?: { id: string | null } };
    check("a LICENSE opening " + JSON.stringify(text) + " is " + want
      + ", as the hub names it: " + run.err, run.code === 0
      && run.err.includes(TERMS + "License: " + want + "\n")
      && hub.license?.id === (want === "see LICENSE" ? null
        : want.slice(0, -" (LICENSE)".length)));
  }
  const bare = { "lic_none.bend": use("two.bend"), "two.bend": two };
  const none = await publish("none", { ...bare, "LICENSE.md": "MIT\n" });
  check("a LICENSE.md alone is left out, and the notice says MIT-0 and"
    + " warns: " + none.err, none.code === 0
    && none.out.startsWith(pkg_hash(bare) + "\n")
    && none.err.includes(TERMS + "License: MIT-0, the default (no LICENSE"
    + " file): https://bend-lang.com/bendai/terms#s18.4\nwarning: no file is"
    + " named exactly LICENSE"));
  const bom  = { "lic_bom.bend": use("two.bend"), "two.bend": two,
    "LICENSE": "SPDX-License-Identifier: MIT\n" };
  const mark = await publish("bom",
    { ...bom, "LICENSE": "\uFEFF" + bom.LICENSE });
  fs.writeFileSync(path.join(TMP, "bom.bend"), "import Base\nimport "
    + pkg_hash(bom) + "/two.bend as T\ndef main() -> Nat:\n  T.two\n");
  const imp  = await bend([path.join(TMP, "bom.bend")], { BEND_HUB: ORIGIN });
  check("a LICENSE opening with a byte order mark goes without it, so the"
    + " package imports: " + mark.err + imp.err, mark.code === 0
    && mark.out.startsWith(pkg_hash(bom) + "\n") && imp.code === 0
    && imp.out === "2n\n");
  const posts = seen.filter((s) => s.startsWith("POST / ")).length;
  const dir  = await publish("dir", { "lic_dir.bend": use("License/two.bend"),
    "License/two.bend": two });
  check("a directory named License is refused before mining: " + dir.err,
    dir.code === 1 && dir.err.includes("in a directory named license")
    && !dir.err.includes("mining")
    && seen.filter((s) => s.startsWith("POST / ")).length === posts);
  fs.writeFileSync(path.join(TMP, "pkg.bend"), "import Base\nimport " + hash
    + "/sub/two.bend as T\nimport bend-ping-probe@1.0.0.0/x.bend as X\n"
    + "def main() -> Nat:\n  T.two\n");
  fresh();
  await bend([path.join(TMP, "pkg.bend")], { BEND_HUB: ORIGIN });
  const ua = (s: string) => s.endsWith(" bend/" + ver);
  check("the publish, a package, a name and the check carry User-Agent:"
    + " bend/" + ver, [/^POST \/ /, /^GET \/0x[0-9a-f]{32}\/manifest /,
    /^GET \/name\/bend-ping-probe@1\.0\.0\.0 /, /^GET \/check /]
    .every((re) => seen.some((s) => re.test(s))
    && seen.filter((s) => re.test(s)).every(ua)));
  const proj = path.join(TMP, "proj");
  const pkg  = "0x0123456789abcdef0123456789abcdef";
  fs.mkdirSync(path.join(proj, "lib", pkg), { recursive: true });
  fs.mkdirSync(path.join(proj, "lib", "names"));
  fs.writeFileSync(path.join(proj, "lib", "names", "bend-ping-probe@1.0.0.0"),
    pkg + "\n");
  fs.writeFileSync(path.join(proj, "bunfig.toml"), 'preload = ["./p.ts"]\n');
  fs.writeFileSync(path.join(proj, "p.ts"),
    'require("node:fs").writeFileSync("preloaded", "");\n');
  fs.writeFileSync(path.join(proj, ".env"), "BEND_LIB=./lib\n");
  fs.writeFileSync(path.join(proj, "lib", pkg, "x.bend"),
    "import Base\ndef five() -> Nat:\n  5n\n");
  fs.writeFileSync(path.join(proj, "main.bend"), "import Base\nimport"
    + " bend-ping-probe@1.0.0.0/x.bend as X\ndef main() -> Nat:\n  X.five\n");
  const own = await lib.exec(BIN, ["main.bend"], undefined, 25_000, { HOME,
    PATH: PATHS, BEND_ORIGIN: ORIGIN, BEND_HUB: ORIGIN,
    BEND_NO_TELEMETRY: "1" }, proj);
  check("a project's bunfig.toml and .env do nothing: " + own.out + own.err,
    !fs.existsSync(path.join(proj, "preloaded")) && own.code === 1
    && own.err.includes("a package named bend-ping-probe@1.0.0.0 on " + ORIGIN));
  script = script.replace(sum, "0".repeat(64));
  const fake = await install();
  script = script.replace("0".repeat(64), sum);
  check("a tampered sha256 installs nothing", fake.code === 1
    && fake.err.includes("does not match") && (await bend(["version"]))
    .out === "bend " + ver + "\n");
  const fakes = path.join(TMP, "fakes");
  fs.mkdirSync(fakes);
  fs.writeFileSync(path.join(fakes, "uname"), "#!/bin/sh\n"
    + "case $1 in -s) echo \"$OS\";; *) echo \"$CPU\";; esac\n",
  { mode: 0o755 });
  const win  = await install({ PATH: fakes + ":" + PATHS,
    OS: "MINGW64_NT-10.0", CPU: "x86_64" });
  const mips = await install({ PATH: fakes + ":" + PATHS, OS: "Linux",
    CPU: "mips" });
  check("a Windows or a MIPS uname is refused in one line", win.code === 1
    && win.err === "bend: Bend needs Linux, macOS or WSL.\n" && mips.code === 1
    && mips.err === "bend: mips is not supported: Bend runs on arm64 and"
    + " x64.\n");
  const ping = await (await fetch(ORIGIN + "/ping", { method: "POST",
    body: "{}" })).json() as Record<string, string>;
  const fall = await (await fetch(ORIGIN + "/dl/latest.json"))
    .json() as Record<string, string>;
  check("an old launcher's ping and its fallback name no sha256 and the"
    + " move", ping.ver === ver && ping.notice.includes(MOVED)
    && ping.sha256 === undefined && fall.ver === ver
    && fall.sha256 === undefined);
} catch (e) {
  check(String(e), false);
} finally {
  hub.kill();
  caddy.stop(true);
  fs.rmSync(TMP, { recursive: true, force: true });
}
if (!lib.GATE) {
  for (const f of fails) {
    console.log("FAIL " + f);
  }
}
lib.verdict(total - fails.length, total);
