#!/usr/bin/env bun
// Runs every test under tests/ on the cluster. The tests split into one
// shard per live mini; each shard is an aggregator that imports its tests,
// sent to its mini, which checks and interprets every module through `bend
// main.bend --checkup`, builds each runnable test alone (`bend t.bend -o t.js
// -o t`: clang -O3, Metal; ten at a time), runs each binary with a `!` once
// untimed (the first launch compiles its Metal shader, which the node then
// caches by source), then runs each program once natively and once under
// bun, each under a 5 s alarm. A test passes when its check, its interpreted
// run, its JS run and its C run all print its `#|` lines; a test whose main
// the compiler refuses to print (a function, a Type, an erased or dependent
// field) is checked and interpreted only; a foreign def with no twin for a
// lane drops that lane; any other build failure fails its lanes. Meanwhile,
// here, one process compares the comptime twins' C, then compiles four
// state fixtures over and over (state_run).

import * as fs from "node:fs";
import * as path from "node:path";

import * as Bend from "../bend2/bend.ts";
import * as Comp from "../bend2/comp.ts";
import * as lib from "./_lib";

// Types
// =====

type Test = {
  name: string;
  src: string;
  want: string;
  main: boolean;
  lanes: string[];
};

type Got = Record<string, string>;

type Fail = { name: string; probe: string; want: string; got: string };

// Constants
// =========

const TESTS = path.join(lib.ROOT, "tests");

const MARK = "@@B4";

const BUN = lib.BUN;

// the head of a failed check's output, or of a failed program's
const FAILS = /^(SOME PROOFS FAIL|Error:)/;

// Test
// ====

function test_read(dir: string, file: string): Test {
  const src = fs.readFileSync(path.join(TESTS, dir, file), "utf8");
  const want = src.split("\n").filter((l) => l.startsWith("#|"))
    .map((l) => l.slice(2)).join("\n");
  const effs = [...src.matchAll(/^\s*import "\.\/[a-z0-9_]+\.(c|js)"$/gm)]
    .map((m) => m[1]);
  // A program compiles only over Base (its IO runs main): a test without
  // it checks and interprets alone.
  const lanes = ["js", "c"].filter((l) => /^(?:[\t\r ]*(?:(?:#|import[\t ])[^\n]*)?\n)*[\t ]*import[\t ]+Base(?=[\t\r ]*(?:#|\n|$))/.test(src)
    && (effs.length === 0 || effs.includes(l)));
  return { name: dir + "_" + path.basename(file, ".bend"), src,
    want: tidy(want), main: /^(def|law) main(\(|:)/m.test(src), lanes };
}

function tidy(text: string): string {
  return text.replace(/[ \t]+$/gm, "").trim();
}

function test_path(t: Test): string {
  return t.name.replace("_", "/") + ".bend";
}

// a test that fails: its check (SOME PROOFS FAIL) or its program (Error:)
function test_fails(t: Test): boolean {
  return FAILS.test(t.want);
}

function test_runs(shard: Test[]): Test[] {
  return shard.filter((t) => t.main && t.lanes.length > 0 && !test_fails(t));
}

function test_probes(t: Test, got: Got): string[] {
  if (!t.main || test_fails(t)) {
    return ["check"];
  }
  const shown = !/^Error: main's type .* cannot be printed/m.test(got.left ?? "");
  return ["check", "interp", ...shown ? t.lanes : []];
}

function test_judge(t: Test, got: Got): Fail[] {
  const fails: Fail[] = [];
  for (const probe of test_probes(t, got)) {
    const seen = got[probe === "interp" ? "check" : probe] ?? got.left
      ?? "(no answer from the node)";
    const ok = probe === "check" && t.main && !test_fails(t)
      ? !FAILS.test(seen) : seen === t.want;
    if (!ok) {
      fails.push({ name: t.name, probe, want: t.want, got: seen });
    }
  }
  return fails;
}

// Shard
// =====

function shard_split(tests: Test[], count: number): Test[][] {
  const shards: Test[][] = Array.from({ length: count }, () => []);
  const sizes = shards.map(() => 0);
  for (const t of [...tests].sort((a, b) => b.src.length - a.src.length)) {
    const at = sizes.indexOf(Math.min(...sizes));
    shards[at].push(t);
    sizes[at] += t.src.length + 2000;
  }
  return shards.filter((s) => s.length > 0);
}

// The shard's aggregator and build list ride in the script, over the pack
// every shard shares. A build that fails leaves its message in
// <name>.left: the test's lanes then read it as their answer, so a
// program the compiler cannot build fails the gate. The io tests write
// fixed names in /tmp, which every Mac user shares: the shard removes its
// own before and after, so a user's leftovers never fail another's run.
function shard_script(shard: Test[], tag: number): string {
  const runs = test_runs(shard);
  const bangs = runs.filter((t) => /!\(/.test(t.src)).map((t) => t.name);
  const main = shard.map((t) =>
    "import ./tests/" + test_path(t) + " as " + t.name + "\n").join("");
  const build = runs.map((t) => [t.name, "tests/" + test_path(t),
    ...t.lanes.map((l) => "-o " + t.name + (l === "js" ? ".js" : ""))]
    .join(" ") + "\n").join("");
  const file = (name: string, text: string): string =>
    `cat > ${name} <<'${MARK}'\n${text}${MARK}\n`;
  const tmp = `find /tmp/ -maxdepth 1 -user $(id -u) \\( -name 'bend_io_*'`
    + ` -o -name 'bend_gfx_*' \\) -delete 2>/dev/null;`;
  const probe = (kind: string, cmd: string): string =>
    `echo "${MARK} ${kind} $m"; perl -e 'alarm 5; exec @ARGV' ${cmd} 2>&1;`
    + ` echo "${MARK} exit $?";`;
  return `export BUN_JSC_maxPerThreadStackUsage=33554432;`
    + ` d=$HOME/bend-test/${tag}; rm -rf $d; mkdir -p $d; cd $d; tar -xzf -;`
    + ` ${tmp}\n`
    + file("main.bend", main) + file("build.txt", build)
    + ` echo "${MARK} checkup"; ${BUN} bend2/main.ts main.bend --checkup 2>&1;`
    + ` xargs -P 10 -L 1 sh -c 'm=$1; shift; ${BUN} bend2/main.ts "$@"`
    + ` > $m.left 2>&1 && rm $m.left' -- < build.txt; echo "${MARK} built";`
    + ` for m in ${bangs.join(" ")}; do perl -e 'alarm 60; exec @ARGV' ./$m`
    + ` >/dev/null 2>&1; done; for m in ${runs.map((t) => t.name).join(" ")};`
    + ` do if [ -f $m.left ]; then echo "${MARK} left $m"; cat $m.left;`
    + ` else ${probe("c", "./$m")} ${probe("js", BUN + " $m.js")} fi; done;`
    + ` cd; rm -rf $d; ${tmp}`;
}

function shard_parse(shard: Test[], out: string): Map<string, Got> {
  const gots = new Map<string, Got>(shard.map((t) => [t.name, {}]));
  const parts = out.split(new RegExp("^" + MARK + " ", "m")).slice(1);
  let last: [Got, string] | null = null;
  for (const part of parts) {
    const nl = part.indexOf("\n");
    const head = part.slice(0, nl).trim().split(" ");
    const body = part.slice(nl + 1);
    if (head[0] === "checkup") {
      const secs = body.split(/^--- \.\/tests\/([A-Za-z0-9_/]+)\.bend ---\n/m);
      for (let i = 1; i + 1 < secs.length; i += 2) {
        const got = gots.get(secs[i].replace("/", "_"));
        if (got !== undefined) {
          got.check = tidy(secs[i + 1]);
        }
      }
    } else if (head[0] === "c" || head[0] === "js" || head[0] === "left") {
      const got = gots.get(head[1]);
      last = got === undefined || head[0] === "left" ? null : [got, head[0]];
      if (got !== undefined) {
        got[head[0]] = tidy(body);
      }
    } else if (head[0] === "exit" && last !== null && head[1] !== "0") {
      const [got, kind] = last;
      const tail = head[1] === "142" ? "timeout" : "exit " + head[1];
      got[kind] = got[kind] === "" ? tail : got[kind] + "\n" + tail;
    }
  }
  return gots;
}

async function shard_run(shard: Test[], pack: Buffer, tag: number,
  node: number, fails: Fail[]): Promise<void> {
  const got = await lib.ssh(node, shard_script(shard, tag), pack,
    20 * 60 * 1000);
  fs.mkdirSync("/tmp/bend-test", { recursive: true });
  fs.writeFileSync("/tmp/bend-test/" + String(tag) + ".txt", got.out + got.err);
  if (!got.out.includes(MARK + " built")) {
    throw new Error("node", { cause: got.err });
  }
  const gots = shard_parse(shard, got.out);
  for (const t of shard) {
    fails.push(...test_judge(t, gots.get(t.name) ?? {}));
  }
}

// State
// =====

// The comptime twins promise the same C up to FID names. Keep every other
// byte, naming each FID by its numeric declaration. Only tmpl may declare
// a List.map instance: without it, the twins match without the template.
async function comptime_run(): Promise<Fail[]> {
  const fail = (probe: string, want: string, got: string): Fail[] =>
    [{ name: "comptime_tmpl", probe, want, got }];
  const emit = async (file: string): Promise<string> => {
    const book = Bend.book_nil();
    await Bend.book_load(book, path.join(TESTS, "comptime", file + ".bend"),
      "", new Map());
    Bend.book_valid(book);
    return Comp.compile_book(book);
  };
  // main awaits this only after the shards, so a twin that fails to check
  // must fail here, not reject (its shard fails it too)
  let c: string[];
  try {
    c = [await emit("hand"), await emit("tmpl")];
  } catch {
    return fail("C matches hand", "both twins compile", "a twin fails");
  }
  const maps = c.map((s) => /^#define FID_List_map\w* /m.test(s));
  if (maps[0] || !maps[1]) {
    return fail("only tmpl declares List.map", "false true",
      maps.join(" "));
  }
  const [hand, tmpl] = c.map((s) => {
    const ids = new Map([...s.matchAll(/^#define (FID_\w+) (\d+)$/gm)]
      .map((m) => [m[1], "FID_" + m[2]]));
    return s.replace(/\bFID_\w+\b/g, (k) => ids.get(k) ?? k).split("\n");
  });
  for (let i = 0; i < hand.length || i < tmpl.length; ++i) {
    if (hand[i] !== tmpl[i]) {
      return fail("C matches hand at line " + String(i + 1),
        hand[i] ?? "(end of file)", tmpl[i] ?? "(end of file)");
    }
  }
  return [];
}

// After comparing the comptime twins, check the four state fixtures.
// A process may compile many books (the bun loader, io_run), and none may
// leave state for the next: #976 kept every book's probes, with the same
// bytes. Each fixture loads and checks twice. Its first copy's C and JS
// are the baseline (compiled after the earlier fixtures' baselines, not in
// a fresh process); its second copy must match them, bytes and probes left,
// at every step: C, C, JS, C, normalization, JS, C, the next fixture (+),
// C, JS. It builds and runs nothing: the shards run these programs.
async function state_run(): Promise<Fail[]> {
  const fails = await comptime_run();
  const files = ["reg/borrow_loop", "run/harness_cache_compile",
    "run/harness_cache_purity", "run/harness_cache_stats"];
  const books: Bend.Book[][] = [];
  for (const file of files) {
    const copies = [Bend.book_nil(), Bend.book_nil()];
    for (const book of copies) {
      await Bend.book_load(book, path.join(TESTS, file + ".bend"), "",
        new Map());
      Bend.book_valid(book);
    }
    books.push(copies);
  }
  const emit = (book: Bend.Book, lane: string): string =>
    Bun.hash(lane === "c" ? Comp.compile_book(book) : Comp.js_book(book))
    + " bytes, " + String(Comp.PROBES.length) + " probes";
  const base = books.map(([book]): Record<string, string> =>
    ({ c: emit(book, "c"), js: emit(book, "js") }));
  for (const [i, file] of files.entries()) {
    const book = books[i][1];
    // the steps on this fixture's second copy; + is the next fixture's
    for (const step of "c c js c snf js c +c +js c js".split(" ")) {
      if (step === "snf") {
        for (const def of Object.values(book.tlds)) {
          if (def.$ === "Def" && def.v !== null) {
            Bend.term_snf(book, def.v);
          }
        }
        continue;
      }
      const at = step[0] === "+" ? (i + 1) % files.length : i;
      const lane = step[0] === "+" ? step.slice(1) : step;
      const got = emit(books[at][1], lane);
      if (got !== base[at][lane]) {
        fails.push({ name: files[at].replace("/", "_"),
          probe: "state " + file + " " + step, want: base[at][lane], got });
      }
    }
  }
  return fails;
}

// Main
// ====

if (import.meta.main) {
  const tests = fs.readdirSync(TESTS).sort().flatMap((dir) =>
    fs.readdirSync(path.join(TESTS, dir)).filter((f) => f.endsWith(".bend"))
      .sort().map((f) => test_read(dir, f)));
  const state = state_run();
  const nodes = await lib.node_lock();
  // Every test's source goes to every shard: a test may import another,
  // or a module from a subdirectory.
  const pack = lib.pack(TESTS);
  const shards = shard_split(tests, nodes.length);
  const fails: Fail[] = [];
  await lib.node_pool(nodes, shards.map((shard, tag) => (node: number) =>
    shard_run(shard, pack, tag, node, fails)));
  fails.push(...await state);
  fails.sort((a, b) => a.name < b.name ? -1 : 1);
  if (!lib.GATE) {
    for (const f of fails) {
      console.log("FAIL " + f.name + " [" + f.probe + "]");
      console.log("  expected: " + f.want.replace(/\n/g, "\\n"));
      console.log("  observed: " + f.got.replace(/\n/g, "\\n"));
    }
  }
  const bad = new Set(fails.map((f) => f.name));
  lib.verdict(tests.length - bad.size, tests.length);
}
