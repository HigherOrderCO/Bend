#!/usr/bin/env bun
// The --verdict gate: for each file of a corpus, bend2's verdict against
// BendTT's, from one `bend <f> --verdict` run on a mini (ALL PROOFS CHECK, or
// SOME PROOFS FAIL and why; safe_node.ts adds why to a mismatch), each
// under a 30 s alarm. Classes: agree (both check), u unsafe (a def relies
// on @unsafe or foreign code: the goal allows it), bend2 rejects (--verdict
// stops there, so the kernel never accepts more), - out of scope (the
// kernel cannot express a def), ! false reject (bend2 checks, the kernel
// rejects), t deadline timeout, e infrastructure error. Optional diagnostics
// cannot change the verdict class; their failures are reported separately.
// The table lands in .tmp/safe/<corpus>.txt; the hub corpus is a pulled
// BendHub store ($SAFE_HUB), sent as BEND_LIB. The kernel binary is built
// on a Lean node (bendtt.lean's CLI). Each worktree stages in its own
// directory on the nodes, so two gates can run at once.
//
//   bun gates/safe.ts tests|hub <bendtt binary>
import * as child from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { ROOT, node_pool, ssh } from "./_lib.ts";
import { judge, type Got } from "./safe_node.ts";

const HUB = process.env.SAFE_HUB ?? "";
const corpus = process.argv[2];
const bin = path.resolve(process.argv[3] ?? path.join(ROOT, ".tmp", "bendtt"));
const OUT = path.join(ROOT, ".tmp", "safe", process.env.GATE_OUT ?? "");
const DIR = "$HOME/bend-safe-gate";
const PAR = 8;
const find = (dir: string, cwd: string) => child.spawnSync("find", [dir, "-name", "*.bend"], { cwd, encoding: "utf8" })
  .stdout.split("\n").filter((l) => l !== "").sort();
let all: string[];
const stage = fs.mkdtempSync("/tmp/bend-safe-");
fs.copyFileSync(bin, path.join(stage, "bendtt"));
const base = ["-czf", "-", "-s", ",^\\./,lib/,", "--exclude", "bend2/docs", "--exclude", "bend2/pack",
  "--exclude", "*.bendtt", "-C", ROOT, "bend2", "gates/safe_node.ts", "gates/safe_diag.ts", "-C", stage, "bendtt"];
let tar: Buffer;
if (corpus === "tests") {
  all = find("tests", ROOT);
  tar = child.spawnSync("tar", [...base, "-C", ROOT, "tests"], { maxBuffer: 1 << 28 }).stdout;
} else if (corpus === "hub" && fs.existsSync(HUB)) {
  all = find(".", HUB).map((f) => "lib/" + f.slice(2));
  const lib = child.spawnSync("find", [".", "-name", "*.bend", "-o", "-path", "./names/*", "-type", "f"], { cwd: HUB, encoding: "utf8" })
    .stdout.split("\n").filter((l) => l !== "");
  tar = child.spawnSync("tar", [...base, "-C", HUB, ...lib], { maxBuffer: 1 << 28 }).stdout;
} else {
  throw new Error("usage: [SAFE_HUB=<hub store>] bun gates/safe.ts tests|hub <bendtt binary>");
}
const nodes = Array.from({ length: 0xe9 - 0xce + 1 }, (_, i) => 0xce + i).filter((n) => n !== 0xda && n !== 0xe6);
const tag = DIR + "/" + path.basename(ROOT) + "/" + corpus;
const live = (await Promise.all(nodes.map(async (node) =>
  (await ssh(node, "mkdir -p " + tag + " && cd " + tag + " && tar xzf - && chmod +x bendtt", tar, 120000)).code === 0 ? node : -1)))
  .filter((n) => n >= 0);
const shards: string[][] = [];
for (let i = 0; i < all.length; i += PAR) {
  shards.push(all.slice(i, i + PAR));
}
const gots: Got[] = [];
const t0 = Date.now();
await node_pool(live, shards.map((fs_) => async (node: number) => {
  const script = "cd " + tag + " && BEND_LIB=" + tag + "/lib BENDTT=" + tag + "/bendtt PAR=" + PAR
    + " /usr/local/bun/bin/bun gates/safe_node.ts <<'EOF'\n" + fs_.join("\n") + "\nEOF\n";
  const got = await ssh(node, script, undefined, 120000);
  try {
    gots.push(...JSON.parse(got.out));
  } catch {
    // a dead node (ssh exits 255) hands its shard back to node_pool
    if (got.code === 255) {
      throw new Error("node", { cause: got.err });
    }
    for (const f of fs_) {
      gots.push({ f, code: -1, ms: 0, timeout: false,
        out: "shard failed on " + node + ": " + got.err });
    }
  }
}));
gots.sort((a, b) => a.f < b.f ? -1 : 1);
const rows = gots.map((g) => {
  const [c, reason] = judge(g);
  const r = reason + (g.diagnostic_error
    ? " [diagnostic failed: " + g.diagnostic_error.replace(/\s+/g, " ").slice(0, 300) + "]" : "");
  return [c, r, g.f, g.out] as const;
});
fs.mkdirSync(OUT, { recursive: true });
fs.writeFileSync(path.join(OUT, corpus + ".json"), JSON.stringify(gots));
fs.writeFileSync(path.join(OUT, corpus + ".txt"), rows.map(([c, r, f]) => c + " " + f + "  " + r).join("\n") + "\n");
const tally = new Map<string, number>();
for (const [c] of rows) {
  tally.set(c, (tally.get(c) ?? 0) + 1);
}
const agree = rows.filter(([c, r]) => c === " " && r === "agree").length;
const b2rej = rows.filter(([c, r]) => c === " " && r !== "agree").length;
console.log(corpus + ": " + rows.length + " files on " + live.length + " nodes in " + (Date.now() - t0) + " ms");
console.log("agree (both check): " + agree + ", u unsafe: " + (tally.get("u") ?? 0) + ", bend2 rejects: " + b2rej
  + ", - out of scope: " + (tally.get("-") ?? 0) + ", ! false reject: " + (tally.get("!") ?? 0)
  + ", t timeout: " + (tally.get("t") ?? 0) + ", e infrastructure: " + (tally.get("e") ?? 0));
// the false rejects by failing def, most files first
const why = new Map<string, number>();
for (const [c, r] of rows) {
  if (c === "!") {
    const k = /^In (\S+):/.exec(r)?.[1] ?? r.slice(0, 40);
    why.set(k, (why.get(k) ?? 0) + 1);
  }
}
[...why].sort((a, b) => b[1] - a[1]).forEach(([k, n]) => console.log("  ! " + String(n).padStart(4) + " " + k));
process.exit(0);
