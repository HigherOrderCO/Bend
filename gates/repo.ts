#!/usr/bin/env bun
// The shape of the repo: every tracked file must match one allow line,
// and the capped files below stay under their ttok caps. Anything else in
// the tree is a failure.
//
// THE CAPS ARE PERMANENT: bend.ts 48k, comp.ts 64k, main.ts 16k, safe.ts
// 24k, bendtt.lean 64k, README.md 4k, GUIDE.md 8k, and no other file has a
// cap. Do NOT raise, lower, add or remove a cap without Taelin's explicit
// authorization. A file over its cap is made smaller by simplification,
// never by moving code out.
// evals/ is not counted: it is the models' arena, not the repo's shape.

import * as child from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";

import * as lib from "./_lib";

// Types
// =====

type Rule = { at: RegExp; cap: number };

// Constants
// =========

const RULES: Rule[] = [];

// Allow
// =====

function allow(at: string | RegExp, cap = Infinity): void {
  RULES.push({ at: typeof at === "string" ? new RegExp("^" + at
    .replace(/[.]/g, "\\.") + "$") : at, cap });
}

allow(/^\.github\/ISSUE_TEMPLATE\/(bug|feature|config)\.yml$/);
allow(".github/workflows/repo-gate.yml");
allow(".gitattributes");
allow(".gitignore");
allow("AGENTS.md");
allow("CHANGELOG.md");
allow("README.md", 4000);
allow("WONTFIX.txt");
allow("LICENSE");
allow("flake.nix");
allow("bend2/base.bend");
allow("bend2/bend.ts", 48000);
allow("bend2/comp.ts", 64000);
allow("bend2/main.ts", 16000);
allow("bend2/safe.ts", 24000);
allow("bend2/bendtt.lean", 64000);
allow(/^bend2\/effs\/[a-z0-9_]+\.(c|js)$/);
allow(/^bend2\/pack\/(\.gitignore|package\.json|tsconfig\.json|bun\.lock)$/);
allow(/^bend2\/docs\/(BendRT|BendTT)\/(main\.typ|refs\.bib)$/);
allow("bend2/docs/bend.sublime-syntax");
allow(/^bend2\/docs\/film\/(film\.ts|[a-z]+\.json)$/);
allow("bend2/docs/gen_anim.ts");
allow("bend2/docs/gen_charts.ts");
allow("bend2/docs/gen_gifs.ts");
allow("bend2/docs/gen_pins.ts");
allow(/^bend2\/docs\/intro\/[a-z.]+$/);
allow(/^bench\/checker\/[a-z]+_[0-9]+\/main\.(bend|agda|lean|thy|v)$/);
allow(/^bench\/checker\/_pin_\/[a-z0-9_]+\.txt$/);
allow(/^bench\/runtime\/[a-z-]+\/main\.(bend|c|lean|ts)$/);
allow(/^bench\/runtime\/_pin_\/[a-z0-9_]+\.txt$/);
allow(/^demos\/[a-z0-9_]+\/[A-Za-z0-9_]+\.bend$/);
allow(/^demos\/[a-z0-9_]+\/[A-Za-z_]+\.(c|sh|md)$/);
allow(/^demos\/[a-z0-9_]+\/web\/(index\.html|main\.js|bunfig\.toml)$/);
allow("guide/GUIDE.md", 8000);
allow("guide/EFFECTS.md");
allow("guide/SHADERS.md");
allow(/^paper\/(BendRT|BendTT)\.pdf$/);
allow(/^media\/intro\.(gif|mp4)$/);
allow("media/kind_devil_theorem.mp4");
allow(/^media\/(runtime|checker|parallel)\.gif$/);
allow(/^media\/hero(_dark)?\.gif$/);
allow(/^media\/logo_(bend|hoc)\.png$/);
allow(/^media\/game_[a-z_]+\.gif$/);
allow(/^media\/slash_boss_3d\/[a-z_]+\.wav$/);
allow(/^gates\/(_lib|_run|perf|ping|repo|test|safe|safe_node|safe_diag)\.ts$/);
allow(/^tests\/[a-z]+\/([a-z0-9-]+\/)?[A-Za-z0-9_]+\.bend$/);
allow(/^tests\/[a-z]+\/[a-z0-9_]+\.(c|js)$/);
allow(/^tools\/bend-fmt-lsp\/(\.gitignore|README\.md|package\.json|package-lock\.json|tsconfig\.json)$/);
allow(/^tools\/bend-fmt-lsp\/src\/(formatter|server)\.ts$/);
allow(/^tools\/bend-fmt-lsp\/src\/test\/[a-z_]+\.test\.ts$/);
allow(/^tools\/bend-lint\/(README\.md|package\.json|tsconfig\.json)$/);
allow(/^tools\/bend-lint\/src\/(lint\.ts|seam\.ts|lint\.test\.ts)$/);
allow(/^tools\/bend-lint\/src\/bend\/(lint\.bend|effects\.js|sample\.bend)$/);
allow(/^tools\/bend-lint\/rules\/[a-z_]+(\.test)?\.(ts|bend)$/);

// Gate
// ====

function ttok(file: string): number {
  const got = child.spawnSync("ttok", [], { input: fs.readFileSync(file) });
  const n = Number(got.stdout?.toString().trim() || NaN);
  if (got.status !== 0 || !(n > 0)) {
    throw new Error("ttok counted nothing for " + file + " (pipx install ttok)");
  }
  return n;
}

function gate(): string[] {
  const fails: string[] = [];
  const files = child.execFileSync("git", ["ls-files"], { cwd: lib.ROOT,
    encoding: "utf8" }).trim().split("\n");
  for (const file of files) {
    if (file.startsWith("evals/")) continue;
    const rule = RULES.find((r) => r.at.test(file));
    if (rule === undefined) {
      fails.push(file + ": not in the allow list");
      continue;
    }
    const full = path.join(lib.ROOT, file);
    const size = fs.statSync(full).size;
    const n = size <= rule.cap ? size : ttok(full);
    if (n > rule.cap) {
      fails.push(file + ": " + String(n) + " > " + String(rule.cap) + " ttok");
    }
  }
  return fails;
}

// Main
// ====

if (import.meta.main) {
  const fails = gate();
  if (!lib.GATE) {
    for (const f of fails) {
      console.log("FAIL " + f);
    }
  }
  lib.verdict(RULES.length - Math.min(RULES.length, fails.length),
    RULES.length);
}
