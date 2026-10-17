#!/usr/bin/env bun
// CgenCache: fail-closed whole-.c cache with exact foreign-fragment refresh.
//
// Default normal-path integration (no flag, no approval gate): cli_emit asks
// here first; a hit serves byte-identical C without running compile_book, a
// proven foreign-only delta derives refreshed C by exact substitution, and
// every uncertain or changed non-FFI input falls through to a full rebuild.
// Reuse fails closed (miss/invalid never serve); generation always runs on
// doubt, so output correctness never depends on the cache.
//
// Compiler-free I/O only: hashing, byte comparison, atomic file writes. This
// module never launches a compiler or linker. A hit proves byte-identity with
// previously stored bytes, not compiler determinism and not speed.

import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import * as url from "node:url";

import * as Bend from "./bend.ts";

export const KEY_SCHEMA = "cgen-ts-cache-key-v1";
export const RECEIPT_SCHEMA = "cgen-ts-cache-receipt-v1";

const HERE = path.dirname(url.fileURLToPath(import.meta.url));
const COMPILER_FILES = ["comp.ts", "bend.ts", "main.ts", "cache.ts",
  "base.bend"];

export function cache_dir(): string {
  return process.env.CGEN_CACHE_DIR
    ?? path.join(os.tmpdir(), "cgen-ts-cache");
}

function sha_bytes(data: Buffer | string): string {
  return crypto.createHash("sha256").update(data).digest("hex");
}

function sha_file(p: string): string | null {
  try {
    return sha_bytes(fs.readFileSync(p));
  } catch {
    return null;
  }
}

function canon(value: unknown): string {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value) ?? "null";
  }
  if (Array.isArray(value)) {
    return "[" + value.map(canon).join(",") + "]";
  }
  return "{" + Object.keys(value as Record<string, unknown>).sort()
    .map((k) => JSON.stringify(k) + ":" + canon(
      (value as Record<string, unknown>)[k])).join(",") + "}";
}

export interface KeyResult {
  key: string;
  manifest: Record<string, unknown>;
}

function file_rows(paths: string[]): { path: string; sha256: string;
  bytes: number }[] | null {
  const rows: { path: string; sha256: string; bytes: number }[] = [];
  for (const p of paths) {
    let real: string;
    try {
      real = fs.realpathSync(p);
    } catch {
      return null;
    }
    const digest = sha_file(real);
    if (digest === null) {
      return null;
    }
    rows.push({ path: real, sha256: digest,
      bytes: fs.statSync(real).size });
  }
  return rows;
}

// The complete fail-closed key. null means uncertain: rebuild, never reuse.
export function key_of(book: Bend.Book, seen: Map<string, string | null>,
  entry: string): KeyResult | null {
  const compiler = file_rows(COMPILER_FILES.map((f) => path.join(HERE, f)));
  if (compiler === null) {
    return null;
  }
  let entryReal: string;
  try {
    entryReal = fs.realpathSync(entry);
  } catch {
    return null;
  }
  const closure = file_rows([...seen.keys()].sort());
  if (closure === null) {
    return null;
  }
  const foreign: string[] = [];
  for (const tld of Object.values(book.tlds)) {
    if (tld.$ === "Def" && (tld as Bend.Def & { i?: string[] }).i
      !== undefined) {
      foreign.push(...((tld as Bend.Def & { i: string[] }).i));
    }
  }
  const effs = file_rows([...new Set(foreign)].sort());
  if (effs === null) {
    return null;
  }
  const bun = (globalThis as { Bun?: { version?: string } }).Bun?.version
    ?? "nobun";
  const manifest = {
    schema: KEY_SCHEMA,
    k1_compiler: compiler,
    k2_toolchain: { bun_version: bun },
    k3_closure_ordered: book.order,
    k3_closure_files: closure,
    k3_namespaces: [...seen.entries()].sort(),
    k3_entry: entryReal,
    k4_laws_proof: path.basename(entryReal) === "PROOF.bend",
    k5_effs: effs,
    k6_emit_kind: "c",
    k7_lib_hub: [],
    k8_diagnostics: process.env.BEND_COMPILER_DIAGNOSTICS ?? "",
    k9_host: { platform: process.platform, arch: process.arch,
      cc: process.env.CC ?? "",
      no_telemetry: process.env.BEND_NO_TELEMETRY ?? "" },
  };
  return { key: sha_bytes(canon(manifest)), manifest };
}

function entry_paths(dir: string, key: string): Record<string, string> {
  const base = path.join(dir, key);
  return { base, receipt: path.join(base, "receipt.json"),
    digest: path.join(base, "receipt.sha256"),
    artifact: path.join(base, "artifact.c") };
}

export type Lookup =
  | { status: "hit"; key: string; artifact: string }
  | { status: "miss"; key: string }
  | { status: "uncertain" }
  | { status: "invalid"; key: string; reason: string };

export function lookup(dir: string,
  keyed: KeyResult | null): Lookup {
  if (keyed === null) {
    return { status: "uncertain" };
  }
  const paths = entry_paths(dir, keyed.key);
  if (!fs.existsSync(paths.artifact)) {
    return { status: "miss", key: keyed.key };
  }
  try {
    const raw = fs.readFileSync(paths.receipt);
    const recorded = fs.readFileSync(paths.digest, "utf8").trim();
    if (sha_bytes(raw) !== recorded) {
      return { status: "invalid", key: keyed.key,
        reason: "receipt digest mismatch" };
    }
    const receipt = JSON.parse(raw.toString());
    if (receipt.schema !== RECEIPT_SCHEMA || receipt.key !== keyed.key) {
      return { status: "invalid", key: keyed.key,
        reason: "receipt schema/key mismatch" };
    }
    const bytes = fs.readFileSync(paths.artifact);
    if (sha_bytes(bytes) !== receipt.artifact_sha256) {
      return { status: "invalid", key: keyed.key,
        reason: "artifact hash mismatch" };
    }
    return { status: "hit", key: keyed.key, artifact: paths.artifact };
  } catch {
    return { status: "invalid", key: keyed.key, reason: "entry unreadable" };
  }
}

function atomic_json(file: string, obj: unknown): void {
  const tmp = file + ".tmp-" + process.pid;
  fs.writeFileSync(tmp, JSON.stringify(obj, null, 2) + "\n");
  const fd = fs.openSync(tmp, "r");
  fs.fsyncSync(fd);
  fs.closeSync(fd);
  fs.renameSync(tmp, file);
}

export function store(dir: string, keyed: KeyResult,
  c: string): "stored" | "existing" | "refused" {
  if (/\bundefined\b/.test(c)) {
    return "refused";
  }
  const paths = entry_paths(dir, keyed.key);
  if (fs.existsSync(paths.base)) {
    return "existing";
  }
  try {
    fs.mkdirSync(dir, { recursive: true });
    const tmp = path.join(dir, ".tmp-" + process.pid + "-" + keyed.key);
    fs.mkdirSync(tmp, { recursive: true });
    const receipt = { schema: RECEIPT_SCHEMA, key: keyed.key,
      manifest: keyed.manifest, artifact_sha256: sha_bytes(c),
      artifact_bytes: c.length, compiler_invoked: false };
    atomic_json(path.join(tmp, "receipt.json"), receipt);
    const digest = sha_file(path.join(tmp, "receipt.json")) ?? "";
    fs.writeFileSync(path.join(tmp, "receipt.sha256"), digest + "\n");
    fs.writeFileSync(path.join(tmp, "artifact.c"), c);
    const foreigns = keyed.manifest.k5_effs as
      { path: string; sha256: string }[];
    foreigns.forEach((row, i) => {
      fs.copyFileSync(row.path, path.join(tmp, "foreign-" + i + ".bin"));
    });
    fs.renameSync(tmp, paths.base);
    return "stored";
  } catch {
    return "refused";
  }
}

// Exact foreign-fragment refresh: find one origin entry whose manifest
// differs from current in exactly one foreign file's bytes (same paths,
// same Bend closure, compiler, kind, env), then substitute exactly once.
// Anything else (no candidate, several candidates, several deltas, a
// non-foreign delta, a non-unique boundary) returns null: full rebuild.
export function try_refresh(dir: string, keyed: KeyResult,
  origin_c_of: (key: string) => string | null): string | null {
  let names: string[];
  try {
    names = fs.readdirSync(dir);
  } catch {
    return null;
  }
  const cur = keyed.manifest;
  const curForeign = cur.k5_effs as { path: string; sha256: string }[];
  let match: { key: string; receipt: Record<string, unknown>;
    oldPath: string; oldBytes: Buffer } | null = null;
  for (const name of names) {
    if (name.startsWith(".") || name.length !== 64) {
      continue;
    }
    let receipt: Record<string, unknown>;
    try {
      receipt = JSON.parse(fs.readFileSync(
        path.join(dir, name, "receipt.json"), "utf8"));
    } catch {
      continue;
    }
    if (receipt.schema !== RECEIPT_SCHEMA) {
      continue;
    }
    const cand = receipt.manifest as Record<string, unknown>;
    const same = ["k1_compiler", "k2_toolchain", "k3_closure_ordered",
      "k3_closure_files", "k3_namespaces", "k3_entry", "k4_laws_proof",
      "k6_emit_kind", "k7_lib_hub", "k8_diagnostics", "k9_host"].every(
      (k) => canon(cand[k]) === canon(
        (cur as Record<string, unknown>)[k]));
    if (!same) {
      continue;
    }
    const candForeign = cand.k5_effs as { path: string; sha256: string }[];
    if (candForeign.length !== curForeign.length
      || !candForeign.every((row, i) => row.path === curForeign[i].path)) {
      continue;
    }
    const diffs = candForeign.map((row, i) => i)
      .filter((i) => candForeign[i].sha256 !== curForeign[i].sha256);
    if (diffs.length !== 1) {
      continue;
    }
    let oldBytes: Buffer;
    try {
      oldBytes = fs.readFileSync(
        path.join(dir, name, "foreign-" + diffs[0] + ".bin"));
    } catch {
      continue;
    }
    if (sha_bytes(oldBytes) !== candForeign[diffs[0]].sha256) {
      continue;
    }
    if (match !== null) {
      return null;
    }
    match = { key: name, receipt, oldPath: curForeign[diffs[0]].path,
      oldBytes };
  }
  if (match === null) {
    return null;
  }
  const originC = origin_c_of(match.key);
  if (originC === null) {
    return null;
  }
  let newBytes: Buffer;
  try {
    newBytes = fs.readFileSync(match.oldPath);
  } catch {
    return null;
  }
  if (sha_bytes(newBytes)
    !== (curForeign.find((r) => r.path === match.oldPath)?.sha256 ?? "")) {
    return null;
  }
  const oldText = match.oldBytes.toString("utf8");
  const first = originC.indexOf(oldText);
  if (first < 0 || originC.indexOf(oldText, first + 1) >= 0) {
    return null;
  }
  return originC.slice(0, first) + newBytes.toString("utf8")
    + originC.slice(first + oldText.length);
}

function log_event(dir: string, event: Record<string, unknown>): void {
  try {
    fs.mkdirSync(dir, { recursive: true });
    fs.appendFileSync(path.join(dir, "log.jsonl"),
      JSON.stringify(event) + "\n");
  } catch {
    // Logging is audit-only; never break the build on it.
  }
}

export interface EmitResult {
  c: string;
  event: "hit" | "refresh" | "compile" | "rebuild-uncertain";
  ms: number;
  key: string | null;
}

// Default-path orchestration. compile() runs only on miss-without-refresh
// or uncertainty: a hit observably skips the intended generation work.
export function emit_c_cached(book: Bend.Book,
  seen: Map<string, string | null>, entry: string,
  compile: () => string): EmitResult {
  const dir = cache_dir();
  const began = performance.now();
  const done = (c: string, event: EmitResult["event"],
    key: string | null): EmitResult => {
    const ms = performance.now() - began;
    log_event(dir, { at: new Date().toISOString(), event, key, ms });
    return { c, event, ms, key };
  };
  // Diagnostics are observable compiler output. A cache hit skips the
  // compiler traversal that produces them, so diagnostic requests always
  // execute the ordinary compiler path.
  if (process.env.BEND_COMPILER_DIAGNOSTICS) {
    return done(compile(), "rebuild-uncertain", null);
  }
  const keyed = key_of(book, seen, entry);
  if (keyed === null) {
    return done(compile(), "rebuild-uncertain", null);
  }
  const found = lookup(dir, keyed);
  if (found.status === "hit") {
    return done(fs.readFileSync(found.artifact, "utf8"), "hit", found.key);
  }
  if (found.status === "invalid") {
    return done(finish_compile(dir, keyed, compile), "compile", keyed.key);
  }
  const origin_c_of = (key: string): string | null => {
    try {
      return fs.readFileSync(
        path.join(dir, key, "artifact.c"), "utf8");
    } catch {
      return null;
    }
  };
  const derived = try_refresh(dir, keyed, origin_c_of);
  if (derived !== null) {
    store(dir, keyed, derived);
    return done(derived, "refresh", keyed.key);
  }
  return done(finish_compile(dir, keyed, compile), "compile", keyed.key);
}

function finish_compile(dir: string, keyed: KeyResult,
  compile: () => string): string {
  const c = compile();
  store(dir, keyed, c);
  return c;
}
