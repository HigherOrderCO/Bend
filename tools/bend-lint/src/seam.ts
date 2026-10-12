// Everything bend-lint knows about bend2's internals: finding (or
// downloading) bend2, patching it as Bun loads it, and every call into it.
// The docs say what the patch does and how drift stops it. Each text edit
// must match exactly once; the wrappers pass every argument through, so
// bend computes what it would without them.

import * as nodeFs from "node:fs";
import * as os from "node:os";
import * as nodePath from "node:path";
import * as url from "node:url";

import type { Book, Ctx, Err, HTerm, LTerm, Name, Quant, Uses } from "bend2/bend.ts";
import type { Span as BendSpan } from "bend2/bend.ts";
import type * as BendModule from "bend2/bend.ts";
import type * as CompModule from "bend2/comp.ts";
import type {
  Diag,
  Fact,
  FactFilter,
  Node,
  Quantity,
  RuleContext,
  Shape,
  Source,
  Span,
  Type,
  View,
} from "./lint.ts";

// Types
// =====

// A file's patch: exact edits, text it must hold once unchanged (needs), a
// tail appended to the file, and names it must declare once and export (the
// tail exports those it does not).
type Patch = { edits: Array<[string, string]>; needs?: string[]; tail: string; exports: string[] };

export type Get = (url: string) => Promise<Response>;

// How bendDir reaches the network, runs `bend`, where it caches, and where
// the bend repo would be; tests give their own. run gives a command's
// stdout, or undefined if it fails.
export type FindOptions = {
  get?: Get;
  run?: (cmd: string[]) => string | undefined;
  cache?: string;
  repo?: string;
};

// comp.ts as the patch exports it.
type Comp = typeof CompModule & { RUNTIME_MAIN: string; js_sat(k: Name): string };

// What bend-lint uses of main.ts, as the patch exports it: book_read throws
// a Check_Fail holding why the check failed.
type Main = {
  book_read(file: string, base?: Book, seen?: Map<string, string | null>): Promise<Book>;
  book_err(e: unknown): string;
  Check_Fail: new (why: unknown) => { why: unknown };
};

// bend2 as load gives it: the patched modules and their folder.
export type Loaded = { Bend: typeof BendModule; Comp: Comp; Main: Main; BEND2: string };

// A source as bend.ts spans point at it.
type File = { str: string; ns: string; al: Record<Name, Name>; path: string };

type Mapper = (s: BendSpan | undefined) => BendSpan | undefined;

// A fact as the checker gives it: `tm` checked (or inferred) as `ty` at
// depth `dep` in `ctx`, in def `def`, demanded `qt` times, using the
// variables in `us`, in book `bok` (a template body has its own). `inst`
// marks an instance's fact.
export type Raw = {
  tm: LTerm;
  ty: HTerm;
  bok: Book;
  ctx: Ctx;
  dep: number;
  def: Name;
  qt: Quant;
  us: Uses;
  inst: boolean;
  spn?: BendSpan;
};

// What the wrappers report for each checked term.
type Report = Omit<Raw, "inst">;

// bend2's own objects, for code that accepts to break when bend2 changes.
export type Unstable = { Bend: typeof BendModule; book: Book; raw: (fact: Fact) => Raw };

// The rule context's operations that ask bend2.
export type Operations = Omit<
  RuleContext,
  "sources" | "root" | "options" | "facts" | "prior" | "diag"
>;

export type Checked = {
  book: Book;
  sources: Source[];
  map: Mapper;
  facts?: Fact[];
  failure?: Diag;
};

// A rule written in Bend, checked and compiled: what its id(), facts() and
// main() give.
type Compiled = { id: string; want: FactFilter | null; main: (args: string[]) => number };

// Constants
// =========

// Marks an error as drift: bend2 changed in a way bend-lint does not follow.
export const DRIFT = Symbol("DriftError");

const HERE = url.fileURLToPath(new URL(".", import.meta.url));
const DRIVE = /^[A-Za-z]:(?=\/)/;
const SHIM = JSON.stringify(url.pathToFileURL(nodePath.join(HERE, "seam.ts")).href);
const SAMPLE = nodePath.join(HERE, "bend", "sample.bend");

export const MARK = "BEND_LINT_PATCH";

const GIT = "https://github.com/bendlang/bend.git/info/refs?service=git-upload-pack";
const RAW = "https://raw.githubusercontent.com/bendlang/bend/";
const RELEASE = /^v\d+\.\d+\.\d+$/;
const DAY = 24 * 60 * 60 * 1000;
const EFF = /^effs\/[\w.-]+$/;
const GIVE = "give a bend checkout with --bend <dir> or BEND_DIR";

// Downloaded bends, one folder per release.
const CACHE = nodePath.join(
  process.env.XDG_CACHE_HOME ??
    (process.platform === "win32"
      ? (process.env.LOCALAPPDATA ?? nodePath.join(os.homedir(), "AppData", "Local"))
      : nodePath.join(os.homedir(), ".cache")),
  "bend-lint",
);

const SHIMMED: Array<[string, string]> = [
  ['import * as fs from "node:fs";', "import { fs } from " + SHIM + ";"],
  ['import * as path from "node:path";', "import { path } from " + SHIM + ";"],
];

const PATCHES: Record<string, Patch> = {
  "bend.ts": {
    edits: [
      ...SHIMMED,
      ["export function term_infer(", "function unseen_term_infer("],
      ["export function term_check(", "function unseen_term_check("],
    ],
    tail: `import { seeInfer, seeCheck } from ${SHIM};\nexport const term_infer = seeInfer(unseen_term_infer);\nexport const term_check = seeCheck(unseen_term_check);\n`,
    exports: [],
  },
  "comp.ts": {
    edits: [],
    needs: ["let cli_args = [];", "function io_run(m) {"],
    tail: "",
    exports: ["RUNTIME_MAIN", "js_sat"],
  },
  "main.ts": { edits: SHIMMED, tail: "", exports: ["book_read", "book_err", "Check_Fail"] },
};

// The bend2 files bend-lint patches and imports.
export const PATCHED = Object.keys(PATCHES);

// Where the wrappers report, set for one check at a time.
export const hook: { see?: (report: Report) => void } = {};

// Text an editor holds unsaved, by real path, set for one check at a time.
// bend.ts reads it in place of the file on disk.
export const unsaved = new Map<string, string>();

// The text bend.ts read in the check running now, by real path. Sources
// take their text from it, so a save during the check cannot set them
// apart from what bend parsed.
let reads: Map<string, string> | undefined;

// The checked base.bend book, with the text of base.bend it was checked
// from: Base is checked once per process, and again only if base.bend
// changes. Only the latest is kept.
let BASE: { key: string; book: Promise<Book> } | undefined;

// The check running now, or done; the next check waits for it.
let queue: Promise<unknown> = Promise.resolve();

const STARTS = new WeakMap<object, number[]>();

// Each checked source's File, and back.
const FILES = new WeakMap<Source, File>();
const SOURCES = new WeakMap<object, Source>();

const QUANTITY: Record<Quant["$"], Quantity> = { None: "erased", Lone: "once", Many: "many" };

// Binders are explicit in checked terms, so Var.v cells are not followed.
// A new term kind is a type error here, and a drift error when walked.
const CHILDREN: { [K in LTerm["$"]]: (tm: Extract<LTerm, { $: K }>) => LTerm[] } = {
  Let: (tm) => [...tm.v, tm.f],
  Lam: (tm) => [tm.f],
  Sub: (tm) => [tm.f],
  App: (tm) => [tm.f, tm.x],
  Ctr: (tm) => tm.x,
  ADT: (tm) => tm.x,
  Mat: (tm) => [tm.h, tm.m],
  Rwt: (tm) => [tm.e, tm.p, tm.f],
  Typ: (tm) => [tm.g],
  Min: (tm) => [tm.a, tm.b],
  All: (tm) => [tm.A, tm.B],
  Eql: (tm) => [tm.a, tm.b, tm.T],
  Ann: (tm) => [tm.x, tm.T],
  Var: () => [],
  Ref: () => [],
  Qnt: () => [],
  Qua: () => [],
  Lit: () => [],
  Efq: () => [],
  Rfl: () => [],
  Hol: () => [],
};

// Functions
// =========

const slash = (p: string): string => p.split(nodePath.sep).join("/");

const held = (p: nodeFs.PathOrFileDescriptor): string | undefined =>
  typeof p === "string" && unsaved.size > 0 && nodeFs.existsSync(p)
    ? unsaved.get(slash(nodeFs.realpathSync(p)))
    : undefined;

// path.posix.resolve from `cwd`, where a drive letter is a root.
export const resolve = (cwd: string, ...ps: string[]): string => {
  const all = [cwd, ...ps].map(slash);
  const drive =
    all
      .filter((p) => DRIVE.test(p))
      .at(-1)
      ?.slice(0, 2) ?? "";
  return drive + nodePath.posix.resolve(...all.map((p) => p.replace(DRIVE, "")));
};

export const relative = (from: string, to: string): string =>
  nodePath.posix.relative(slash(from).replace(DRIVE, ""), slash(to).replace(DRIVE, ""));

// fs and path for bend.ts and main.ts.
export const fs = {
  ...nodeFs,
  realpathSync: (p: nodeFs.PathLike): string => slash(nodeFs.realpathSync(p)),
  readFileSync: ((p: nodeFs.PathOrFileDescriptor, ...rest: unknown[]) => {
    const out =
      held(p) ?? (nodeFs.readFileSync as (...a: unknown[]) => unknown)(p, ...rest);
    if (reads !== undefined && typeof p === "string" && typeof out === "string") {
      reads.set(slash(nodeFs.realpathSync(p)), out);
    }
    return out;
  }) as typeof nodeFs.readFileSync,
};

export const path = {
  ...nodePath,
  join: (...ps: string[]): string => slash(nodePath.join(...ps)),
  resolve: (...ps: string[]): string => slash(nodePath.resolve(...ps)),
  posix: {
    ...nodePath.posix,
    resolve: (...ps: string[]): string => resolve(process.cwd(), ...ps),
    relative,
  },
};

// f.length counts the parameters before the first default.
const arity = (f: (...args: never[]) => unknown, n: number, name: string): void => {
  if (f.length !== n) {
    throw drift(
      `${name} takes ${f.length} parameters, not ${n}; update seeInfer and seeCheck in tools/bend-lint/src/seam.ts`,
    );
  }
};

// term_infer and term_check as bend.ts calls them: every argument passes
// through, and each result is also reported to hook.see. tsc checks the
// names against bend.ts's signatures; a function that no longer takes the
// arguments read here stops loading.
export const seeInfer = (f: typeof BendModule.term_infer): typeof BendModule.term_infer => {
  arity(f, 6, "term_infer");
  return (...args) => {
    const r = f(...args);
    const [bok, lhs, tm, qt, ctx, dep] = args;
    hook.see?.({ tm: r.tm, ty: r.ty, bok, ctx, dep, def: lhs.def, qt, us: r.us, spn: tm.s });
    return r;
  };
};

export const seeCheck = (f: typeof BendModule.term_check): typeof BendModule.term_check => {
  arity(f, 7, "term_check");
  return (...args) => {
    const r = f(...args);
    const [bok, lhs, tm, qt, ty, ctx, dep] = args;
    hook.see?.({ tm: r.tm, ty, bok, ctx, dep, def: lhs.def, qt, us: r.us, spn: tm.s });
    return r;
  };
};

// `file` is one of PATCHED.
export const patch = (file: string, src: string): string => {
  const { edits, needs = [], tail, exports } = PATCHES[file];
  const mismatch = (what: string, n: number): never => {
    throw drift(
      `cannot patch bend2/${file}: found ${n} of ${what}, expected 1. Update PATCHES in tools/bend-lint/src/seam.ts.`,
    );
  };
  const edited = [...edits, ...needs.map((t): [string, string] => [t, t])].reduce(
    (out, [at, to]) => {
      const n = out.split(at).length - 1;
      return n === 1 ? out.replace(at, () => to) : mismatch(JSON.stringify(at), n);
    },
    src,
  );
  const declared = (name: string, exported: string): RegExp =>
    new RegExp(`^${exported}(?:async function|function|class|const|let) ${name}\\b`, "gm");
  const missing = exports.filter((name) => {
    const n = edited.match(declared(name, "(?:export )?"))?.length ?? 0;
    return n === 1
      ? !declared(name, "export ").test(edited)
      : mismatch("a declaration of " + name, n);
  });
  const named = missing.length === 0 ? "" : `export { ${missing.join(", ")} };\n`;
  return `${edited}\n${tail}${named}export const ${MARK} = 1;\n`;
};

export const drift = (message: string): Error =>
  Object.assign(new Error(message), { name: "DriftError", [DRIFT]: true });

const message = (e: unknown): string => (e instanceof Error ? e.message : String(e));

// A response's text, or a rejection naming its status.
const text = (res: Response, what: string): Promise<string> =>
  res.ok ? res.text() : Promise.reject(new Error(`GitHub answered ${res.status}${what}`));

// fetch, given up after 30 seconds.
const download = (url: string): Promise<Response> =>
  fetch(url, { signal: AbortSignal.timeout(30_000) });

// A command's stdout, or undefined if it does not run or fails.
const spawn = (cmd: string[]): string | undefined => {
  try {
    const r = Bun.spawnSync(cmd, { stdout: "pipe", stderr: "ignore" });
    return r.exitCode === 0 ? r.stdout.toString() : undefined;
  } catch {
    return undefined;
  }
};

// The installed bend's release ("v2.0.36"), or undefined if `bend version`
// does not run.
export const installedTag = (
  run: (cmd: string[]) => string | undefined = spawn,
): string | undefined => {
  const version = run(["bend", "version"])?.match(/^bend (\d+\.\d+\.\d+)\b/)?.[1];
  return version === undefined ? undefined : "v" + version;
};

// The newest release, from git's list of refs (GitHub's API limits calls).
// It asks at most once a day (a missing or damaged note asks again).
export const latestTag = async (cache: string = CACHE, get: Get = download): Promise<string> => {
  const note = nodePath.join(cache, "latest.json");
  const known = await Bun.file(note)
    .json()
    .catch(() => undefined);
  if (RELEASE.test(String(known?.tag)) && Date.now() - Number(known?.at) < DAY) {
    return String(known.tag);
  }
  const newest = (tags: string[]): string | undefined =>
    tags
      .filter((t) => RELEASE.test(t))
      .map((t) => t.slice(1).split(".").map(Number))
      .sort((a, b) => b[0] - a[0] || b[1] - a[1] || b[2] - a[2])
      .map((v) => "v" + v.join("."))[0];
  const got = await get(GIT)
    .then((res) => text(res, ""))
    .then(
      (refs) => ({ refs }),
      (e: unknown) => ({ e }),
    );
  if ("e" in got) {
    // Offline: the newest cached release, left out of the note, so the
    // next run asks again.
    const cached = newest(nodeFs.existsSync(cache) ? nodeFs.readdirSync(cache) : []);
    if (cached === undefined) {
      throw new Error(`cannot list bend's releases (${message(got.e)}); ${GIVE}`);
    }
    return cached;
  }
  const listed = newest([...got.refs.matchAll(/refs\/tags\/(v[\d.]+)$/gm)].map((m) => m[1]));
  if (listed === undefined) {
    throw new Error(`bend has no release tags; ${GIVE}`);
  }
  nodeFs.mkdirSync(cache, { recursive: true });
  nodeFs.writeFileSync(note, JSON.stringify({ tag: listed, at: Date.now() }));
  return listed;
};

// bend2 at a release (the PATCHED files, safe.ts, base.bend and the effs/
// files Base imports), kept in <cache>/<tag>/bend2.
// A download goes to a temporary folder first, so the cache never holds a
// partial one.
export const fetchBend = async (
  tag: string,
  cache: string = CACHE,
  get: Get = download,
): Promise<string> => {
  if (!RELEASE.test(tag)) {
    throw new Error("not a bend release: " + tag);
  }
  const dir = nodePath.join(cache, tag, "bend2");
  const whole = (): boolean =>
    [...PATCHED, "safe.ts", "base.bend"].every((f) => nodeFs.existsSync(nodePath.join(dir, f)));
  if (whole()) {
    return dir;
  }
  const fetched = (file: string): Promise<[string, string]> =>
    get(`${RAW}${tag}/bend2/${file}`)
      .then((res) => text(res, ` for bend2/${file}`))
      .then((body) => [file, body]);
  const base = await fetched("base.bend");
  const effs = [
    ...new Set([...base[1].matchAll(/^\s*import "\.\/(effs\/[^"]+)"/gm)].map((m) => m[1])),
  ];
  const odd = effs.find((f) => !EFF.test(f));
  if (odd !== undefined) {
    throw new Error("base.bend imports " + odd + ", which is not a plain file in effs/");
  }
  const files = [base, ...(await Promise.all([...PATCHED, "safe.ts", ...effs].map(fetched)))];
  nodeFs.mkdirSync(nodePath.join(cache, tag), { recursive: true });
  const part = nodeFs.mkdtempSync(nodePath.join(cache, tag, ".part-"));
  files.forEach(([file, body]) => {
    nodeFs.mkdirSync(nodePath.dirname(nodePath.join(part, file)), { recursive: true });
    nodeFs.writeFileSync(nodePath.join(part, file), body);
  });
  try {
    nodeFs.rmSync(dir, { recursive: true, force: true });
    nodeFs.renameSync(part, dir);
  } catch (e) {
    nodeFs.rmSync(part, { recursive: true, force: true });
    if (!whole()) {
      throw e;
    }
  }
  return dir;
};

// The bend2 folder to load, found in the order the docs give.
export const bendDir = async (
  given: string | undefined,
  { get = download, run = spawn, cache = CACHE, repo }: FindOptions = {},
): Promise<string> => {
  const chosen = given ?? process.env.BEND_DIR;
  const dir = path.resolve(chosen ?? repo ?? path.join(HERE, "..", "..", "..", "bend2"));
  const found = [path.join(dir, "bend2"), dir].find((d) =>
    nodeFs.existsSync(path.join(d, "bend.ts")),
  );
  if (found !== undefined) {
    return fs.realpathSync(found);
  }
  if (chosen !== undefined) {
    throw new Error(`no bend2 at ${dir} (it needs bend.ts); ${GIVE}`);
  }
  const tag = installedTag(run) ?? (await latestTag(cache, get));
  const fresh = !nodeFs.existsSync(nodePath.join(cache, tag, "bend2"));
  const got = await fetchBend(tag, cache, get).catch((e: unknown) => {
    throw new Error(`could not download bend ${tag} (${message(e)}); ${GIVE}`);
  });
  if (fresh) {
    console.error("bend-lint: downloaded bend " + tag + " to " + got);
  }
  return fs.realpathSync(got);
};

// Every sub-term, parents first, with an explicit stack: linear in the
// size of the term, at any depth.
export function* walk(tm: LTerm): Generator<LTerm> {
  const stack = [tm];
  for (let t = stack.pop(); t !== undefined; t = stack.pop()) {
    const next = children(t);
    yield t;
    for (let i = next.length - 1; i >= 0; i--) {
      stack.push(next[i]);
    }
  }
}

// A term's sub-terms, in bend's order.
const children = (t: LTerm): LTerm[] => {
  const of = (CHILDREN as Record<string, ((tm: LTerm) => LTerm[]) | undefined>)[t.$];
  if (of === undefined) {
    throw drift(`unknown term kind ${t.$}; update CHILDREN in tools/bend-lint/src/seam.ts`);
  }
  return of(t);
};

// Where each line of `text` starts, computed once per owner of the text.
export const starts = (owner: object, text: string): number[] => {
  const known = STARTS.get(owner);
  if (known !== undefined) {
    return known;
  }
  const out = [0, ...[...text.matchAll(/\n/g)].map((m) => m.index! + 1)];
  STARTS.set(owner, out);
  return out;
};

// The index of the line that holds `off`, given each line's start.
export const line = (starts: number[], off: number): number => {
  let [lo, hi] = [0, starts.length - 1];
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    [lo, hi] = starts[mid] <= off ? [mid, hi] : [lo, mid - 1];
  }
  return lo;
};

// bend.ts parses a copy of each file with its import lines blanked, so a
// span moves to the file on disk by line and column. The copy's namespace,
// folder and lines pick the file; a copy that matches no file, or two, is
// a drift error.
export const mapper = (files: File[]): Mapper => {
  const own = new Set<unknown>(files);
  const memo = new WeakMap<object, { file: File; from: number[]; to: number[] }>();
  // Each file's lines, split once.
  const split = new Map<File, string[]>();
  const linesOf = (file: File): string[] => {
    const known = split.get(file) ?? file.str.split("\n");
    split.set(file, known);
    return known;
  };
  const find = (f: BendSpan["file"] & { dir?: string }) => {
    const lines = f.str.split("\n");
    const found = files.filter((file) => {
      if (
        f.ns !== file.ns ||
        (f.dir !== undefined && f.dir !== file.path.slice(0, file.path.lastIndexOf("/") + 1))
      ) {
        return false;
      }
      const theirs = linesOf(file);
      return (
        lines.length === theirs.length &&
        lines.every(
          (l, i) => l === theirs[i] || (l.trim() === "" && /^\s*import(\s|$)/.test(theirs[i])),
        )
      );
    });
    if (found.length !== 1) {
      throw drift(
        `cannot map a bend.ts span to a file on disk (${found.length} candidates); bend.ts may mask imports another way now`,
      );
    }
    Object.assign(found[0].al, f.al);
    const known = { file: found[0], from: starts(f, f.str), to: starts(found[0], found[0].str) };
    memo.set(f, known);
    return known;
  };
  return (s) => {
    if (s === undefined || own.has(s.file)) {
      return s;
    }
    const { file, from, to } = memo.get(s.file) ?? find(s.file);
    const at = (off: number): number => {
      const i = line(from, off);
      return to[i] + off - from[i];
    };
    return { file, beg: at(s.beg), end: at(s.end) };
  };
};

// A mapped bend.ts span as a span of a checked source, and back.
const toSpan = (s: BendSpan | undefined): Span | undefined => {
  const file = s && SOURCES.get(s.file);
  return s && file && { file, beg: s.beg, end: s.end };
};

const fromSpan = (s: Span | undefined): BendSpan | undefined =>
  s && { file: FILES.get(s.file)!, beg: s.beg, end: s.end };

// A fact or a type handle as what it holds, and back.
const raw = (fact: Fact): Raw => fact as unknown as Raw;
const handle = (r: Raw): Fact => r as unknown as Fact;
const term = (t: Type): HTerm => t as unknown as HTerm;
const typed = (t: HTerm): Type => t as unknown as Type;
const tree = (n: Node): LTerm => n as unknown as LTerm;
const node = (t: LTerm): Node => t as unknown as Node;

// Throws a drift error that names each check that failed.
const demand = (checks: Array<[string, boolean]>, say: (wrong: string) => string): void => {
  const wrong = checks.flatMap(([what, ok]) => (ok ? [] : [what]));
  if (wrong.length > 0) {
    throw drift(say(wrong.join(", ")));
  }
};

const isErr = (e: unknown): e is Err =>
  typeof e === "object" && e !== null && (e as { $?: unknown }).$ === "Err";

// A term's kind, and the name a Var or Ref points to.
const kindOf = (t: LTerm): { kind: string; name: Name } => ({
  kind: t.$,
  name: t.$ === "Var" || t.$ === "Ref" ? t.k : "",
});

// Whether a fact passes a filter, its scope aside.
const matches = (f: FactFilter, kind: string, def: Name, name: Name): boolean => {
  const all = (xs: string[] | undefined, x: string): boolean =>
    xs === undefined || xs.length === 0 || xs.includes(x);
  return (
    all(f.kinds, kind) &&
    (all(f.defs, def) || all(f.defs, def.replace(/~\d+$/, ""))) &&
    all(f.names, name)
  );
};

// bend's words for a failed check, its location aside (a finding has its
// own): expected and observed, with the names in scope, and the note.
const failure = (m: Loaded, e: unknown): string => {
  if (!isErr(e)) {
    return m.Main.book_err(e).replace(/^Error: /, "");
  }
  const show = (x: Err["exp"]): string =>
    m.Bend.expr_show(e.bok, x, m.Bend.ctx_scope(e.ctx), e.spn?.file);
  return (
    (e.obs === undefined
      ? show(e.exp)
      : "expected: " + show(e.exp) + "\nobserved: " + show(e.obs)) +
    (e.nte === undefined ? "" : "\n" + e.nte)
  );
};

// check's work, in its turn. A failure in a file bend did not finish
// loading may not map; it still reports, without a span.
const checked = async (
  m: Loaded,
  file: string,
  filters: FactFilter[],
  signal: AbortSignal,
): Promise<Checked> => {
  const { Bend, Main } = m;
  const seen = new Map<string, string | null>();
  const found: Report[] = [];
  const real = fs.existsSync(file) ? fs.realpathSync(file) : "";
  const home = real.slice(0, real.lastIndexOf("/") + 1);
  const far = filters.filter((f) => f.scope === "program");
  const program = far.length > 0;
  const texts = new Map<string, string>();
  reads = texts;
  const seeded = real !== "" && /^import Base$/m.test(fs.readFileSync(real, "utf8"));
  const key = fs.readFileSync(Bend.BASE_BEND, "utf8");
  if (seeded && BASE?.key !== key) {
    BASE = { key, book: Main.book_read(Bend.BASE_BEND) };
  }
  const see = (report: Report): void => {
    const at = report.spn?.file as { dir?: string; ns?: string } | undefined;
    const near = at?.dir === home && at.ns === "" ? filters : far;
    const { kind, name } =
      near.length > 0 ? kindOf(Bend.term_strip(report.tm)) : { kind: "", name: "" };
    if (near.some((f) => matches(f, kind, report.def, name))) {
      found.push(report);
    }
  };
  const read = await Promise.resolve(seeded ? BASE?.book : undefined)
    .then((base) => {
      hook.see = filters.length > 0 ? see : undefined;
      return Main.book_read(file, base, seen);
    })
    .then(
      (book) => ({ book }),
      (e: unknown) => ({ e: e instanceof Main.Check_Fail ? e.why : e }),
    );
  reads = undefined;
  signal.throwIfAborted();
  const root = [...seen.keys()].find((real) => real !== Bend.BASE_BEND || !seeded);
  const sources = [...seen.keys()]
    .filter((real) => texts.has(real) || fs.existsSync(real))
    .map((real): Source => {
      const text = texts.get(real) ?? fs.readFileSync(real, "utf8");
      const source = { path: real, text, root: real === root, base: real === Bend.BASE_BEND };
      const file = { str: text, ns: seen.get(real) ?? "", al: {}, path: real };
      FILES.set(source, file);
      SOURCES.set(file, source);
      return source;
    });
  const map = mapper(sources.map((s) => FILES.get(s)!));
  if ("book" in read) {
    const { book } = read;
    const insts = new Set(Object.values(book.tmps).flatMap((t) => [...t.values()]));
    const own = new Set<unknown>(
      sources.filter((s) => (program ? !s.base : s.root)).map((s) => FILES.get(s)),
    );
    const facts = found.flatMap((f): Array<[LTerm, Fact]> => {
      const spn = map(f.spn);
      return spn !== undefined && own.has(spn.file)
        ? [[f.tm, handle({ ...f, inst: insts.has(f.def), spn })]]
        : [];
    });
    return {
      book,
      sources,
      map,
      facts: filters.length > 0 ? [...new Map(facts).values()] : undefined,
    };
  }
  const err = isErr(read.e) ? read.e : undefined;
  let span: Span | undefined;
  try {
    span = toSpan(map(err?.spn));
  } catch {
    span = undefined;
  }
  return {
    book: Bend.book_nil(),
    sources,
    map,
    failure: {
      code: "bend/check",
      severity: "error",
      message: failure(m, read.e),
      span,
      def: err?.def,
      fixes: [],
      core: read.e,
    },
  };
};

// Checks a book as `bend <file>` does (not main.ts's verdict on @unsafe and
// foreign code), with `text` in place of the files on disk, one check at a
// time. A fact is kept only if a filter wants it, and only if its span maps
// to the linted file (or, with scope program, to any file but Base).
export const check = (
  m: Loaded,
  file: string,
  filters: FactFilter[],
  signal: AbortSignal,
  text?: ReadonlyMap<string, string>,
): Promise<Checked> => {
  const turn = queue.then(async (): Promise<Checked> => {
    signal.throwIfAborted();
    [...(text ?? [])]
      .filter(([at]) => fs.existsSync(at))
      .forEach(([at, str]) => unsaved.set(fs.realpathSync(at), str));
    try {
      return await checked(m, file, filters, signal);
    } finally {
      hook.see = undefined;
      reads = undefined;
      unsaved.clear();
    }
  });
  queue = turn.catch(() => undefined);
  return turn;
};

// The facts one rule asked for, of those kept for all rules. A filter that
// matches all, in the scope of every kept fact, gets them as they are.
export const select = (
  m: Loaded,
  { sources, facts }: Checked,
  want: FactFilter,
  program: boolean,
): Fact[] | undefined => {
  const root = FILES.get(sources.find((s) => s.root)!);
  const all = [want.kinds, want.defs, want.names].every((xs) => !xs?.length);
  return facts === undefined || (all && (want.scope === "program" || !program))
    ? facts
    : facts.filter((fact) => {
        const r = raw(fact);
        const { kind, name } = kindOf(m.Bend.term_strip(r.tm));
        return (
          (want.scope === "program" || r.spn?.file === root) && matches(want, kind, r.def, name)
        );
      });
};

// Whether `text` declares what the linted file declares: both are parsed
// with the same imported declarations, and compared as parsed terms, not
// checked ones, since checking unfolds definitions and would hide changes
// in meaning. No typecheck, disk writes or import fetches. A proof made
// only of {==} has no body span, so its aliases come from the sources. A
// proof of an imported law fills its declaration rather than creating one.
const sameDeclarations = (m: Loaded, { book, sources }: Checked, text: string): boolean => {
  const { Bend } = m;
  const root = sources.find((s) => s.root)!;
  const ns = FILES.get(root)!.ns;
  const dir = root.path.slice(0, root.path.lastIndexOf("/") + 1);
  const header = (s: string) => {
    const lines = s.split("\n");
    const imports: [string, string][] = [];
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      if (line === "" || line.startsWith("#")) continue;
      const m = /^import\s+(\S+)(?:\s+as\s+([A-Za-z_]\w*))?\s*(?:#.*)?$/.exec(line);
      if (m === null || (m[2] === undefined && m[1] !== "Base")) break;
      if (m[2] !== undefined) imports.push([m[1], m[2]]);
      lines[i] = "";
    }
    return { body: lines.join("\n"), imports };
  };
  const original = header(root.text);
  const spanned = (): Record<Name, Name> => {
    for (const tld of Object.values(book.tlds)) {
      for (const t of [tld.T, ...(tld.$ === "Def" && tld.v ? [tld.v] : [])]) {
        for (const tm of walk(Bend.term_lower(t))) {
          if (tm.s?.file.str === original.body) {
            return tm.s.file.al;
          }
        }
      }
    }
    return {};
  };
  const aliases = original.imports.reduce<Record<Name, Name>>(
    (known, [at, alias]) => {
      const imported = sources.find((s) => s.path === resolve(root.path, "..", at));
      return known[alias] !== undefined || imported === undefined
        ? known
        : { ...known, [alias]: FILES.get(imported)!.ns };
    },
    original.imports.length ? spanned() : {},
  );
  const qualify = (name: string) => {
    const dot = name.indexOf(".");
    return dot >= 0 && aliases[name.slice(0, dot)] !== undefined
      ? aliases[name.slice(0, dot)] + ":" + name.slice(dot + 1)
      : (ns ? ns + ":" : "") + name;
  };
  const own = new Set(
    [...root.text.matchAll(/^(?:@unsafe\s+)?(?:def|type|law)\s+([\w.]+)/gm)].map((match) =>
      qualify(match[1]),
    ),
  );
  const seed = (): Book => {
    const fresh = Bend.book_nil();
    fresh.tlds = { ...book.tlds };
    fresh.ctrs = { ...book.ctrs };
    for (const k of own) {
      const tld = fresh.tlds[k];
      if (k.includes(":") && !k.startsWith(ns + ":") && tld?.$ === "Def") {
        fresh.tlds[k] = { ...tld, v: null, i: undefined, u: false };
      } else {
        if (tld?.$ === "ADT") for (const c of tld.c) delete fresh.ctrs[c.k];
        delete fresh.tlds[k];
      }
    }
    return fresh;
  };
  const snapshot = (s: string) => {
    const parsed = seed();
    Bend.parse_book(parsed, dir, header(s).body, ns, aliases);
    const lower = (t: HTerm | null) => (t === null ? null : Bend.term_lower(t));
    return JSON.stringify(
      parsed.order.map((k) => {
        const t = parsed.tlds[k];
        return t.$ === "ADT"
          ? [k, t.n, t.g, lower(t.T), t.c.map((c) => [c.k, c.n, lower(c.T)])]
          : [k, t.n, t.x, t.u ?? false, t.i, lower(t.T), lower(t.v)];
      }),
      (k, v) => (k === "s" ? undefined : v),
    );
  };
  try {
    return snapshot(root.text) === snapshot(text);
  } catch {
    return false;
  }
};

// What a rule asks bend2, over one check's book and facts.
export const operations = (m: Loaded, run: Checked): Operations => {
  const { Bend } = m;
  let byTerm: Map<LTerm, Fact> | undefined;
  return {
    view: (fact): View => {
      const r = raw(fact);
      return {
        owner: r.def,
        inst: r.inst,
        ...kindOf(Bend.term_strip(r.tm)),
        quantity: QUANTITY[r.qt.$],
        span: toSpan(r.spn),
        inner: toSpan(run.map(Bend.term_strip(r.tm).s)),
      };
    },
    type: (fact) => typed(raw(fact).ty),
    binder: (fact) => {
      const r = raw(fact);
      const v = Bend.term_strip(r.tm);
      const ann = v.$ === "Var" ? Bend.pmap_get(r.ctx, v.i) : null;
      return ann === null ? undefined : typed(ann.T);
    },
    same: (fact, a, b) => Bend.term_compare("EQ", raw(fact).bok, term(a), term(b), raw(fact).dep),
    show: (fact, t) => Bend.term_show(Bend.term_lower(term(t), raw(fact).dep)),
    normal: (fact, t) => typed(Bend.term_snf(raw(fact).bok, term(t))),
    uses: (fact) => {
      const r = raw(fact);
      return Bend.pmap_to_array(r.us).flatMap(([v, q]) =>
        q.$ === "None" ? [] : [{ name: Bend.pmap_get(r.ctx, v)?.k ?? "", quantity: QUANTITY[q.$] }],
      );
    },
    sameDeclarations: (text) => sameDeclarations(m, run, text),
    body: (name) => {
      const tld = run.book.tlds[name];
      return tld?.$ === "Def" && tld.e !== undefined ? node(tld.e) : undefined;
    },
    node: (fact) => node(raw(fact).tm),
    shape: (n): Shape => {
      const t = tree(n);
      return { ...kindOf(t), span: toSpan(run.map(t.s)), children: children(t).map(node) };
    },
    fact: (n) => {
      byTerm ??= new Map((run.facts ?? []).map((f) => [raw(f).tm, f]));
      return byTerm.get(tree(n));
    },
    unstable: { Bend, book: run.book, raw },
  };
};

// bend's own error layout for a finding, under `head`: its message,
// context and location.
export const layout = (m: Loaded, d: Diag, head: string): string => {
  const r = d.fact && raw(d.fact);
  return m.Bend.err_show(
    isErr(d.core)
      ? d.core
      : m.Bend.Err(
          r?.bok ?? m.Bend.book_nil(),
          r?.ctx ?? m.Bend.ctx_nil(),
          d.message,
          undefined,
          fromSpan(d.span),
          d.def,
        ),
  ).replace(/^Error:/, head);
};

// A Bend rule's id(), facts() and main(), compiled as comp.ts io_run does.
// facts(), checked: NoFacts{} gives null, Want{...} a filter.
export const compile = (m: Loaded, { book }: Checked, file: string): Compiled => {
  const { Bend, Comp } = m;
  const value = (k: Name): LTerm | undefined => {
    const tld = book.tlds[k];
    return tld?.$ === "Def" && tld.n === 0 && tld.v !== null
      ? Bend.term_lower(Bend.term_snf(book, tld.v))
      : undefined;
  };
  const shown = value("id");
  const id = shown === undefined ? undefined : Bend.term_show(shown).match(/^"([^"\\]*)"$/)?.[1];
  const args = (t: LTerm | undefined, ctr: string): LTerm[] | undefined =>
    t?.$ === "Ctr" && (t.k === ctr || t.k.endsWith(":" + ctr)) ? t.x : undefined;
  const texts = (t: LTerm | undefined): string[] | undefined => {
    const [head, tail] = args(t, "Con") ?? [];
    const rest = tail && texts(tail);
    return args(t, "Nil") !== undefined
      ? []
      : head?.$ === "Lit" && typeof head.v === "string" && rest
        ? [head.v, ...rest]
        : undefined;
  };
  const asked = value("facts");
  const [scope, kinds, defs, names] = args(asked, "Want") ?? [];
  const want =
    args(asked, "NoFacts") !== undefined
      ? null
      : {
          scope: (["file", "program"] as const).find(
            (s) => args(scope, s === "file" ? "File" : "Program") !== undefined,
          ),
          kinds: texts(kinds),
          defs: texts(defs),
          names: texts(names),
        };
  if (
    id === undefined ||
    Comp.io_type(book) === null ||
    (want !== null && [want.scope, want.kinds, want.defs, want.names].includes(undefined))
  ) {
    throw new Error(
      file + " must define id() -> String, facts() -> Lint.Want and main() -> IO(Unit)",
    );
  }
  const main = new Function(
    "require",
    `${Comp.js_lib(book)}\n${Comp.RUNTIME_MAIN}\nreturn (args) => { cli_args = args; return io_run(${Comp.js_sat("main")}); };`,
  )(import.meta.require) as (args: string[]) => number;
  return { id, want: want as FactFilter | null, main };
};

// What main.ts must give, as bend-lint calls it; a mismatch is a drift
// error.
export const guardMain = (Main: Main): void =>
  demand(
    [
      ["book_read(file, base, seen = ...)", Main.book_read?.length === 2],
      ["book_err(e)", Main.book_err?.length === 1],
      [
        "new Check_Fail(why).why",
        typeof Main.Check_Fail === "function" && new Main.Check_Fail(MARK).why === MARK,
      ],
    ],
    (wrong) =>
      `bend2/main.ts no longer has ${wrong}; update PATCHES in tools/bend-lint/src/seam.ts`,
  );

// Checks src/bend/sample.bend as a rule sees it. `x` in `def id(x: N) -> N: x`
// must be a Var typed N, bound as an N, used once, at its own span; `id` in
// main is reported only by term_infer, and the Lam only by term_check.
const selfCheck = async (m: Loaded): Promise<void> => {
  const run = await check(m, SAMPLE, [{}], new AbortController().signal);
  const ops = operations(m, run);
  const views = (run.facts ?? []).map((fact) => ({ fact, ...ops.view(fact) }));
  const x = views.find((v) => v.kind === "Var" && v.name === "x");
  const bound = x && ops.binder(x.fact);
  demand(
    [
      ["check", run.failure === undefined],
      ["Ref id (term_infer)", views.some((v) => v.kind === "Ref" && v.name === "id")],
      ["Lam (term_check)", views.some((v) => v.kind === "Lam")],
      ["type", x !== undefined && ops.show(x.fact, ops.type(x.fact)) === "N"],
      ["scope", x !== undefined && bound !== undefined && ops.show(x.fact, bound) === "N"],
      ["quantity", x?.quantity === "once"],
      ["uses", JSON.stringify(x && ops.uses(x.fact)) === '[{"name":"x","quantity":"once"}]'],
      ["span", x?.span?.file.text.slice(x.span.beg, x.span.end) === "x"],
    ],
    (wrong) =>
      `self-check failed: the patched bend2 gave the wrong ${wrong} for src/bend/sample.bend; update tools/bend-lint/src/seam.ts`,
  );
};

// Finds bend2, patches its PATCHED files as Bun loads them, imports them,
// and checks them (guardMain, selfCheck).
export const load = async (given: string | undefined): Promise<Loaded> => {
  const dir = await bendDir(given);
  const patched = new Map(
    PATCHED.map((f) => [path.join(dir, f), patch(f, fs.readFileSync(path.join(dir, f), "utf8"))]),
  );
  const exact = dir.replace(/[.*+?^${}()|[\]\\]/g, "\\$&").replaceAll("/", "[\\\\/]");
  const names = PATCHED.map((f) => f.replace(/\.ts$/, "")).join("|");
  const filter = new RegExp(
    `^${exact}[\\\\/](${names})\\.ts$`,
    process.platform === "win32" ? "i" : "",
  );
  Bun.plugin({
    name: "bend-lint",
    setup: (build) => {
      build.onLoad({ filter }, (args) => {
        const contents = patched.get(fs.realpathSync(args.path));
        if (contents === undefined) {
          throw drift(`bend-lint matched ${args.path} but did not patch it`);
        }
        return { contents, loader: "ts" };
      });
    },
  });
  const modules = await Promise.all(
    PATCHED.map((f) => import(url.pathToFileURL(path.join(dir, f)).href)),
  );
  if (modules.some((module) => module[MARK] !== 1)) {
    throw drift("bend2 was loaded before bend-lint could patch it; import bend-lint first");
  }
  const [B, C, M] = modules as [typeof BendModule, Comp, Main];
  guardMain(M);
  const loaded = { Bend: B, Comp: C, Main: M, BEND2: dir };
  await selfCheck(loaded);
  return loaded;
};
