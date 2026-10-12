#!/usr/bin/env bun
// bend-lint checks a Bend file with bend's checker, then runs rules over
// its source and the checker's results. Rules are TS modules, or Bend files
// built on ./bend/lint.bend. It reaches bend2 only through ./seam.ts; anything
// there it cannot follow stops it with a drift error. The types below are
// bend-lint's own, so a rule does not depend on bend2's internals. As a
// CLI, it exits 0 when ok, 1 when it found an error, 2 on bad usage or a
// tool failure.

import * as url from "node:url";
import * as util from "node:util";

import {
  check,
  compile,
  fs,
  layout,
  line,
  load,
  operations,
  path,
  select,
  starts,
} from "./seam.ts";
import type { Unstable } from "./seam.ts";

// Types
// =====

export type Severity = "error" | "warning" | "information" | "hint";
// safe keeps behavior; suggested may change it; dangerous may break code.
export type Applicability = "safe" | "suggested" | "dangerous";

// A file of the book, as it is on disk (or as the editor holds it unsaved).
export type Source = { path: string; text: string; root: boolean; base: boolean };

// A range of a source, in UTF-16 offsets.
export type Span = { file: Source; beg: number; end: number };

export type Edit = { span: Span; text: string };
export type Fix = { title: string; applicability: Applicability; edits: Edit[] };

declare const OPAQUE: unique symbol;

// A checked term, a type, and a node of a checked body, as handles: what
// they hold is bend2's, and a rule reaches it only through the RuleContext.
export type Fact = { readonly [OPAQUE]: "fact" };
export type Type = { readonly [OPAQUE]: "type" };
export type Node = { readonly [OPAQUE]: "node" };

// A node: its kind, annotations kept (Ann, Var, App, ...), the name a Var
// or Ref points to (else ""), its span, and its children, in bend's order.
export type Shape = { kind: string; name: string; span?: Span; children: Node[] };

// How many times a term is demanded, or a variable is used.
export type Quantity = "erased" | "once" | "many";

// A fact's term: its kind (annotations stripped: Var, Ref, App, ...), the
// name a Var or Ref points to, the def whose body holds it, and how many
// times it is demanded. A template body is checked as written, then again
// for each instance (generic~0) at the same spans; `inst` marks the facts
// of an instance. `inner` is the term's span without its annotations.
export type View = {
  owner: string;
  inst: boolean;
  kind: string;
  name: string;
  quantity: Quantity;
  span?: Span;
  inner?: Span;
};

export type Diag = {
  code: string;
  severity: Severity;
  message: string;
  span?: Span;
  def?: string;
  fact?: Fact; // its scope shows as the finding's context
  fixes: Fix[];
  core?: unknown; // bend's own failure, when the check failed
};

export type DiagInit = {
  message: string;
  severity?: Severity;
  span?: Span;
  fact?: Fact;
  def?: string;
  fixes?: Fix[];
};

// The checker's facts a rule gets. A fact must match each list given; an
// absent or empty list matches all. In defs, a template's instances
// (generic~0) count as the template.
export type FactFilter = {
  scope?: "file" | "program"; // file (the default): the linted file; program: its imports too, never Base
  kinds?: string[]; // the term's kind, annotations stripped: Var, Ref, App, ...
  defs?: string[]; // the def whose body holds the term
  names?: string[]; // the name a Var or Ref points to
};

// `type`, `binder`, `same`, `show` and `normal` work in the fact's scope.
// `unstable` is bend2's own objects: code that uses it breaks when bend2
// changes.
export type RuleContext = {
  sources: Source[];
  root: Source;
  options: Options; // the rule's defaults, with the config's values
  facts?: Fact[]; // only for a rule with facts, and only those it asked for
  prior: readonly Diag[]; // what earlier rules found
  view(fact: Fact): View;
  type(fact: Fact): Type;
  binder(fact: Fact): Type | undefined; // the declared type of the variable a Var fact uses
  same(fact: Fact, a: Type, b: Type): boolean;
  show(fact: Fact, type: Type): string;
  normal(fact: Fact, type: Type): Type;
  uses(fact: Fact): Array<{ name: string; quantity: Quantity }>;
  sameDeclarations(text: string): boolean; // whether `text` declares what the linted file declares
  body(name: string): Node | undefined; // a def's checked body
  node(fact: Fact): Node; // the node a fact is about
  shape(node: Node): Shape;
  fact(node: Node): Fact | undefined; // the node's fact, if this rule asked for it
  diag(init: DiagInit): Diag;
  unstable: Unstable;
};

// A rule option's value. A default's type is its option's type.
export type OptionValue = number | boolean | string;
export type Options = Record<string, OptionValue>;

// Config: per rule id, "off", or a severity and option values.
export type Config = { rules?: Record<string, "off" | ({ severity?: Severity } & Options)> };

// `unsaved` maps file paths to text an editor holds but has not saved. bend
// and the rules read it in place of the file, for that run only.
export type LintOptions = {
  signal?: AbortSignal;
  config?: Config;
  unsaved?: ReadonlyMap<string, string>;
};

export type LintRule = {
  id: string; // namespace/name; the code of its findings
  facts?: true | FactFilter; // the checker's facts it needs; true: all of the linted file's
  options?: Options; // defaults; without them (a Bend rule), any option is passed as given
  run(cx: RuleContext, signal: AbortSignal): Diag[] | Promise<Diag[]>;
};

export type LintResult = {
  ok: boolean;
  diags: Diag[];
  sources: Source[];
  facts?: Fact[];
  unstable: Unstable;
};

// An LSP position: 0-based line and character, in UTF-16 units.
export type Position = { line: number; character: number };

// A span as lint.bend has it: a file's path and two code-point offsets.
type Spot = { path: string; beg: number; end: number };

// What a Bend rule reports, as effects.js reads it.
type Reported = {
  severity: Severity;
  message: string;
  span?: Spot;
  fixes: Array<{
    title: string;
    applicability: Applicability;
    edits: Array<{ span: Spot; text: string }>;
  }>;
};

// What effects.js asks of bend-lint while a Bend rule runs. Facts and types
// cross as indexes into the run's tables.
type Channel = {
  input(): { sources: Array<{ path: string; text: string; root: boolean }>; options: Options };
  next(): number | undefined;
  report(diags: Reported[]): void;
  view(fact: number): Omit<View, "span" | "inner"> & { span?: Spot; inner?: Spot };
  type(fact: number): number;
  binder(fact: number): number | undefined;
  same(fact: number, a: number, b: number): boolean;
  show(fact: number, t: number): string;
  normal(fact: number, t: number): number;
  uses(fact: number): Array<{ name: string; quantity: Quantity }>;
  text(span: Spot): string;
  body(name: string): number | undefined;
  node(fact: number): number;
  shape(node: number): Omit<Shape, "span" | "children"> & { span?: Spot; children: number[] };
  fact(node: number): number | undefined;
};

// Constants
// =========

const RULE_ID = /^[^/\s]+\/[^/\s]+$/;
const SCOPES = ["file", "program"];
const CONFIG_FILES = ["bend-lint.json", "bend-lint.js", "bend-lint.ts"];
const LINE = /[^\n]*\n|[^\n]+$/g;
const HEAD: Record<Severity, string> = {
  error: "Error",
  warning: "Warning",
  information: "Information",
  hint: "Hint",
};

const shared = globalThis as typeof globalThis & { BEND_LINT?: Channel };

const OPTIONS = {
  rules: { type: "string", multiple: true },
  fix: { type: "boolean" },
  "fix-suggested": { type: "boolean" },
  "fix-dangerously": { type: "boolean" },
  json: { type: "boolean" },
  config: { type: "string" },
  bend: { type: "string" },
  help: { type: "boolean", short: "h" },
} as const;

const USAGE =
  "usage: bun src/lint.ts <file.bend> [--rules <rules.ts|rule.bend>]... [--config <config.json|config.js|config.ts>] [--fix | --fix-suggested | --fix-dangerously] [--json] [--bend <dir>]";

// The fix levels each flag applies; the widest flag given wins.
const FIXES: ReadonlyArray<
  readonly ["fix" | "fix-suggested" | "fix-dangerously", Applicability[]]
> = [
  ["fix-dangerously", ["safe", "suggested", "dangerous"]],
  ["fix-suggested", ["safe", "suggested"]],
  ["fix", ["safe"]],
];

// Functions
// =========

export function readConfig(file: string): Config {
  try {
    if ([".js", ".ts"].includes(path.extname(file))) {
      const module = import.meta.require(url.pathToFileURL(path.resolve(file)).href);
      if (!Object.hasOwn(module, "config")) {
        throw new Error("must export a named `config` object");
      }
      if (
        typeof module.config !== "object" ||
        module.config === null ||
        Array.isArray(module.config)
      ) {
        throw new Error("`config` must be an object");
      }
      return module.config;
    }
    return JSON.parse(fs.readFileSync(file, "utf8"));
  } catch (e) {
    throw new Error(file + ": " + (e instanceof Error ? e.message : String(e)));
  }
}

// The nearest config; JSON, JS, then TS within each directory.
export function findConfig(file: string): Config {
  for (let dir = path.dirname(path.resolve(file)); ; dir = path.dirname(dir)) {
    const config = CONFIG_FILES.map((name) => path.join(dir, name)).find((candidate) =>
      fs.existsSync(candidate),
    );
    if (config !== undefined) {
      return readConfig(config);
    }
    if (path.dirname(dir) === dir) {
      return {};
    }
  }
}

// What the config says for a rule: off, a severity, and its options (the
// defaults, with the given values, which must be known and of their type).
function settings(
  rule: LintRule,
  config: Config,
): { off: boolean; severity?: Severity; options: Options } {
  const given = config.rules?.[rule.id];
  const where = "bend-lint config: " + rule.id;
  if (given === undefined || given === "off") {
    return { off: given === "off", options: { ...rule.options } };
  }
  if (typeof given !== "object" || given === null) {
    throw new Error(where + ' must be "off" or an object');
  }
  const { severity, ...options } = given;
  if (severity !== undefined && !Object.hasOwn(HEAD, severity)) {
    throw new Error(where + ": severity must be one of " + Object.keys(HEAD).join(", "));
  }
  for (const [key, value] of Object.entries(options)) {
    const want = rule.options === undefined ? typeof value : typeof rule.options[key];
    if (rule.options !== undefined && !Object.hasOwn(rule.options, key)) {
      throw new Error(where + " has no option " + key);
    }
    if (typeof value !== want || !["number", "boolean", "string"].includes(want)) {
      throw new Error(
        where +
          ": " +
          key +
          " must be a " +
          (rule.options === undefined ? "number, boolean or string" : want),
      );
    }
  }
  return { off: false, severity, options: { ...rule.options, ...options } };
}

// Rules run in order; the config may turn one off, set its severity, and
// give its options. A finding with severity error stops the run. A rule
// that throws, and an abort, reach the caller.
export async function lint(
  file: string,
  rules: LintRule[],
  {
    signal = new AbortController().signal,
    config = findConfig(file),
    unsaved: held,
  }: LintOptions = {},
): Promise<LintResult> {
  const strings = (xs: unknown): boolean =>
    xs === undefined || (Array.isArray(xs) && xs.every((x) => typeof x === "string"));
  const bad = rules.findIndex(
    (r) =>
      !RULE_ID.test(String(r?.id)) ||
      typeof r?.run !== "function" ||
      !(
        r.facts === undefined ||
        r.facts === true ||
        (typeof r.facts === "object" &&
          r.facts !== null &&
          (r.facts.scope === undefined || SCOPES.includes(r.facts.scope)) &&
          strings(r.facts.kinds) &&
          strings(r.facts.defs) &&
          strings(r.facts.names))
      ),
  );
  if (bad >= 0) {
    throw new TypeError(
      "invalid rule at " +
        bad +
        " (" +
        JSON.stringify(rules[bad]?.id) +
        "): it needs an id like ns/name, a run function, and facts, if given, true or a FactFilter",
    );
  }
  const plans = rules
    .map((rule) => ({
      rule,
      ...settings(rule, config),
      want: rule.facts === true ? {} : rule.facts,
    }))
    .filter((p) => !p.off);
  const program = plans.some((p) => p.want?.scope === "program");
  const checked = await check(
    loaded,
    file,
    plans.flatMap((p) => (p.want === undefined ? [] : [p.want])),
    signal,
    held,
  );
  const { sources, facts, failure } = checked;
  const ops = operations(loaded, checked);
  if (failure !== undefined) {
    return { ok: false, diags: [failure], sources, unstable: ops.unstable };
  }
  const root = sources.find((s) => s.root)!;
  let diags: Diag[] = [];
  for (const { rule, severity, options, want } of plans) {
    signal.throwIfAborted();
    const mine = want === undefined ? undefined : select(loaded, checked, want, program);
    const asked = new Set(mine);
    const out = await rule.run(
      {
        ...ops,
        sources,
        root,
        options,
        facts: mine,
        fact: (node) => {
          const fact = ops.fact(node);
          return fact !== undefined && asked.has(fact) ? fact : undefined;
        },
        prior: diags,
        diag: (d) => ({
          code: rule.id,
          severity: d.severity ?? "warning",
          message: d.message,
          span: d.span,
          def: d.def ?? (d.fact && ops.view(d.fact).owner),
          fact: d.fact,
          fixes: d.fixes ?? [],
        }),
      },
      signal,
    );
    signal.throwIfAborted();
    if (!Array.isArray(out)) {
      throw new TypeError("rule " + rule.id + " must return an array of diagnostics");
    }
    const settled = out.map((d): Diag => ({
      ...d,
      code: rule.id,
      severity: severity ?? d.severity,
    }));
    const broken = settled
      .flatMap((d) => d.fixes)
      .find((f) =>
        f.edits.some(
          ({ span }, i) =>
            !validRange(span.beg, span.end, span.file.text.length) ||
            f.edits.some((o, j) => j !== i && clash(f.edits[i], o)),
        ),
      );
    if (broken !== undefined) {
      throw new TypeError(
        "rule " +
          rule.id +
          ': fix "' +
          broken.title +
          '" has an edit out of bounds, or two that clash',
      );
    }
    const stop = settled.findIndex((d) => d.severity === "error");
    diags = [...diags, ...(stop < 0 ? settled : settled.slice(0, stop + 1))];
    if (stop >= 0) {
      return { ok: false, diags, sources, facts, unstable: ops.unstable };
    }
  }
  return { ok: true, diags, sources, facts, unstable: ops.unstable };
}

function validRange(beg: number, end: number, length: number): boolean {
  return Number.isInteger(beg) && Number.isInteger(end) && beg >= 0 && beg <= end && end <= length;
}

// Whether two edits to one file cannot both apply: their ranges overlap,
// or they insert at the same point.
function clash(a: Edit, b: Edit): boolean {
  return (
    a.span.file === b.span.file &&
    ((a.span.beg < b.span.end && b.span.beg < a.span.end) ||
      (a.span.beg === b.span.beg && a.span.end === b.span.end))
  );
}

// `text` with edits applied; their spans are offsets into `text`, and they
// do not clash.
function apply(text: string, edits: Edit[]): string {
  return [...edits]
    .sort((a, b) => b.span.beg - a.span.beg || b.span.end - a.span.end)
    .reduce((s, { span, text: t }) => s.slice(0, span.beg) + t + s.slice(span.end), text);
}

// The text of `file` with the fixes of the given levels applied, in order.
// An edit equal to one already taken is merged; a fix with an edit that
// clashes with one already taken is skipped and counted. A fix for other
// files is left out; one that edits `file` and another file is skipped
// whole, never half applied, and counted apart.
export function applyFixes(
  file: Source,
  diags: Diag[],
  levels: Applicability[] = ["safe"],
): { text: string; skipped: number; elsewhere: number } {
  const kept: Edit[] = [];
  let skipped = 0;
  let elsewhere = 0;
  for (const fix of diags.flatMap((d) => d.fixes).filter((f) => levels.includes(f.applicability))) {
    if (fix.edits.every((e) => e.span.file !== file)) {
      continue;
    }
    if (fix.edits.some((e) => e.span.file !== file)) {
      elsewhere += 1;
      continue;
    }
    const fresh = fix.edits.filter(
      (e) =>
        !kept.some(
          (k) => k.span.beg === e.span.beg && k.span.end === e.span.end && k.text === e.text,
        ),
    );
    if (fresh.some((e) => kept.some((k) => clash(k, e)))) {
      skipped += 1;
    } else {
      kept.push(...fresh);
    }
  }
  return { text: apply(file.text, kept), skipped, elsewhere };
}

// bend's own error layout; the head names the severity and the code, and
// each fix follows as a unified diff of the lines it touches.
export function render(d: Diag): string {
  const fixes = d.fixes.map(
    (fix) =>
      "\n\nFix: " +
      fix.title +
      " [" +
      fix.applicability +
      "]" +
      [...new Set(fix.edits.map((e) => e.span.file))]
        .map((file) => {
          const mine = fix.edits.filter((e) => e.span.file === file);
          const ls = starts(file, file.text);
          const first = line(ls, Math.min(...mine.map((e) => e.span.beg)));
          const from = ls[first];
          const to = ls[line(ls, Math.max(...mine.map((e) => e.span.end))) + 1] ?? file.text.length;
          const old = file.text.slice(from, to);
          const gone = old.match(LINE) ?? [];
          const came =
            apply(
              old,
              mine.map((e) => ({
                ...e,
                span: { ...e.span, beg: e.span.beg - from, end: e.span.end - from },
              })),
            ).match(LINE) ?? [];
          const name = file.path;
          const range = (n: number): string => (n === 0 ? first : first + 1) + "," + n;
          const show = (xs: string[], sign: string): string[] =>
            xs.map(
              (l) =>
                sign +
                l.replace(/\r?\n$/, "") +
                (l.endsWith("\n") ? "" : "\n\\ No newline at end of file"),
            );
          return (
            "\n--- " +
            name +
            "\n+++ " +
            name +
            "\n@@ -" +
            range(gone.length) +
            " +" +
            range(came.length) +
            " @@\n" +
            [...show(gone, "-"), ...show(came, "+")].join("\n")
          );
        })
        .join(""),
  );
  return layout(loaded, d, HEAD[d.severity] + " [" + d.code + "]:") + fixes.join("");
}

// The LSP range of a span in a file on disk.
export function position(span: Span): { start: Position; end: Position } {
  const ss = starts(span.file, span.file.text);
  const at = (off: number): Position => {
    const i = line(ss, off);
    return { line: i, character: off - ss[i] };
  };
  return { start: at(span.beg), end: at(span.end) };
}

// A rule written in Bend: a file built on ./bend/lint.bend (see there). It is
// checked and compiled once; each run calls its main, while effects.js
// reaches bend-lint through globalThis.BEND_LINT. Offsets cross as code points.
export async function bendRule(file: string): Promise<LintRule> {
  const checked = await check(loaded, file, [], new AbortController().signal);
  if (checked.failure !== undefined) {
    throw new Error(file + " does not check:\n" + render(checked.failure));
  }
  const { id, want, main } = compile(loaded, checked, file);
  return {
    id,
    ...(want === null ? {} : { facts: want }),
    run: (cx) => {
      const tables = new Map<string, { points: number[]; units: number[] }>();
      const table = (text: string): { points: number[]; units: number[] } => {
        const known = tables.get(text);
        if (known !== undefined) {
          return known;
        }
        const points: number[] = [];
        const units: number[] = [];
        for (const ch of text) {
          units.push(points.length);
          points.push(...Array<number>(ch.length).fill(units.length - 1));
        }
        units.push(points.length);
        points.push(units.length - 1);
        tables.set(text, { points, units });
        return { points, units };
      };
      const spot = (span: Span | undefined): Spot | undefined => {
        const t = span && table(span.file.text);
        return (
          span && t && { path: span.file.path, beg: t.points[span.beg], end: t.points[span.end] }
        );
      };
      const span = (s: Spot): Span => {
        const src = cx.sources.find((x) => x.path === s.path);
        if (src === undefined) {
          throw new Error(
            "rule " + id + " reported a span in " + s.path + ", which is not in the book",
          );
        }
        const { units } = table(src.text);
        const at = (n: number): number => units[Math.min(n, units.length - 1)];
        return { file: src, beg: at(s.beg), end: at(s.end) };
      };
      const facts = cx.facts ?? [];
      let given = 0;
      const types: Type[] = [];
      const nodes: Node[] = [];
      const index = new Map(facts.map((f, i) => [f, i]));
      const pick = <T>(xs: T[], i: number, what: string): T => {
        if (xs[i] === undefined) {
          throw new Error(
            "rule " + id + " asked about " + what + " " + i + ", which it was not given",
          );
        }
        return xs[i];
      };
      const keep = (t: Type): number => types.push(t) - 1;
      const hold = (n: Node): number => nodes.push(n) - 1;
      const found: Reported[][] = [];
      shared.BEND_LINT = {
        input: () => {
          const odd = Object.entries(cx.options).find(
            ([, v]) => typeof v === "number" && !(Number.isInteger(v) && v >= 0 && v <= 0xffffffff),
          );
          if (odd !== undefined) {
            throw new Error(
              "rule " +
                id +
                ": option " +
                odd[0] +
                " must be a whole number from 0 to 4294967295 (a U32)",
            );
          }
          return {
            sources: cx.sources.map((s) => ({ path: s.path, text: s.text, root: s.root })),
            options: cx.options,
          };
        },
        next: () => (given < facts.length ? given++ : undefined),
        report: (diags) => void found.push(diags),
        view: (i) => {
          const { span, inner, ...rest } = cx.view(pick(facts, i, "fact"));
          return { ...rest, span: spot(span), inner: spot(inner) };
        },
        type: (i) => keep(cx.type(pick(facts, i, "fact"))),
        binder: (i) => {
          const t = cx.binder(pick(facts, i, "fact"));
          return t === undefined ? undefined : keep(t);
        },
        same: (i, a, b) =>
          cx.same(pick(facts, i, "fact"), pick(types, a, "term"), pick(types, b, "term")),
        show: (i, t) => cx.show(pick(facts, i, "fact"), pick(types, t, "term")),
        normal: (i, t) => keep(cx.normal(pick(facts, i, "fact"), pick(types, t, "term"))),
        uses: (i) => cx.uses(pick(facts, i, "fact")),
        text: (s) => {
          const { file, beg, end } = span(s);
          return file.text.slice(beg, end);
        },
        body: (name) => {
          const n = cx.body(name);
          return n === undefined ? undefined : hold(n);
        },
        node: (i) => hold(cx.node(pick(facts, i, "fact"))),
        shape: (i) => {
          const { span, children, ...rest } = cx.shape(pick(nodes, i, "node"));
          return { ...rest, span: spot(span), children: children.map(hold) };
        },
        fact: (i) => {
          const f = cx.fact(pick(nodes, i, "node"));
          return f === undefined ? undefined : index.get(f);
        },
      };
      let code: number;
      try {
        code = main([file]);
      } finally {
        shared.BEND_LINT = undefined;
      }
      if (code !== 0 || found.length !== 1) {
        throw new Error(
          "rule " +
            id +
            " exited with " +
            code +
            " after " +
            found.length +
            " reports; it must report once",
        );
      }
      return found[0].map((d) =>
        cx.diag({
          message: d.message,
          severity: d.severity,
          span: d.span && span(d.span),
          fixes: d.fixes.map((f) => ({
            ...f,
            edits: f.edits.map((e) => {
              const at = span(e.span);
              if (!validRange(e.span.beg, e.span.end, table(at.file.text).units.length - 1)) {
                throw new TypeError(
                  "rule " + id + ': fix "' + f.title + '" has an edit out of bounds',
                );
              }
              return { span: at, text: e.text };
            }),
          })),
        }),
      );
    },
  };
}

async function cli(argv: string[]): Promise<number> {
  const { values, positionals } = util.parseArgs({
    args: argv,
    options: OPTIONS,
    allowPositionals: true,
  });
  if (values.help) {
    console.log(USAGE);
    return 0;
  }
  if (positionals.length !== 1) {
    throw new Error("give one .bend file\n" + USAGE);
  }
  const modules = await Promise.all(
    (values.rules ?? []).map(async (file): Promise<LintRule[]> => {
      if (file.endsWith(".bend")) {
        return [await bendRule(file)];
      }
      const { rules } = await import(url.pathToFileURL(path.resolve(file)).href);
      if (!Array.isArray(rules)) {
        throw new Error(file + " must export `rules`, an array of rules");
      }
      return rules;
    }),
  );
  const res = await lint(positionals[0], modules.flat(), {
    config: values.config === undefined ? undefined : readConfig(values.config),
  });
  const root = res.sources.find((s) => s.root);
  const levels = FIXES.find(([flag]) => values[flag])?.[1];
  const fixed =
    levels !== undefined && root !== undefined ? applyFixes(root, res.diags, levels) : undefined;
  const where = (span: Span) => ({ path: span.file.path, range: position(span) });
  console.log(
    values.json
      ? JSON.stringify(
          {
            ok: res.ok,
            findings: res.diags.map((d) => ({
              code: d.code,
              severity: d.severity,
              message: d.message,
              def: d.def,
              ...(d.span && where(d.span)),
              fixes: d.fixes.map((f) => ({
                ...f,
                edits: f.edits.map((e) => ({ ...where(e.span), text: e.text })),
              })),
            })),
          },
          null,
          2,
        )
      : [
          ...res.diags.map(render),
          res.ok ? "bend-lint: " + res.diags.length + " finding(s)" : "bend-lint: FAIL",
        ].join("\n\n"),
  );
  if (root !== undefined && fixed !== undefined && fixed.text !== root.text) {
    fs.writeFileSync(root.path, fixed.text);
    console.error("bend-lint: fixed " + root.path);
  }
  if (fixed !== undefined && fixed.skipped > 0) {
    console.error(
      "bend-lint: skipped " +
        fixed.skipped +
        " fix(es) that clash with earlier ones; run the fix again to apply them",
    );
  }
  if (fixed !== undefined && fixed.elsewhere > 0) {
    console.error(
      "bend-lint: skipped " +
        fixed.elsewhere +
        " fix(es) that also edit other files; --fix writes only " +
        root!.path,
    );
  }
  return res.ok ? 0 : 1;
}

function fail(e: unknown): never {
  console.error("bend-lint: " + (e instanceof Error ? e.message : String(e)));
  process.exit(2);
}

// Side effects
// ============

// The CLI's --bend is read here, before bend loads; a library uses $BEND_DIR.
const given = import.meta.main
  ? util.parseArgs({
      args: process.argv.slice(2),
      options: OPTIONS,
      allowPositionals: true,
      strict: false,
    }).values.bend
  : undefined;

const loaded = await load(typeof given === "string" ? given : undefined).catch((e: unknown) =>
  import.meta.main ? fail(e) : Promise.reject(e),
);

export const { BEND2 } = loaded;

if (import.meta.main) {
  process.exit(await cli(process.argv.slice(2)).catch(fail));
}
