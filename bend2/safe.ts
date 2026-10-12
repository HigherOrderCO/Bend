// Safe
// ====
// `bend f.bend --verdict` checks f.bend twice: bend2 checks it, then
// safe_book elaborates the checked book to BendTT (bend2/bendtt.lean,
// the minimal kernel with a proof) and the kernel's CLI checks the text.
// The elaborator reads bend2's checked terms (Def.e: every node of a
// def's body wrapped in its type), so it needs no inference of its own;
// the kernel trusts none of it. A term inside a type stays bare, so the
// elaborator carries the type bend2 checked it at down to it: a λ's body
// at the codomain, a match arm at its constructor's fields. A book bend2
// rejects never reaches the kernel. bend2
// converts functions up to η; the kernel does not, and unfolds a def
// only when applied. So a term of a function type goes out η-long: an
// equation bend2 closes by η has λs on both sides, which the kernel
// compares under the binder.
//
// A datatype D<ps> with constructors cs is two defs: D.arms(ps, t)
// switches on the tag t and gives the Σ chain of that constructor's
// fields, ending in <()>, and D(ps) is Σt:<cs> -> D.arms(ps, t). A
// field recurs through the name D. A constructor k{x, y} is the tuple
// (.k, x, y, ()), each field at its Σ quantity. A match is a λ-match: a
// λ{(,): ..} splits the tag from the fields, a λ{.k: ..; ..} chain
// switches on the tag, and one λ{(,): ..} per field splits the chain,
// which ends in λ{(): ..; λ{}}. A default arm binds the tag and the
// fields, and its binder is their pair. A D with no constructors also
// has D.efq(ps) : D(ps) -> <>, so the kernel checks that D is empty.
//
// The kernel's live check reads the case tree: a self-call must pass,
// left to right, each live column whole until one gets a piece of
// itself, and it counts a variable's uses in both arms of a match. A
// self-call that passes a column rebuilt from the pattern it matched
// (bend2 compares it equal) goes out as that constructor: the kernel
// reads it as the column (K3-R).
//
// A kind goes out as the kernel's *(q), and a meet as (a <&> b), so a
// Quant is a kernel value like any other, and a kind converts in the
// kernel where it does in bend2: Kind(&0) is *(.Q0), and fits *(.Q1)
// (Type) only as bend2's LE does. An item (a book name) goes out
// once per tuple of closed arguments at its template (~) parameters, with
// those parameters gone. Every def goes out after the defs its live code
// names; a name in a type may come later. A def with no body (a law, a
// native, a foreign fill) goes out opaque at a model: the kernel checks
// the model, then never unfolds the def. What the kernel cannot express
// is out of scope: it goes, with every def that names it, and --verdict
// fails.

import * as child from "node:child_process";
import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import * as B from "./bend.ts";
import type { Book, Def, HTerm, Name, Quant, ADT } from "./bend.ts";

// Types
// =====

type Q = 0 | 1 | 2;

// a kernel term; a binder names its variable by its level
type O =
  | { $: "Var"; l: number }
  | { $: "Ref"; k: string }
  | { $: "Ann"; x: O; T: O }
  | { $: "Let"; q: Q; l: number; v: O; f: O }
  | { $: "Typ"; q: O }
  | { $: "Min"; a: O; b: O }
  | { $: "All"; q: Q; l: number; A: O; B: O }
  | { $: "Lam"; q: Q; l: number; f: O }
  | { $: "App"; q: Q; f: O; x: O }
  | { $: "Sig"; q: Q; l: number; A: O; B: O }
  | { $: "Tup"; q: Q; a: O; b: O }
  | { $: "Prj"; h: O }
  | { $: "Enu"; ks: string[] }
  | { $: "Lab"; k: string }
  | { $: "Mat"; k: string; h: O; m: O }
  | { $: "Efq" }
  | { $: "Eql"; a: O; b: O; T: O }
  | { $: "Rfl" }
  | { $: "Rwt"; e: O; l: number; P: O; f: O };

// a bend2 variable: the kernel term it stands for, its bend2 type, and
// the term a specialized parameter or inlined let stands for (built at
// each use's depth and mode, so its o is unused)
type Bind = { o: O; T: HTerm | null; v?: HTerm };

// the argument of each parameter of an item, or of each column of a
// tree, that is specialized; null at the rest
type Cols = Array<HTerm | null>;

// an argument: its binder's quantity, the argument, the binder's type,
// and its value when the binder is specialized
type Arg = [Q, HTerm, HTerm, HTerm | null];

// a kernel binder: its quantity, level and type
type Binder = [Q, number, O];

// a Σ chain being split: fields left, and the variables its match
// convoys, which the arm binds again after the chain
type Chain = { n: number; cv: number[] };

// a scope: bend2 variables by level, the kernel depth, the tree's
// columns left, the def's kernel name, the refutations at hand (a live
// variable of an empty type), whether the term is a self-call's argument
// (a word literal there stays a constructor, which the kernel compares to
// a pattern), each kernel binder's tentative quantity, the default tags
// (a tag's fields follow it), whether the tree is built only to count
// its uses, and whether it is built again for a convoy
type Scope = { c: Bind[]; d: number; D: number; cols: Cols;
  self: string; empty: O[]; sub: boolean; kq: Q[]; tags: number[]; dry: boolean; again: boolean };

// the elaboration: the book, the book models read (a root's constant at
// its model), the defs out (an opaque one flagged), each item's kernel
// name, the items out or going out, the ones named but not yet out, the
// kernel names taken, why each failed item is out of
// scope, the groups found (kept from pass to pass), each template
// instance's template and ~ arguments (its key in book.tmps), the items
// going out (outermost first, and as a set), whether this pass grew a
// group, the root constants by place and type, and each kernel term's
// live uses, kept once counted
type Safe = {
  book: Book;
  mb: Book;
  out: Array<[string, O, O, boolean]>;
  names: Map<string, string>;
  seen: Set<string>;
  todo: Array<[Name, Cols]>;
  taken: Set<string>;
  fail: Map<string, string>;
  groups: Map<Name, Group>;
  inst: Map<Name, [Name, HTerm[]]>;
  stack: string[];
  going: Set<string>;
  grew: boolean;
  consts: Map<string, Name>;
  uses: WeakMap<O, Map<number, number>>;
};

// a model search's fuel left, its round's depth, and whether that round
// cut a branch at the depth or the fuel
type Probe = { left: number; depth: number; cut: boolean };

// Constants
// =========

// a Nat literal longer than this goes out as arithmetic on shorter ones
const NAT_MAX = 4096;

// a model search's work: a step per type it visits and per character of
// the datatype keys it builds, in rounds of doubling depth from
// MODEL_DEPTH
const MODEL_FUEL = 1 << 22;
const MODEL_DEPTH = 8;

// only this compiler release may build a cached kernel
const LEAN_VERSION = "4.34.0";

// Errors
// ======

// a book the kernel cannot express
class Scope_Error extends Error {}

// a live cycle found below the item at key, now a group: the emission
// since key began is undone, and its caller retries (Reroute)
class Regroup extends Error {
  constructor(readonly key: string) {
    super(key);
  }
}
class Reroute extends Error {}

function oos(why: string): never {
  throw new Scope_Error(why);
}

// Book
// ====

// the BendTT text of a checked book: every def and datatype not in base,
// in bend2's fill order, and what they name; a def out of scope goes, as
// does every def that names it, and oos says why, for each book name
function safe_book(book: Book): { text: string; oos: Array<[Name, string]> } {
  const n0 = Object.keys(book.tlds).length;
  const groups = new Map<Name, Group>();
  const inst = new Map(Object.entries(book.tmps).flatMap(([k, is]) => [...is].map(([key, n]): [Name, [Name, HTerm[]]] =>
    [n, [k, key.split("\n").map((a) => B.term_higher(JSON.parse(a) as B.LTerm))]])));
  for (let r = safe_pass(book, groups, inst); ; r = safe_pass(book, groups, inst)) {
    if (r !== null) {
      return r;
    }
    // the next pass's root constants are named afresh
    Object.keys(book.tlds).slice(n0).forEach((k) => delete book.tlds[k]);
  }
}

// one pass at the groups found so far, or null when it is stale: it grew
// a group some item had already gone out at, or a mention named a member
// on its own before the member's group was found
function safe_pass(book: Book, groups: Map<Name, Group>, inst: Safe["inst"]): { text: string; oos: Array<[Name, string]> } | null {
  const g0 = groups.size;
  const e: Safe = { book, mb: { ...book, tlds: Object.create(book.tlds) as Book["tlds"] }, out: [], names: new Map(), seen: new Set(),
    todo: [], taken: new Set(), fail: new Map(), groups, inst, stack: [], going: new Set(), grew: false, consts: new Map(), uses: new WeakMap() };
  const roots: Array<[Name, string]> = [];
  for (const k of [...new Set([...book.order].reverse())].reverse().filter((k) => book.tlds[k].b !== true)) {
    try {
      roots.push(...root_cols(e, k, book.tlds[k].T, 0).map((cols): [Name, string] => [k, item_try(e, k, cols)]));
    } catch (x) {
      if (!(x instanceof Scope_Error)) {
        throw x;
      }
      const n = fresh(e, name_tt(k));
      e.fail.set(n, x.message);
      roots.push([k, n]);
    }
  }
  for (let it = e.todo.pop(); it !== undefined; it = e.todo.pop()) {
    item_try(e, it[0], it[1]);
  }
  if (e.grew || groups.size > g0 && [...e.names.keys()].some((key) => key[0] !== "\t" && groups.has(key.split("\n")[0]))) {
    return null;
  }
  // a def that names a def out of scope is out too
  const bad = new Map(e.fail);
  for (let more = bad.size > 0; more;) {
    more = false;
    for (const [k, T, v] of e.out) {
      const r = bad.has(k) ? undefined : [...o_refs(T), ...o_refs(v)].find((r) => bad.has(r));
      if (r !== undefined) {
        bad.set(k, "names " + B.name_key(r) + ", out of scope: " + (e.fail.get(r) ?? bad.get(r)));
        more = true;
      }
    }
  }
  e.out = e.out.filter(([k]) => !bad.has(k));
  const oos = roots.filter(([, n]) => bad.has(n)).map(([k, n]): [Name, string] => [k, bad.get(n) as string]);
  return { text: book_show(e), oos };
}

// the columns root k checks at, from its telescope T's parameter j on:
// a specialized parameter of a finite type (Quant, or a datatype whose
// constructors have no fields) at each value, any other at an opaque
// constant of its type, one per place for all roots, which models read at
// its model (as bend2 checks a template: its body holds at every argument)
function root_cols(e: Safe, k: Name, T: HTerm, j: number): Cols[] {
  const tld = e.book.tlds[k];
  const F = j === tld.n ? null : B.term_wnf(e.book, T);
  if (F?.$ !== "All") {
    return [[]];
  }
  const at = (v: HTerm | null): Cols[] => root_cols(e, k, F.B(v ?? B.Var(F.k, j)), j + 1).map((cs) => [v, ...cs]);
  if (tld.$ !== "Def" || j >= tld.x) {
    return at(null);
  }
  const A = B.term_wnf(e.book, F.A);
  const adt = A.$ === "ADT" && A.x.length === 0 ? e.book.tlds[A.k] as ADT : null;
  const vs = A.$ === "Qnt" ? [B.None(), B.Lone(), B.Many()].map((q) => B.Qua(q))
    : adt !== null && adt.c.every((c) => B.term_wnf(e.book, c.T).$ !== "All") ? adt.c.map((c) => B.Ctr(c.k, [])) : null;
  if (vs !== null) {
    return vs.flatMap(at);
  }
  if (mentions(B.term_lower(F.A, j), (i) => i >= 0 && i < j)) {
    oos("a specialized parameter whose type names a parameter");
  }
  const key = String(j) + "\n" + B.term_key(B.term_lower(F.A));
  const kept = e.consts.get(key);
  if (kept !== undefined) {
    return at(B.Ref(kept));
  }
  let c = k + "~" + F.k;
  while (e.book.tlds[c] !== undefined) {
    c += "~";
  }
  const def: Def = { $: "Def", n: 0, x: 0, T: F.A, v: null };
  e.book.tlds[c] = def;
  const m = model(e, F.A);
  if (m !== null) {
    e.mb.tlds[c] = { ...def, v: m };
  }
  e.consts.set(key, c);
  return at(B.Ref(c));
}

// item_ref, with a failure kept as the item's reason
function item_try(e: Safe, k: Name, cols: Cols): string {
  try {
    return item_ref(e, k, cols, true);
  } catch (x) {
    if (x instanceof Reroute) {
      return item_try(e, k, cols);
    }
    if (!(x instanceof Scope_Error)) {
      throw x;
    }
    return e.names.get(item_key(k, cols)) as string;
  }
}

// Item
// ====

// book name k at the arguments cols of its specialized parameters (a
// tab and a def's name is the def's group)
function item_key(k: Name, cols: Cols): string {
  return [k, ...cols.flatMap((v) => v === null ? [] : [B.term_key(B.term_lower(v))])].join("\n");
}

// the kernel name of book name k at cols: a Quant literal names itself;
// a live name goes out now (before its caller), a dead one later. A live
// name still going out (not its own) closes a live cycle: a group
function item_ref(e: Safe, k: Name, cols: Cols, live: boolean): string {
  const key = item_key(k, cols);
  const m = live && !e.seen.has(key) ? trail(e) : null;
  let n = e.names.get(key);
  if (n === undefined) {
    const tag = cols.flatMap((v) => v === null ? [] : [v.$ === "Qua" ? String(quant(v.q)) : "v"]).join("");
    n = fresh(e, name_tt(k[0] === "\t" ? k.slice(1) + ".group" : k) + (tag !== "" ? ".q" + tag : ""));
    e.names.set(key, n);
    e.todo.push([k, cols]);
  }
  const why = e.fail.get(n);
  if (why !== undefined && live) {
    oos(why);
  }
  if (m !== null) {
    e.seen.add(key);
    e.stack.push(key);
    e.going.add(key);
    try {
      item_emit(e, k, cols, n);
    } catch (x) {
      if (x instanceof Scope_Error) {
        e.fail.set(n, x.message);
      }
      if (x instanceof Regroup && x.key === key) {
        undo(e, m);
        throw new Reroute();
      }
      throw x;
    } finally {
      e.stack.pop();
      e.going.delete(key);
    }
  } else if (live && e.going.has(key) && e.stack[e.stack.length - 1] !== key) {
    regroup(e, key);
  }
  return n;
}

function item_emit(e: Safe, k: Name, cols: Cols, n: string): void {
  if (k[0] === "\t") {
    return group_emit(e, group_of(e, k.slice(1)) as Group, cols, n);
  }
  const tld = e.book.tlds[k];
  if (tld === undefined) {
    oos("an unknown name " + B.name_key(k));
  }
  if (tld.$ === "ADT") {
    return adt_emit(e, cols, n, tld);
  }
  if (tld.u === true) {
    oos("uses " + (tld.b === true ? "base's" : "the") + " @unsafe def " + B.name_key(k));
  }
  def_emit(e, k, cols, n, tld);
}

// a def at its specialized arguments: non-null cols are the leading
// template prefix, filled into its type; the tree takes them. A def with
// no body goes out opaque, at a model of its type
function def_emit(e: Safe, k: Name, cols: Cols, n: string, tld: Def): void {
  const T = B.tele_fill(e.book, tld.T, cols.slice(0, tld.x) as HTerm[], B.ctx_nil());
  const t = tld.e !== undefined ? null : model(e, T) ?? oos("no model for " + (tld.i === undefined ? "" : (tld.b === true ? "base's" : "the") + " foreign def ") + B.name_key(k));
  const s = { ...scope_nil(), self: n };
  const To = term(e, s, T, false);
  e.out.push([n, To, t === null ? arm(e, s, k, cols, []) : tree(e, s, t, []), t !== null]);
}

// D.arms and D, at D's specialized arguments
function adt_emit(e: Safe, cols: Cols, n: string, tld: ADT): void {
  const { s, ps, xs, T } = tele_open(e, scope_nil(), tld.T, cols, tld.n);
  const K = B.term_wnf(e.book, T);
  if (K.$ !== "Typ") {
    oos("a datatype kind");
  }
  const G = term(e, s, K, false);
  const am = fresh(e, n + ".arms");
  const t = s.D;
  // a constructor's fields as a Σ chain ending in <()>
  const fs = (c: B.Ctr): O => alls(tele_open(e, s, B.tele_fill(e.book, c.T, xs, B.ctx_nil()), [], Infinity).ps, { $: "Enu", ks: ["()"] }, "Sig");
  const arms = tld.c.reduceRight<O>((m, c) => ({ $: "Mat", k: name_tt(c.k), h: fs(c), m }), { $: "Efq" });
  const Enu: O = { $: "Enu", ks: tld.c.map((c) => name_tt(c.k)) };
  e.out.push([am, alls(ps, { $: "All", q: 1, l: t, A: Enu, B: G }), lams(e, ps, arms), false]);
  const at = (k: string): O => ps.reduce<O>((f, [q, l]) => ({ $: "App", q, f, x: { $: "Var", l } }), { $: "Ref", k });
  e.out.push([n, alls(ps, G), lams(e, ps, { $: "Sig", q: 1, l: t, A: Enu, B: { $: "App", q: 1, f: at(am), x: { $: "Var", l: t } } }), false]);
  if (tld.c.length === 0) {
    e.out.push([fresh(e, n + ".efq"), alls(ps, { $: "All", q: 1, l: t, A: at(n), B: { $: "Enu", ks: [] } }), lams(e, ps, { $: "Prj", h: { $: "Efq" } }), false]);
  }
}

// the first n parameters of the telescope T (all, at most), from scope s on: one cols
// specializes takes its argument, any other binds a kernel variable; the
// binders, the arguments and the rest of T
function tele_open(e: Safe, s: Scope, T: HTerm, cols: Cols, n: number): { s: Scope; ps: Binder[]; xs: HTerm[]; T: HTerm } {
  const r = { s, ps: [] as Binder[], xs: [] as HTerm[], T };
  for (let j = 0; j < n; j++) {
    const F = B.term_wnf(e.book, r.T);
    if (F.$ !== "All") {
      break;
    }
    const q = quant(F.q);
    if (cols[j] == null) {
      r.ps.push([q, r.s.D, term(e, r.s, F.A, false)]);
      r.s = scope_kq(scope_bind(r.s, { $: "Var", l: r.s.D }, F.A, true), r.s.D, q);
    }
    r.xs.push(cols[j] ?? B.Var(F.k, r.s.d - 1));
    r.T = F.B(r.xs[j]);
  }
  return r;
}

// Specialize
// ----------

// a specialized argument: closed, in normal form
function spec_val(e: Safe, s: Scope, x: HTerm): HTerm {
  return closed_val(e, s, x) ?? oos("a template argument that depends on a run-time value");
}

// x's normal form, when it is closed; null when it depends on a run-time value
function closed_val(e: Safe, s: Scope, x: HTerm): HTerm | null {
  // bend2's annotations name a variable by its level
  const v = B.term_snf(e.book, subst(x, s.d, (o) => o.$ === "Var" && (o.i as number) >= 0 && (o.i as number) < s.d
    ? s.c[o.i as number]?.v ?? B.Var(o.k as Name, o.i as number) : undefined));
  return mentions(B.term_lower(v, s.d), (i) => i >= 0 && i < s.d) ? null : v;
}

// t at depth d, with each node f maps replaced, through its lowered form
// (so no closure hides one)
function subst(t: HTerm, d: number, f: (o: Record<string, unknown>) => HTerm | undefined): HTerm {
  const go = (u: unknown): unknown => {
    if (typeof u !== "object" || u === null) {
      return u;
    }
    const o = u as Record<string, unknown>;
    const v = f(o);
    return v !== undefined ? B.Var(o.k as Name, -1, undefined, v)
      : Array.isArray(u) ? u.map(go) : Object.fromEntries(Object.entries(o).map(([k, x]) => [k, k === "s" ? x : go(x)]));
  };
  return B.term_higher(go(B.term_lower(t, d)) as B.LTerm);
}

// Model
// -----
// a model of type T: λs around a model of the codomain, a datatype's
// first constructor whose fields all have one (none for a datatype
// already on the path), Unit for a kind, {==} for an equation bend2
// converts, else a live λ variable of type T (the codomain's own, so it
// is used once); none for an empty type. A projection model takes that
// variable first: a law like {a == sub(add(a, b), b)} holds of it, one
// like {add(a, b) == add(b, a)} of a constant. It reads the model book,
// as the kernel checks a model with every opaque def at its own

function model(e: Safe, T: HTerm): HTerm | null {
  return model_by(e, T, false) ?? model_by(e, T, true);
}

// a round that cuts no branch finds what an unbounded search would; one
// that does is retried deeper while fuel lasts, and the first model a
// cut round found stands only when none is left (the kernel checks it)
function model_by(e: Safe, T: HTerm, proj: boolean): HTerm | null {
  const probe: Probe = { left: MODEL_FUEL, depth: MODEL_DEPTH, cut: true };
  let m: HTerm | null = null;
  for (; probe.cut && probe.left > 0; probe.depth *= 2) {
    probe.cut = false;
    const v = model_at(e, T, 0, [], [], proj, probe);
    m = probe.cut ? m ?? v : v;
  }
  return m;
}

// a datatype's key on the path: binders inside it count from d, so a
// field under a ∀ meets the same datatype again
function model_key(F: HTerm, d: number): string {
  const at = (i: unknown): unknown => typeof i === "number" && i >= d ? "^" + (i - d) : i;
  return JSON.stringify(B.term_lower(F, d), (k, v) => k === "s" ? undefined : k !== "i" ? v : Array.isArray(v) ? v.map(at) : at(v));
}

function model_at(e: Safe, T: HTerm, d: number, path: string[], hs: Array<[HTerm, HTerm]>, proj: boolean, probe: Probe): HTerm | null {
  if (path.length + d > probe.depth || probe.left <= 0) {
    probe.cut = true;
    return null;
  }
  probe.left -= 1;
  const F = B.term_wnf(e.mb, T);
  const hyp = (): HTerm | null => hs.find(([, A]) => B.term_compare("EQ", e.mb, A, F, d))?.[0] ?? null;
  switch (F.$) {
    case "Typ": {
      const tld = e.mb.tlds["Unit"];
      return tld?.$ === "ADT" && tld.n === 0 ? B.ADT("Unit", []) : null;
    }
    case "All": {
      // the body is searched once, then each use binds level d in it
      const x: HTerm = B.Var(F.k, d);
      const t = model_at(e, F.B(x), d + 1, path, F.q.$ === "None" ? hs : [...hs, [x, F.A]], proj, probe);
      return t === null ? null : B.Ann(B.Lam(F.k, d, (v: HTerm) =>
        subst(t, d + 1, (o) => o.$ !== "Var" || (o.i as number) > d ? undefined : o.i === d ? v : B.Var(o.k as Name, o.i as number))), F);
    }
    case "ADT": {
      const key = model_key(F, d);
      probe.left -= key.length;
      const tld = e.mb.tlds[F.k];
      if (tld?.$ !== "ADT") {
        return hyp();
      }
      const h = proj ? hyp() : null;
      for (const c of path.includes(key) || h !== null ? [] : tld.c.filter((c) => !F.r.includes(c.k))) {
        const xs: HTerm[] = [];
        let U = B.term_wnf(e.mb, B.tele_fill(e.mb, c.T, F.x, B.ctx_nil()));
        let x: HTerm | null = null;
        while (U.$ === "All" && (x = model_at(e, U.A, d, [...path, key], [], proj, probe)) !== null) {
          xs.push(x);
          U = B.term_wnf(e.mb, U.B(x));
        }
        if (U.$ !== "All") {
          return B.Ann(B.Ctr(c.k, xs), F);
        }
      }
      return hyp();
    }
    case "Eql": {
      return B.term_compare("EQ", e.mb, F.a, F.b, d) ? B.Ann(B.Rfl(), F) : null;
    }
    default: {
      return hyp();
    }
  }
}

// Names
// =====

// a def name, taken here
function fresh(e: Safe, n: string): string {
  let k = n;
  for (let i = 1; e.taken.has(k); i++) {
    k = n + "_" + String(i);
  }
  e.taken.add(k);
  return k;
}

// a bend2 name spelled with BendTT's name characters: any other one is
// _hex_
function name_tt(k: Name): string {
  return B.name_key(k).replace(/[^A-Za-z0-9_.]|^[.0-9]/g, (c) => "_" + (c.codePointAt(0) ?? 0).toString(16) + "_");
}

// Quant
// =====

function quant(q: Quant): Q {
  return q.$ === "None" ? 0 : q.$ === "Lone" ? 1 : 2;
}

// Scope
// =====

function scope_nil(): Scope {
  return { c: [], d: 0, D: 0, cols: [], self: "", empty: [], sub: false, kq: [], tags: [], dry: false, again: false };
}

// binds the next bend2 variable to o, or to its original term v when
// specialized or inlined; a kernel binder when kb
function scope_bind(s: Scope, o: O, T: HTerm | null, kb: boolean, v?: HTerm): Scope {
  const c = s.c.slice();
  c[s.d] = { o, T, v };
  return { ...s, c, d: s.d + 1, D: s.D + (kb ? 1 : 0) };
}

// a kernel binder with no bend2 variable
function scope_hide(s: Scope): Scope {
  return { ...s, D: s.D + 1 };
}

// the (tentative) quantity of the kernel binder at level l: a q=1 one
// may ride a convoy
function scope_kq(s: Scope, l: number, q: Q): Scope {
  const kq = s.kq.slice();
  kq[l] = q;
  return { ...s, kq };
}

// the kernel variable at level a stands at level b from here on
function scope_move(s: Scope, a: number, b: number): Scope {
  const mv = (o: O): O => o.$ === "Var" ? (o.l === a ? { $: "Var", l: b } : o)
    : Object.fromEntries(Object.entries(o).map(([k, v]) => [k, is_o(v) ? mv(v) : v])) as O;
  return { ...s, c: s.c.map((x) => x === undefined ? x : { ...x, o: mv(x.o) }), empty: s.empty.map(mv),
    tags: s.tags.flatMap((t) => t === a ? [t, b] : [t]) };
}

// binds the convoyed variables cv again, each at its quantity, then the
// tree t (or k's term)
function convoy_bind(e: Safe, s: Scope, cv: number[], t: HTerm | ((s: Scope) => O), fs: Chain[]): O {
  if (cv.length === 0) {
    return typeof t === "function" ? t(s) : tree(e, s, t, fs);
  }
  const l = s.D;
  const q = s.kq[cv[0]] ?? 1;
  const f = convoy_bind(e, scope_move(scope_kq(scope_hide(s), l, q), cv[0], l), cv.slice(1), t, fs);
  return lams(e, [[q, l]], f);
}

// Open
// ====

// a term without its type wrappers, and the type the outermost gives
function open(t: HTerm): [HTerm, HTerm | null] {
  let T: HTerm | null = null;
  let x = B.term_force(t);
  while (x.$ === "Ann") {
    T ??= B.term_force(x.T);
    x = B.term_force(x.x);
  }
  return [x, T];
}

// a spine's head and arguments, through type wrappers
function unapply(t: HTerm): [HTerm, HTerm[]] {
  const xs: HTerm[] = [];
  let [x] = open(t);
  let h = t;
  while (x.$ === "App") {
    xs.push(x.x);
    h = x.f;
    [x] = open(x.f);
  }
  return [h, xs.reverse()];
}

// Tree
// ====

// a case tree: λs, λ-matches, then a branch. fs holds the Σ chains the
// tree is splitting, innermost last: a λ or a match there first splits
// the field off its chain, and a chain done splits off its unit
function tree(e: Safe, s: Scope, t: HTerm, fs: Chain[]): O {
  const top = fs[fs.length - 1];
  if (top !== undefined && top.n === 0) {
    return { $: "Mat", k: "()", h: convoy_bind(e, s, top.cv, t, fs.slice(0, -1)), m: { $: "Efq" } };
  }
  const [x, T] = open(t);
  const all = all_of(e, T);
  // a specialized column takes its argument: a λ binds it (no kernel
  // binder), a match goes to the arm it takes, whose fields it binds so
  const v = top === undefined ? s.cols[0] ?? null : null;
  if (v !== null && x.$ === "Lam") {
    const s2 = scope_bind({ ...s, cols: s.cols.slice(1) }, { $: "Efq" }, all?.A ?? null, false, v);
    return tree(e, s2, x.f(B.Var(x.k, s.d)), fs);
  }
  if (v !== null && (x.$ === "Mat" || x.$ === "Efq")) {
    const [arm, vs] = pick(e, x, v);
    return tree(e, { ...s, cols: [...vs, ...s.cols.slice(1)] }, arm, fs);
  }
  // a leaf of a function type goes η-long: a chain splits its fields
  // under λs, and the kernel converts without η, so each side of an
  // equation bend2 closes by η must be a λ
  if (x.$ !== "Lam" && x.$ !== "Mat" && x.$ !== "Efq") {
    if (all !== null) {
      return tree(e, s, eta(t, all), fs);
    }
    return top === undefined ? term(e, s, t, true) : oos("a match arm with no known type");
  }
  // inside a chain, split the field off first; else take the column
  const fs2 = top === undefined ? fs : [...fs.slice(0, -1), { ...top, n: top.n - 1 }];
  const wrap = (o: O): O => top === undefined ? o : { $: "Prj", h: o };
  const s1 = top === undefined ? { ...s, cols: s.cols.slice(1) } : s;
  if (x.$ === "Lam") {
    const l = s.D;
    let q = all === null ? 1 : quant(all.q);
    let s2 = scope_kq(scope_bind(s1, { $: "Var", l }, all?.A ?? null, true), l, q);
    if (q > 0 && all !== null && no_ctr(e, all.A)) {
      s2 = { ...s2, empty: [...s2.empty, { $: "App", q: 1, f: { $: "Prj", h: { $: "Efq" } }, x: { $: "Var", l } }] };
    }
    const y: HTerm = B.Var(x.k, s.d);
    const f = tree(e, s2, typed(x.f(y), all?.B(y)), fs2);
    if (q === 1 && uses(e, f, l) > 1) {
      // bend2 checks a ~ argument dead, so its λ may use a linear variable
      // twice: it matches the variable once and rebuilds it at each use,
      // unless its type reduces to Data (a match-refined view): copied
      const A = all === null ? null : B.term_wnf(e.book, all.A);
      if (all !== null && kind(e, s, all.A) === 1 && A!.$ !== "Eql" && kind(e, s, A!) !== 2) {
        return tree(e, s, rebuild(e, s, x, all) ?? oos("a λ that uses a variable twice, of a type whose fields are not Data (a ~ argument)"), fs);
      }
      q = 2;
    }
    return wrap({ $: "Lam", q, l, f });
  }
  // a match consumes the column, with the variables it convoys after it,
  // λx => (λ{..} x y..)
  const q = all === null ? 1 : quant(all.q);
  const mat = (cv: number[], dry: boolean): O => {
    if (cv.length === 0) {
      return wrap({ $: "Prj", h: swi(e, { ...s1, dry }, x, T, fs2, []) });
    }
    const l = s.D;
    const sw = swi(e, scope_kq(scope_hide({ ...s1, dry, again: true }), l, q), x, T, fs2, cv);
    const app = cv.reduce<O>((f, y) => ({ $: "App", q: s.kq[y] ?? 1, f, x: { $: "Var", l: y } }), { $: "App", q, f: { $: "Prj", h: sw }, x: { $: "Var", l } });
    return wrap({ $: "Lam", q, l, f: app });
  };
  // the arms, to count uses: dry inside a match being built again (an
  // inner match is then only the variables it names, once each, as after
  // its convoy, dead if only dead there), so no match is built more than twice
  const o = mat([], s.dry || s.again);
  // the live uses of each level the arms name, 0 when named only dead
  const us = o_uses(e, o);
  const use = (l: number): number => us.get(l) ?? 0;
  // a q=1 variable used in two arms rides into them, unless it is Data:
  // then its binder copies it (a q=2 λ)
  const data = (l: number): boolean => s.c.some((b) => b?.o.$ === "Var" && b.o.l === l && b.T !== null && kind(e, s, b.T) === 2);
  const cv0 = s.kq.flatMap((k, l) => k === 1 && l < s.D && use(l) > 1 && !data(l) ? [l] : []);
  // a default's tag and fields go together: the fields' type names the tag
  const cv1 = [...new Set(cv0.flatMap((l) => s.tags.includes(l) ? [l, l + 1] : s.tags.includes(l - 1) ? [l - 1, l] : [l]))];
  // a variable the arms name (its levels: a default's binder has two), at
  // a type that names one riding, rides too (at its own quantity): left
  // out, its type names the levels outside
  const cv = s.c.reduce<number[]>((vs, b, i) => {
    if (vs.length === 0 || b === undefined || b.T === null) {
      return vs;
    }
    const ls = [...o_uses(e, b.o).keys()].filter((l) => !vs.includes(l));
    const js = ls.some((l) => us.has(l)) ? s.c.flatMap((x, j) => j < i && x !== undefined && [...o_uses(e, x.o).keys()].some((l) => vs.includes(l)) ? [j] : []) : [];
    return js.length > 0 && mentions(B.term_lower(b.T, s.d), (k) => js.includes(k)) ? [...vs, ...ls] : vs;
  }, cv1).sort((a, b) => a - b);
  if (s.dry) {
    return [...Array(s.D).keys()].filter((l) => us.has(l)).reduce<O>((f, l) => ({ $: "App", q: use(l) > 0 ? 1 : 0, f, x: { $: "Var", l } }), { $: "Efq" });
  }
  if (cv.length === 0 && !s.again) {
    return o;
  }
  return mat(cv, false);
}

// the tag switch of a match chain on a scrutinee of goal T; an arm splits
// its constructor's fields, and a default binds the tag and the fields
function swi(e: Safe, s: Scope, t: HTerm, T: HTerm | null, fs: Chain[], cv: number[]): O {
  const [x, U] = open(t);
  const all = all_of(e, U ?? T);
  switch (x.$) {
    case "Efq": {
      if (all === null || no_ctr(e, all.A) || s.empty.length === 0) {
        return { $: "Efq" };
      }
      // tags remain, but a live tag in scope is of the empty enum
      const q = quant(all.q);
      const f = convoy_bind(e, scope_hide(scope_hide(s)), cv, (s2: Scope) => s2.empty[0], fs);
      return { $: "Lam", q, l: s.D, f: { $: "Lam", q, l: s.D + 1, f } };
    }
    case "Mat": {
      const ctr = e.book.ctrs[x.k];
      if (ctr === undefined) {
        oos("an unknown constructor " + B.name_key(x.k));
      }
      const [hT, mT] = goals(e, all, ctr);
      const h = tree(e, s, typed(x.h, hT), [...fs, { n: ctr.n, cv }]);
      const dead = mT?.$ === "All" && no_ctr(e, mT.A) && x.m.$ !== "Mat";
      const m = swi(e, s, dead ? B.Efq() : x.m, mT, fs, cv);
      return { $: "Mat", k: name_tt(x.k), h, m };
    }
    default: {
      if (all === null) {
        return oos("a default arm with no known type");
      }
      const q = quant(all.q);
      const [lt, la] = [s.D, s.D + 1];
      const s2 = scope_hide(scope_hide(s));
      const f = x.$ === "Lam" ? x : open(eta(t, all))[0] as Extract<HTerm, { $: "Lam" }>;
      // the default's binder is the pair of the tag and the fields
      const pair: O = { $: "Tup", q: 1, a: { $: "Var", l: lt }, b: { $: "Var", l: la } };
      const s2e = no_ctr(e, all.A) ? { ...s2, empty: [...s2.empty,
        { $: "App", q: 1, f: { $: "Efq" }, x: { $: "Var", l: lt } } as O] } : s2;
      const s3 = scope_kq(scope_kq({ ...s2e, tags: [...s2e.tags, lt] }, lt, q), la, q);
      const y: HTerm = B.Var(f.k, s.d);
      const body = convoy_bind(e, scope_bind(s3, pair, all.A, false), cv, typed(f.f(y), all.B(y)), fs);
      const r2: Q = uses(e, body, lt) > 1 || uses(e, body, la) > 1 ? 2 : q;
      return { $: "Lam", q: r2, l: lt, f: { $: "Lam", q: r2, l: la, f: body } };
    }
  }
}

// the goals of a bare match of type all: c's arm at c's fields, then
// all's codomain at c{fields}; the other arms at the datatype less c
function goals(e: Safe, all: Extract<HTerm, { $: "All" }> | null, c: B.Ctr): [HTerm | null, HTerm | null] {
  const A = all === null ? null : B.term_wnf(e.book, all.A);
  if (all === null || A?.$ !== "ADT") {
    return [null, null];
  }
  const go = (U: HTerm, xs: HTerm[]): HTerm => {
    const F = B.term_wnf(e.book, U);
    return F.$ === "All" ? B.All(F.q, F.k, F.i, F.A, (x: HTerm) => go(F.B(x), [...xs, x])) : all.B(B.Ctr(c.k, xs));
  };
  return [go(B.tele_fill(e.book, c.T, A.x, B.ctx_nil()), []), { ...all, A: { ...A, r: [...A.r, c.k] } }];
}

// t at T, when bend2 left it bare
function typed(t: HTerm, T?: HTerm | null): HTerm {
  return T == null || open(t)[1] !== null ? t : B.Ann(t, T);
}

// the arm of the λ-match t that the closed constructor v takes, and
// what it binds: v's fields (a default arm binds v whole)
function pick(e: Safe, t: HTerm, v: HTerm): [HTerm, HTerm[]] {
  const w = B.term_wnf(e.book, v);
  const c = w.$ === "Lit" ? B.term_higher(B.lit_step(w)) : w;
  let [m] = open(t);
  while (m.$ === "Mat" && c.$ === "Ctr" && m.k !== c.k) {
    [m] = open(m.m);
  }
  if (c.$ !== "Ctr" || m.$ === "Efq") {
    return oos("a specialized argument that is not a constructor");
  }
  return m.$ === "Mat" ? [m.h, c.x] : [m, [c]];
}

// λ{c: λfs => f(c{fs}); ..} for x = λy => f(y) at ∀y:A -> B, when every
// field of A's constructors is Data; else null
function rebuild(e: Safe, s: Scope, x: Extract<HTerm, { $: "Lam" }>, all: Extract<HTerm, { $: "All" }>): HTerm | null {
  const F = B.term_wnf(e.book, all.A);
  if (F.$ !== "ADT") {
    return null;
  }
  const data = (U: HTerm): boolean => {
    const G = B.term_wnf(e.book, U);
    return G.$ !== "All" || (kind(e, s, B.term_wnf(e.book, G.A)) === 2 && data(G.B(B.Var(G.k, -1))));
  };
  const arm = (k: Name, U: HTerm, xs: HTerm[]): HTerm => {
    const G = B.term_wnf(e.book, U);
    return G.$ !== "All" ? x.f(B.Ann(B.Ctr(k, xs), F)) : B.Lam(G.k, 0, (v: HTerm) => arm(k, G.B(v), [...xs, v]));
  };
  const cs = (e.book.tlds[F.k] as ADT).c.filter((c) => !F.r.includes(c.k)).map((c): [Name, HTerm] => [c.k, B.tele_fill(e.book, c.T, F.x, B.ctx_nil())]);
  return cs.every(([, U]) => data(U)) ? B.Ann(cs.reduceRight<HTerm>((m, [k, U]) => B.Mat(k, arm(k, U, []), m), B.Efq()), all) : null;
}

// T's weak head, when a ∀
function all_of(e: Safe, T: HTerm | null): Extract<HTerm, { $: "All" }> | null {
  const G = T === null ? null : B.term_wnf(e.book, T);
  return G?.$ === "All" ? G : null;
}

// λy => t(y), at t's type ∀y:A -> B
function eta(t: HTerm, all: Extract<HTerm, { $: "All" }>): HTerm {
  return B.Ann(B.Lam(all.k, 0, (y: HTerm) => B.Ann(B.App(t, y), all.B(y))), all);
}

// whether a lowered term mentions a variable at a level p holds for
function mentions(t: unknown, p: (i: number) => boolean): boolean {
  if (typeof t !== "object" || t === null) {
    return false;
  }
  const o = t as Record<string, unknown>;
  if (o.$ === "Var" && p(o.i as number)) {
    return true;
  }
  return Object.entries(o).some(([k, v]) => k !== "s" && mentions(v, p));
}

// whether T is a datatype with no constructor left
function no_ctr(e: Safe, T: HTerm): boolean {
  const x = B.term_wnf(e.book, T);
  const tld = x.$ === "ADT" ? e.book.tlds[x.k] : undefined;
  return x.$ === "ADT" && tld?.$ === "ADT" && tld.c.every((c) => x.r.includes(c.k));
}

// T's kind, Data (2) or Type (1), as the kernel infers it: a datatype's
// declared kind, or the kind a type-valued def returns (never through
// the def's body), when its quantity is closed; null when neither, or
// when the quantity is open, which the kernel decides
function kind(e: Safe, s: Scope, T: HTerm): Q | null {
  const [x] = open(T);
  const [h, xs] = x.$ === "ADT" ? [x, x.x] : unapply(x);
  const [f] = open(h);
  const tld = f.$ === "ADT" || f.$ === "Ref" ? e.book.tlds[f.k] : undefined;
  if (tld === undefined) {
    return null;
  }
  try {
    const K = B.term_wnf(e.book, B.tele_fill(e.book, tld.T, xs, B.ctx_nil()));
    const g = K.$ === "Typ" ? closed_val(e, s, K.g) : null;
    return g === null ? null : g.$ === "Qua" && g.q.$ === "Many" ? 2 : 1;
  } catch {
    return null;
  }
}

// Term
// ====

// the kernel term of t; live says whether it runs
function term(e: Safe, s0: Scope, t: HTerm, live: boolean): O {
  const [x, T] = open(t);
  const s = s0.sub && x.$ !== "Ctr" && x.$ !== "Lit" ? { ...s0, sub: false } : s0;
  switch (x.$) {
    case "Var": {
      const b = s.c[x.i];
      if (b === undefined) {
        oos("a free variable " + x.k);
      }
      return b.v !== undefined ? term(e, s0, typed(b.v, b.T), live) : b.o;
    }
    case "Ref":
    case "App": {
      return spine(e, s, t, live);
    }
    case "ADT": {
      const tld = e.book.tlds[x.k];
      if (tld?.$ !== "ADT") {
        oos("an unknown datatype " + B.name_key(x.k));
      }
      return args(e, s, x.k, tld.T, x.x, live);
    }
    case "Typ": {
      return { $: "Typ", q: term(e, s, x.g, false) };
    }
    case "All": {
      const l = s.D;
      const A = term(e, s, x.A, false);
      const Bo = term(e, scope_bind(s, { $: "Var", l }, x.A, true), x.B(B.Var(x.k, s.d)), false);
      return { $: "All", q: quant(x.q), l, A, B: Bo };
    }
    case "Lam":
    case "Mat":
    case "Efq": {
      return tree(e, { ...s, cols: [] }, t, []);
    }
    case "Let": {
      return let_term(e, s, x, live);
    }
    case "Ctr": {
      return ctr_term(e, s, x, T, live);
    }
    case "Lit": {
      if (x.k === "Nat" && x.v > NAT_MAX && e.book.tlds.Nat?.b === true) {
        // a long Nat is q * NAT_MAX + r, by base's Nat.mul and Nat.add
        const [q, r] = [Math.floor(x.v / NAT_MAX), x.v % NAT_MAX];
        const mul = B.App(B.App(B.Ref("Nat.mul"), B.Lit("Nat", q)), B.Lit("Nat", NAT_MAX));
        return term(e, s, B.App(B.App(B.Ref("Nat.add"), mul), B.Lit("Nat", r)), live);
      }
      return term(e, s, B.term_higher(B.lit_step(x)), live);
    }
    case "Eql": {
      return { $: "Eql", a: arg_term(e, s, x.a, x.T, false), b: arg_term(e, s, x.b, x.T, false), T: term(e, s, x.T, false) };
    }
    case "Rfl": {
      return { $: "Rfl" };
    }
    case "Rwt": {
      return rwt_term(e, s, x, live);
    }
    case "Qnt": {
      return { $: "Enu", ks: ["Q0", "Q1", "Q2"] };
    }
    case "Qua": {
      return { $: "Lab", k: "Q" + String(quant(x.q)) };
    }
    case "Min": {
      return { $: "Min", a: term(e, s, x.a, live), b: term(e, s, x.b, live) };
    }
    case "Hol": {
      return oos("a hole");
    }
    default: {
      return oos("a " + x.$ + " term");
    }
  }
}

// a head applied to its arguments: a def, a template at bend2's instance,
// a variable, or an annotated term
function spine(e: Safe, s: Scope, t: HTerm, live: boolean): O {
  const [h, xs] = unapply(t);
  const [f, T] = open(h);
  if (f.$ !== "Ref") {
    const U = f.$ === "Var" ? s.c[f.i]?.T ?? null : T;
    const o = term(e, s, h, live);
    return args(e, s, inferable(o) ? o : { $: "Ann", x: o, T: term(e, s, U ?? oos("an application with no known head type"), false) }, U, xs, live);
  }
  // bend2's instance of a template is the template at its ~ arguments
  const g = e.inst.get(f.k);
  const [k, ys] = g === undefined ? [f.k, xs] : [g[0], [...g[1], ...xs]];
  const tld = e.book.tlds[k] ?? oos("an unknown name " + B.name_key(k));
  if (tld.$ === "Def" && ys.length < tld.x) {
    oos("a template " + B.name_key(k) + " short of its ~ arguments");
  }
  return args(e, s, k, tld.T, ys, live);
}

// the item named k (or the term k) applied to xs along its telescope T:
// the arguments of an item's specialized parameters pick its instance and go
function args(e: Safe, s: Scope, k: Name | O, T: HTerm | null, xs: HTerm[], live: boolean): O {
  const tld = typeof k === "string" ? e.book.tlds[k] : null;
  // a def's first x parameters, its ~ ones, are specialized
  const nx = tld?.$ === "Def" ? tld.x : 0;
  const ps: Arg[] = [];
  let U = T;
  xs.forEach((x, j) => {
    const F = U === null ? U : B.term_wnf(e.book, U);
    if (F?.$ !== "All") {
      return oos("an application past its head's known type");
    }
    const v = j < nx ? spec_val(e, s, x) : null;
    ps.push([quant(F.q), x, F.A, v]);
    U = F.B(v ?? x);
  });
  // a call that closed a live cycle goes again, through its group
  let n: string | null;
  try {
    const g = typeof k === "string" ? group_of(e, k) : null;
    if (g !== null) {
      return group_call(e, s, g, k as Name, ps, U as HTerm, live);
    }
    n = typeof k === "string" ? item_ref(e, k, ps.map((p) => p[3]), live) : null;
  } catch (x) {
    if (x instanceof Reroute) {
      return args(e, s, k, T, xs, live);
    }
    throw x;
  }
  const a = { ...s, sub: live && n === s.self };
  return ps.filter((p) => p[3] === null).reduce<O>((f, [q, x, A]) => ({ $: "App", q, f, x: arg_term(e, a, x, A, live && q > 0) }),
    n === null ? k as O : { $: "Ref", k: n });
}

// Group
// -----
// base declares some defs by a law and fills them after a helper that
// calls them back (String.cmp.fin calls String.cmp, which calls it). The
// kernel has no mutual recursion, so a def k and its helpers go out as
// one def, their group. A mention of a member is a call at its selector,
// k's (.k, ()), a helper h's (.h, (.k, ())): so a helper's call back
// passes a piece of its selector, k's call to a helper a piece of the
// lead, and all mentions of a member convert.

// a group: k, its members (k first), and the lead's quantities (k's)
type Group = { k: Name; ms: Name[]; qs: Q[] };

// k's group, once a live cycle through k has gone out
function group_of(e: Safe, k: Name): Group | null {
  return e.groups.get(k) ?? null;
}

// the live cycle from the item at key to the top of the stack, as a group:
// a group item on it brings its members, and the lead is the member
// filled last (the law its helpers call back). A cycle that adds no
// member (one def, or one group, at other specialized arguments) is not
// one def. Growing a group stales the pass, which may have sent the group
// out already
function regroup(e: Safe, key: string): never {
  const ns = e.stack.slice(e.stack.indexOf(key)).map((x) => x.split("\n")[0]);
  const gs = [...new Set(ns.filter((x) => x[0] === "\t").map((x) => group_of(e, x.slice(1)) as Group))];
  const ms = [...new Set(ns.flatMap((x) => x[0] === "\t" ? (group_of(e, x.slice(1)) as Group).ms : [x]))];
  if (ms.length < 2 || gs.length === 1 && ms.length === gs[0].ms.length) {
    oos("a recursion through " + B.name_key(ms[0]) + " at other specialized arguments");
  }
  const at = (x: Name): number => e.book.order.lastIndexOf(x);
  const k = ms.reduce((a, b) => at(b) > at(a) ? b : a);
  const g = group_new(e, k, ms.filter((x) => x !== k).sort((a, b) => at(a) - at(b)))
    ?? oos("a mutual recursion through " + ms.map(B.name_key).join(", ") + " that no live parameter of " + B.name_key(k) + " leads");
  for (const x of g.ms) {
    e.groups.set(x, g);
  }
  e.grew ||= gs.length > 0;
  throw new Regroup(key);
}

// k's group with the helpers hs: each calls k back, and k's call to a
// helper must shrink a live lead parameter
function group_new(e: Safe, k: Name, hs: Name[]): Group | null {
  const lead = Math.min(...hs.map((h) => {
    const t = (e.book.tlds[h] as Def).e as B.LTerm;
    let n = 0;
    for (let x = t; x.$ === "Ann" || x.$ === "Lam"; x = x.$ === "Ann" ? x.x : x.f) {
      n += x.$ === "Lam" ? 1 : 0;
    }
    // its parameter j, bound by a leading λ, in place in each call back
    const cs = back(t, k);
    const at = (x: B.LTerm | undefined, j: number): boolean => x?.$ === "Ann" ? at(x.x, j) : x?.$ === "Var" && x.i === j;
    let j = 0;
    while (j < n && cs.length > 0 && cs.every((xs) => at(xs[j], j))) {
      j++;
    }
    return j;
  }));
  const tld = e.book.tlds[k];
  const doms = B.tele_unbind(e.book, tld.T).doms.slice(0, lead);
  if (hs.length === 0 || !doms.some(([q], j) => q.$ !== "None" && (tld.$ !== "Def" || j >= tld.x))) {
    return null;
  }
  return { k, ms: [k, ...hs], qs: doms.map(([q]) => quant(q)) };
}

// the arguments of each call to k in the lowered term t
function back(t: unknown, k: Name): B.LTerm[][] {
  if (typeof t !== "object" || t === null) {
    return [];
  }
  let [f, xs]: [B.LTerm, B.LTerm[]] = [t as B.LTerm, []];
  while (f.$ === "App" || f.$ === "Ann") {
    [f, xs] = f.$ === "App" ? [f.f, [f.x, ...xs]] : [f.x, xs];
  }
  return f.$ === "Ref" && f.k === k && xs.length > 0 ? [xs] : Object.entries(t).flatMap(([j, v]) => j === "s" ? [] : back(v, k));
}

// what an emission has added, which a Regroup below it undoes. Emission
// only appends to these six, so their sizes mark it; other state an
// emission changes would outlive the undo
type Trail = [number, number, number, number, number, number];

function trail(e: Safe): Trail {
  return [e.out.length, e.names.size, e.seen.size, e.todo.length, e.taken.size, e.fail.size];
}

function undo(e: Safe, m: Trail): void {
  e.out.length = m[0];
  e.todo.length = m[3];
  for (const [x, n] of [[e.names, m[1]], [e.seen, m[2]], [e.taken, m[4]], [e.fail, m[5]]] as Array<[Map<string, string> | Set<string>, number]>) {
    [...x.keys()].slice(n).forEach((k) => x.delete(k));
  }
}

// a call to member m at the arguments ps (R its type): the group's at
// m's selector, annotated (the kernel infers an unreduced mode)
function group_call(e: Safe, s: Scope, g: Group, m: Name, ps: Arg[], R: HTerm, live: boolean): O {
  const n = g.qs.length;
  if (ps.length < (e.book.tlds[m] as Def).n || ps.slice(n).some((p) => p[3] !== null)) {
    oos("a partial or specialized call to " + m + ", mutually recursive");
  }
  const k = item_ref(e, "\t" + g.k, ps.slice(0, n).map((p) => p[3]), live);
  const a = { ...s, sub: live && k === s.self };
  const args = (xs: Arg[], qs: Q[]): Array<[Q, O]> => xs.flatMap(([q, x, A, v], j): Array<[Q, O]> =>
    v !== null ? [] : [[qs[j] ?? q, arg_term(e, a, x, A, live && (qs[j] ?? q) > 0)]]);
  const sel: O = { $: "Tup", q: 1, a: { $: "Lab", k: name_tt(g.k) }, b: { $: "Lab", k: "()" } };
  const x = [...args(ps.slice(0, n), g.qs), [1, m === g.k ? sel : { $: "Tup", q: 1, a: { $: "Lab", k: name_tt(m) }, b: sel }] as [Q, O],
    ...args(ps.slice(n), [])].reduce<O>((f, [q, x]) => ({ $: "App", q, f, x }), { $: "Ref", k });
  return { $: "Ann", x, T: term(e, s, R, false) };
}

// the group at the specialized lead arguments cols: ∀lead -> ∀s:Sel ->
// (MODE s), MODE the selected member's other parameters and type; its
// tree convoys the lead's q=1 variables through the selector's match
function group_emit(e: Safe, g: Group, cols: Cols, n: string): void {
  const { s, ps: ls, xs: lead } = tele_open(e, { ...scope_nil(), self: n }, e.book.tlds[g.k].T, cols, g.qs.length);
  // member m past the lead, from scope s1 on: its binders, its terms (but
  // at its specialized parameters), its cols and type
  const rest = (s1: Scope, m: Name) => {
    const tld = e.book.tlds[m];
    const nx = tld.$ === "Def" ? tld.x : 0;
    const r = tele_open(e, s1, tld.T, lead, tld.n);
    const qs = B.tele_unbind(e.book, tld.T).doms.map(([q]) => quant(q));
    return { ...r, vs: r.xs.flatMap((x, j): Array<[Q, O]> => j < nx ? [] : [[qs[j], term(e, r.s, x, false)]]), cs: r.xs.map((x, j) => j < nx ? x : null) };
  };
  // the selector's match: a member's arm past its tag (and a helper's (.k, ()))
  const efq: O = { $: "Efq" };
  const unit = (h: O): O => ({ $: "Mat", k: "()", h, m: efq });
  const sel = (f: (m: Name) => O): O => ({ $: "Prj", h: g.ms.reduceRight<O>((m, k, i) => ({ $: "Mat", k: name_tt(k), m,
    h: i === 0 ? unit(f(k)) : { $: "Prj", h: { $: "Mat", k: name_tt(g.k), h: unit(f(k)), m: efq } } }), efq) });
  const l0 = s.D;
  const s0 = scope_kq(scope_hide(s), l0, 1);
  const mode = sel((m) => {
    const r = rest(s0, m);
    return alls(r.ps, term(e, r.s, r.T, false));
  });
  const Sel: O = { $: "Sig", q: 1, l: l0, A: { $: "Enu", ks: g.ms.map(name_tt) }, B: { $: "App", q: 1, x: { $: "Var", l: l0 },
    f: g.ms.reduceRight<O>((m, k, i) => ({ $: "Mat", k: name_tt(k), m, h: i === 0 ? { $: "Enu", ks: ["()"] }
      : { $: "Sig", q: 1, l: l0 + 1, A: { $: "Enu", ks: [name_tt(g.k)] }, B: { $: "Enu", ks: ["()"] } } }), efq) } };
  const To = alls(ls, { $: "All", q: 1, l: l0, A: Sel, B: { $: "App", q: 1, f: mode, x: { $: "Var", l: l0 } } });
  const ks = ls.filter(([q]) => q === 1).map(([, l]) => l);
  const body = sel((m) => convoy_bind(e, s0, ks, (s2) => {
    const r = rest(s2, m);
    return lams(e, r.ps, arm(e, r.s, m, r.cs, r.vs));
  }, []));
  const v = ks.reduce<O>((f, l) => ({ $: "App", q: 1, f, x: { $: "Var", l } }), { $: "App", q: 1, f: body, x: { $: "Var", l: l0 } });
  e.out.push([n, To, lams(e, [...ls, [1, l0]], v), false]);
}

// def k's checked tree at the terms vs (its parameters left to right,
// but the specialized ones, in cols; a group's variables): a λ binds its
// term, and the terms left go to the tree as arguments, (λ{..} x ..), so
// its matches stay nodes of the case tree. bend2 checks a template once,
// each ~ parameter p an opaque constant k~p its tree names, which stands
// for its ~ argument, a column the tree drops
function arm(e: Safe, s: Scope, k: Name, cols: Cols, vs: Array<[Q, O]>): O {
  const tld = e.book.tlds[k] as Def;
  const ps = tld.x === 0 ? null : new Map(B.tele_unbind(e.book, tld.T).doms.slice(0, tld.x).map(([, p], j) => [k + "~" + p, cols[j]]));
  const t0 = B.term_higher(tld.e as B.LTerm);
  let t = ps === null ? t0 : subst(t0, 0, (o) => o.$ === "Ref" ? ps.get(o.k as Name) ?? undefined : undefined);
  let si: Scope = { ...s, c: [], d: 0, cols: cols.slice(tld.x), sub: false };
  let j = 0;
  for (let [x, T] = open(t); x.$ === "Lam" && T !== null; [x, T] = open(t)) {
    const F = B.term_wnf(e.book, T);
    const v = si.cols[0] ?? null;
    if (F.$ !== "All" || (v === null && j === vs.length)) {
      break;
    }
    si = { ...si, cols: si.cols.slice(1) };
    si = v !== null ? scope_bind(si, { $: "Efq" }, F.A, false, v) : scope_bind(si, vs[j++][1], F.A, false);
    t = x.f(B.Var(x.k, si.d - 1));
  }
  return vs.slice(j).reduce<O>((f, [q, x]) => ({ $: "App", q, f, x }), tree(e, si, t, []));
}

// an argument at its domain A: an untyped λ or match (in a type) takes
// A as its goal. Any other argument of a function type goes η-long at A
// (a tree leaf does): the kernel converts without η, and an argument may
// reach a type, as a side of an equation whose carrier only the call
// knows. A live self-call's argument stays whole, as the live check
// reads it, unless its binders differ from A's
function arg_term(e: Safe, s: Scope, x: HTerm, A: HTerm, live: boolean): O {
  const [y, T] = open(x);
  const tree = y.$ === "Lam" || y.$ === "Mat" || y.$ === "Efq";
  const all = tree || y.$ === "Ctr" ? null : all_of(e, A);
  if (all !== null && (!(live && s.sub) || T !== null && !qsig_eq(e, T, A, s.d))) {
    return term(e, s, eta(x, all), live);
  }
  return term(e, s, T === null && tree ? B.Ann(x, A) : x, live);
}

// whether two function types bind at the same quantities: the kernel's
// fit compares binders exactly, and bend2 lets a function fit a domain
// whose binders differ; the kernel fits their domains, codomains and kinds
// as bend2 does
function qsig_eq(e: Safe, T: HTerm, A: HTerm, d: number): boolean {
  const F = B.term_wnf(e.book, T);
  const G = B.term_wnf(e.book, A);
  if (F.$ !== "All" || G.$ !== "All") {
    return F.$ !== "All" && G.$ !== "All";
  }
  const x = B.Var(G.k, d);
  return F.q.$ === G.q.$ && qsig_eq(e, F.B(x), G.B(x), d + 1);
}

// a constructor as a tuple of its tag and fields
function ctr_term(e: Safe, s: Scope, x: Extract<HTerm, { $: "Ctr" }>, T: HTerm | null, live: boolean): O {
  const ctr = e.book.ctrs[x.k];
  if (ctr === undefined) {
    oos("an unknown constructor " + B.name_key(x.k));
  }
  const f = B.book_fam(e.book, x.k);
  const fam = e.book.tlds[f] as ADT;
  const w = B.u32_from_term(x) ?? B.u32_from_term(x, "F32");
  if (!s.sub && w !== null) {
    if (fam.n !== 0) {
      oos("a word literal of a parameterized datatype");
    }
    return word_ref(e, x.k, f, w);
  }
  const G = T === null || fam.n === 0 ? null : B.term_wnf(e.book, T);
  const ps = G?.$ === "ADT" ? G.x : Array.from({ length: fam.n }, () => B.Var("_", -1));
  let F = B.tele_fill(e.book, ctr.T, ps, B.ctx_nil());
  const fs: Array<[Q, O]> = [];
  for (const a of x.x) {
    const A = B.term_wnf(e.book, F);
    if (A.$ !== "All") {
      oos("a constructor past its fields");
    }
    const q = quant(A.q);
    fs.push([q, arg_term(e, s, a, A.A, live && q > 0)]);
    F = A.B(a);
  }
  const k = name_tt(x.k);
  const tail = fs.reduceRight<O>((b, [q, a]) => ({ $: "Tup", q, a, b }), { $: "Lab", k: "()" });
  return { $: "Tup", q: 1, a: { $: "Lab", k }, b: tail };
}

// a U32 or F32 word, as a def of its own
function word_ref(e: Safe, T: Name, fam: Name, n: number): O {
  const key = "\tword " + T + " " + String(n);
  let k = e.names.get(key);
  if (k === undefined) {
    k = fresh(e, T + ".lit" + String(n));
    e.names.set(key, k);
    const v = term(e, { ...scope_nil(), sub: true }, B.Ctr(T, [B.word_to_term(n)]), true);
    e.out.push([k, { $: "Ref", k: item_ref(e, fam, [], false) }, v, false]);
  }
  return { $: "Ref", k };
}

// parallel lets as nested kernel lets; a let of a variable is inlined,
// and so is a constructor a self-call takes. Build the latter at each
// use's depth and mode, where the kernel reads the column it rebuilds
function let_term(e: Safe, s: Scope, x: Extract<HTerm, { $: "Let" }>, live: boolean, put: Set<number> = new Set()): O {
  let s2 = s;
  const ls: Array<[Q, number, O, number]> = [];
  for (let j = 0; j < x.k.length; j++) {
    const q = quant(x.q[j]);
    const V = open(x.v[j])[1];
    if (put.has(j)) {
      s2 = scope_bind(s2, { $: "Efq" }, V, false, x.v[j]);
      continue;
    }
    // a parallel let's value sees s's variables, below the lets before it
    const at = { ...s, D: s2.D, kq: s2.kq };
    const v = term(e, at, x.v[j], live && q > 0);
    if (v.$ === "Var") {
      s2 = scope_bind(s2, v, V, false);
    } else {
      const l = s2.D;
      const w: O = inferable(v) ? v : V === null ? oos("a let with no known type")
        : { $: "Ann", x: v, T: term(e, at, V, false) };
      ls.push([q, l, w, j]);
      s2 = scope_kq(scope_bind(s2, { $: "Var", l }, V, true), l, q);
    }
  }
  const xs = x.k.map((k, j) => B.Var(k, s.d + j));
  const f = term(e, s2, (x.f as (xs: HTerm[]) => HTerm)(xs), live);
  const more = ls.filter(([, l, v]) => live && v.$ !== "App" && v.$ !== "Ref" && self_arg(f, s.self, l)).map(([, , , j]) => j);
  if (more.length > 0) {
    return let_term(e, s, x, live, new Set([...put, ...more]));
  }
  return ls.reduceRight<O>((b, [q, l, v]) => ({ $: "Let", q: q === 1 && uses(e, b, l) > 1 ? 2 : q, l, v, f: b }), f);
}

// a rewrite: the kernel's J, whose motive binds the endpoint at l and
// its evidence at l + 1
function rwt_term(e: Safe, s: Scope, x: Extract<HTerm, { $: "Rwt" }>, live: boolean): O {
  const E0 = open(x.e)[1];
  const e0 = term(e, s, x.e, live);
  const ev: O = inferable(e0) || E0 === null ? e0 : { $: "Ann", x: e0, T: term(e, s, E0, false) };
  const [p] = open(x.p);
  if (p.$ !== "Lam") {
    oos("a rewrite motive that is not a λ");
  }
  const l = s.D;
  const Q = E0 === null ? null : B.term_wnf(e.book, E0);
  const q = Q !== null && Q.$ === "Eql" ? Q : null;
  const s2 = scope_bind(s, { $: "Var", l }, q && q.T, true);
  const [p2] = open(p.f(B.Var(p.k, s.d)));
  if (p2.$ !== "Lam") {
    oos("a rewrite motive that is not a λ over its evidence");
  }
  const s3 = scope_bind(s2, { $: "Var", l: l + 1 }, q && B.Eql(q.a, B.Var(p.k, s.d), q.T), true);
  const P = term(e, s3, p2.f(B.Var(p2.k, s2.d)), false);
  return { $: "Rwt", e: ev, l, P, f: term(e, s, x.f, live) };
}

// Kernel terms
// ============

// whether o calls def k with the variable at level l as an argument
function self_arg(o: O, k: string, l: number): boolean {
  let h: O = o;
  let found = false;
  while (h.$ === "App") {
    found ||= h.x.$ === "Var" && h.x.l === l && h.q > 0;
    h = h.f;
  }
  return (found && h.$ === "Ref" && h.k === k)
    || Object.values(o).some((v) => is_o(v) && self_arg(v, k, l));
}

// the def names o mentions
function o_refs(o: O, out: Set<string> = new Set()): Set<string> {
  if (o.$ === "Ref") {
    out.add(o.k);
  }
  for (const v of Object.values(o)) {
    if (is_o(v)) {
      o_refs(v, out);
    }
  }
  return out;
}

function is_o(v: unknown): v is O {
  return typeof v === "object" && v !== null && "$" in v;
}

// ∀s (or Σs) over b, at the binders ps
function alls(ps: Binder[], b: O, $: "All" | "Sig" = "All"): O {
  return ps.reduceRight<O>((B, [q, l, A]) => ({ $, q, l, A, B }), b);
}

// λs over b, at the binders ps; one that uses its variable twice copies it
function lams(e: Safe, ps: Array<[Q, number, ...unknown[]]>, b: O): O {
  return ps.reduceRight<O>((f, [q, l]) => ({ $: "Lam", q: q === 1 && uses(e, f, l) > 1 ? 2 : q, l, f }), b);
}

function inferable(o: O): boolean {
  return o.$ === "App" ? inferable(o.f) : ["Var", "Ref", "Ann", "Typ", "Min", "All", "Enu", "Eql"].includes(o.$);
}

// the live uses of level l in o, as the kernel counts them
function uses(e: Safe, o: O, l: number): number {
  return o_uses(e, o).get(l) ?? 0;
}

// the live uses of each level o names, in one walk: a level named only
// where the kernel counts no use (a type, a q=0 argument) counts 0. A
// term counted on its own keeps its counts, which a walk of a term built
// around it reads
function o_uses(e: Safe, o: O, live: boolean = true, out?: Map<number, number>): Map<number, number> {
  const kept = e.uses.get(o);
  if (out === undefined && kept === undefined) {
    e.uses.set(o, o_uses(e, o, true, new Map()));
  }
  if (out === undefined) {
    return e.uses.get(o) as Map<number, number>;
  }
  if (kept !== undefined) {
    kept.forEach((n, l) => out.set(l, (out.get(l) ?? 0) + (live ? n : 0)));
    return out;
  }
  switch (o.$) {
    case "Var": return out.set(o.l, (out.get(o.l) ?? 0) + (live ? 1 : 0));
    case "Ann": return o_uses(e, o.T, false, o_uses(e, o.x, live, out));
    case "Let": return o_uses(e, o.f, live, o_uses(e, o.v, live && o.q > 0, out));
    case "All": case "Sig": return o_uses(e, o.B, false, o_uses(e, o.A, false, out));
    case "Lam": return o_uses(e, o.f, live, out);
    case "App": return o_uses(e, o.x, live && o.q > 0, o_uses(e, o.f, live, out));
    case "Tup": return o_uses(e, o.b, live, o_uses(e, o.a, live && o.q > 0, out));
    case "Prj": return o_uses(e, o.h, live, out);
    case "Mat": return o_uses(e, o.m, live, o_uses(e, o.h, live, out));
    case "Min": return o_uses(e, o.b, live, o_uses(e, o.a, live, out));
    case "Eql": return o_uses(e, o.T, false, o_uses(e, o.b, false, o_uses(e, o.a, false, out)));
    case "Rwt": return o_uses(e, o.f, live, o_uses(e, o.P, false, o_uses(e, o.e, live, out)));
    default: return out;
  }
}

// Show
// ====

function book_show(e: Safe): string {
  let p = "x";
  while ([...e.taken].some((k) => new RegExp("^" + p + "[0-9]+$").test(k))) {
    p += "x";
  }
  return e.out.map(([k, T, v, m]) => (m ? "opaque " : "") + k + " : " + o_show(T, p) + " =\n  " + o_show(v, p)).join("\n\n") + "\n";
}

function mark(q: Q): string {
  return q === 0 ? "-" : q === 2 ? "+" : "";
}

function o_show(o: O, p: string): string {
  const nm = (l: number): string => p + String(l);
  switch (o.$) {
    case "Var": return nm(o.l);
    case "Ref": return o.k;
    case "Ann": return "{" + o_show(o.x, p) + " : " + o_show(o.T, p) + "}";
    case "Let": return "!" + mark(o.q) + nm(o.l) + " = " + o_show(o.v, p) + "; " + o_show(o.f, p);
    case "Typ": return "*(" + o_show(o.q, p) + ")";
    case "Min": return "(" + o_show(o.a, p) + " <&> " + o_show(o.b, p) + ")";
    case "All": return "∀" + mark(o.q) + nm(o.l) + " : " + o_show(o.A, p) + " -> " + o_show(o.B, p);
    case "Lam": return "λ" + mark(o.q) + nm(o.l) + " => " + o_show(o.f, p);
    case "App": {
      const xs: string[] = [];
      let f: O = o;
      while (f.$ === "App") {
        xs.push(mark(f.q) + o_show(f.x, p));
        f = f.f;
      }
      return "(" + o_show(f, p) + " " + xs.reverse().join(" ") + ")";
    }
    case "Sig": return "Σ" + mark(o.q) + nm(o.l) + " : " + o_show(o.A, p) + " -> " + o_show(o.B, p);
    case "Tup": return "(" + mark(o.q) + o_show(o.a, p) + ", " + o_show(o.b, p) + ")";
    case "Prj": return "λ{(,): " + o_show(o.h, p) + "}";
    case "Enu": return "<" + o.ks.join(", ") + ">";
    case "Lab": return o.k === "()" ? "()" : "." + o.k;
    case "Mat": return "λ{" + o_show({ $: "Lab", k: o.k }, p) + ": " + o_show(o.h, p) + "; " + o_show(o.m, p) + "}";
    case "Efq": return "λ{}";
    case "Eql": return "{" + o_show(o.a, p) + " == " + o_show(o.b, p) + " : " + o_show(o.T, p) + "}";
    case "Rfl": return "{==}";
    case "Rwt": return "%" + o_show(o.e, p) + " : " + nm(o.l) + ", " + nm(o.l + 1) + " => " + o_show(o.P, p) + "; " + o_show(o.f, p);
  }
}

// Kernel
// ======

// the kernel's CLI: $BENDTT, else a build of bendtt.lean (in BEND_DIR,
// beside base.bend) cached by source and Lean version, made once with
// v4.34.0 (elan's toolchain, or lean and leanc on the PATH)
function kernel_bin(): string {
  const env = process.env.BENDTT;
  if (env !== undefined && env !== "") {
    return env;
  }
  const src = path.join(B.BEND_DIR, "bendtt.lean");
  const text = fs.readFileSync(src, "utf8");
  const hash = crypto.createHash("sha256").update(LEAN_VERSION).update("\0").update(text).digest("hex").slice(0, 16);
  const dir = path.join(os.homedir(), ".bend", "bendtt", hash);
  const bin = path.join(dir, "bendtt");
  if (fs.existsSync(bin)) {
    return bin;
  }
  const home = path.join(os.homedir(), ".elan", "toolchains", "leanprover--lean4---v" + LEAN_VERSION, "bin");
  const tool = (t: string): string => fs.existsSync(path.join(home, t)) ? path.join(home, t) : t;
  fs.mkdirSync(dir, { recursive: true });
  const fail = (why: string): never => {
    throw new Error("BendTT infrastructure: the kernel did not build (" + why
      + "); --verdict needs Lean v" + LEAN_VERSION + " (elan toolchain leanprover/lean4:v" + LEAN_VERSION + "), or $BENDTT set to a built kernel");
  };
  const run = (bin: string, args: string[]): string => {
    const [got, text] = run_read(bin, args, { cwd: dir });
    if (got.status !== 0) {
      fail(bin + ": " + (got.error?.message ?? text.slice(0, 300)));
    }
    return text;
  };
  const version = run(tool("lean"), ["--version"]);
  if (version.match(/^Lean \(version ([^,\s)]+)/m)?.[1] !== LEAN_VERSION) {
    fail("lean --version: " + version.trim().slice(0, 300));
  }
  fs.copyFileSync(src, path.join(dir, "bendtt.lean"));
  run(tool("lean"), ["-c", "bendtt.c", "bendtt.lean"]);
  run(tool("leanc"), ["-O3", "-DNDEBUG", "bendtt.c", "-o", "bendtt"]);
  return bin;
}

// run_read runs a child to its end: its result, and its stdout and stderr
// as one text. The text goes through a file, not a pipe: bun's spawnSync
// (1.3.14) can lose a pipe's bytes under load (status 0, stdout ""; seen
// for the kernel, and for clang --version behind a tee that got its text)
export function run_read(bin: string, args: string[],
  opts: child.SpawnSyncOptionsWithBufferEncoding = {}): [child.SpawnSyncReturns<Buffer>, string] {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-run-"));
  const log = path.join(dir, "out");
  const fd = fs.openSync(log, "w");
  try {
    const got = child.spawnSync(bin, args, { ...opts, stdio: ["ignore", fd, fd] });
    return [got, fs.readFileSync(log, "utf8")];
  } finally {
    fs.closeSync(fd);
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

// the kernel's verdict on a book's text: ok on exit 0 with its exact
// success line. It checks a copy in a fresh private dir, which no one
// else can swap, and a kernel reading /dev/stdin got EBADF (1 in 1486)
function kernel_check(text: string): boolean {
  const env = { ...process.env, LEAN_STACK_SIZE_KB: "4194304" };
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "bendtt-"));
  try {
    const inp = path.join(dir, "in.bendtt");
    fs.writeFileSync(inp, text, { flag: "wx" });
    const [got, out] = run_read(kernel_bin(), [inp], { env });
    if (got.error || got.signal || got.status === null
      || (out.trim() !== "ALL PROOFS CHECK" && !out.startsWith("SOME PROOFS FAIL"))) {
      throw new Error("BendTT infrastructure: " + (got.error?.message
        ?? (got.signal ? "kernel terminated by " + got.signal : out.trim()
          || "kernel exited " + got.status + " without a verdict")));
    }
    return got.status === 0 && out.trim() === "ALL PROOFS CHECK";
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

// -o <out>.bendtt: writes the elaboration of a book bend2 checked to
// out; gives a line per def out of scope, with why
export function safe_emit(book: Book, out: string): string[] {
  const got = safe_book(book);
  fs.writeFileSync(out, got.text);
  return got.oos.map(([k, why]) => "- " + B.name_key(k) + ": " + why + "\n");
}

// --verdict: whether every def of a book bend2 checked is in the kernel's
// scope, and the kernel checks them all
export function safe_check(book: Book): boolean {
  const got = safe_book(book);
  return got.oos.length === 0 && kernel_check(got.text);
}
