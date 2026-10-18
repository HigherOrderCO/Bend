// Safe
//
// --verdict runs source checking, safe_book, then bendtt.lean.
// The kernel trusts no exporter output.
//
// Checked Def.e body nodes have types; terms inside types do not. Pass
// expected codomains and field types to bare lambdas and matches. Source
// conversion includes η. Kernel conversion lacks η and unfolds only
// applied definitions. Export function values η-long.
//
// Encode D<ps> as Σt:<tags> -> D.arms(ps,t). Each arm is a Σ field chain
// ending in <()>. Preserve field quantities; recursive fields name D.
// k{x,y} becomes (.k,x,y,()). Matches split the tag/fields pair, then the
// tag, fields, and unit. Defaults bind the pair. Empty D also emits the
// checked D.efq(ps):D(ps)-><>.
//
// Kernel live descent reads case trees. A self-call passes live columns
// whole, left to right, until one becomes a piece of itself. Count uses
// in both arms. Keep reconstructed constructors for kernel rule K3-R.
//
// Kind(&0) exports as *(.Q0), which fits Type (*(.Q1)) under source LE.
// Specialize each item once per closed ~ argument tuple.
//
// Emit live dependencies first; type dependencies may follow. Bodyless
// laws, natives, and foreign fills use checked opaque models.
// Out-of-scope items, their dependents, and items with no kernel definition
// fail --verdict.

import * as child from "node:child_process";
import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import * as B from "./bend.ts";
import type { Book, Def, HTerm, Name, Quant, ADT } from "./bend.ts";

// Types

type Q = 0 | 1 | 2;
type Source<K extends HTerm["$"]> = Extract<HTerm, { $: K }>;

// Kernel terms use variable levels.
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

// Specialized parameters and inlined lets use v, rebuilt at each use's
// depth and mode.
type Bind = { o: O; T: HTerm | null; v?: HTerm };

// Specialized item/tree arguments; null at other parameters/columns.
type Cols = Array<HTerm | null>;

// [quantity, term, domain, specialized value]
type Arg = [Q, HTerm, HTerm, HTerm | null];

// Kernel binder: quantity, level and type
type Binder = [Q, number, O];

// Remaining Σ fields and convoy variables, rebound after the chain.
type Chain = { n: number; cv: number[] };

// c and d track source levels; D tracks kernel depth; self is the kernel
// name. kq holds tentative quantities; empty holds refutations. sub keeps
// word constructors for descent. tags lists default tag levels (fields
// follow). dry counts uses; again rebuilds convoys.
type Scope = { c: Bind[]; d: number; D: number; cols: Cols;
  self: string; empty: O[]; sub: boolean; kq: Q[]; tags: number[]; dry: boolean; again: boolean };

// The lead k is first in ms; qs contains its quantities.
type Group = { k: Name; ms: Name[]; qs: Q[] };

// Constants

const NAT_MAX = 4096;

// Model search charges one step per type and per datatype key character.
const MODEL_FUEL = 1 << 22;
const MODEL_DEPTH = 8;

const LEAN_VERSION = "4.34.0";

const QS = ["Q0", "Q1", "Q2"];

// Constructors

function Var(l: number) {
  return { $: "Var" as const, l };
}

function Ref(k: string) {
  return { $: "Ref" as const, k };
}

function Ann(x: O, T: O) {
  return { $: "Ann" as const, x, T };
}

function Let(q: Q, l: number, v: O, f: O) {
  return { $: "Let" as const, q, l, v, f };
}

function All(q: Q, l: number, A: O, B: O) {
  return { $: "All" as const, q, l, A, B };
}

function Lam(q: Q, l: number, f: O) {
  return { $: "Lam" as const, q, l, f };
}

function App(q: Q, f: O, x: O) {
  return { $: "App" as const, q, f, x };
}

function Sig(q: Q, l: number, A: O, B: O) {
  return { $: "Sig" as const, q, l, A, B };
}

function Tup(q: Q, a: O, b: O) {
  return { $: "Tup" as const, q, a, b };
}

function Prj(h: O) {
  return { $: "Prj" as const, h };
}

function Lab(k: string) {
  return { $: "Lab" as const, k };
}

function Efq() {
  return { $: "Efq" as const };
}

const UT: O = { $: "Enu", ks: ["()"] };
const UV = Lab("()");

// Errors

// An export outside the kernel's supported scope.
class Scope_Error extends Error {}

class Open_Error extends Scope_Error {}

// On a live cycle, undo emission since key began and retry with Reroute.
class Regroup extends Error {
  constructor(readonly key: string) {
    super(key);
  }
}
class Reroute extends Error {}

function oos(why: string, E = Scope_Error): never {
  throw new E(why);
}

// Book

// Export a source-checked book; visit non-base roots in source fill order.
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

// Restart when group growth invalidates emissions or a member was named
// before its group was known.
function safe_pass(book: Book, groups: Map<Name, Group>, inst: Map<Name, [Name, HTerm[]]>): { text: string; oos: Array<[Name, string]> } | null {
  const g0 = groups.size;
  const wnf = (t: HTerm) => B.term_wnf(book, t);
  const fill = (T: HTerm, xs: HTerm[]) => B.tele_fill(book, T, xs, B.ctx_nil());
  // out entries are [name, type, value, opaque]. mb exposes models of root
  // constants; book keeps them opaque. stack is outermost first.
  const mb: Book = { ...book, tlds: Object.create(book.tlds) as Book["tlds"] };
  let out: Array<[string, O, O, boolean]> = [];
  const names = new Map<string, string>();
  const seen = new Set<string>();
  const todo: Array<[Name, Cols]> = [];
  const taken = new Set<string>();
  const fail = new Map<string, string>();
  const log: Array<[Map<string, string> | Set<string>, string]> = [];
  const stack: string[] = [];
  const going = new Set<string>();
  let grew = false;
  const consts = new Map<string, Name>();
  const use_counts = new WeakMap<O, Map<number, number>>();
  const roots: Array<[Name, string[], string | null]> = [];
  for (const k of [...new Set([...book.order].reverse())].reverse().filter((k) => book.tlds[k].b !== true)) {
    const m = trail();
    try {
      roots.push([k, ...root_emit(k)]);
    } catch (x) {
      if (!(x instanceof Scope_Error)) {
        throw x;
      }
      undo(m);
      const n = fresh(name_tt(k));
      put(fail, n, x.message);
      roots.push([k, [n], null]);
    }
  }
  for (let it = todo.pop(); it !== undefined; it = todo.pop()) {
    item_try(it[0], it[1]);
  }
  if (grew || groups.size > g0 && [...names.keys()].some((key) => key[0] !== "\t" && groups.has(key.split("\n")[0]))) {
    return null;
  }
  const bad = new Map(fail);
  for (let more = bad.size > 0; more;) {
    more = false;
    for (const [k, T, v] of out) {
      const r = bad.has(k) ? undefined : [...o_refs(T), ...o_refs(v)].find((r) => bad.has(r));
      if (r !== undefined) {
        bad.set(k, "names " + B.name_key(r) + ", out of scope: " + (fail.get(r) ?? bad.get(r)));
        more = true;
      }
    }
  }
  out = out.filter(([k]) => !bad.has(k));
  const why = (n: string) => bad.has(n) ? [bad.get(n) as string] : [];
  const rejected = roots.flatMap(([k, ns, top]) => {
    const ws = ns.flatMap(why);
    const all = ws.length > 0 ? ws : top !== null ? why(top) : ns.length === 0 ? ["no kernel def"] : [];
    return [...new Set(all)].map((w): [Name, string] => [k, w]);
  });
  return { text: book_show(), oos: rejected };

  // Enumerate finite ~ parameters: Quant or parameterless fieldless
  // datatypes. Use typed opaque constants for other parameters; share by
  // position, type, and model. Templates must hold for all arguments. Finite
  // λ-matches check coverage and each instance's type against G, not which
  // instance belongs to each arm. A bound ~ call in G permits bare references;
  // other G export failures reject k.
  function root_emit(k: Name): [string[], string | null] {
    const tld = book.tlds[k];
    const ns: string[] = [];
    // G is k's exported type: undefined before a finite parameter, null if
    // it cannot export.
    let G: O | null | undefined;
    let ps: Binder[] = [];
    const kc = new Map<HTerm, O>();
    const pick = tld.$ === "Def" && tld.x > 0 ? inst_pick(k) : null;
    const col = (v: HTerm) => kc.get(v) ?? kc.set(v, term(scope_nil(), v, false)).get(v) as O;
    const go = (T: HTerm, cs: Cols, d: number): O => {
      const j = cs.length;
      const F = j === tld.n ? null : wnf(T);
      if (F?.$ !== "All") {
        ns.push(item_try(k, cs));
        const x = Ref(ns[ns.length - 1]);
        return G == null ? x : Ann(x, ps.reduce<O>((f, [q], j) =>
          App(q, f, col(cs[j] as HTerm)), Var(0)));
      }
      const at = (v: HTerm | null, d1 = d) =>
        go(F.B(v ?? B.Var(F.k, j)), [...cs, v], d1);
      if (tld.$ !== "Def" || j >= tld.x) {
        return at(null);
      }
      const A = wnf(F.A);
      const adt = A.$ === "ADT" && A.x.length === 0 ? book.tlds[A.k] as ADT : null;
      const vs: HTerm[] | null = A.$ === "Qnt" ? [B.None(), B.Lone(), B.Many()].map((q) => B.Qua(q))
        : adt !== null && adt.c.every((c) => wnf(c.T).$ !== "All") ? adt.c.map((c) => B.Ctr(c.k, [])) : null;
      if (vs !== null) {
        if (G === undefined) {
          G = null;
          const m = trail();
          try {
            const t = tele_open(scope_nil(), tld.T, [], tld.x);
            const b = term(t.s, t.T, false);
            G = Ann(lams(t.ps, b), alls(t.ps, { $: "Typ", q: Lab("Q1") }));
            ps = t.ps;
          } catch (x) {
            if (!(x instanceof Open_Error)) {
              throw x;
            }
            undo(m);
          }
        }
        const hs = vs.map((v): O => Let(0, d, at(v, d + 1), UV));
        const M = adt === null ? mats(QS, (i) => hs[i])
          : Prj(mats(adt.c.map((c) => name_tt(c.k)), (i) => unit(hs[i])));
        return Ann(M, All(1, d, term(scope_nil(), A, false), UT));
      }
      if (mentions(B.term_lower(F.A, j), (i) => i >= 0 && i < j)) {
        oos("a specialized parameter whose type names a parameter");
      }
      const f = pick === null ? null : fit(k, pick[j], F.A);
      const key = String(j) + "\n" + B.term_key(B.term_lower(F.A)) + "\n" + (f === null ? "" : B.term_key(B.term_lower(f)));
      const kept = consts.get(key);
      if (kept !== undefined) {
        return at(B.Ref(kept));
      }
      let c = k + "~" + F.k;
      while (book.tlds[c] !== undefined) {
        c += "~";
      }
      const m = f ?? model(F.A);
      const def: Def = { $: "Def", n: 0, x: 0, T: F.A, v: null };
      book.tlds[c] = def;
      if (m !== null) {
        mb.tlds[c] = { ...def, v: m };
      }
      consts.set(key, c);
      return at(B.Ref(c));
    };
    const h = go(tld.T, [], 2);
    if (G === undefined) {
      return [ns, null];
    }
    const v = Let(0, 1, h, UV);
    const n = fresh(name_tt(k));
    out.push([n, UT, G === null ? v : Let(0, 0, G, v), false]);
    return [ns, n];
  }

  function item_try(k: Name, cols: Cols): string {
    try {
      return item_ref(k, cols, true);
    } catch (x) {
      if (x instanceof Reroute) {
        return item_try(k, cols);
      }
      if (!(x instanceof Scope_Error)) {
        throw x;
      }
      return names.get(item_key(k, cols)) as string;
    }
  }

  function item_ref(k: Name, cols: Cols, live: boolean): string {
    const key = item_key(k, cols);
    const m = live && !seen.has(key) ? trail() : null;
    let n = names.get(key);
    if (n === undefined) {
      const tag = cols.flatMap((v) => v === null ? [] : [v.$ === "Qua" ? String(quant(v.q)) : "v"]).join("");
      n = fresh(name_tt(k[0] === "\t" ? k.slice(1) + ".group" : k) + (tag !== "" ? ".q" + tag : ""));
      put(names, key, n);
      todo.push([k, cols]);
    }
    const why = fail.get(n);
    if (why !== undefined && live) {
      oos(why);
    }
    if (m !== null) {
      put(seen, key);
      stack.push(key);
      going.add(key);
      try {
        item_emit(k, cols, n);
      } catch (x) {
        if (x instanceof Scope_Error) {
          put(fail, n, x.message);
        }
        if (x instanceof Regroup && x.key === key) {
          undo(m);
          throw new Reroute();
        }
        throw x;
      } finally {
        stack.pop();
        going.delete(key);
      }
    } else if (live && going.has(key) && stack[stack.length - 1] !== key) {
      regroup(key);
    }
    return n;
  }

  function item_emit(k: Name, cols: Cols, n: string): void {
    if (k[0] === "\t") {
      return group_emit(group_of(k.slice(1)) as Group, cols, n);
    }
    const tld = book.tlds[k];
    if (tld === undefined) {
      oos("an unknown name " + B.name_key(k));
    }
    if (tld.$ === "ADT") {
      return adt_emit(cols, n, tld);
    }
    if (tld.u === true) {
      oos("uses " + (tld.b === true ? "base's" : "the") + " @unsafe def " + B.name_key(k));
    }
    def_emit(k, cols, n, tld);
  }

  // Specialize the type here; arm consumes the template columns in the body.
  function def_emit(k: Name, cols: Cols, n: string, tld: Def): void {
    const T = fill(tld.T, cols.slice(0, tld.x) as HTerm[]);
    const t = tld.e !== undefined ? null : (Object.hasOwn(mb.tlds, k) ? (mb.tlds[k] as Def).v : model(T)) ?? oos("no model for " + (tld.i === undefined ? "" : (tld.b === true ? "base's" : "the") + " foreign def ") + B.name_key(k));
    const s = { ...scope_nil(), self: n };
    const To = term(s, T, false);
    out.push([n, To, t === null ? arm(s, k, cols, []) : tree(s, t, []), t !== null]);
  }

  function adt_emit(cols: Cols, n: string, tld: ADT): void {
    const { s, ps, xs, T } = tele_open(scope_nil(), tld.T, cols, tld.n);
    const K = wnf(T);
    if (K.$ !== "Typ") {
      oos("a datatype kind");
    }
    const G = term(s, K, false);
    const am = fresh(n + ".arms");
    const t = s.D;
    const fs = (c: B.Ctr) => alls(tele_open(s, fill(c.T, xs), [], Infinity).ps, UT, "Sig");
    const ks = tld.c.map((c) => name_tt(c.k));
    const arms = mats(ks, (i) => fs(tld.c[i]));
    const Enu: O = { $: "Enu", ks };
    out.push([am, alls(ps, All(1, t, Enu, G)), lams(ps, arms), false]);
    const at = (k: string) => ps.reduce<O>((f, [q, l]) => App(q, f, Var(l)), Ref(k));
    out.push([n, alls(ps, G), lams(ps, Sig(1, t, Enu, App(1, at(am), Var(t)))), false]);
    if (tld.c.length === 0) {
      out.push([fresh(n + ".efq"), alls(ps, All(1, t, at(n), { $: "Enu", ks: [] })), lams(ps, Prj(Efq())), false]);
    }
  }

  // Specialized parameters consume no kernel binder.
  function tele_open(s: Scope, T: HTerm, cols: Cols, n: number): { s: Scope; ps: Binder[]; xs: HTerm[]; T: HTerm } {
    const r = { s, ps: [] as Binder[], xs: [] as HTerm[], T };
    for (let j = 0; j < n; j++) {
      const F = all_of(r.T);
      if (F === null) {
        break;
      }
      const q = quant(F.q);
      if (cols[j] == null) {
        r.ps.push([q, r.s.D, term(r.s, F.A, false)]);
        r.s = scope_bind(r.s, F.A, q);
      }
      r.xs.push(cols[j] ?? B.Var(F.k, r.s.d - 1));
      r.T = F.B(r.xs[j]);
    }
    return r;
  }

  // Specialize

  function spec_val(s: Scope, x: HTerm) {
    return closed_val(s, x) ?? oos("a template argument that depends on a run-time value", Open_Error);
  }

  // Normalize a closed argument; return null if it depends on runtime values.
  function closed_val(s: Scope, x: HTerm) {
    const v = B.term_snf(book, subst(x, s.d, (o) => o.$ === "Var" && (o.i as number) >= 0 && (o.i as number) < s.d
      ? s.c[o.i as number]?.v ?? B.Var(o.k as Name, o.i as number) : undefined));
    return mentions(B.term_lower(v, s.d), (i) => i >= 0 && i < s.d) ? null : v;
  }

  // Model
  // Try constructors in declaration order without repeating datatype keys
  // on a path. Reuse lambda bodies by substitution. On failure, retry
  // preferring a live codomain parameter. Read opaque definitions at their models in mb.

  function model(T: HTerm): HTerm | null {
    for (const proj of [false, true]) {
      let left = MODEL_FUEL, depth = MODEL_DEPTH, cut = true;
      function model_at(T: HTerm, d: number, path: string[], hs: Array<[HTerm, HTerm]>): HTerm | null {
        if (path.length + d > depth || left <= 0) {
          cut = true;
          return null;
        }
        left -= 1;
        const F = B.term_wnf(mb, T);
        const hyp = () => hs.find(([, A]) => B.term_compare("EQ", mb, A, F, d))?.[0] ?? null;
        switch (F.$) {
          case "Typ": {
            const tld = mb.tlds["Unit"];
            return tld?.$ === "ADT" && tld.n === 0 ? B.ADT("Unit", []) : null;
          }
          case "All": {
            const x: HTerm = B.Var(F.k, d);
            const t = model_at(F.B(x), d + 1, path, F.q.$ === "None" ? hs : [...hs, [x, F.A]]);
            return t === null ? null : B.Ann(B.Lam(F.k, d, (v: HTerm) =>
              subst(t, d + 1, (o) => o.$ !== "Var" || (o.i as number) > d ? undefined : o.i === d ? v : B.Var(o.k as Name, o.i as number))), F);
          }
          case "ADT": {
            const key = model_key(F, d);
            left -= key.length;
            const tld = mb.tlds[F.k];
            if (tld?.$ !== "ADT") {
              return hyp();
            }
            const h = proj ? hyp() : null;
            for (const c of path.includes(key) || h !== null ? [] : tld.c.filter((c) => !F.r.includes(c.k))) {
              const xs: HTerm[] = [];
              let U = B.term_wnf(mb, B.tele_fill(mb, c.T, F.x, B.ctx_nil()));
              let x: HTerm | null = null;
              while (U.$ === "All" && (x = model_at(U.A, d, [...path, key], [])) !== null) {
                xs.push(x);
                U = B.term_wnf(mb, U.B(x));
              }
              if (U.$ !== "All") {
                return B.Ann(B.Ctr(c.k, xs), F);
              }
            }
            return hyp();
          }
          case "Eql": return B.term_compare("EQ", mb, F.a, F.b, d) ? B.Ann(B.Rfl(), F) : null;
          default: return hyp();
        }
      }

      // Double the depth after a cut round. Keep an incomplete round's first
      // model only if fuel expires before a complete round replaces it.
      let m: HTerm | null = null;
      for (; cut && left > 0; depth *= 2) {
        cut = false;
        const v = model_at(T, 0, [], []);
        m = cut ? m ?? v : v;
      }
      if (m !== null) return m;
    }
    return null;
  }

  // Use one fitting instance for all models of k's constants.
  function inst_pick(k: Name) {
    const fits = (T: HTerm, [x, ...xs]: HTerm[]): boolean => {
      if (x === undefined) {
        return true;
      }
      const F = B.term_wnf(mb, T);
      return F.$ === "All" && fit(k, x, F.A) !== null && fits(F.B(x), xs);
    };
    return [...inst.values()].find(([t, xs]) => t === k && fits(book.tlds[k].T, xs))?.[1] ?? null;
  }

  // Check in the model book under definition name k.
  function fit(k: Name, x: HTerm, T: HTerm) {
    try {
      return B.term_higher(B.term_check(mb, { t: B.Ref(k), n: 0, def: k, qs: [] }, x, B.Lone(), T, B.ctx_nil(), 0).tm);
    } catch (err) {
      if ((err as B.Err).$ !== "Err") {
        throw err;
      }
      return null;
    }
  }

  function fresh(n: string) {
    let k = n;
    for (let i = 1; taken.has(k); i++) {
      k = n + "_" + String(i);
    }
    put(taken, k);
    return k;
  }

  function convoy_bind(s: Scope, cv: number[], t: HTerm | ((s: Scope) => O), fs: Chain[]): O {
    if (cv.length === 0) {
      return typeof t === "function" ? t(s) : tree(s, t, fs);
    }
    const l = s.D;
    const q = s.kq[cv[0]] ?? 1;
    const f = convoy_bind(scope_move(scope_bind(s, undefined, q), cv[0], l), cv.slice(1), t, fs);
    return lams([[q, l]], f);
  }

  // Tree

  // fs lists split Σ chains, innermost last. Split each field before its
  // lambda/match, then split the final unit.
  function tree(s: Scope, t: HTerm, fs: Chain[]): O {
    const top = fs[fs.length - 1];
    if (top !== undefined && top.n === 0) {
      return unit(convoy_bind(s, top.cv, t, fs.slice(0, -1)));
    }
    const [x, T] = open(t);
    const all = all_of(T);
    const v = top === undefined ? s.cols[0] ?? null : null;
    if (v !== null && x.$ === "Lam") {
      const s2 = scope_alias({ ...s, cols: s.cols.slice(1) }, Efq(), all?.A ?? null, v);
      return tree(s2, x.f(B.Var(x.k, s.d)), fs);
    }
    if (v !== null && (x.$ === "Mat" || x.$ === "Efq")) {
      const [arm, vs] = pick(x, v);
      return tree({ ...s, cols: [...vs, ...s.cols.slice(1)] }, arm, fs);
    }
    if (x.$ !== "Lam" && x.$ !== "Mat" && x.$ !== "Efq") {
      if (all !== null) {
        return tree(s, eta(t, all), fs);
      }
      return top === undefined ? term(s, t, true) : oos("a match arm with no known type");
    }
    const fs2 = top === undefined ? fs : [...fs.slice(0, -1), { ...top, n: top.n - 1 }];
    const wrap = (o: O) => top === undefined ? o : Prj(o);
    const s1 = top === undefined ? { ...s, cols: s.cols.slice(1) } : s;
    if (x.$ === "Lam") {
      const l = s.D;
      let q = all === null ? 1 : quant(all.q);
      let s2 = scope_bind(s1, all?.A ?? null, q);
      if (q > 0 && all !== null && no_ctr(all.A)) {
        s2 = { ...s2, empty: [...s2.empty, App(1, Prj(Efq()), Var(l))] };
      }
      const y: HTerm = B.Var(x.k, s.d);
      const f = tree(s2, typed(x.f(y), all?.B(y)), fs2);
      if (q === 1 && uses(f, l) > 1) {
        // ~ arguments are checked dead. Reconstruct reused linear variables;
        // match-refined Data can copy.
        const A = all === null ? null : wnf(all.A);
        if (all !== null && kind(s, all.A) === 1 && A!.$ !== "Eql" && kind(s, A!) !== 2) {
          return tree(s, rebuild(s, x, all) ?? oos("a λ that uses a variable twice, of a type whose fields are not Data (a ~ argument)"), fs);
        }
        q = 2;
      }
      return wrap(Lam(q, l, f));
    }
    const q = all === null ? 1 : quant(all.q);
    const mat = (cv: number[], dry: boolean) => {
      if (cv.length === 0) {
        return wrap(Prj(swi({ ...s1, dry }, x, T, fs2, [])));
      }
      const l = s.D;
      const sw = swi(scope_bind({ ...s1, dry, again: true }, undefined, q), x, T, fs2, cv);
      const app = cv.reduce<O>((f, y) => App(s.kq[y] ?? 1, f, Var(y)), App(q, Prj(sw), Var(l)));
      return wrap(Lam(q, l, app));
    };
    // When rebuilding a match, count nested matches dry: each post-convoy
    // variable once, dead if used only dead. Each match is built at most
    // twice.
    const o = mat([], s.dry || s.again);
    const us = o_uses(o);
    const use = (l: number) => us.get(l) ?? 0;
    const data = (l: number) => s.c.some((b) => b.o.$ === "Var" && b.o.l === l && b.T !== null && kind(s, b.T) === 2);
    const cv0 = s.kq.flatMap((k, l) => k === 1 && l < s.D && use(l) > 1 && !data(l) ? [l] : []);
    // a default's tag and fields go together: the fields' type names the tag
    const cv1 = [...new Set(cv0.flatMap((l) => s.tags.includes(l) ? [l, l + 1] : s.tags.includes(l - 1) ? [l - 1, l] : [l]))];
    // Convoy dependent variables too, at their own quantities, so their types
    // do not retain outside levels. A default binder contributes two levels.
    const cv = s.c.reduce<number[]>((vs, b, i) => {
      if (vs.length === 0 || b.T === null) {
        return vs;
      }
      const ls = [...o_uses(b.o).keys()].filter((l) => !vs.includes(l));
      const js = ls.some((l) => us.has(l)) ? s.c.flatMap((x, j) => j < i && [...o_uses(x.o).keys()].some((l) => vs.includes(l)) ? [j] : []) : [];
      return js.length > 0 && mentions(B.term_lower(b.T, s.d), (k) => js.includes(k)) ? [...vs, ...ls] : vs;
    }, cv1).sort((a, b) => a - b);
    if (s.dry) {
      return [...Array(s.D).keys()].filter((l) => us.has(l)).reduce<O>((f, l) => App(use(l) > 0 ? 1 : 0, f, Var(l)), Efq());
    }
    if (cv.length === 0 && !s.again) {
      return o;
    }
    return mat(cv, false);
  }

  function swi(s: Scope, t: HTerm, T: HTerm | null, fs: Chain[], cv: number[]): O {
    const [x, U] = open(t);
    const all = all_of(U ?? T);
    switch (x.$) {
      case "Efq": {
        if (all === null || no_ctr(all.A) || s.empty.length === 0) {
          return Efq();
        }
        // tags remain, but a live tag in scope is of the empty enum
        const q = quant(all.q);
        const f = convoy_bind(scope_bind(scope_bind(s)), cv, (s2) => s2.empty[0], fs);
        return Lam(q, s.D, Lam(q, s.D + 1, f));
      }
      case "Mat": {
        const ctr = book.ctrs[x.k];
        if (ctr === undefined) {
          oos("an unknown constructor " + B.name_key(x.k));
        }
        const [hT, mT] = goals(all, ctr);
        const h = tree(s, typed(x.h, hT), [...fs, { n: ctr.n, cv }]);
        const dead = mT?.$ === "All" && no_ctr(mT.A) && x.m.$ !== "Mat";
        const m = swi(s, dead ? B.Efq() : x.m, mT, fs, cv);
        return { $: "Mat", k: name_tt(x.k), h, m };
      }
      default: {
        if (all === null) {
          return oos("a default arm with no known type");
        }
        const q = quant(all.q);
        const [lt, la] = [s.D, s.D + 1];
        const s2 = scope_bind(scope_bind(s, undefined, q), undefined, q);
        const f = x.$ === "Lam" ? x : open(eta(t, all))[0] as Source<"Lam">;
        const pair = Tup(1, Var(lt), Var(la));
        const s2e = no_ctr(all.A) ? { ...s2, empty: [...s2.empty,
          App(1, Efq(), Var(lt))] } : s2;
        const s3 = { ...s2e, tags: [...s2e.tags, lt] };
        const y: HTerm = B.Var(f.k, s.d);
        const body = convoy_bind(scope_alias(s3, pair, all.A), cv, typed(f.f(y), all.B(y)), fs);
        const r2 = uses(body, lt) > 1 || uses(body, la) > 1 ? 2 : q;
        return Lam(r2, lt, Lam(r2, la, body));
      }
    }
  }

  // For bare match type all, return c's field goals and codomain at
  // c{fields}; remaining arms exclude c from the datatype.
  function goals(all: Source<"All"> | null, c: B.Ctr): [HTerm | null, HTerm | null] {
    const A = all === null ? null : wnf(all.A);
    if (all === null || A?.$ !== "ADT") {
      return [null, null];
    }
    const go = (U: HTerm, xs: HTerm[]): HTerm => {
      const F = all_of(U);
      return F === null ? all.B(B.Ctr(c.k, xs)) : B.All(F.q, F.k, F.i, F.A, (x) => go(F.B(x), [...xs, x]));
    };
    return [go(fill(c.T, A.x), []), { ...all, A: { ...A, r: [...A.r, c.k] } }];
  }

  function pick(t: HTerm, v: HTerm): [HTerm, HTerm[]] {
    const w = wnf(v);
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

  // Reconstruct λy => f(y) as λ{c: λfs => f(c{fs}); ..} only when all
  // constructor fields are Data.
  function rebuild(s: Scope, x: Source<"Lam">, all: Source<"All">): HTerm | null {
    const F = wnf(all.A);
    if (F.$ !== "ADT") {
      return null;
    }
    const data = (U: HTerm): boolean => {
      const G = all_of(U);
      return G === null || (kind(s, wnf(G.A)) === 2 && data(G.B(B.Var(G.k, -1))));
    };
    const arm = (k: Name, U: HTerm, xs: HTerm[]): HTerm => {
      const G = all_of(U);
      return G === null ? x.f(B.Ann(B.Ctr(k, xs), F)) : B.Lam(G.k, 0, (v) => arm(k, G.B(v), [...xs, v]));
    };
    const cs = (book.tlds[F.k] as ADT).c.filter((c) => !F.r.includes(c.k)).map((c): [Name, HTerm] => [c.k, fill(c.T, F.x)]);
    return cs.every(([, U]) => data(U)) ? B.Ann(cs.reduceRight<HTerm>((m, [k, U]) => B.Mat(k, arm(k, U, []), m), B.Efq()), all) : null;
  }

  function all_of(T: HTerm | null) {
    return T === null ? null : B.tele_open(book, T);
  }

  function no_ctr(T: HTerm) {
    const x = wnf(T);
    const tld = x.$ === "ADT" ? book.tlds[x.k] : undefined;
    return x.$ === "ADT" && tld?.$ === "ADT" && tld.c.every((c) => x.r.includes(c.k));
  }

  // Use a datatype's declared kind or a type-valued def's result kind, never
  // its body. Closed quantities give Data (2) or Type (1); otherwise let the
  // kernel decide.
  function kind(s: Scope, T: HTerm): Q | null {
    const [x] = open(T);
    const [h, xs] = x.$ === "ADT" ? [x, x.x] : unapply(x);
    const [f] = open(h);
    const tld = f.$ === "ADT" || f.$ === "Ref" ? book.tlds[f.k] : undefined;
    if (tld === undefined) {
      return null;
    }
    try {
      const K = wnf(fill(tld.T, xs));
      const g = K.$ === "Typ" ? closed_val(s, K.g) : null;
      return g === null ? null : g.$ === "Qua" && g.q.$ === "Many" ? 2 : 1;
    } catch {
      return null;
    }
  }

  // Term

  function term(s0: Scope, t: HTerm, live: boolean): O {
    const [x, T] = open(t);
    const s = s0.sub && x.$ !== "Ctr" && x.$ !== "Lit" ? { ...s0, sub: false } : s0;
    switch (x.$) {
      case "Var": {
        const b = s.c[x.i];
        if (b === undefined) {
          oos("a free variable " + x.k);
        }
        return b.v !== undefined ? term(s0, typed(b.v, b.T), live) : b.o;
      }
      case "Ref":
      case "App": return spine(s, t, live);
      case "ADT": {
        const tld = book.tlds[x.k];
        if (tld?.$ !== "ADT") {
          oos("an unknown datatype " + B.name_key(x.k));
        }
        return args(s, x.k, tld.T, x.x, live);
      }
      case "Typ": return { $: "Typ", q: term(s, x.g, false) };
      case "All": {
        const l = s.D;
        const A = term(s, x.A, false);
        const Bo = term(scope_bind(s, x.A), x.B(B.Var(x.k, s.d)), false);
        return All(quant(x.q), l, A, Bo);
      }
      case "Lam":
      case "Mat":
      case "Efq": return tree({ ...s, cols: [] }, t, []);
      case "Let": return let_term(s, x, live);
      case "Ctr": return ctr_term(s, x, T, live);
      case "Lit": {
        if (x.k === "Nat" && x.v > NAT_MAX && book.tlds.Nat?.b === true) {
          const [q, r] = [Math.floor(x.v / NAT_MAX), x.v % NAT_MAX];
          const mul = B.App(B.App(B.Ref("Nat.mul"), B.Lit("Nat", q)), B.Lit("Nat", NAT_MAX));
          return term(s, B.App(B.App(B.Ref("Nat.add"), mul), B.Lit("Nat", r)), live);
        }
        return term(s, B.term_higher(B.lit_step(x)), live);
      }
      case "Eql": return { $: "Eql", a: arg_term(s, x.a, x.T, false), b: arg_term(s, x.b, x.T, false), T: term(s, x.T, false) };
      case "Rfl": return { $: "Rfl" };
      case "Rwt": return rwt_term(s, x, live);
      case "Qnt": return { $: "Enu", ks: QS };
      case "Qua": return Lab(QS[quant(x.q)]);
      case "Min": return { $: "Min", a: term(s, x.a, live), b: term(s, x.b, live) };
      case "Hol": return oos("a hole");
      default: return oos("a " + x.$ + " term");
    }
  }

  function spine(s: Scope, t: HTerm, live: boolean): O {
    const [h, xs] = unapply(t);
    const [f, T] = open(h);
    if (f.$ !== "Ref") {
      const U = f.$ === "Var" ? s.c[f.i]?.T ?? null : T;
      const o = term(s, h, live);
      return args(s, inferable(o) ? o : Ann(o, term(s, U ?? oos("an application with no known head type"), false)), U, xs, live);
    }
    // bend2's instance of a template is the template at its ~ arguments
    const g = inst.get(f.k);
    const [k, ys] = g === undefined ? [f.k, xs] : [g[0], [...g[1], ...xs]];
    const tld = book.tlds[k] ?? oos("an unknown name " + B.name_key(k));
    if (tld.$ === "Def" && ys.length < tld.x) {
      oos("a template " + B.name_key(k) + " short of its ~ arguments");
    }
    return args(s, k, tld.T, ys, live);
  }

  function args(s: Scope, k: Name | O, T: HTerm | null, xs: HTerm[], live: boolean): O {
    const tld = typeof k === "string" ? book.tlds[k] : null;
    // a def's first x parameters, its ~ ones, are specialized
    const nx = tld?.$ === "Def" ? tld.x : 0;
    const ps: Arg[] = [];
    let U = T;
    xs.forEach((x, j) => {
      const F = all_of(U) ?? oos("an application past its head's known type");
      const v = j < nx ? spec_val(s, x) : null;
      ps.push([quant(F.q), x, F.A, v]);
      U = F.B(v ?? x);
    });
    let n: string | null;
    try {
      const g = typeof k === "string" ? group_of(k) : null;
      if (g !== null) {
        return group_call(s, g, k as Name, ps, U as HTerm, live);
      }
      n = typeof k === "string" ? item_ref(k, ps.map((p) => p[3]), live) : null;
    } catch (x) {
      if (x instanceof Reroute) {
        return args(s, k, T, xs, live);
      }
      throw x;
    }
    const a = { ...s, sub: live && n === s.self };
    return ps.filter((p) => p[3] === null).reduce<O>((f, [q, x, A]) => App(q, f, arg_term(a, x, A, live && q > 0)),
      n === null ? k as O : Ref(n));
  }

  // Group
  //
  // The kernel has no mutual recursion. Combine a lead k and its helpers in
  // one definition. Select k with (.k,()) and helper h with
  // (.h,((.k,()),())). Helper callbacks pass selector fields; calls from k
  // to helpers pass pieces of the lead so kernel descent can check them.

  function group_of(k: Name) {
    return groups.get(k) ?? null;
  }

  // The last-filled member leads because helpers call back to that law. A
  // cycle with no new member changes only specialization and cannot form
  // one definition. Group growth invalidates earlier emissions.
  function regroup(key: string): never {
    const ns = stack.slice(stack.indexOf(key)).map((x) => x.split("\n")[0]);
    const gs = [...new Set(ns.filter((x) => x[0] === "\t").map((x) => group_of(x.slice(1)) as Group))];
    const ms = [...new Set(ns.flatMap((x) => x[0] === "\t" ? (group_of(x.slice(1)) as Group).ms : [x]))];
    if (ms.length < 2 || gs.length === 1 && ms.length === gs[0].ms.length) {
      oos("a recursion through " + B.name_key(ms[0]) + " at other specialized arguments");
    }
    const at = (x: Name) => book.order.lastIndexOf(x);
    const k = ms.reduce((a, b) => at(b) > at(a) ? b : a);
    const g = group_new(k, ms.filter((x) => x !== k).sort((a, b) => at(a) - at(b)))
      ?? oos("a mutual recursion through " + ms.map(B.name_key).join(", ") + " that no live parameter of " + B.name_key(k) + " leads");
    for (const x of g.ms) {
      groups.set(x, g);
    }
    grew ||= gs.length > 0;
    throw new Regroup(key);
  }

  // Each helper must pass its shared leading parameters unchanged to k.
  // A group needs a live lead parameter for kernel descent.
  function group_new(k: Name, hs: Name[]): Group | null {
    const lead = Math.min(...hs.map((h) => {
      const t = (book.tlds[h] as Def).e as B.LTerm;
      let n = 0;
      for (let x = t; x.$ === "Ann" || x.$ === "Lam"; x = x.$ === "Ann" ? x.x : x.f) {
        n += x.$ === "Lam" ? 1 : 0;
      }
      const cs = back(t, k);
      const at = (x: B.LTerm | undefined, j: number): boolean => x?.$ === "Ann" ? at(x.x, j) : x?.$ === "Var" && x.i === j;
      let j = 0;
      while (j < n && cs.length > 0 && cs.every((xs) => at(xs[j], j))) {
        j++;
      }
      return j;
    }));
    const tld = book.tlds[k];
    const doms = B.tele_unbind(book, tld.T).doms.slice(0, lead);
    if (hs.length === 0 || !doms.some(([q], j) => q.$ !== "None" && (tld.$ !== "Def" || j >= tld.x))) {
      return null;
    }
    return { k, ms: [k, ...hs], qs: doms.map(([q]) => quant(q)) };
  }

  // Rollback truncates out/todo and removes new names/seen/taken/fail keys
  // recorded by put. Other mutations survive.
  type Trail = [number, number, number];

  function trail(): Trail {
    return [out.length, todo.length, log.length];
  }

  function undo(m: Trail) {
    out.length = m[0];
    todo.length = m[1];
    for (const [x, k] of log.splice(m[2])) {
      x.delete(k);
    }
  }

  function put(x: Map<string, string> | Set<string>, k: string, v = "") {
    if (!x.has(k)) {
      log.push([x, k]);
    }
    if (x instanceof Map) {
      x.set(k, v);
    } else {
      x.add(k);
    }
  }

  // Annotate group calls because the kernel infers an unreduced mode.
  function group_call(s: Scope, g: Group, m: Name, ps: Arg[], R: HTerm, live: boolean): O {
    const n = g.qs.length;
    if (ps.length < (book.tlds[m] as Def).n || ps.slice(n).some((p) => p[3] !== null)) {
      oos("a partial or specialized call to " + m + ", mutually recursive");
    }
    const k = item_ref("\t" + g.k, ps.slice(0, n).map((p) => p[3]), live);
    const a = { ...s, sub: live && k === s.self };
    const args = (xs: Arg[], qs: Q[]) => xs.flatMap(([q, x, A, v], j): Array<[Q, O]> =>
      v !== null ? [] : [[qs[j] ?? q, arg_term(a, x, A, live && (qs[j] ?? q) > 0)]]);
    const sel = Tup(1, Lab(name_tt(g.k)), UV);
    const x = [...args(ps.slice(0, n), g.qs), [1, m === g.k ? sel : Tup(1, Lab(name_tt(m)), Tup(1, sel, UV))] as [Q, O],
      ...args(ps.slice(n), [])].reduce<O>((f, [q, x]) => App(q, f, x), Ref(k));
    return Ann(x, term(s, R, false));
  }

  // At specialized lead cols, emit ∀lead -> ∀s:Sel -> MODE(s), where MODE
  // gives the selected member's remaining parameters/result. Convoy q=1 lead
  // variables through the selector match.
  function group_emit(g: Group, cols: Cols, n: string): void {
    const { s, ps: ls, xs: lead } = tele_open({ ...scope_nil(), self: n }, book.tlds[g.k].T, cols, g.qs.length);
    const rest = (s1: Scope, m: Name) => {
      const tld = book.tlds[m];
      const nx = tld.$ === "Def" ? tld.x : 0;
      const r = tele_open(s1, tld.T, lead, tld.n);
      const qs = B.tele_unbind(book, tld.T).doms.map(([q]) => quant(q));
      return { ...r, vs: r.xs.flatMap((x, j): Array<[Q, O]> => j < nx ? [] : [[qs[j], term(r.s, x, false)]]), cs: r.xs.map((x, j) => j < nx ? x : null) };
    };
    const tags = g.ms.map(name_tt);
    const sel = (f: (m: Name) => O): O => Prj(mats(tags, (i) => i === 0 ? unit(f(g.ms[i]))
      : Prj(Prj(mats([name_tt(g.k)], () => unit(unit(f(g.ms[i]))))))));
    const l0 = s.D;
    const s0 = scope_bind(s, undefined, 1);
    const mode = sel((m) => {
      const r = rest(s0, m);
      return alls(r.ps, term(r.s, r.T, false));
    });
    const Sel = Sig(1, l0, { $: "Enu", ks: tags }, { $: "App", q: 1, x: Var(l0),
      f: mats(tags, (i) => i === 0 ? UT
        : Sig(1, l0 + 1, Sig(1, l0 + 1, { $: "Enu", ks: [name_tt(g.k)] }, UT), UT)) });
    const To = alls(ls, All(1, l0, Sel, App(1, mode, Var(l0))));
    const ks = ls.filter(([q]) => q === 1).map(([, l]) => l);
    const body = sel((m) => convoy_bind(s0, ks, (s2) => {
      const r = rest(s2, m);
      return lams(r.ps, arm(r.s, m, r.cs, r.vs));
    }, []));
    const v = ks.reduce<O>((f, l) => App(1, f, Var(l)), App(1, body, Var(l0)));
    out.push([n, To, lams([...ls, [1, l0]], v), false]);
  }

  // Keep match nodes when applying leftover arguments. Templates were
  // checked with opaque k~p constants; substitute ~ arguments and remove
  // their columns.
  function arm(s: Scope, k: Name, cols: Cols, vs: Array<[Q, O]>): O {
    const tld = book.tlds[k] as Def;
    const ps = tld.x === 0 ? null : new Map(B.tele_unbind(book, tld.T).doms.slice(0, tld.x).map(([, p], j) => [k + "~" + p, cols[j]]));
    const t0 = B.term_higher(tld.e as B.LTerm);
    let t = ps === null ? t0 : subst(t0, 0, (o) => o.$ === "Ref" ? ps.get(o.k as Name) ?? undefined : undefined);
    let si: Scope = { ...s, c: [], d: 0, cols: cols.slice(tld.x), sub: false };
    let j = 0;
    for (let [x, T] = open(t); x.$ === "Lam" && T !== null; [x, T] = open(t)) {
      const F = all_of(T);
      const v = si.cols[0] ?? null;
      if (F === null || (v === null && j === vs.length)) {
        break;
      }
      si = { ...si, cols: si.cols.slice(1) };
      si = v !== null ? scope_alias(si, Efq(), F.A, v) : scope_alias(si, vs[j++][1], F.A);
      t = x.f(B.Var(x.k, si.d - 1));
    }
    return vs.slice(j).reduce<O>((f, [q, x]) => App(q, f, x), tree(si, t, []));
  }

  // Use expected A for bare lambdas and matches. Expand other function
  // arguments η-long: only the call may know an equation's carrier. Keep
  // live self-call arguments for descent unless A's quantities differ.
  function arg_term(s: Scope, x: HTerm, A: HTerm, live: boolean): O {
    const [y, T] = open(x);
    const tree = y.$ === "Lam" || y.$ === "Mat" || y.$ === "Efq";
    const all = tree || y.$ === "Ctr" ? null : all_of(A);
    if (all !== null && (!(live && s.sub) || T !== null && !qsig_eq(T, A, s.d))) {
      return term(s, eta(x, all), live);
    }
    return term(s, T === null && (tree || y.$ === "Ctr") ? B.Ann(x, A) : x, live);
  }

  // Kernel function quantities must match exactly; source fit permits
  // different quantities.
  function qsig_eq(T: HTerm, A: HTerm, d: number): boolean {
    const F = all_of(T);
    const G = all_of(A);
    if (F === null || G === null) {
      return F === G;
    }
    const x = B.Var(G.k, d);
    return F.q.$ === G.q.$ && qsig_eq(F.B(x), G.B(x), d + 1);
  }

  function ctr_term(s: Scope, x: Source<"Ctr">, T: HTerm | null, live: boolean): O {
    const ctr = book.ctrs[x.k];
    if (ctr === undefined) {
      oos("an unknown constructor " + B.name_key(x.k));
    }
    const f = B.book_fam(book, x.k);
    const fam = book.tlds[f] as ADT;
    const w = B.u32_from_term(x) ?? B.u32_from_term(x, "F32");
    if (!s.sub && w !== null) {
      if (fam.n !== 0) {
        oos("a word literal of a parameterized datatype");
      }
      return word_ref(x.k, f, w);
    }
    const G = T === null || fam.n === 0 ? null : wnf(T);
    const ps = G?.$ === "ADT" ? G.x : Array.from({ length: fam.n }, () => B.Var("_", -1));
    let F = fill(ctr.T, ps);
    const fs: Array<[Q, O]> = [];
    for (const a of x.x) {
      const A = all_of(F) ?? oos("a constructor past its fields");
      const q = quant(A.q);
      fs.push([q, arg_term(s, a, A.A, live && q > 0)]);
      F = A.B(a);
    }
    const k = name_tt(x.k);
    const tail = fs.reduceRight<O>((b, [q, a]) => Tup(q, a, b), UV);
    return Tup(1, Lab(k), tail);
  }

  function word_ref(T: Name, fam: Name, n: number): O {
    const key = "\tword " + T + " " + String(n);
    let k = names.get(key);
    if (k === undefined) {
      k = fresh(T + ".lit" + String(n));
      put(names, key, k);
      const v = term({ ...scope_nil(), sub: true }, B.Ctr(T, [B.word_to_term(n)]), true);
      out.push([k, Ref(item_ref(fam, [], false)), v, false]);
    }
    return Ref(k);
  }

  // Rebuild inlined self-call constructor arguments at each use's depth and
  // mode so the kernel can recognize columns.
  function let_term(s: Scope, x: Source<"Let">, live: boolean, put: Set<number> = new Set()): O {
    let s2 = s;
    const ls: Array<[Q, number, O, number]> = [];
    for (let j = 0; j < x.k.length; j++) {
      const q = quant(x.q[j]);
      const V = open(x.v[j])[1];
      if (put.has(j)) {
        s2 = scope_alias(s2, Efq(), V, x.v[j]);
        continue;
      }
      // Parallel values use the outer source scope but the deeper kernel scope.
      const at = { ...s, D: s2.D, kq: s2.kq };
      const v = term(at, x.v[j], live && q > 0);
      if (v.$ === "Var") {
        s2 = scope_alias(s2, v, V);
      } else {
        const l = s2.D;
        const w: O = inferable(v) ? v : V === null ? oos("a let with no known type")
          : Ann(v, term(at, V, false));
        ls.push([q, l, w, j]);
        s2 = scope_bind(s2, V, q);
      }
    }
    const xs = x.k.map((k, j) => B.Var(k, s.d + j));
    const f = term(s2, (x.f as (xs: HTerm[]) => HTerm)(xs), live);
    const more = ls.filter(([, l, v]) => live && v.$ !== "App" && v.$ !== "Ref" && self_arg(f, s.self, l)).map(([, , , j]) => j);
    if (more.length > 0) {
      return let_term(s, x, live, new Set([...put, ...more]));
    }
    return ls.reduceRight<O>((b, [q, l, v]) => Let(q === 1 && uses(b, l) > 1 ? 2 : q, l, v, b), f);
  }

  // The kernel's J motive binds its endpoint at l and evidence at l + 1.
  function rwt_term(s: Scope, x: Source<"Rwt">, live: boolean): O {
    const E0 = open(x.e)[1];
    const e0 = term(s, x.e, live);
    const ev = inferable(e0) || E0 === null ? e0 : Ann(e0, term(s, E0, false));
    const [p] = open(x.p);
    if (p.$ !== "Lam") {
      oos("a rewrite motive that is not a λ");
    }
    const l = s.D;
    const Q = E0 === null ? null : wnf(E0);
    const q = Q !== null && Q.$ === "Eql" ? Q : null;
    const s2 = scope_bind(s, q && q.T);
    const [p2] = open(p.f(B.Var(p.k, s.d)));
    if (p2.$ !== "Lam") {
      oos("a rewrite motive that is not a λ over its evidence");
    }
    const s3 = scope_bind(s2, q && B.Eql(q.a, B.Var(p.k, s.d), q.T));
    const P = term(s3, p2.f(B.Var(p2.k, s2.d)), false);
    return { $: "Rwt", e: ev, l, P, f: term(s, x.f, live) };
  }

  function lams(ps: Array<[Q, number, ...unknown[]]>, b: O) {
    return ps.reduceRight<O>((f, [q, l]) => Lam(q === 1 && uses(f, l) > 1 ? 2 : q, l, f), b);
  }

  function uses(o: O, l: number) {
    return o_uses(o).get(l) ?? 0;
  }

  // Count live uses by level in one walk, recording 0 for type-only or q=0
  // uses. Cache each term's counts so surrounding walks reuse them.
  function o_uses(o: O, live: boolean = true, out?: Map<number, number>): Map<number, number> {
    const kept = use_counts.get(o);
    if (out === undefined && kept === undefined) {
      use_counts.set(o, o_uses(o, true, new Map()));
    }
    if (out === undefined) {
      return use_counts.get(o) as Map<number, number>;
    }
    if (kept !== undefined) {
      kept.forEach((n, l) => out.set(l, (out.get(l) ?? 0) + (live ? n : 0)));
      return out;
    }
    switch (o.$) {
      case "Var": return out.set(o.l, (out.get(o.l) ?? 0) + (live ? 1 : 0));
      case "Ann": return o_uses(o.T, false, o_uses(o.x, live, out));
      case "Let": return o_uses(o.f, live, o_uses(o.v, live && o.q > 0, out));
      case "All": case "Sig": return o_uses(o.B, false, o_uses(o.A, false, out));
      case "Lam": return o_uses(o.f, live, out);
      case "App": return o_uses(o.x, live && o.q > 0, o_uses(o.f, live, out));
      case "Tup": return o_uses(o.b, live, o_uses(o.a, live && o.q > 0, out));
      case "Prj": return o_uses(o.h, live, out);
      case "Mat": return o_uses(o.m, live, o_uses(o.h, live, out));
      case "Min": return o_uses(o.b, live, o_uses(o.a, live, out));
      case "Eql": return o_uses(o.T, false, o_uses(o.b, false, o_uses(o.a, false, out)));
      case "Rwt": return o_uses(o.f, live, o_uses(o.P, false, o_uses(o.e, live, out)));
      default: return out;
    }
  }

  // Show

  function book_show(): string {
    let p = "x";
    while ([...taken].some((k) => new RegExp("^" + p + "[0-9]+$").test(k))) {
      p += "x";
    }
    const nm = (l: number) => p + String(l);
    function show(o: O): string {
      switch (o.$) {
        case "Var": return nm(o.l);
        case "Ref": return o.k;
        case "Ann": return `{${show(o.x)} : ${show(o.T)}}`;
        case "Let": return `!${mark(o.q)}${nm(o.l)} = ${show(o.v)}; ${show(o.f)}`;
        case "Typ": return `*(${show(o.q)})`;
        case "Min": return `(${show(o.a)} <&> ${show(o.b)})`;
        case "All": case "Sig":
          return `${o.$ === "All" ? "∀" : "Σ"}${mark(o.q)}${nm(o.l)} : ${show(o.A)} -> ${show(o.B)}`;
        case "Lam": return `λ${mark(o.q)}${nm(o.l)} => ${show(o.f)}`;
        case "App": {
          const xs: string[] = [];
          let f: O = o;
          while (f.$ === "App") {
            xs.push(mark(f.q) + show(f.x));
            f = f.f;
          }
          return `(${show(f)} ${xs.reverse().join(" ")})`;
        }
        case "Tup": return `(${mark(o.q)}${show(o.a)}, ${show(o.b)})`;
        case "Prj": return `λ{(,): ${show(o.h)}}`;
        case "Enu": return `<${o.ks.join(", ")}>`;
        case "Lab": return o.k === "()" ? "()" : "." + o.k;
        case "Mat": return `λ{${show(Lab(o.k))}: ${show(o.h)}; ${show(o.m)}}`;
        case "Efq": return "λ{}";
        case "Eql": return `{${show(o.a)} == ${show(o.b)} : ${show(o.T)}}`;
        case "Rfl": return "{==}";
        case "Rwt": return `%${show(o.e)} : ${nm(o.l)}, ${nm(o.l + 1)} => ${show(o.P)}; ${show(o.f)}`;
      }
    }
    return out.map(([k, T, v, m]) => `${m ? "opaque " : ""}${k} : ${show(T)} =\n  ${show(v)}`).join("\n\n") + "\n";
  }

}

// Item

// A tab-prefixed name identifies a group.
function item_key(k: Name, cols: Cols) {
  return [k, ...cols.flatMap((v) => v === null ? [] : [B.term_key(B.term_lower(v))])].join("\n");
}

// Lower before substitution so closures cannot hide nodes.
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

// Count binders from d, so a field under ∀ repeats the same datatype key.
function model_key(F: HTerm, d: number) {
  const at = (i: unknown) => typeof i === "number" && i >= d ? "^" + (i - d) : i;
  return JSON.stringify(B.term_lower(F, d), (k, v) => k === "s" ? undefined : k !== "i" ? v : Array.isArray(v) ? v.map(at) : at(v));
}

function name_tt(k: Name) {
  return B.name_key(k).replace(/[^A-Za-z0-9_.]|^[.0-9]/g, (c) => "_" + (c.codePointAt(0) ?? 0).toString(16) + "_");
}

function quant(q: Quant): Q {
  return q.$ === "None" ? 0 : q.$ === "Lone" ? 1 : 2;
}

// Scope

function scope_nil(): Scope {
  return { c: [], d: 0, D: 0, cols: [], self: "", empty: [], sub: false, kq: [], tags: [], dry: false, again: false };
}

// Source bindings fill c in level order.
function scope_alias(s: Scope, o: O, T: HTerm | null, v?: HTerm): Scope {
  return { ...s, c: [...s.c, { o, T, v }], d: s.d + 1 };
}

// An undefined T hides the binder; null binds an unknown source type.
function scope_bind(s: Scope, T?: HTerm | null, q?: Q): Scope {
  const s2 = { ...s, D: s.D + 1 };
  if (q !== undefined) {
    s2.kq = s.kq.slice();
    s2.kq[s.D] = q;
  }
  if (T !== undefined) {
    const o = Var(s.D);
    s2.c = [...s.c, { o, T, v: undefined }];
    s2.d += 1;
  }
  return s2;
}

function scope_move(s: Scope, a: number, b: number): Scope {
  const mv = (o: O): O => o.$ === "Var" ? (o.l === a ? Var(b) : o)
    : Object.fromEntries(Object.entries(o).map(([k, v]) => [k, is_o(v) ? mv(v) : v])) as O;
  return { ...s, c: s.c.map((x) => ({ ...x, o: mv(x.o) })), empty: s.empty.map(mv),
    tags: s.tags.flatMap((t) => t === a ? [t, b] : [t]) };
}

// Open

// Keep the outermost annotation's type.
function open(t: HTerm): [HTerm, HTerm | null] {
  let T: HTerm | null = null;
  let x = B.term_force(t);
  while (x.$ === "Ann") {
    T ??= B.term_force(x.T);
    x = B.term_force(x.x);
  }
  return [x, T];
}

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

function typed(t: HTerm, T?: HTerm | null): HTerm {
  return T == null || open(t)[1] !== null ? t : B.Ann(t, T);
}

function eta(t: HTerm, all: Source<"All">): HTerm {
  return B.Ann(B.Lam(all.k, 0, (y: HTerm) => B.Ann(B.App(t, y), all.B(y))), all);
}

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

// Kernel terms

function self_arg(o: O, k: string, l: number): boolean {
  let h = o;
  let found = false;
  while (h.$ === "App") {
    found ||= h.x.$ === "Var" && h.x.l === l && h.q > 0;
    h = h.f;
  }
  return (found && h.$ === "Ref" && h.k === k)
    || Object.values(o).some((v) => is_o(v) && self_arg(v, k, l));
}

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

function alls(ps: Binder[], b: O, $: "All" | "Sig" = "All") {
  return ps.reduceRight<O>((B, [q, l, A]) => ({ $, q, l, A, B }), b);
}

// Build match arms from last to first; h may emit definitions.
function mats(ks: string[], h: (i: number) => O) {
  return ks.reduceRight<O>((m, k, i) => ({ $: "Mat", k, h: h(i), m }), Efq());
}

function unit(h: O) {
  return mats(["()"], () => h);
}

function inferable(o: O): boolean {
  return o.$ === "App" ? inferable(o.f) : ["Var", "Ref", "Ann", "Typ", "Min", "All", "Enu", "Eql"].includes(o.$);
}

function mark(q: Q): string {
  return q === 0 ? "-" : q === 2 ? "+" : "";
}

// Kernel

// Use $BENDTT or cache a build by source and pinned Lean version.
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
  const tool = (t: string) => fs.existsSync(path.join(home, t)) ? path.join(home, t) : t;
  fs.mkdirSync(dir, { recursive: true });
  const fail = (why: string) => {
    throw new Error("BendTT infrastructure: the kernel did not build (" + why
      + "); --verdict needs Lean v" + LEAN_VERSION + " (elan toolchain leanprover/lean4:v" + LEAN_VERSION + "), or $BENDTT set to a built kernel");
  };
  const run = (bin: string, args: string[]) => {
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

// Use a file because Bun 1.3.14 spawnSync pipes lost output under load,
// even with exit status 0.
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

// Require exit 0 and the exact success line. A private input copy prevents
// swaps and avoids the observed /dev/stdin EBADF failures.
function kernel_check(text: string): string | null {
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
    const why = out.trim().split("\n").slice(1).join("\n");
    return got.status === 0 && out.trim() === "ALL PROOFS CHECK" ? null
      : why || "kernel exited " + got.status + " after " + out.trim();
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

export function safe_emit(book: Book, out: string): string[] {
  const got = safe_book(book);
  fs.writeFileSync(out, got.text);
  return got.oos.map(([k, why]) => "- " + B.name_key(k) + ": " + why + "\n");
}

// Succeed only if every source root exports and passes the kernel.
export function safe_check(book: Book): string | null {
  const got = safe_book(book);
  return got.oos.length === 0 ? kernel_check(got.text) : "BendTT: out of scope:\n"
    + got.oos.map(([k, why]) => "- " + B.name_key(k) + ": " + why).join("\n");
}
