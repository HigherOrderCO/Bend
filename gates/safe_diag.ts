// why the kernel's live check fails on a .bendtt def: a port of
// bendtt.lean's Term.tree / Term.live (affinity, call order, descent)
// that names the failing binder or call. A diagnosis, not a verdict.
//   bun gates/safe_diag.ts <file.bendtt> [def]
import * as fs from "node:fs";

type Q = 0 | 1 | 2;
type T =
  | { $: "Var"; i: number } | { $: "Ref"; k: string } | { $: "Ann"; x: T; T: T }
  | { $: "Let"; q: Q; v: T; f: T; n: string } | { $: "Typ" } | { $: "All"; q: Q; A: T; B: T }
  | { $: "Lam"; q: Q; f: T; n: string } | { $: "App"; q: Q; f: T; x: T } | { $: "Sig"; q: Q; A: T; B: T }
  | { $: "Tup"; q: Q; a: T; b: T } | { $: "Prj"; h: T } | { $: "Enu" } | { $: "Lab"; k: string }
  | { $: "Mat"; k: string; h: T; m: T } | { $: "Efq" } | { $: "Eql"; a: T; b: T; T: T } | { $: "Rfl" }
  | { $: "Rwt"; e: T; P: T; f: T } | { $: "Min"; a: T; b: T };

let src = "";
let pos = 0;
function skip(): void {
  for (;;) {
    while (pos < src.length && /\s/.test(src[pos])) pos++;
    if (src[pos] === "#") { while (pos < src.length && src[pos] !== "\n") pos++; } else return;
  }
}
function take(w: string): boolean { skip(); if (src.startsWith(w, pos)) { pos += w.length; return true; } return false; }
function eat(w: string): void { if (!take(w)) throw new Error("expected " + w + " at " + src.slice(pos, pos + 30)); }
function name(): string { skip(); const m = /^[A-Za-z0-9_.]+/.exec(src.slice(pos)); if (!m) throw new Error("name at " + src.slice(pos, pos + 30)); pos += m[0].length; return m[0]; }
function quan(): Q { if (take("-")) return 0; if (take("+")) return 2; return 1; }
function label(): string { return take("()") ? "()" : name(); }
function term(vs: string[]): T {
  skip();
  const c = src[pos];
  if (c === "{") {
    eat("{");
    if (take("==")) { eat("}"); return { $: "Rfl" }; }
    const a = term(vs);
    if (take("==")) { const b = term(vs); eat(":"); const T = term(vs); eat("}"); return { $: "Eql", a, b, T }; }
    eat(":"); const T = term(vs); eat("}"); return { $: "Ann", x: a, T };
  }
  if (c === "*") { eat("*"); if (src[pos] === "(") term(vs); else pos++; return { $: "Typ" }; }
  if (c === "!") { eat("!"); const q = quan(); const n = name(); eat("="); const v = term(vs); eat(";"); return { $: "Let", q, v, f: term([n, ...vs]), n }; }
  if (c === "∀" || c === "Σ") { pos++; const q = quan(); const n = name(); eat(":"); const A = term(vs); eat("->"); const B = term([n, ...vs]); return c === "∀" ? { $: "All", q, A, B } : { $: "Sig", q, A, B }; }
  if (c === "λ") {
    pos++;
    if (take("{")) {
      if (take("}")) return { $: "Efq" };
      if (take("(,)")) { eat(":"); const h = term(vs); eat("}"); return { $: "Prj", h }; }
      const k = take("()") ? "()" : (eat("."), name());
      eat(":"); const h = term(vs); eat(";"); const m = term(vs); eat("}"); return { $: "Mat", k, h, m };
    }
    const q = quan(); const n = name(); eat("=>"); return { $: "Lam", q, f: term([n, ...vs]), n };
  }
  if (c === "(") {
    eat("(");
    if (take(")")) return { $: "Lab", k: "()" };
    const q = quan(); const a = term(vs);
    if (take(",")) return tup(vs, q, a);
    if (take("<&>")) { const b = term(vs); eat(")"); return { $: "Min", a, b }; }
    let f = a;
    while (!take(")")) { const p = quan(); f = { $: "App", q: p, f, x: term(vs) }; }
    return f;
  }
  if (c === "<") { eat("<"); while (!take(">")) { label(); take(","); } return { $: "Enu" }; }
  if (c === ".") { eat("."); return { $: "Lab", k: name() }; }
  if (c === "%") { eat("%"); const e = term(vs); eat(":"); const n = name(); eat("=>"); const P = term([n, ...vs]); eat(";"); return { $: "Rwt", e, P, f: term(vs) }; }
  const k = name();
  const i = vs.indexOf(k);
  return i >= 0 ? { $: "Var", i } : { $: "Ref", k };
}
function tup(vs: string[], q: Q, a: T): T {
  const p = quan(); const b = term(vs);
  if (take(",")) return { $: "Tup", q, a, b: tup(vs, p, b) };
  eat(")"); return { $: "Tup", q, a, b };
}

type Tag = [number, "eq" | "lt"] | null;
type G = { ks: string[]; i: number; cs: boolean[]; ts: Tag[]; ns: string[] };
const why: string[] = [];
const live = (q: Q): boolean => q !== 0;
const allows = (q: Q, n: number): boolean => q === 0 ? n === 0 : q === 1 ? n <= 1 : true;
function uses(t: T, i: number): number {
  switch (t.$) {
    case "Var": return t.i === i ? 1 : 0;
    case "Ann": return uses(t.x, i);
    case "Let": return (live(t.q) ? uses(t.v, i) : 0) + uses(t.f, i + 1);
    case "Lam": return uses(t.f, i + 1);
    case "App": return uses(t.f, i) + (live(t.q) ? uses(t.x, i) : 0);
    case "Tup": return (live(t.q) ? uses(t.a, i) : 0) + uses(t.b, i);
    case "Prj": return uses(t.h, i);
    case "Mat": return uses(t.h, i) + uses(t.m, i);
    case "Rwt": return uses(t.e, i) + uses(t.f, i);
    case "Min": return uses(t.a, i) + uses(t.b, i);
    default: return 0;
  }
}
function unspine(t: T): [T, Array<[Q, T]>] {
  const xs: Array<[Q, T]> = [];
  while (t.$ === "App") { xs.unshift([t.q, t.x]); t = t.f; }
  return [t, xs];
}
const takes = (t: T): boolean => ["Lam", "Prj", "Mat", "Efq"].includes(t.$);
function bind(g: G, o: Tag, n: string): G { return { ...g, ts: [o, ...g.ts], ns: [n, ...g.ns] }; }
function next(g: G, ps: Tag[], l: boolean): [Tag, G, Tag[]] {
  return ps.length > 0 ? [ps[0], g, ps.slice(1)] : [[g.cs.length, "eq"], { ...g, cs: [...g.cs, l] }, []];
}
function show(t: T, ns: string[]): string {
  switch (t.$) {
    case "Var": return ns[t.i] ?? "?" + t.i;
    case "Ref": return t.k;
    case "App": { const [h, xs] = unspine(t); return "(" + show(h, ns) + " " + xs.map(([q, x]) => (q === 0 ? "-" : q === 2 ? "+" : "") + show(x, ns)).join(" ") + ")"; }
    case "Tup": return "(" + show(t.a, ns) + ", " + show(t.b, ns) + ")";
    case "Lab": return t.k === "()" ? "()" : "." + t.k;
    default: return t.$;
  }
}
function called(g: G, t: T): boolean {
  const [h, xs] = unspine(t);
  if (h.$ !== "Ref") return true;
  const j = g.ks.indexOf(h.k);
  if (j < 0) { why.push("unknown " + h.k); return false; }
  if (j < g.i) return true;
  if (j > g.i) { why.push("a forward call " + show(t, g.ns)); return false; }
  let o = "eq";
  for (let c = 0; c < xs.length && o === "eq"; c++) {
    const [q, x] = xs[c];
    if (g.cs[c] !== live(q)) o = "gt";
    else if (!live(q)) o = "eq";
    else if (x.$ === "Var") { const tg = g.ts[x.i]; o = tg && tg[0] === c ? tg[1] : "gt"; }
    else o = "gt";
    if (o === "gt") why.push("no descent at column " + c + " in " + show(t, g.ns));
  }
  if (o === "eq") why.push("no column shrinks in " + show(t, g.ns));
  return o === "lt";
}
function lv(g: G, top: boolean, t: T): boolean {
  switch (t.$) {
    case "App": { const s = !top || called(g, t); const f = lv(g, false, t.f); const x = !live(t.q) || lv(g, true, t.x); return s && f && x; }
    case "Ref": { const ok = !top || g.ks.indexOf(t.k) < g.i && g.ks.indexOf(t.k) >= 0; if (!ok) why.push("a bare later ref " + t.k); return ok; }
    case "Ann": return lv(g, true, t.x);
    case "Let": { const a = allows(t.q, uses(t.f, 0)); if (!a) why.push("let " + t.n + " used " + uses(t.f, 0) + " at q" + t.q); const v = !live(t.q) || lv(g, true, t.v); return a && v && lv(bind(g, null, t.n), true, t.f); }
    case "Lam": { const a = allows(t.q, uses(t.f, 0)); if (!a) why.push("λ " + t.n + " used " + uses(t.f, 0) + " at q" + t.q); return a && lv(bind(g, null, t.n), true, t.f); }
    case "Tup": return (!live(t.q) || lv(g, true, t.a)) && lv(g, true, t.b);
    case "Prj": return lv(g, true, t.h);
    case "Mat": { const h = lv(g, true, t.h); const m = lv(g, true, t.m); return h && m; }
    case "Rwt": { const e = lv(g, true, t.e); const f = lv(g, true, t.f); return e && f; }
    case "Min": { const a = lv(g, true, t.a); const b = lv(g, true, t.b); return a && b; }
    default: return true;
  }
}
function tree(g: G, ps: Tag[], t: T): boolean {
  switch (t.$) {
    case "Lam": { const [p, g2, ps2] = next(g, ps, live(t.q)); const a = allows(t.q, uses(t.f, 0)); if (!a) why.push("tree λ " + t.n + " used " + uses(t.f, 0) + " at q" + t.q); return tree(bind(g2, p, t.n), ps2, t.f) && a; }
    case "Prj": { const [p, g2, ps2] = next(g, ps, true); const pp: Tag = p === null ? null : [p[0], "lt"]; return tree(g2, [pp, pp, ...ps2], t.h); }
    case "Mat": { const [p, g2, ps2] = next(g, ps, true); const h = tree(g2, ps2, t.h); const m = tree(g2, [p, ...ps2], t.m); return h && m; }
    case "Efq": return true;
    case "App": {
      if (t.x.$ === "Var" && takes(unspine(t.f)[0])) return tree(g, [g.ts[t.x.i] ?? null, ...ps], t.f);
      return lv(g, true, t);
    }
    default: return lv(g, true, t);
  }
}

src = fs.readFileSync(process.argv[2], "utf8");
const defs: Array<[string, T]> = [];
for (skip(); pos < src.length; skip()) {
  let k = name();
  if (k === "opaque" && (skip(), src[pos] !== ":")) k = name();
  eat(":"); term([]); eat("="); defs.push([k, term([])]);
}
const ks = defs.map(([k]) => k);
for (const [i, [k, v]] of defs.entries()) {
  if (process.argv[3] !== undefined && process.argv[3] !== k) continue;
  why.length = 0;
  if (!tree({ ks, i, cs: [], ts: [], ns: [] }, [], v)) console.log(k + ": " + [...new Set(why)].join("; "));
}
