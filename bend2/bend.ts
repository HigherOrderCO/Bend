// NOTE: this file was 99% human-designed and audited.
// It includes Bend's trusted kernel, including interpreter and checker.
// It has a bit of AI slop, but it is the most robust file in the repo. 
// 
// Bend
// ====
//
// SYNTAX
// ------
//
// Quant ::=
//   | "-"
//   | ""
//   | "+"
//
// Bind ::=
//   | Quant Name ":" Term
//   | "~" Name ":" Term
//   | Name
//
// Term ::=
//   | Var ::= Name
//   | Ref ::= Name
//   | Ann ::= "{" Term ":" Term "}"
//   | Typ ::= "Type" | "Data" | "Kind" "(" Term ")"
//   | Qnt ::= "Quant"
//   | Qua ::= "&0" | "&1" | "&2"
//   | Min ::= Term "<&>" Term
//   | All ::= "@" Bind "->" Term
//   | Lam ::= Name "=>" Body
//   | App ::= Term "(" [Term ","?] ")"
//   | ADT ::= Name "<" [Term ","?] ">"
//   | Ctr ::= Name "{" [Term ","?] "}"
//   | Mat ::= "\" "{" Name ":" Term ";"? Term "}"
//   | Efq ::= "\" "{" "}"
//   | Eql ::= "{" Term "==" Term ":" Term "}"
//   | Rfl ::= "{" "==" "}"
//   | Rwt ::= "%" (Name "@")? Term ":" Term ";"? Body
//   | Hol ::= "?" Name
//   | Grp ::= "(" Body ")"
//
// Case   ::= "case" [Term] ":" Body
// Match  ::= "match" [Term] ":" [Case]
// Local  ::= (Quant Name | Term)+ "=" Term+ ";"? Body
// Reply  ::= Term
// Body   ::= Match | Local | Reply
// Ctr    ::= Name "{" [Bind ","?] "}"
// ADT    ::= "type" Name ("<" [Bind ","?] ">")? "is" Term ":" [Ctr]
// Clause ::= ("for" (Quant | "~") | "exs") Name ":" Term ("where" Term)?
// Law    ::= "law" Name ":" [Clause] Body
// Def    ::= ("@unsafe")? "def" Name "?"? "(" [Bind ","?] ")" ("->" Term)? ":" (Body | ["import" STRING]+)
// TLD    ::= ADT | Law | Def
// Import ::= "import" "Base" | "import" Path "as" Name   (Path ends in .bend)
// Book   ::= [Import] [TLD]
//
// SUGARS
// ------
//
// Name   | Grammar                    | Term
// ------ | -------------------------- | ----
// Arrow  | A "->" B                   | @_:A -> B
// Exists | "&" Name ":" A "->" B      | Exists(A, x => B)
// Pair   | A "&" B                    | Pair(A, B)
// Or     | A "|" B                    | Or(A, B)
// Not    | "{" a "!=" b ":" T "}"     | {a == b : T} -> Empty
// Tuple  | "(" A ("," B)+ ")"         | Tuple{A, Tuple{B, ..}}
// List   | "[" [A ","?] "]", A "<>" B | Con{A, ..Nil{}}, Con{A, B}
// Array  | "[" A ":" T ("*" 2^D "n" | "^" D) "]" | Array.new(T, D, A)
// Nat    | NUMBER "n" ("+" T)?        | Lit, read as Succ{..Zero{}}; Succ{..T}
// U32    | NUMBER                     | Lit, read as U32{WCon{b, ..WNil{}}}
// F32    | NUMBER "." NUMBER [EXP]    | Lit, read as F32{WCon{b, ..WNil{}}}
// Chr    | "'" CHAR "'"               | Chr{U32}, its U32 a Lit
// Str    | "\"" [CHAR] "\""           | Lit, read as SCon{Chr, ..SNil{}}
// Index  | x "[" i "]" ("<-" v)?      | Array.get(U32, x, i), ..set(..)
// Fill   | D "<" [A ","?] ">"         | D<&1.., A..>
// Plus   | "+" D ("<" [A ","?] ">")?  | D<&2.., A..>
// Ops    | "(" a "+" b ":" T ")"      | T.add(a, b)
//
// every word the parser dispatches on is reserved and names nothing:
// def, type, law, match, case, do, return, for, exs, where,
// is, import, Type, Data, Kind, Quant ("as" reads only on an import
// line and "unsafe" only after "@", so they stay free).
// a file's namespace is its real path without ".bend", from the entry's
// directory; a "0x<hash>/" path is its own namespace, read from BEND_LIB
// and fetched from BEND_HUB on a miss; a "<name>@<version>/" path is the
// hash the hub names it, kept under BEND_LIB/names. "as Name" binds a
// per-file alias: Name.x resolves to the file's canonical name, so two
// aliases of one file agree, and a def of an aliased name fills it.
// "import Base" is the empty namespace; an unknown name is the file's own,
// unless its bare spelling is Base's.
// a def with no prior law types itself: a Bind telescope and a
// "->" return type. a def after its law takes bare names, no "->".
// "def f?(..)" is "@unsafe def f(..)".
// a bare Bind name is -Name: Quant. Fill and Plus omit a datatype's
// leading Quant parameters as a block; Plus alone fills a quant-only D.
// a literal expands to one node per unit, unbounded by design, but a
// number, a string and a char's code stay one Lit node and unfold where
// a chain is read (a match, a comparison, a descent, a pattern); a full
// word, a Nat, a Char, a String, a list, a tuple and an array (its
// slots) print back as literals; "*" takes a power of two. Arrow is
// right-associative; the domain of a written @ or & binder stops at the
// first bare "->", so an arrow (or a nested binder) there needs parens.
// infix "&" and "|" are right-associative, share one precedence, and
// bind tighter than "->". a rewrite "%e@E : P; f" binds e and "_" inside
// its motive P; "%E : P; f" is the nameless form: the equation binder is
// spelled "" and cannot be referenced.
//
// an operator is a method with no namespace: the ": T" before the ")"
// around it names T's head as its namespace, and nested parens
// inherit, so (a + b * c : U32) is U32.add(a, U32.mul(b, c)) and an
// argument is f((a + b : U32)); an index x[i + 1] is U32's; with no
// parens it is refused; braces only annotate. || && ++ <> & | are
// fixed, and equality is a call: T.is_eq(a, b).
//
// juxtaposition laws: a call or index suffix may be spaced but a
// newline ends the spine. after a term, "<" takes one argument above
// comparison level, then one token decides: ">" or "," commits to
// type args (x<y>, X<A, B>), anything else is less-than -- so a
// compound FIRST type argument needs parens (X<(A & B), C>; later
// arguments are full terms). a glued
// ">"-headed operator never fires -- space your comparisons and
// shifts -- so nested closers stack (List<List<A>>). "-" or "+" glued
// to a name heads a binder or literal, never an operator. a + on a
// pattern, let, lambda or do binder x re-binds it reusable: +x = x
// opens its body, below the destructures and match arms that head it.
// statements
// live in bodies: a def body, a case body, a lambda body or a
// rewrite tail; parens hold one body, or a tuple.
// a parallel let "x y z = a b c" is one Let binding n names to n
// values, each checked in the outer scope; the compiler forks its
// calls. a write a[i] <- v followed by a body statement is the let
// a = a[i] <- v. one token after a parsed term decides, but a ": T" with
// no "=" after it rewinds.
//
// "~k: T" heading a def's telescope, or "for ~k: T" in a law, makes it a
// template: a call T(~a, ..) writes ~ before each ~ argument, or none; a
// live closed call is the instance T~n, the def at a, minted and checked
// when the call checks; an open one is refused, and a call in a type or
// in a template's own text stays T's.
//
// Do-Notation
// -----------
//
// DoBlock ::=
//   | "do" Name "<" [Term ","?] ">" ":" [DoStmt]
//
// DoStmt ::=
//   | DoLet  ::= Name ":" Term "=" Term ";"? DoStmt
//   | DoExec ::= Term "<-" Term ";"? DoStmt
//   | DoBind ::= Name ":" Term "<-" Term ";"? DoStmt
//   | DoStep ::= Term (";" | NEWLINE) DoStmt
//   | DoPure ::= "return" Term
//   | DoRetr ::= Term
//
// do M<ls.., R>: desugars onto M.bind(ls.., A, R, v, x => ..) and
// M.pure(ls.., R, e); an empty list (do M<>:) drops R from both. A
// step is Unit <- Term: a term is a step when a ";" or a line at the
// block's column (its first statement's) follows it, else the value.
//
// THEORY
// ------
//
// Bend is a dependent affine calculus: two checking modes (dead and
// live) and a well-founded descent judgment on live recursion. a live
// variable is consumed at most once; a + binder licenses many uses and
// forms only when its type is Data. every live self-call descends, and
// no dead inhabitant is promoted to live evidence. this wall permits
// Type : Type, impredicativity and negative recursive types with no
// universe hierarchy and no positivity check: omega, Curry and Hurkens
// each contract a live function-valued binding, and no function type
// is Data. the gate is the wall.
// quantities None | Lone | Many (-x, x, +x) add sequentially (two live
// uses saturate to Many) and join pointwise-max across branches. the
// demand qt is None (dead) or Lone (live), never Many: a term checked
// at demand Many lets a binder inside it contract (McBride's system;
// Atkey 2018, 2.3, shows it also breaks substitution). an argument to
// a q binder checks at demand dem(q, qt), dead if q is -, else the
// ambient demand, and its measure adds once, unscaled: certify-once.
// so a Lone Data value may enter a + binder, let or field, and the
// callee copies the value. QTT forbids this: a 1 never becomes an ω
// (Atkey scales the argument's measure by the binder's ω), and a
// linear value is copied only by walking it. QTT affords that because
// its default is ω; Bend's default is Lone, so without promotion only
// closed data would ever be reusable, and input only by an O(n) walk.
// the price is a narrower theorem: term-substitution reduction does
// not preserve the measure (unfold f(+x) at f(y): y counts twice under
// its plain binder), so subject reduction for usage is claimed for
// weak by-value reduction of closed terms, the only reduction the
// machine performs: y is a value when f unfolds, its resources were
// consumed once, and the copy is Data, which owns nothing by the gate.
// three invariants outside the checker carry this: no pass duplicates
// a term (wnf shares every argument, let value and field in a cell;
// the compiler is strict); a type with runtime ownership (File,
// Socket, Array) is Type, never Data; a compiler may drop a copy the
// source spelled, never add one.
// every type has a kind Kind(q) over a quantity q : Quant, &0 (None),
// &1 (Lone) or &2 (Many); Type is Kind(&1), Data is Kind(&2), and a
// - binder's domain checks at Kind(&0). term_compare (LE) orders
// kinds by the quantity order with &0 and &1 one rung (a single use
// is what every type affords): Kind(g) fits Kind(h) when h <= g, so
// Data fits every kind, every kind fits Type and Kind(&0), and a meet
// fits every kind under one of its sides, never else. a binder q x: A needs A to
// fit Kind(q): its type's quantity is at least q. a function type
// is Type (a closure captures), an equation is Data (evidence is
// erased), a datatype declares its kind, Kind(G) over its parameters,
// and the meet a <&> b is the minimum, reduced only when forced: &2 is
// the identity, &0 absorbs, two literals meet, a stuck side stays
// stuck, so no definition order can decide it early. both its operands
// check at the ambient demand and their measures add, so a meet never
// launders a live occurrence past the tally. book_valid earns
// G: every live field's kind must fit Kind(G) in the real constructor
// context, so a constructor-local quantity never reaches G and a
// function field never sits in Data.
// a type position, an erased (-) argument, an equality endpoint or a
// motive checks dead: it may diverge and inhabit Empty; no rule coerces
// dead to live. a +field is QTT's ω-tensor read the Bend way: it forms
// only at Data, building it certifies its argument once, and matching
// a Lone node still yields it Many; a -field is absent from storage
// and dead; a pattern binder's quantity is its field's times its
// scrutinee's; a + mark on a 1 binder over Data makes it +.
// equality is intensional; elimination is the J axiom, %e@E : P; f,
// and a stuck rewrite fires only when its evidence reaches {==}.
// conversion is term_compare LE -- a fits b, a preorder, up to eta:
// directional only at kinds and ADT residuals (removing more
// constructors fits removing fewer), a function type compares its
// domains swapped, and every part that flows both ways (an argument,
// a parameter, a field, an equation endpoint) compares EQ.
// every live self-call descends on a strict
// subterm in its own case tree, live columns compared EQ left to right,
// erased columns skipped. trusted claims: subject reduction (for
// by-value reduction), progress, weak normalization of closed live
// terms, no closed live inhabitant of Empty.
// an @unsafe def opts out of the wall: its self-calls skip descent, its
// binder domains form + at any kind, and it calls an unfilled law or a
// def below it, so the claims above do not cover a book that uses one.
// a hole ?name fails every check,
// shown against the goal; ?TODO alone checks at any goal and marks
// the book incomplete.

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import * as url from "node:url";

// Core
// ====

// Types
// =====

export type Bool = boolean;
export type U32  = number;
export type Name = string;

export type Cmp =
  | "LT"
  | "EQ"
  | "GT";

// PMap
export type PMap<Item> =
  | { $: "Emp" }
  | { $: "Bin"; v: Item | null; l: PMap<Item>; r: PMap<Item> };

// List
export type List<Item> = { k: U32; v: Item; n: List<Item> } | null;

// Quant
export type Quant =
  | { $: "None" }
  | { $: "Lone" }
  | { $: "Many" };

// Uses
export type Uses = PMap<Quant>;

// Term
export type BodyOf<B> = B extends [infer T] ? T : B;
export type LetsOf<B> = BodyOf<B> extends Function ? (xs: TermOf<B>[]) => TermOf<B> : BodyOf<B>;
export type SubsOf<B> = BodyOf<B> extends Function ? TermOf<NoInfer<B>> : HTerm;
export type TermOf<B> = (
  | { $: "Var"; k: Name; i: number; v?: SubsOf<B> }                                // x
  | { $: "Ref"; k: Name; b?: Bool }                                                // x
  | { $: "Sub"; i: number; v: Patt; f: TermOf<B> }                                 // x <- v; f
  | { $: "Let"; k: Name[]; i: number[]; q: Quant[]; v: TermOf<B>[]; f: LetsOf<B> } // x y = v w; f
  | { $: "Typ"; g: TermOf<B> }                                                     // Kind(g)
  | { $: "Qnt" }                                                                   // Quant
  | { $: "Qua"; q: Quant }                                                         // &1, &2
  | { $: "Min"; a: TermOf<B>; b: TermOf<B> }                                       // a <&> b
  | { $: "All"; q: Quant; k: Name; i: number; A: TermOf<B>; B: BodyOf<B> }         // @x:A -> B
  | { $: "Lam"; k: Name; i: number; f: BodyOf<B>; q?: Quant }                      // x => f
  | { $: "App"; f: TermOf<B>; x: TermOf<B> }                                       // f(x)
  | { $: "ADT"; k: Name; x: TermOf<B>[]; r: Name[] }                               // A<x0,x1,...>
  | { $: "Ctr"; k: Name; x: TermOf<B>[] }                                          // A{x0,x1,...}
  | { $: "Lit"; k: "Nat" | "U32" | "F32"; v: number }                              // 3n, 7, 1.5 (its bits)
  | { $: "Lit"; k: "String"; v: string }                                           // "text"
  | { $: "Mat"; k: Name; h: TermOf<B>; m: TermOf<B> }                              // \{A: h; m}
  | { $: "Efq" }                                                                   // \{}
  | { $: "Eql"; a: TermOf<B>; b: TermOf<B>; T: TermOf<B> }                         // {a == b : T}
  | { $: "Rfl" }                                                                   // {==}
  | { $: "Rwt"; e: TermOf<B>; p: TermOf<B>; f: TermOf<B> }                         // %e@E : P; f (p = _ => e => P)
  | { $: "Hol"; k: Name }                                                          // ?name
  | { $: "Ann"; x: TermOf<B>; T: TermOf<B> }                                       // {x : T}
) & { s?: Span };

export type LTerm = TermOf<[LTerm]>;
export type HBody = (x: HTerm) => HTerm;
export type HTerm = TermOf<HBody>;

// Env
export type Env = List<HTerm | ((s?: Span) => HTerm)>;

// Definitions & Book
export type Ctr  = { k: Name; n: number; T: HTerm }
export type Ctrs = Array<Ctr>;
export type ADT  = { $: "ADT"; n: number; g: number; T: HTerm; c: Ctrs; b?: Bool; };
export type Def  = { $: "Def"; n: number; x: number; T: HTerm; v: HTerm | null; e?: LTerm; b?: Bool; u?: Bool; i?: string[]; m?: string; };
export type TLD  = ADT | Def;
export type Book = { tlds: Record<Name, TLD>; ctrs: Record<Name, Ctr>; order: Name[]; hols: number; tmps: Record<Name, Map<string, Name>>; };

// Context
export type Ann = { q: Quant; k: Name; T: HTerm };
export type Ctx = PMap<Ann>;

// Body
export type PVar  = { $: "PVar"; k: Name; i: number; q: Quant; s?: Span };
export type PCtr  = { $: "PCtr"; k: Name; x: Patt[]; s?: Span };
export type Patt  = PVar | PCtr;
export type Case  = { p: Patt[]; f: Body };
export type Rows  = Array<Case>;
export type Match = { $: "Match"; e: LTerm[]; r: Rows; s?: Span };
export type Local = { $: "Local"; k: Patt[]; q: Quant; v: LTerm[]; f: Body };
export type Body  = Match | Local | LTerm

// Parser
export type File  = { str: string; ns: string; al: Record<Name, Name>; };
export type Parse = File & { book: Book; dir: string; pos: number; stk: Array<[Name, number]>; frs: number; };
export type Span  = { file: File; beg: number; end: number; };

// Machine
export type LHS   = { t: HTerm; n: number; def: Name; qs: Quant[]; u?: Bool; z?: number };
export type Frame =
  | { $: "APP"; x: HTerm; s?: Span }                                                                             // _(x)
  | { $: "MAT"; t: Extract<HTerm, { $: "Mat" }>; e: HTerm; lhs: { t: () => HTerm; n: number } | null; s?: Span } // \{c:h;m}(_)
  | { $: "VAR"; l: Extract<HTerm, { $: "Var" }>; a?: Extract<HTerm, { $: "Ann" }> }                              // a share cell being filled
  | { $: "MNA"; b: HTerm; s?: Span }                                                                             // _ <&> b
  | { $: "MNB"; a: HTerm; s?: Span }                                                                             // a <&> _

// Infer
export type Infer = { tm: LTerm; ty: HTerm; us: Uses; x?: number };
export type Check = { tm: LTerm; us: Uses };

// Error
export type Expr = HTerm | string;
export type Err  = { $: "Err"; bok: Book; exp: Expr; obs?: Expr; ctx: Ctx; def?: Name; spn?: Span; nte?: string; };

// Constructors
// ============

// Term
// ----

export function Var<X>(k: Name, i: number, s?: Span, v?: SubsOf<X>): TermOf<X> {
  return { $: "Var", k, i, s, v };
}

export function Ref<X>(k: Name, s?: Span, b?: Bool): TermOf<X> {
  return { $: "Ref", k, s, b };
}

export function Sub<X>(i: number, v: Patt, f: TermOf<X>, s?: Span): TermOf<X> {
  return { $: "Sub", i, v, f, s };
}

export function Let<X>(k: Name[], i: number[], v: TermOf<X>[], f: LetsOf<X>, s?: Span, q?: Quant[]): TermOf<X> {
  return { $: "Let", k, i, q: q ?? k.map(() => Lone()), v, f, s };
}

export function Typ<X>(g: TermOf<X>, s?: Span): TermOf<X> {
  return { $: "Typ", g, s };
}

export function Qnt<X>(s?: Span): TermOf<X> {
  return { $: "Qnt", s };
}

export function Qua<X>(q: Quant, s?: Span): TermOf<X> {
  return { $: "Qua", q, s };
}

export function Min<X>(a: TermOf<X>, b: TermOf<X>, s?: Span): TermOf<X> {
  return { $: "Min", a, b, s };
}

export function All<X>(q: Quant, k: Name, i: number, A: NoInfer<TermOf<[X]>>, B: X, s?: Span): TermOf<[X]> {
  return { $: "All", q, k, i, A, B, s };
}

export function Lam<X>(k: Name, i: number, f: X, s?: Span, q?: Quant): TermOf<[X]> {
  return { $: "Lam", k, i, f, s, q };
}

export function App<X>(f: TermOf<X>, x: TermOf<X>, s?: Span): TermOf<X> {
  return { $: "App", f, x, s };
}

export function ADT<X>(k: Name, x: TermOf<X>[], s?: Span, r: Name[] = []): TermOf<X> {
  return { $: "ADT", k, x, r, s };
}

export function Ctr<X>(k: Name, x: TermOf<X>[], s?: Span): TermOf<X> {
  return { $: "Ctr", k, x, s };
}

export function Lit<X>(k: "Nat" | "U32" | "F32", v: number, s?: Span): TermOf<X>;
export function Lit<X>(k: "String", v: string, s?: Span): TermOf<X>;
export function Lit<X>(k: Name, v: number | string, s?: Span): TermOf<X> {
  return { $: "Lit", k, v, s } as TermOf<X>;
}

export function Mat<X>(k: Name, h: TermOf<X>, m: TermOf<X>, s?: Span): TermOf<X> {
  return { $: "Mat", k, h, m, s };
}

export function Efq<X>(s?: Span): TermOf<X> {
  return { $: "Efq", s };
}

export function Eql<X>(a: TermOf<X>, b: TermOf<X>, T: TermOf<X>, s?: Span): TermOf<X> {
  return { $: "Eql", a, b, T, s };
}

export function Rfl<X>(s?: Span): TermOf<X> {
  return { $: "Rfl", s };
}

export function Rwt<X>(e: TermOf<X>, p: TermOf<X>, f: TermOf<X>, s?: Span): TermOf<X> {
  return { $: "Rwt", e, p, f, s };
}

export function Hol<X>(k: Name, s?: Span): TermOf<X> {
  return { $: "Hol", k, s };
}

export function Ann<X>(x: TermOf<X>, T: TermOf<X>, s?: Span): TermOf<X> {
  return { $: "Ann", x, T, s };
}

// PMap
// ----

export function Emp<T>(): PMap<T> {
  return { $: "Emp" };
}

export function Bin<T>(v: T | null, l: PMap<T>, r: PMap<T>): PMap<T> {
  return { $: "Bin", v, l, r };
}

// Quant
// -----

export function None(): Quant {
  return { $: "None" };
}

export function Lone(): Quant {
  return { $: "Lone" };
}

export function Many(): Quant {
  return { $: "Many" };
}

// Infer
// -----

export function Infer(tm: LTerm, ty: HTerm, us: Uses, x?: number): Infer {
  return { tm: Ann(tm, Var("_", -1, undefined, ty)), ty, us, x };
}

export function Check(tm: LTerm, ty: HTerm, us: Uses): Check {
  return { tm: Ann(tm, Var("_", -1, undefined, ty)), us };
}

// Err
// ---

export function Err(bok: Book, ctx: Ctx, exp: Expr, obs?: Expr, spn?: Span, def?: Name, nte?: string): Err {
  return { $: "Err", bok, ctx, exp, obs, spn, def, nte };
}

// Char
// ====

export function char_is_head(c: string): boolean {
  const n = c.charCodeAt(0);
  return (n >= 65 && n <= 90) || (n >= 97 && n <= 122) || n === 95;
}

export function char_is_name(c: string): boolean {
  const n = c.charCodeAt(0);
  return char_is_head(c) || (n >= 48 && n <= 57) || n === 46;
}

// PMap
// ====

export function pmap_get<T>(map: PMap<T>, key: U32): T | null {
  let m = map;
  let k = key;
  while (true) {
    switch (m.$) {
      case "Emp": {
        return null;
      }
      case "Bin": {
        if (k === 0) {
          return m.v;
        }
        const odd = k % 2 === 1;
        m = odd ? m.l : m.r;
        k = odd ? (k - 1) / 2 : (k - 2) / 2;
        break;
      }
    }
  }
}

export function pmap_set<T>(map: PMap<T>, key: U32, val: T): PMap<T> {
  switch (map.$) {
    case "Emp": {
      return pmap_set(Bin<T>(null, map, map), key, val);
    }
    case "Bin": {
      if (key === 0) {
        return Bin(val, map.l, map.r);
      }
      if (key % 2 === 1) {
        const l = pmap_set(map.l, (key - 1) / 2, val);
        return Bin(map.v, l, map.r);
      }
      const r = pmap_set(map.r, (key - 2) / 2, val);
      return Bin(map.v, map.l, r);
    }
  }
}

export function pmap_union<T>(a: PMap<T>, b: PMap<T>, f: (x: T, y: T) => T): PMap<T> {
  if (a.$ === "Emp") {
    return b;
  }
  if (b.$ === "Emp") {
    return a;
  }
  const v = a.v === null ? b.v : b.v === null ? a.v : f(a.v, b.v);
  const l = pmap_union(a.l, b.l, f);
  const r = pmap_union(a.r, b.r, f);
  return Bin(v, l, r);
}

export function pmap_to_array<T>(map: PMap<T>, acc: U32 = 0, scl: U32 = 1): Array<[U32, T]> {
  switch (map.$) {
    case "Emp": {
      return [];
    }
    case "Bin": {
      const v: Array<[U32, T]> = map.v === null ? [] : [[acc, map.v]];
      const l = pmap_to_array(map.l, acc + scl * 1, scl * 2);
      const r = pmap_to_array(map.r, acc + scl * 2, scl * 2);
      return v.concat(l, r);
    }
  }
}

// List
// =====

export function list_get<T>(list: List<T>, key: U32): T | null {
  for (let l = list; l !== null; l = l.n) {
    if (l.k === key) {
      return l.v;
    }
  }
  return null;
}

export function list_set<T>(list: List<T>, key: U32, val: T): List<T> {
  return { k: key, v: val, n: list };
}

// Quant
// =====

export function quant_add(a: Quant, b: Quant): Quant {
  if (a.$ === "None") {
    return b;
  }
  if (b.$ === "None") {
    return a;
  }
  return Many();
}

export function quant_join(a: Quant, b: Quant): Quant {
  if (a.$ === "Many" || b.$ === "Many") {
    return Many();
  }
  if (a.$ === "None") {
    return b;
  }
  return a;
}

export function quant_dem(q: Quant, qt: Quant): Quant {
  if (q.$ === "None") {
    return None();
  }
  return qt;
}

// Uses
// ====

export function uses_nil(): Uses {
  return Emp<Quant>();
}

export function uses_add(a: Uses, b: Uses): Uses {
  return pmap_union(a, b, quant_add);
}

// LHS
// ===

export function lhs_ext(lhs: HTerm, k: Name, n: number, xs: HTerm[] = []): HTerm {
  if (n === 0) {
    return term_apply(lhs, Ctr(k, xs));
  } else {
    return Lam("_", 0, (x: HTerm) => {
      return lhs_ext(lhs, k, n - 1, [...xs, x]);
    });
  }
}

export function lhs_kind(lhs: LHS, q: Quant): Quant {
  return lhs.u === true && q.$ === "Many" ? Lone() : q;
}

// Term
// ====

export function term_apply(fn: HTerm, tm: HTerm, s?: Span): HTerm {
  const f = term_strip(fn);
  if (f.$ === "Lam") {
    return f.f(tm);
  }
  return App(f, tm, s);
}

export function term_unapply<X>(tm: TermOf<X>): [TermOf<X>, TermOf<X>[]] {
  const xs: TermOf<X>[] = [];
  let cur = tm;
  while (cur.$ === "App") {
    xs.push(cur.x);
    cur = cur.f;
  }
  return [cur, xs.reverse()];
}

export function term_cell(t: HTerm, k: Name = "_"): HTerm {
  if (t.$ === "Var" && t.i < 0) {
    return t;
  }
  return Var(k, -1, t.s, t);
}

export function term_force<X>(t: TermOf<X>): TermOf<X> {
  while (t.$ === "Var" && t.v !== undefined) {
    t = t.v as TermOf<X>;
  }
  return t;
}

export function term_strip<X>(tm: TermOf<X>): TermOf<X> {
  let t = term_force(tm);
  while (t.$ === "Ann") {
    t = term_force(t.x);
  }
  return t;
}

export function term_higher(tm: LTerm, env: Env = null): HTerm {
  switch (tm.$) {
    case "Var": {
      if (tm.i < 0) {
        return tm;
      }
      const v = list_get(env, tm.i);
      if (v === null) {
        return tm.v ?? Ref(tm.k, tm.s);
      } else if (typeof v === "function") {
        return v(tm.s);
      } else {
        return v.s !== undefined || tm.s === undefined || (v.$ === "Var" && v.i < 0) ? v : { ...v, s: tm.s };
      }
    }
    case "Ref": {
      if (tm.k.lastIndexOf(".") === 0) {
        const op = tm.s === undefined ? tm.k : tm.s.file.str.slice(tm.s.beg, tm.s.end);
        throw Err(book_nil(), ctx_nil(), "a type for this operator (write (a " + op + " b : Nat))", undefined, tm.s, undefined,
          "Note: we broke this after launch, sorry. Until 2.0.16 a bare operator meant Nat.\n"
          + "That was a bug: operators demand annotation. Wrap the expression and it'll work again.");
      }
      return Ref(tm.k, tm.s, tm.b);
    }
    case "Sub": {
      return term_higher(tm.f, list_set(env, tm.i, (s?: Span) => term_higher(patt_term(tm.v, s), env)));
    }
    case "Let": {
      const b = tm;
      const v = b.v.map((x) => term_higher(x, env));
      return Let(b.k, b.i, v, (xs: HTerm[]) => {
        let e = env;
        for (let j = 0; j < xs.length; j++) {
          e = list_set(e, b.i[j], xs[j]);
        }
        return term_higher(b.f, e);
      }, b.s, b.q);
    }
    case "All": {
      const b = tm;
      const A = term_higher(b.A, env);
      return All(b.q, b.k, b.i, A, (x: HTerm) => {
        return term_higher(b.B, list_set(env, b.i, x));
      }, b.s);
    }
    case "Lam": {
      const b = tm;
      return Lam(b.k, b.i, (x: HTerm) => {
        return term_higher(b.f, list_set(env, b.i, x));
      }, b.s, b.q);
    }
    case "Typ": {
      return Typ(term_higher(tm.g, env), tm.s);
    }
    case "Min": {
      return Min(term_higher(tm.a, env), term_higher(tm.b, env), tm.s);
    }
    case "App": {
      const f = term_higher(tm.f, env);
      const x = term_higher(tm.x, env);
      if (f.$ === "Lam") {
        return f.f(x);
      }
      return App(f, x, tm.s);
    }
    case "ADT": {
      return ADT(tm.k, tm.x.map((x) => term_higher(x, env)), tm.s, tm.r);
    }
    case "Ctr": {
      return Ctr(tm.k, tm.x.map((x) => term_higher(x, env)), tm.s);
    }
    case "Mat": {
      return Mat(tm.k, term_higher(tm.h, env), term_higher(tm.m, env), tm.s);
    }
    case "Eql": {
      return Eql(term_higher(tm.a, env), term_higher(tm.b, env), term_higher(tm.T, env), tm.s);
    }
    case "Rwt": {
      return Rwt(term_higher(tm.e, env), term_higher(tm.p, env), term_higher(tm.f, env), tm.s);
    }
    case "Ann": {
      return Ann(term_higher(tm.x, env), term_higher(tm.T, env), tm.s);
    }
    default: {
      return tm;
    }
  }
}

export function term_lower(term: HTerm, d: number = 0): LTerm {
  const tm = term_force(term);
  switch (tm.$) {
    case "Let": {
      const xs = tm.k.map((k, j): HTerm => Var(k, d + j));
      const vs = tm.v.map((v) => term_lower(v, d));
      return Let(tm.k, xs.map((_, j) => d + j), vs, term_lower(tm.f(xs), d + tm.k.length), tm.s, tm.q);
    }
    case "Typ": {
      return Typ(term_lower(tm.g, d), tm.s);
    }
    case "Min": {
      return Min(term_lower(tm.a, d), term_lower(tm.b, d), tm.s);
    }
    case "All": {
      const x: HTerm = Var(tm.k, d);
      return All(tm.q, tm.k, d, term_lower(tm.A, d), term_lower(tm.B(x), d + 1), tm.s);
    }
    case "Lam": {
      const x: HTerm = Var(tm.k, d);
      return Lam(tm.k, d, term_lower(tm.f(x), d + 1), tm.s, tm.q);
    }
    case "App": {
      return App(term_lower(tm.f, d), term_lower(tm.x, d), tm.s);
    }
    case "ADT": {
      return ADT(tm.k, tm.x.map((x) => term_lower(x, d)), tm.s, tm.r);
    }
    case "Ctr": {
      return Ctr(tm.k, tm.x.map((x) => term_lower(x, d)), tm.s);
    }
    case "Mat": {
      return Mat(tm.k, term_lower(tm.h, d), term_lower(tm.m, d), tm.s);
    }
    case "Eql": {
      return Eql(term_lower(tm.a, d), term_lower(tm.b, d), term_lower(tm.T, d), tm.s);
    }
    case "Rwt": {
      return Rwt(term_lower(tm.e, d), term_lower(tm.p, d), term_lower(tm.f, d), tm.s);
    }
    case "Ann": {
      return Ann(term_lower(tm.x, d), term_lower(tm.T, d), tm.s);
    }
    default: {
      return tm as LTerm;
    }
  }
}

// Ctx
// ===

export function ctx_nil(): Ctx {
  return Emp<Ann>();
}

export function ctx_bind(ctx: Ctx, i: number, q: Quant, k: Name, T: HTerm): Ctx {
  return pmap_set(ctx, i, { q, k, T });
}

export function ctx_dead(book: Book, ctx: Ctx): boolean {
  for (const [, a] of pmap_to_array(ctx)) {
    if (a.q.$ === "None") {
      continue;
    }
    const t = term_wnf(book, a.T);
    if (t.$ === "ADT" && book_adt(book, t, ctx).c.length === 0) {
      return true;
    }
  }
  return false;
}

export function ctx_scope(ctx: Ctx): Name[] {
  const bnd: Name[] = [];
  for (const [i, a] of pmap_to_array(ctx)) {
    bnd[i] = a.k;
  }
  return Array.from(bnd, (k) => k ?? "_");
}

// Book
// ====
// BEND_DIR, which holds base.bend and effs/, is this file's directory,
// or bend2/ beside bin/ in the compiled bend. NAMED is the hub's rule
// for a package's <name>@<version>.

export function book_nil(): Book {
  return { tlds: Object.create(null), ctrs: Object.create(null), order: [], hols: 0, tmps: Object.create(null) };
}

export function book_ctr(book: Book, k: Name): Ctr | null {
  return book.ctrs[k] ?? null;
}

export function book_fam(book: Book, k: Name): Name {
  const t = tele_unbind(book, (book_ctr(book, k) as Ctr).T).ret;
  return t.$ === "ADT" ? t.k : k;
}

export function book_adt(book: Book, tm: Extract<HTerm, { $: "ADT" }>, ctx: Ctx, def?: Name): ADT {
  const tld = book.tlds[tm.k];
  if (tld === undefined || tld.$ !== "ADT") {
    throw Err(book, ctx, "a declared datatype (unknown: " + name_key(tm.k) + ")", undefined, tm.s, def);
  }
  if (tm.r.length === 0) {
    return tld;
  }
  const r = new Set(tm.r);
  return { $: "ADT", n: tld.n, g: tld.g, T: tld.T, c: tld.c.filter((c) => !r.has(c.k)) };
}

export const BEND_DIR = import.meta.url.startsWith("file:///$bunfs/")
  ? path.join(path.dirname(fs.realpathSync(process.execPath)), "..", "bend2")
  : url.fileURLToPath(new URL(".", import.meta.url));
export const BASE_BEND = fs.realpathSync(path.join(BEND_DIR, "base.bend"));
const BEND_LIB = path.resolve(process.env.BEND_LIB ?? path.join(os.homedir(), ".bend", "lib"));
export const BEND_HUB   = process.env.BEND_HUB ?? "https://hub.bend-lang.com";

export const NAMED = /^([a-z][a-z0-9-]{0,63})@((?:0|[1-9][0-9]*)(?:\.(?:0|[1-9][0-9]*)){3})$/;

async function hub_get(book: Book, sub: string, hash: string, spn?: Span): Promise<string> {
  const res = await fetch(BEND_HUB + "/" + sub, { signal: AbortSignal.timeout(30000) }).catch(() => null);
  const src = res?.ok ? await res.text().catch(() => "") : "";
  const sum = Buffer.from(await crypto.subtle.digest("SHA-256", new TextEncoder().encode(src))).toString("hex");
  if (!res?.ok || hash.length < 32 || sum.slice(0, hash.length) !== hash || path.posix.normalize("/" + sub) !== "/" + sub) {
    throw Err(book, ctx_nil(), "a file at " + BEND_HUB + "/" + sub + " hashing to " + hash, undefined, spn);
  }
  return src;
}

async function name_hash(book: Book, nv: string, spn?: Span): Promise<string> {
  if (!NAMED.test(nv)) {
    throw Err(book, ctx_nil(), "a package as <name>@<version>: a-z, 0-9 and -, 1 to 64 characters, at four numbers like 1.0.0.0", "'" + nv + "'", spn);
  }
  const at  = path.join(BEND_LIB, "names", nv);
  const old = fs.existsSync(at) ? fs.readFileSync(at, "utf8").trim() : "";
  if (/^0x[0-9a-f]{32}$/.test(old)) {
    return old;
  }
  const res = await fetch(BEND_HUB + "/name/" + nv, { signal: AbortSignal.timeout(30000) }).catch(() => null);
  const got = res?.ok ? (await res.text().catch(() => "")).trim() : "";
  if (!/^0x[0-9a-f]{32}$/.test(got)) {
    throw Err(book, ctx_nil(), "a package named " + nv + " on " + BEND_HUB
      + (res?.status === 410 ? " (it was taken down)" : ""), undefined, spn);
  }
  fs.mkdirSync(path.dirname(at), { recursive: true });
  fs.writeFileSync(at, got + "\n");
  return got;
}

async function book_file(book: Book, file: string, spn?: Span): Promise<string> {
  if (file.startsWith(BEND_LIB + "/") && !fs.existsSync(file)) {
    const pkg = file.slice(BEND_LIB.length + 1).split("/")[0];
    const man = await hub_get(book, pkg + "/manifest", pkg.slice(2), spn);
    const fls = man.trim().split("\n").map((l) => l.split(" "));
    const srs = await Promise.all(fls.map(([h, p]) =>
      hub_get(book, pkg + "/" + p, h, spn)));
    fls.forEach(([, p], i) => {
      const at = BEND_LIB + "/" + pkg + "/" + p;
      fs.mkdirSync(path.dirname(at), { recursive: true });
      fs.writeFileSync(at, srs[i]);
    });
  }
  if (!fs.existsSync(file)) {
    throw Err(book, ctx_nil(), "no such file: " + file, undefined, spn);
  }
  return fs.realpathSync(file);
}

export async function book_load(book: Book, file: string, ns: string, seen: Map<string, string | null>, spn?: Span, root?: string): Promise<number> {
  const real = await book_file(book, file, spn);
  if (seen.has(real)) {
    if (seen.get(real) === null) {
      throw Err(book, ctx_nil(), "an import cycle through " + file, undefined, spn);
    }
    return book.order.length;
  }
  seen.set(real, null);
  const dir   = real.slice(0, real.lastIndexOf("/") + 1);
  const top   = root ?? dir;
  const text  = fs.readFileSync(real, "utf8");
  const lines = text.split("\n");
  const body  = lines.slice();
  const al    : Record<Name, Name> = Object.create(null);
  const hub   = (s: string): boolean => /^0x[0-9a-f]+\//.test(s);
  const ok    = (s: string, lib: boolean): boolean => hub(s) === lib
    && /^(\/|(\.\.\/)*)([A-Za-z_][\w-]*\/)*[A-Za-z_][\w-]*$/.test(s.replace(/^0x[0-9a-f]+\//, ""));
  for (let i = 0, at = 0; i < lines.length; at += lines[i].length + 1, i++) {
    const line = lines[i].trim();
    if (line === "" || line.startsWith("#")) {
      continue;
    }
    if (!/^import(\s|$)/.test(line)) {
      break;
    }
    const m   = /^import\s+(\S+)(?:\s+as\s+([A-Za-z_]\w*))?\s*(?:#.*)?$/.exec(line);
    const beg = at + lines[i].indexOf(m?.[1] ?? line);
    const sp  = { file: { str: text, ns, al }, beg, end: beg };
    if (m === null || (m[2] === undefined && m[1] !== "Base")) {
      throw Err(book, ctx_nil(), "an import ('import Base', or 'import <path> as <Name>')", "'" + line + "'", sp);
    }
    body[i] = "";
    if (m[2] === undefined) {
      await book_load(book, BASE_BEND, "", seen, sp);
      continue;
    }
    if (!m[1].endsWith(".bend")) {
      throw Err(book, ctx_nil(), "an import of a .bend file", "'" + m[1] + "'", sp);
    }
    if (m[2] in al) {
      throw Err(book, ctx_nil(), "a fresh alias (" + m[2] + " names an earlier import)", "'" + line + "'", sp);
    }
    const bad = () => Err(book, ctx_nil(), "an import path of plain names (letters, digits, _ and -; the hub's files import the hub's)", "'" + m[1] + "'", sp);
    const nv  = /^([^/]*@[^/]*)\//.exec(m[1]);
    const as  = nv === null ? m[1] : await name_hash(book, nv[1], sp) + m[1].slice(nv[1].length);
    const rel = path.posix.normalize(as);
    if (!ok(as.replace(/^\.\//, "").slice(0, -5), hub(as))) {
      throw bad();
    }
    const got = await book_file(book, hub(as) ? BEND_LIB + "/" + rel : path.posix.resolve(dir, rel), sp);
    const lib = fs.existsSync(BEND_LIB) ? fs.realpathSync(BEND_LIB) + "/" : "\0";
    const sub = (got.startsWith(lib) ? got.slice(lib.length)
      : path.posix.relative(top, got)).replace(/\.bend$/, "");
    if (!ok(sub, got.startsWith(lib)) || (hub(ns) && !got.startsWith(lib))) {
      throw bad();
    }
    al[m[2]] = sub;
    await book_load(book, got, sub, seen, sp, top);
  }
  const n0 = book.order.length;
  parse_book(book, dir, body.join("\n"), ns, al);
  if (real === BASE_BEND) {
    for (const k of book.order.slice(n0)) {
      book.tlds[k].b = true;
    }
  }
  seen.set(real, ns);
  return n0;
}

// Tele
// ====

export function tele_bind(tele: Array<[Quant, Name, number, LTerm, Span]>, end: LTerm): LTerm {
  return tele.reduceRight((out, [q, k, i, T, s]) => All(q, k, i, T, out, s), end);
}

export function tele_open(book: Book, tel: HTerm): Extract<HTerm, { $: "All" }> | null {
  const t = term_wnf(book, tel);
  return t.$ === "All" ? t : null;
}

export function tele_head(book: Book, tel: HTerm, ctx: Ctx, def?: Name, s?: Span): Extract<HTerm, { $: "All" }> {
  const t = tele_open(book, tel);
  if (t === null) {
    throw Err(book, ctx, "unreachable (a telescope binds its parameters and fields)", undefined, s, def);
  }
  return t;
}

export function tele_fill(book: Book, tel: HTerm, xs: HTerm[], ctx: Ctx, def?: Name, s?: Span): HTerm {
  let out = tel;
  for (const x of xs) {
    out = tele_head(book, out, ctx, def, s).B(x);
  }
  return out;
}

export function tele_unbind(book: Book, T: HTerm): { doms: Array<[Quant, Name, HTerm]>; ret: HTerm } {
  const doms: Array<[Quant, Name, HTerm]> = [];
  let tel = T;
  for (let t = tele_open(book, tel); t !== null; t = tele_open(book, tel)) {
    doms.push([t.q, t.k, t.A]);
    tel = t.B(Var(t.k, doms.length - 1));
  }
  return { doms, ret: term_wnf(book, tel) };
}

// Word
// ====

export function word_to_term<X>(n: U32, s?: Span): TermOf<X> {
  let out: TermOf<X> = Ctr("WNil", [], s);
  for (let i = 31; i >= 0; i--) {
    out = Ctr("WCon", [Ctr((n >>> i) & 1 ? "True" : "False", [], s), out], s);
  }
  return out;
}

// Lit
// ===
// a literal is one node holding a closed value of its Base type k. it
// means its constructor tree and unfolds one layer at a time where a head
// is read, so the checker pays nothing per unit. a JS string holds
// Unicode scalar values only, so a literal that spells a surrogate or a
// code point past U+10FFFF is its chain. past NAT_LITERAL_MAX, "n + T"
// is Nat.add(n, T), and the compiler emits U32.to_nat(n).

export function lit_of(cs: U32[], s?: Span): LTerm {
  if (cs.every((c) => c <= 0x10ffff && (c < 0xd800 || c > 0xdfff))) {
    return Lit("String", cs.map((c) => String.fromCodePoint(c)).join(""), s);
  }
  return cs.reduceRight<LTerm>((out, c) =>
    Ctr("SCon", [Ctr("Chr", [Lit("U32", c, s)], s), out], s), Ctr("SNil", [], s));
}

export function lit_step<X>({ k, v, s }: Extract<TermOf<X>, { $: "Lit" }>): TermOf<X> {
  if (k === "String") {
    const c = v.codePointAt(0);
    return c === undefined ? Ctr("SNil", [], s)
      : Ctr("SCon", [Ctr("Chr", [Lit("U32", c, s)], s), Lit(k, v.slice(c > 0xffff ? 2 : 1), s)], s);
  }
  if (k !== "Nat") {
    return Ctr(k, [word_to_term(v, s)], s);
  }
  return v === 0 ? Ctr("Zero", [], s) : Ctr("Succ", [Lit(k, v - 1, s)], s);
}

export const NAT_LITERAL_MAX = 256;

export function nat_from_term(t: LTerm): number | null {
  let n = 0;
  while (t.$ === "Ctr" && t.k === "Succ" && t.x.length === 1) {
    n += 1;
    t = t.x[0];
  }
  return t.$ === "Ctr" && t.k === "Zero" && t.x.length === 0 ? n
    : t.$ === "Lit" && t.k === "Nat" && n + t.v <= 0xffffffff
    ? n + t.v : null;
}

export function u32_from_term<X>(tm: TermOf<X>, k: "U32" | "F32" = "U32"): number | null {
  const w0 = term_strip(tm);
  if (w0.$ === "Lit") {
    return w0.k === k ? w0.v : null;
  }
  if (w0.$ !== "Ctr" || w0.k !== k || w0.x.length !== 1) {
    return null;
  }
  let n = 0;
  let i = 0;
  let w = term_strip(w0.x[0]);
  while (w.$ === "Ctr" && w.k === "WCon" && w.x.length === 2) {
    const b = term_strip(w.x[0]);
    if (b.$ !== "Ctr" || b.x.length !== 0 || (b.k !== "True" && b.k !== "False")) {
      return null;
    }
    n += b.k === "True" ? 2 ** i : 0;
    i += 1;
    w = term_strip(w.x[1]);
  }
  if (i !== 32 || w.$ !== "Ctr" || w.k !== "WNil" || w.x.length !== 0) {
    return null;
  }
  return n;
}

// F32
// ===

const F32_VIEW = new DataView(new ArrayBuffer(4));

export function f32_to_bits(v: number): U32 {
  F32_VIEW.setFloat32(0, v);
  return F32_VIEW.getUint32(0);
}

export function f32_from_bits(n: U32): number {
  F32_VIEW.setUint32(0, n);
  return F32_VIEW.getFloat32(0);
}

// f32_round rounds a decimal text to the nearest f32, once. Number rounds
// it to f64 first, which only errs when that f64 is the midpoint g of f
// and its other f32 neighbour; there the exact text picks the side, and a
// text that is the midpoint itself keeps f, the even one. comp.ts splices
// this function's source into the JS runtime, so it names nothing else.
export function f32_round(s: string): number {
  const d = Number(s);
  const a = Math.abs(d);
  const f = Math.fround(a);
  const g = 2 * a - Math.min(f, 2 ** 128);
  if (g === f || Math.fround(g) !== g || g === Infinity) {
    return Math.sign(d) * f;
  }
  let k = 0;
  while (a * 2 ** k % 1 !== 0) {
    k += 1;
  }
  const [, i, r, e] = /(\d*)\.?(\d*)(?:e([+-]?\d+))?$/i.exec(s)!;
  const n = Number(e ?? 0) - r.length;
  const x = BigInt(i + r) * 2n ** BigInt(k) * 10n ** BigInt(Math.max(n, 0));
  const y = BigInt(a * 2 ** k) * 10n ** BigInt(Math.max(-n, 0));
  return Math.sign(d) * (x === y || x > y !== g > f ? f : g);
}

// Show
// ====
// f32_show prints the shortest decimal that reads back to the same f32,
// as a literal (a point before an e); nan, inf and -inf print as such.

export function quant_show(q: Quant): string {
  return { None: "-", Lone: "", Many: "+" }[q.$];
}

const ESCAPES: Record<string, U32> = {
  "n": 10, "t": 9, "r": 13, "0": 0, "\\": 92, "'": 39, '"': 34,
};

export function term_key(tm: LTerm): string {
  return JSON.stringify(tm, (k, v) => k === "s" ? undefined : v);
}

function f32_show(x: number): string {
  let s = "nan";
  for (let p = 1; x === x && p <= 9 && f32_round(s) !== x; p += 1) {
    s = String(Number(x.toExponential(p - 1)));
  }
  return (Object.is(x, -0) ? "-0" : s).replace(/^-?\d+(?=e|$)/, "$&.0").replace("Infinity", "inf");
}

// a key as output spells it: ns:name is internal, and ns.name is shown
export function name_key(k: Name): string {
  return k.replace(":", ".");
}

// a key as a file spells it: its own names bare, an imported file's
// through its alias; any other key as name_key does
export function name_show(file: File | undefined, k: Name): string {
  if (file === undefined) {
    return name_key(k);
  }
  const [ns, nm] = k.includes(":") ? k.split(":") : ["", k];
  const a = Object.keys(file.al).find((a) => file.al[a] === ns);
  return ns === file.ns ? nm : a === undefined ? name_key(k) : a + "." + nm;
}

export function term_show(term: LTerm, top: number = -1, bnd: Name[] = [], file?: File): string {
  function term_show_sugar_exi(tm: LTerm, prc: number): string | null {
    const [h, xs] = term_unapply(tm);
    const b = xs[1];
    if (h.$ !== "Ref" || h.k !== "Exists" || xs.length !== 2 || b.$ !== "Lam") {
      return null;
    }
    const A = go(xs[0], 2);
    bnd.push(b.k);
    const f = go(b.f, 1);
    bnd.pop();
    const s = "&" + b.k + ":" + A + " -> " + f;
    return prc > 1 ? "(" + s + ")" : s;
  }
  function term_show_chain(tm: LTerm, k: Name, n: number): [LTerm[], LTerm] {
    const xs: LTerm[] = [];
    let t = tm;
    while (t.$ === "Ctr" && t.k === k && t.x.length === n) {
      xs.push(t.x[0]);
      t = t.x[n - 1];
    }
    return [xs, t];
  }
  function term_show_sugar_nat(tm: LTerm, prc: number): string | null {
    const k = nat_from_term(tm);
    if (k !== null) {
      return String(k) + "n";
    }
    const [xs, t] = term_show_chain(tm, "Succ", 1);
    const n = xs.length;
    if (n === 0) {
      return null;
    }
    const s = String(n) + "n+" + go(t, 1);
    return prc > 1 ? "(" + s + ")" : s;
  }
  function term_show_sugar_lst(tm: LTerm, prc: number): string | null {
    const [xs, t] = term_show_chain(tm, "Con", 2);
    if (t.$ === "Ctr" && t.k === "Nil" && t.x.length === 0) {
      return "[" + xs.map((x) => go(x, 0)).join(", ") + "]";
    }
    if (xs.length === 0) {
      return null;
    }
    const s = xs.map((x) => go(x, 2)).join(" <> ") + " <> " + go(t, 1);
    return prc > 1 ? "(" + s + ")" : s;
  }
  function term_show_sugar_tup(tm: LTerm): string | null {
    const [xs, t] = term_show_chain(tm, "Tuple", 2);
    if (xs.length === 0) {
      return null;
    }
    return "(" + xs.concat(t).map((x) => go(x, 0)).join(", ") + ")";
  }
  function term_show_sugar_arr(tm: LTerm): string[] | null {
    if (tm.$ === "Ctr" && tm.k === "ALeaf" && tm.x.length === 1) {
      return [go(tm.x[0], 0)];
    }
    if (tm.$ !== "Ctr" || tm.k !== "ANode" || tm.x.length !== 2) {
      return null;
    }
    const l = term_show_sugar_arr(tm.x[0]);
    const r = term_show_sugar_arr(tm.x[1]);
    if (l === null || r === null) {
      return null;
    }
    return l.concat(r);
  }
  function chr_show(n: U32, quote: string): string {
    const k = Object.keys(ESCAPES).find((k) => ESCAPES[k] === n && ((k !== "'" && k !== '"') || k === quote));
    if (k !== undefined) {
      return "\\" + k;
    }
    if (n < 32 || n === 127 || (n >= 0xd800 && n <= 0xdfff) || n > 0x10ffff) {
      return "\\u{" + n.toString(16) + "}";
    }
    return String.fromCodePoint(n);
  }
  function lit_text(v: string): string {
    return [...v].map((c) => chr_show(c.codePointAt(0) as U32, "\"")).join("");
  }
  function term_show_sugar_chr(tm: LTerm, quote: string): string | null {
    const n = tm.$ === "Ctr" && tm.k === "Chr" && tm.x.length === 1
      ? u32_from_term(tm.x[0]) : null;
    return n === null ? null : chr_show(n, quote);
  }
  function term_show_sugar_str(tm: LTerm): string | null {
    const [cs, t] = term_show_chain(tm, "SCon", 2);
    const ss = cs.map((c) => term_show_sugar_chr(c, "\""));
    const tl = t.$ === "Lit" && t.k === "String" ? lit_text(t.v)
             : t.$ === "Ctr" && t.k === "SNil" && t.x.length === 0 ? "" : null;
    if (tl === null || ss.includes(null)) {
      return null;
    }
    return "\"" + ss.join("") + tl + "\"";
  }
  function go(tm: LTerm, prc: number): string {
    switch (tm.$) {
      case "Var": {
        return bnd.lastIndexOf(tm.k) === tm.i ? tm.k : tm.k + "^" + String(tm.i);
      }
      case "Ref": {
        const k = name_show(file, tm.k);
        return (bnd.includes(k) ? k + "^" : k) + (tm.b === true ? "!" : "");
      }
      case "Sub": {
        return go(tm.f, prc);
      }
      case "Let": {
        const vs = tm.v.map((v) => go(v, 1));
        for (const k of tm.k) {
          bnd.push(k);
        }
        const f = go(tm.f, -1);
        bnd.length -= tm.k.length;
        const ks = tm.k.map((k, j) => quant_show(tm.q[j]) + k);
        const s  = ks.join(" ") + " = " + vs.join(" ") + "; " + f;
        return prc >= 0 ? "(" + s + ")" : s;
      }
      case "Typ": {
        const g = tm.g;
        if (g.$ === "Qua" && g.q.$ === "Lone") {
          return "Type";
        }
        if (g.$ === "Qua" && g.q.$ === "Many") {
          return "Data";
        }
        return "Kind(" + go(tm.g, 0) + ")";
      }
      case "Qnt": {
        return "Quant";
      }
      case "Qua": {
        return { None: "&0", Lone: "&1", Many: "&2" }[tm.q.$];
      }
      case "Min": {
        const s = go(tm.a, 2) + " <&> " + go(tm.b, 2);
        return prc > 1 ? "(" + s + ")" : s;
      }
      case "All": {
        const A = go(tm.A, 2);
        bnd.push(tm.k);
        const B = go(tm.B, 1);
        bnd.pop();
        const s = "@" + quant_show(tm.q) + tm.k + ":" + A + " -> " + B;
        return prc > 1 ? "(" + s + ")" : s;
      }
      case "Lam": {
        bnd.push(tm.k);
        const f = go(tm.f, -1);
        bnd.pop();
        const s = quant_show(tm.q ?? Lone()) + tm.k + " => " + f;
        return prc > 0 ? "(" + s + ")" : s;
      }
      case "App": {
        const sug = term_show_sugar_exi(tm, prc);
        if (sug !== null) {
          return sug;
        }
        const [h, xs] = term_unapply(tm);
        const hs = go(h, 2);
        const as = xs.map((x) => go(x, 0));
        return hs + "(" + as.join(", ") + ")";
      }
      case "ADT": {
        const as = tm.x.map((x) => go(x, 0));
        const rs = tm.r.map((c) => " - " + name_show(file, c) + "{}").join("");
        const s  = name_show(file, tm.k) + (as.length === 0 && rs === "" ? "" : "<" + as.join(", ") + ">") + rs;
        return rs !== "" && prc > 1 ? "(" + s + ")" : s;
      }
      case "Ctr": {
        const u32 = u32_from_term(tm);
        const f32 = u32_from_term(tm, "F32");
        const chr = term_show_sugar_chr(tm, "'");
        const arr = term_show_sugar_arr(tm);
        const sug = u32 !== null ? String(u32) : f32 !== null ? f32_show(f32_from_bits(f32))
                 : term_show_sugar_nat(tm, prc)
                 ?? (chr !== null ? "'" + chr + "'" : null)
                 ?? term_show_sugar_str(tm)
                 ?? term_show_sugar_lst(tm, prc)
                 ?? term_show_sugar_tup(tm)
                 ?? (arr !== null ? "[" + arr.join(", ") + "]" : null);
        if (sug !== null) {
          return sug;
        }
        const as = tm.x.map((x) => go(x, 0));
        return name_show(file, tm.k) + "{" + as.join(", ") + "}";
      }
      case "Lit": {
        return go(lit_step(tm), prc);
      }
      case "Mat": {
        const arms: string[] = [];
        let m: LTerm = tm;
        while (m.$ === "Mat") {
          arms.push(name_show(file, m.k) + ": " + go(m.h, 1));
          m = m.m;
        }
        if (m.$ !== "Efq") {
          arms.push(go(m, 1));
        }
        return "\\{" + arms.join("; ") + "}";
      }
      case "Efq": {
        return "\\{}";
      }
      case "Eql": {
        return "{" + go(tm.a, 1) + " == " + go(tm.b, 1) + " : " + go(tm.T, 1) + "}";
      }
      case "Rfl": {
        return "{==}";
      }
      case "Hol": {
        return "?" + tm.k;
      }
      case "Rwt": {
        const e  = go(tm.e, 1);
        const mp = term_strip(tm.p);
        const mb = mp.$ === "Lam" ? term_strip(mp.f) : mp;
        let n = "";
        let P;
        if (mp.$ === "Lam" && mb.$ === "Lam") {
          bnd.push(mp.k, mb.k);
          P = go(mb.f, 1);
          bnd.length -= 2;
          n = mb.k === "" ? "" : mb.k + "@";
        } else {
          P = go(tm.p, 1);
        }
        const f = go(tm.f, -1);
        const s = "%" + n + e + " : " + P + "; " + f;
        return prc > 0 ? "(" + s + ")" : s;
      }
      case "Ann": {
        return "{" + go(tm.x, 1) + " : " + go(tm.T, 1) + "}";
      }
    }
  }
  return go(term, top);
}

export function expr_show(book: Book, x: Expr, bnd: Name[] = [], file?: File): string {
  if (typeof x === "string") {
    return x;
  } else {
    const t = term_lower(term_snf(book, x), bnd.length);
    return term_show(t, -1, bnd, file);
  }
}

export function err_show(err: Err): string {
  const file = err.spn?.file;
  const bnd  = ctx_scope(err.ctx);
  const anns = pmap_to_array(err.ctx).sort((a, b) => a[0] - b[0]);
  const wid  = Math.max(0, ...anns.map(([, a]) => a.k.length));
  const msg  = err.obs === undefined
    ? "\n- message  : " + expr_show(err.bok, err.exp, bnd, file)
    : "\n- expected : " + expr_show(err.bok, err.exp, bnd, file)
    + "\n- observed : " + expr_show(err.bok, err.obs, bnd, file);
  const ctx  = anns.map(([i, a]) =>
    "\n- " + a.k.padEnd(wid) + " : " + expr_show(err.bok, a.T, bnd.slice(0, i), file)).join("");
  const def  = err.def === undefined ? "" : " " + name_show(file, err.def);
  let   spn  = "";
  if (err.spn !== undefined) {
    const lns = err.spn.file.str.split("\n");
    const pre = err.spn.file.str.slice(0, err.spn.beg).split("\n");
    const at  = pre.length;
    const beg = Math.max(1, at - 1);
    const end = Math.min(lns.length, at + 1);
    const num = String(end).length;
    const lft = pre[at - 1];
    const car = " ".repeat(num) + " | " + lft.replace(/[^\t]/g, " ") + "^".repeat(Math.max(1, Math.min(err.spn.end - err.spn.beg, lns[at - 1].length - lft.length)));
    spn = "\n" + lns.slice(beg - 1, end).map((l, j) =>
      String(beg + j).padStart(num) + (beg + j === at ? ">| " + l + "\n" + car : " | " + l)).join("\n");
  }
  const loc  = def === "" && spn === "" ? "" : "\nLocation:" + def + spn;
  const nte = err.nte === undefined ? "" : "\n" + err.nte;
  return "Error:" + msg + (anns.length === 0 ? "" : "\nContext:") + ctx + loc + nte;
}

// Parse
// =====
// x : T = v is a typed let, its value {v : T}; a reply's : T is a group's.
// a declared name is fresh in its table, and so is its bare name (a
// module may not repeat Base's); its first word is no import's alias.

const KEYWORDS = new Set([
  "def", "type", "law", "match", "case", "do", "return",
  "for", "exs", "where", "is", "import",
  "Type", "Data", "Kind", "Quant",
]);

const QUAS: Record<string, Quant> = { "0": None(), "1": Lone(), "2": Many() };

export function parse_col(src: string, pos: number): number {
  return pos - src.lastIndexOf("\n", pos - 1);
}

export function parse_span(p: Parse, beg: number): Span {
  return { file: p, beg, end: p.pos };
}

export function parse_fail(p: Parse, exp: string, beg = p.pos, end = p.pos): never {
  const obs = beg < end ? "'" + p.str.slice(beg, end) + "'" : p.pos < p.str.length ? "'" + p.str[p.pos] + "'" : "end of input";
  throw Err(p.book, ctx_nil(), exp, obs, { file: p, beg, end });
}

export function parse_peek(p: Parse): string {
  return p.pos < p.str.length ? p.str[p.pos] : "";
}

export function parse_bump(p: Parse): string {
  const c = parse_peek(p);
  p.pos += 1;
  return c;
}

export function parse_at(p: Parse, s: string): boolean {
  if (p.str.charCodeAt(p.pos) !== s.charCodeAt(0)) {
    return false;
  }
  return s.length === 1 || p.str.startsWith(s, p.pos);
}

export function parse_take(p: Parse, s: string): boolean {
  if (!parse_at(p, s)) {
    return false;
  }
  p.pos += s.length;
  return true;
}

export function parse_skip(p: Parse): void {
  const s = p.str;
  while (p.pos < s.length) {
    const n = s.charCodeAt(p.pos);
    if (n === 32 || n === 10 || n === 13 || n === 9) {
      p.pos += 1;
      continue;
    }
    if (n === 35) {
      while (p.pos < s.length && s.charCodeAt(p.pos) !== 10) {
        p.pos += 1;
      }
      continue;
    }
    return;
  }
}

export function parse_eat(p: Parse, s: string): void {
  parse_skip(p);
  if (!parse_take(p, s)) {
    parse_fail(p, "'" + s + "'");
  }
}

export function parse_at_word(p: Parse, w: string): boolean {
  parse_skip(p);
  if (!parse_at(p, w)) {
    return false;
  }
  return !char_is_name(p.str[p.pos + w.length] ?? "");
}

export function parse_word(p: Parse, w: string): boolean {
  if (!parse_at_word(p, w)) {
    return false;
  }
  parse_take(p, w);
  return true;
}

export function parse_lexeme(p: Parse): Name {
  parse_skip(p);
  if (!char_is_head(parse_peek(p))) {
    parse_fail(p, "a name");
  }
  const beg = p.pos;
  while (p.pos < p.str.length && char_is_name(p.str[p.pos])) {
    p.pos += 1;
  }
  const k = p.str.slice(beg, p.pos);
  if (!/^[A-Za-z_]\w*(\.[A-Za-z_]\w*)*$/.test(k)) {
    parse_fail(p, "a name (words joined by dots, got '" + k + "')", beg);
  }
  return k;
}

export function parse_name(p: Parse): Name {
  const k = parse_lexeme(p);
  if (KEYWORDS.has(k)) {
    parse_fail(p, "a name (got the keyword '" + k + "')", p.pos - k.length);
  }
  return k;
}

export function parse_char(p: Parse): U32 {
  if (parse_take(p, "\\")) {
    const u = /^u\{([0-9a-f]+)\}/i.exec(p.str.slice(p.pos, p.pos + 11));
    if (u !== null) {
      p.pos += u[0].length;
      return parseInt(u[1], 16);
    }
    const c = ESCAPES[parse_bump(p)];
    if (c === undefined) {
      parse_fail(p, "an escape (\\n \\t \\r \\0 \\\\ \\' \\\" \\u{1F600})");
    }
    return c;
  }
  const n = p.str.codePointAt(p.pos);
  if (n === undefined) {
    parse_fail(p, "a character");
  }
  p.pos += n > 0xffff ? 2 : 1;
  return n;
}

// Binders
// -------

export function parse_open(p: Parse, k: Name): number {
  const i = p.frs++;
  if (k !== "_") {
    p.stk.push([k, i]);
  }
  return i;
}

export function parse_close(p: Parse, n: number): void {
  p.stk.length = n;
}

export function parse_var(p: Parse, k: Name, s?: Span): LTerm {
  for (let j = p.stk.length - 1; j >= 0; j--) {
    if (p.stk[j][0] === k) {
      return Var(k, p.stk[j][1], s);
    }
  }
  const q = parse_reso(p, k);
  if (k.includes(".")) {
    return Ref(q, s);
  }
  return Var(k, p.frs++, s, Ref(q, s));
}

export function parse_qual(p: Parse, k: Name): Name {
  return p.ns === "" ? k : p.ns + ":" + k;
}

export function parse_reso(p: Parse, k: Name): Name {
  const dot = k.indexOf(".");
  let q = parse_qual(p, k);
  if (dot !== -1 && k.slice(0, dot) in p.al) {
    q = p.al[k.slice(0, dot)] + ":" + k.slice(dot + 1);
    if (q !== k && (q in p.book.tlds || q in p.book.ctrs) && (k in p.book.tlds || k in p.book.ctrs)) {
      parse_fail(p, "an unambiguous name (the alias " + k.slice(0, dot) + " shadows " + k + ")");
    }
  }
  const own = q in p.book.tlds || q in p.book.ctrs;
  const far = k in p.book.tlds || k in p.book.ctrs;
  return own || !far ? q : k;
}

export function parse_call(p: Parse, k: Name, xs: LTerm[], s?: Span): LTerm {
  return xs.reduce<LTerm>((f, x) => App(f, x, s), Ref(parse_reso(p, k), s));
}

// Quant
// -----

export function parse_quant(p: Parse): Quant {
  parse_skip(p);
  if (parse_take(p, "-")) {
    return None();
  }
  if (parse_take(p, "+")) {
    return Many();
  }
  return Lone();
}

// Patt
// ----

export function parse_bind(p: Parse, t: LTerm): PVar {
  if (t.$ !== "Var") {
    parse_fail(p, "a lambda binder (one name: k => body)");
  }
  return { $: "PVar", k: t.k, i: parse_open(p, t.k), q: t.i < 0 ? Many() : Lone(), s: t.s };
}

export function parse_patt(p: Parse, t: LTerm): Patt {
  const book = p.book;
  switch (t.$) {
    case "Var": {
      if (book_ctr(book, parse_reso(p, t.k)) !== null) {
        throw Err(book, ctx_nil(), "a braced constructor pattern (" + name_key(t.k) + " is a constructor: write " + name_key(t.k) + "{}, or rename the binder)", undefined, t.s);
      }
      return parse_bind(p, t);
    }
    case "Ctr": {
      const ctr = book_ctr(book, t.k);
      if (ctr === null) {
        throw Err(book, ctx_nil(), "a declared constructor (unknown: " + name_key(t.k) + ")", undefined, t.s);
      }
      if (ctr.n !== t.x.length) {
        throw Err(book, ctx_nil(), "a " + name_key(t.k) + " pattern with " + String(ctr.n) + (ctr.n === 1 ? " field" : " fields"), undefined, t.s);
      }
      return { $: "PCtr", k: t.k, x: t.x.map((x) => parse_patt(p, x)), s: t.s };
    }
    case "Lit": {
      return parse_patt(p, lit_step(t));
    }
    default: {
      throw Err(book, ctx_nil(), "a pattern (a binder or a constructor)", term_show(term_lower(term_higher(t), 0)), t.s);
    }
  }
}

// Term
// ----

export function parse_term(p: Parse, lvl: number = 0): LTerm {
  parse_skip(p);
  const beg  = p.pos;
  const base = parse_term_base(p, beg);
  base.s ??= parse_span(p, beg);
  return parse_term_ops(p, base, base.s.beg, lvl);
}

export function parse_term_base(p: Parse, beg: number): LTerm {
  const c = parse_peek(p);
  if (char_is_head(c)) {
    const k = parse_lexeme(p);
    if (k === "Type") {
      return Typ(Qua(Lone()));
    }
    if (k === "Data") {
      return Typ(Qua(Many()));
    }
    if (k === "Quant") {
      return Qnt();
    }
    if (k === "Kind") {
      parse_eat(p, "(");
      const g = parse_term(p);
      parse_eat(p, ")");
      return Typ(g);
    }
    if (k === "do") {
      const m = parse_name(p);
      parse_eat(p, "<");
      const ts = parse_fill(p, parse_reso(p, m), parse_term_args(p, ">"),
        parse_span(p, beg));
      parse_eat(p, ":");
      parse_skip(p);
      return parse_term_do_stmt(p, m, ts, parse_col(p.str, p.pos));
    }
    if (k === "match") {
      parse_fail(p, "a term (a match heads a def body, not a term)", beg);
    }
    if (k === "case") {
      parse_fail(p, "a match heading this case (this case is orphaned)", beg);
    }
    if (k === "return") {
      parse_fail(p, "a do-block heading this return", beg);
    }
    if (KEYWORDS.has(k)) {
      parse_fail(p, "a term (the keyword '" + k + "' cannot head one)", beg);
    }
    if (parse_take(p, "{")) {
      const xs = parse_term_args(p, "}");
      return Ctr(parse_reso(p, k), xs);
    }
      return parse_var(p, k, parse_span(p, beg));
  }
  if (/[0-9]/.test(c)) {
    return parse_term_num(p);
  }
  switch (c) {
    case "@": {
      return parse_term_all(p, false);
    }
    case "&": {
      const q = QUAS[p.str[p.pos + 1] ?? ""];
      if (q !== undefined) {
        parse_bump(p);
        parse_bump(p);
        return Qua(q);
      }
      return parse_term_all(p, true);
    }
    case "+": {
      parse_bump(p);
      const t = parse_term(p, 12);
      const s = parse_span(p, beg);
      const k   = t.$ === "ADT" ? t.k : t.$ === "Var" || t.$ === "Ref" ? parse_reso(p, t.k) : "";
      const tld = p.book.tlds[k];
      if (t.$ === "Var" && (tld === undefined || tld.$ !== "ADT")) {
        return Var(t.k, -1, s);
      }
      if (tld === undefined || tld.$ !== "ADT" || tld.g === 0 || tld.g < tld.n && t.$ !== "ADT") {
        parse_fail(p, "a quantified datatype after + (+D<..> sets D's leading quantities to &2)");
      }
      const xs = t.$ === "ADT" ? t.x : Array.from({ length: tld.n }, (): LTerm => Qua(Lone(), s));
      return ADT(k, xs.map((x, i) => i < tld.g ? Qua(Many(), s) : x), s);
    }
    case "\\": {
      parse_bump(p);
      parse_eat(p, "{");
      const arms: Array<[Name, LTerm]> = [];
      let tail: LTerm = Efq();
      while (true) {
        parse_skip(p);
        if (parse_take(p, "}")) {
          break;
        }
        const t = parse_term(p);
        parse_skip(p);
        if ((t.$ === "Var" || t.$ === "Ref") && parse_take(p, ":")) {
          const h = parse_term(p);
          arms.push([parse_reso(p, t.k), h]);
          parse_skip(p);
          parse_take(p, ";");
          continue;
        }
        tail = t;
        parse_skip(p);
        parse_take(p, ";");
        parse_eat(p, "}");
        break;
      }
      const s = parse_span(p, beg);
      tail.s ??= s;
      return arms.reduceRight((out, arm) => Mat(arm[0], arm[1], out, s), tail);
    }
    case "%": {
      parse_bump(p);
      const e0 = parse_term(p);
      parse_skip(p);
      let k = "";
      let e = e0;
      if (parse_take(p, "@")) {
        if (e0.$ !== "Var") {
          parse_fail(p, "a name before @ (a rewrite binder is one name: %e@E : P)");
        }
        k = e0.k;
        e = parse_term(p);
      }
      parse_eat(p, ":");
      const n0 = p.stk.length;
      const xi = p.frs++;
      p.stk.push(["_", xi]);
      const ei = parse_open(p, k);
      const P  = parse_term(p);
      parse_close(p, n0);
      parse_skip(p);
      parse_take(p, ";");
      const f = parse_block(p);
      const s = parse_span(p, beg);
      return Rwt(e, Lam("_", xi, Lam(k, ei, P, e0.s), s), f, s);
    }
    case "{": {
      parse_bump(p);
      parse_skip(p);
      if (parse_take(p, "==")) {
        parse_eat(p, "}");
        return Rfl();
      }
      const a = parse_term(p);
      parse_skip(p);
      const ne = parse_take(p, "!=");
      const ns = parse_span(p, p.pos - 2);
      const b  = ne || parse_take(p, "==") ? parse_term(p) : null;
      parse_eat(p, ":");
      const T = parse_term(p);
      parse_eat(p, "}");
      if (b === null) {
        return Ann(a, T);
      }
      if (!ne) {
        return Eql(a, b, T);
      }
      const s = parse_span(p, beg);
      return All(Lone(), "_", parse_open(p, "_"), Eql(a, b, T, s), Ref("Empty", ns), s);
    }
    case "(": {
      parse_bump(p);
      return parse_term_tup(p, beg);
    }
    case "[": {
      parse_bump(p);
      parse_skip(p);
      const xs = parse_at(p, "]") ? [] : [parse_term(p)];
      parse_skip(p);
      if (xs.length !== 0 && parse_take(p, ":")) {
        const T = parse_term(p, 12);
        parse_skip(p);
        const cnt = parse_take(p, "*");
        if (!cnt) {
          parse_eat(p, "^");
        }
        const n = parse_term(p);
        parse_eat(p, "]");
        const s = parse_span(p, beg);
        parse_term_ns(p, xs[0], T);
        let d = n;
        if (cnt) {
          const k = Math.log2(nat_from_term(n) ?? 0);
          if (!Number.isInteger(k)) {
            throw Err(p.book, ctx_nil(), "a power of two count (^d takes a depth)", undefined, n.s);
          }
          d = Lit("Nat", k, n.s);
        }
        return App(App(App(Ref("Array.new", s), T, s), d, s), xs[0], s);
      }
      parse_take(p, ",");
      const ys  = xs.concat(parse_term_args(p, "]"));
      const spn = parse_span(p, beg);
      return ys.reduceRight<LTerm>((t, x) => Ctr("Con", [x, t], spn), Ctr("Nil", [], spn));
    }
    case "'": {
      parse_bump(p);
      const n = parse_char(p);
      if (!parse_take(p, "'")) {
        parse_fail(p, "a closing '");
      }
      const spn = parse_span(p, beg);
      return Ctr("Chr", [Lit("U32", n, spn)], spn);
    }
    case '"': {
      parse_bump(p);
      const cs: U32[] = [];
      while (!parse_take(p, '"')) {
        if (p.pos >= p.str.length) {
          parse_fail(p, "a closing \"");
        }
        cs.push(parse_char(p));
      }
      return lit_of(cs, parse_span(p, beg));
    }
    case "?": {
      parse_bump(p);
      const k = parse_name(p);
      if (k === "TODO") {
        p.book.hols += 1;
      }
      return Hol(k);
    }
    default: {
      parse_fail(p, "a term");
    }
  }
}

const INFIX: Record<string, [number, Bool, Name]> = {
  "<-":  [-1, false, ""],
  "->":  [0,  true,  ""],
  "&":   [1,  true,  "Pair"],
  "|":   [1,  true,  "Or"],
  "||":  [2,  false, "Bool.or"],
  "&&":  [3,  false, "Bool.and"],
  "<":   [4,  false, ".is_lt"],
  "<=":  [4,  false, ".is_le"],
  ">":   [4,  false, ".is_gt"],
  ">=":  [4,  false, ".is_ge"],
  "<>":  [5,  true,  "Con"],
  "++":  [5,  true,  "String.append"],
  "<&>": [5,  true,  ""],
  ".|.": [6,  false, ".or"],
  ".^.": [7,  false, ".xor"],
  ".&.": [8,  false, ".and"],
  "<<":  [9,  false, ".shln"],
  ">>":  [9,  false, ".shrn"],
  "+":   [10, false, ".add"],
  "-":   [10, false, ".sub"],
  "*":   [11, false, ".mul"],
  "/":   [11, false, ".div"],
  "%":   [11, false, ".mod"],
};

export function parse_nl(p: Parse): boolean {
  for (let j = p.pos - 1; j >= 0; j--) {
    const c = p.str[j];
    if (c === "\n") {
      return true;
    }
    if (c !== " " && c !== "\r" && c !== "\t") {
      return false;
    }
  }
  return true;
}

export function parse_term_ops(p: Parse, tm: LTerm, beg: number, lvl: number): LTerm {
  let out = tm;
  while (true) {
    parse_skip(p);
    if (parse_nl(p) && (parse_at(p, "(") || parse_at(p, "["))) {
      return out;
    }
    if (parse_at(p, "(") || parse_at(p, "!(")) {
      if (out.$ === "Var" && out.v !== undefined) {
        out = out.v as LTerm;
      }
      if (parse_at(p, "!")) {
        if (out.$ !== "Ref") {
          parse_fail(p, "a named def before ! (only f!(..) offloads)");
        }
        parse_bump(p);
        out.b = true;
        continue;
      }
      parse_bump(p);
      const hd = p.book.tlds[out.$ === "Ref" ? out.k : ""];
      const x  = hd?.$ === "Def" ? hd.x : 0;
      const ts: LTerm[] = [];
      for (parse_skip(p); x > 0 && parse_at(p, "~"); parse_skip(p)) {
        if (ts.length === x) {
          parse_fail(p, "a term (" + name_key(out.k) + " takes " + String(x) + " ~)");
        }
        parse_bump(p);
        ts.push(parse_term(p));
        parse_skip(p);
        parse_take(p, ",");
      }
      const xs = ts.concat(parse_term_args(p, ")"));
      const s  = parse_span(p, beg);
      for (const a of xs) {
        out = App(out, a, s);
      }
      continue;
    }
    if (parse_at(p, "[")) {
      parse_bump(p);
      const ix = parse_term(p);
      parse_eat(p, "]");
      const s = parse_span(p, beg);
      parse_term_ns(p, ix, Ref("U32", s));
      parse_skip(p);
      if (!parse_nl(p) && parse_take(p, "<-")) {
        const v = parse_term(p, 2);
        out = App(App(App(App(Ref("Array.set", s), Ref("U32", s), s),
          out, s), ix, s), v, s);
      } else {
        out = App(App(App(Ref("Array.get", s), Ref("U32", s), s),
          out, s), ix, s);
      }
      continue;
    }
    if (lvl === 0 && parse_take(p, "=>")) {
      const n0 = p.stk.length;
      const x  = parse_bind(p, out);
      const f  = parse_block(p);
      parse_close(p, n0);
      out = Lam(x.k, x.i, f, x.s, x.q);
      continue;
    }
    let op = p.str.slice(p.pos, p.pos + 3);
    while (op !== "" && INFIX[op] === undefined) {
      op = op.slice(0, -1);
    }
    if (op === "") {
      return out;
    }
    const [prc, right, k] = INFIX[op];
    const nx = p.str[p.pos + op.length] ?? "";
    const gl = /\S/.test(p.str[p.pos - 1] ?? "");
    if ((prc < lvl && !(op === "<" && gl)) || ((op === "-" || op === "+") && (nx === ">" || char_is_head(nx)))
      || (op[0] === ">" && gl) || (op === "%" && !/\s/.test(nx))) {
      return out;
    }
    p.pos += op.length;
    const t = parse_span(p, p.pos - op.length);
    const b = parse_term(p, right ? prc : prc + 1);
    const s = parse_span(p, beg);
    parse_skip(p);
    // a glued < whose first operand stops at a type operator is a type
    // argument that needs parens, since a comparison never types there
    if (op === "<" && gl && /^(->|[&|](?![&|]))/.test(p.str.slice(p.pos, p.pos + 2))) {
      parse_fail(p, "'>' or ',' (a compound type argument takes parens: F<(A & B)>)");
    }
    if (op === "<" && (parse_at(p, ">") || parse_at(p, ","))) {
      if (out.$ !== "Var" && out.$ !== "Ref") {
        parse_fail(p, "a family name before <..> (a comparison here needs parens)");
      }
      parse_take(p, ",");
      const d = parse_reso(p, out.k);
      out = ADT(d, parse_fill(p, d, [b, ...parse_term_args(p, ">")], s), s);
    } else if (op === "->") {
      out = All(Lone(), "_", parse_open(p, "_"), out, b, s);
    } else if (op === "<&>") {
      out = Min(out, b, s);
    } else if (op === "<>") {
      out = Ctr(k, [out, b], s);
    } else if (op === "&" || op === "|") {
      out = App(App(Ref(k, s), out, s), b, s);
    } else {
      out = App(App(Ref(k, t), out, s), b, s);
    }
  }
}

export function parse_term_ns(p: Parse, tm: LTerm, T: LTerm): void {
  if (tm.$ === "Let") {
    return parse_term_ns(p, tm.f, T);
  }
  const [f, xs] = term_unapply(tm);
  if (f.$ !== "Ref") {
    return;
  }
  if (f.k.lastIndexOf(".") === 0) {
    const h = term_unapply(T)[0];
    if (h.$ !== "Var" && h.$ !== "Ref" && h.$ !== "ADT") {
      throw Err(p.book, ctx_nil(), "a type name after : (the operators' namespace)", undefined, h.s);
    }
    f.k = parse_reso(p, h.k + f.k);
  } else if (f.k !== "Bool.and" && f.k !== "Bool.or" && f.k !== "String.append") {
    return;
  }
  for (const x of xs) {
    parse_term_ns(p, x, T);
  }
}

export function parse_term_args(p: Parse, close: string): LTerm[] {
  const xs: LTerm[] = [];
  while (true) {
    parse_skip(p);
    if (parse_take(p, close)) {
      return xs;
    }
    const x = parse_term(p);
    xs.push(x);
    parse_skip(p);
    parse_take(p, ",");
  }
}

export function parse_term_all(p: Parse, exi: boolean): LTerm {
  const beg = p.pos;
  parse_bump(p);
  const q = exi ? Lone() : parse_quant(p);
  const k = parse_name(p);
  parse_eat(p, ":");
  const A = parse_term(p, 1);
  parse_eat(p, "->");
  const n0 = p.stk.length;
  const i  = parse_open(p, k);
  const B  = parse_term(p);
  parse_close(p, n0);
  if (exi) {
    const s = parse_span(p, beg);
    return App(App(Ref("Exists", s), A, s), Lam(k, i, B, s), s);
  }
  return All(q, k, i, A, B);
}

export function parse_term_tup(p: Parse, beg: number): LTerm {
  parse_skip(p);
  const b = parse_body(p, parse_col(p.str, p.pos) - 1);
  parse_skip(p);
  if (b.$ !== "Match" && b.$ !== "Local" && parse_take(p, ",")) {
    const rest = parse_term_tup(p, beg);
    return Ctr("Tuple", [b, rest], parse_span(p, beg));
  }
  const out = body_flatten(b, [], () => p.frs++);
  if (parse_take(p, ":")) {
    parse_term_ns(p, out, parse_term(p));
  }
  parse_eat(p, ")");
  return out;
}

const NUMBER = /(\d+)(n|\.\d+([eE][+-]?\d+)?)?/y;

export function parse_term_num(p: Parse): LTerm {
  const beg = p.pos;
  NUMBER.lastIndex = p.pos;
  const m = NUMBER.exec(p.str) as RegExpExecArray;
  const n = Number(m[1]);
  p.pos += m[0].length;
  if (m[2] !== undefined && m[2] !== "n") {
    const v = f32_round(m[0]);
    if (!isFinite(v)) {
      parse_fail(p, "a float literal with a finite f32 value (got " + m[0] + ")", beg);
    }
    return Lit("F32", f32_to_bits(v));
  }
  if (m[2] === undefined) {
    if (char_is_name(parse_peek(p))) {
      parse_fail(p, "a numeric literal (NUMBER is U32, NUMBER n is Nat)");
    }
    if (n > 0xffffffff) {
      parse_fail(p, "a u32 literal up to 4294967295 (got " + m[1] + ")", beg);
    }
    return Lit("U32", n);
  }
  if (n > 0xffffffff) {
    parse_fail(p, "a nat literal up to 4294967295n (got " + m[1] + "n)");
  }
  if (parse_take(p, "+")) {
    let out = parse_term(p);
    const spn = parse_span(p, beg);
    if (out.$ === "Lit" && out.k === "Nat" && n + out.v <= 0xffffffff) {
      return Lit("Nat", n + out.v, spn);
    }
    if (n > NAT_LITERAL_MAX) {
      return App(App(Ref("Nat.add", spn), Lit("Nat", n, spn), spn), out, spn);
    }
    for (let i = 0; i < n; i++) {
      out = Ctr("Succ", [out], spn);
    }
    return out;
  }
  if (char_is_name(parse_peek(p))) {
    parse_fail(p, "a nat literal (NUMBER n)");
  }
  return Lit("Nat", n);
}

export function parse_fill(p: Parse, k: Name, xs: LTerm[], s?: Span): LTerm[] {
  const tld = p.book.tlds[k];
  return tld?.$ !== "ADT" || xs.length + tld.g !== tld.n ? xs
    : Array.from({ length: tld.g }, (): LTerm => Qua(Lone(), s)).concat(xs);
}

export function parse_term_do_stmt(p: Parse, m: Name, ts: LTerm[], col: number): LTerm {
  parse_skip(p);
  const beg = p.pos;
  if (parse_word(p, "return")) {
    const e = parse_term(p);
    return parse_call(p, m + ".pure", [...ts, e], parse_span(p, beg));
  }
  const t = parse_term(p);
  parse_skip(p);
  const typed = t.$ === "Var" && parse_take(p, ":");
  const step  = !typed && parse_more(p, col);
  const A     = typed ? parse_term(p, 1) : step ? parse_var(p, "Unit", t.s) : t;
  parse_skip(p);
  const asg = typed && !parse_at(p, "==") && parse_take(p, "=");
  if (typed && !asg) {
    parse_eat(p, "<-");
  } else if (!typed && !step && !parse_take(p, "<-")) {
    const k = parse_reso(p, m);
    return Ann(t, p.book.tlds[k]?.$ === "ADT" ? ADT(k, ts, t.s)
      : parse_call(p, m, ts, t.s), t.s);
  }
  const v = step ? t : parse_term(p);
  parse_skip(p);
  parse_take(p, ";");
  const s  = parse_span(p, beg);
  const n0 = p.stk.length;
  const x  = parse_bind(p, typed ? t : Var("_", 0));
  const f  = parse_term_do_stmt(p, m, ts, col);
  parse_close(p, n0);
  if (asg) {
    return Let([x.k], [x.i], [Ann(v, A, s)], f, s, [x.q]);
  }
  return parse_call(p, m + ".bind", [...ts.slice(0, -1), A, ...ts.slice(-1), v, Lam(x.k, x.i, f, s, x.q)], s);
}

// Body
// ----

export function parse_body(p: Parse, col: number = 0): Body {
  parse_skip(p);
  const beg = p.pos;
  if (parse_word(p, "match")) {
    const es = parse_terms(p);
    parse_skip(p);
    const ccol = parse_col(p.str, p.pos);
    const rows: Rows = [];
    while (ccol > col && parse_at_word(p, "case") && parse_col(p.str, p.pos) >= ccol) {
      const rcol = parse_col(p.str, p.pos);
      parse_word(p, "case");
      parse_skip(p);
      const qbeg = p.pos;
      const qs = parse_terms(p);
      if (qs.length !== es.length) {
        parse_fail(p, String(es.length) + " patterns (one per scrutinee)", qbeg, p.pos - 1);
      }
      const n0 = p.stk.length;
      const pp = qs.map((q) => parse_patt(p, q));
      const f  = parse_body(p, rcol);
      parse_close(p, n0);
      rows.push({ p: pp, f });
    }
    return { $: "Match", e: es, r: rows, s: parse_span(p, beg) };
  }
  const q = parse_take(p, "-") ? None() : Lone();
  const vs: LTerm[] = [];
  let ts: LTerm[] = [q.$ === "None" ? Var(parse_name(p), 0, parse_span(p, beg)) : parse_term(p)];
  parse_skip(p);
  while (!parse_nl(p) && (char_is_head(parse_peek(p)) || q.$ === "Lone" && parse_at(p, "+")) && !KEYWORDS.has(p.str.slice(p.pos).match(/^[A-Za-z0-9_.]*/)?.[0] ?? "")) {
    ts.push(parse_term(p));
    parse_skip(p);
  }
  const at = p.pos;
  let T = ts.length === 1 && parse_take(p, ":") ? parse_term(p) : null;
  parse_skip(p);
  if (T !== null && !parse_at(p, "=")) {
    [p.pos, T] = [at, null];
  }
  if (T === null && q.$ === "Lone" && ts.length === 1 && !(parse_at(p, "=") && !parse_at(p, "=="))) {
    const w = term_write(ts[0], beg);
    if (w === null || !parse_more(p, parse_col(p.str, beg))) {
      return ts[0];
    }
    vs.push(ts[0]);
    ts = [w];
  } else {
    parse_eat(p, "=");
    T !== null && vs.push(Ann(parse_term(p), T, parse_span(p, beg)));
  }
  while (vs.length < ts.length) {
    vs.push(parse_term(p));
  }
  parse_skip(p);
  parse_take(p, ";");
  const n0 = p.stk.length;
  const ks = ts.map((x): Patt => {
    if ((ts.length > 1 || T !== null) && x.$ !== "Var") {
      throw Err(p.book, ctx_nil(), "a name (a parallel or typed let binds names; destructure in its body)", undefined, x.s);
    }
    return parse_patt(p, x);
  });
  const f = parse_body(p, col);
  parse_close(p, n0);
  return { $: "Local", k: ks, q, v: vs, f };
}

export function term_write(t: LTerm, beg: number): LTerm | null {
  const [h, xs] = term_unapply(t);
  if (h.$ === "Ref" && h.k === "Array.set" && xs.length === 4 && xs[1].$ === "Var" && xs[1].s?.beg === beg) {
    return xs[1];
  }
  return null;
}

export function parse_more(p: Parse, col: number): boolean {
  return parse_at(p, ";") || p.pos < p.str.length && parse_col(p.str, p.pos) === col;
}

export function parse_block(p: Parse): LTerm {
  parse_skip(p);
  const b = parse_body(p, parse_col(p.str, p.pos) - 1);
  return body_flatten(b, [], () => p.frs++);
}

export function parse_terms(p: Parse): LTerm[] {
  const xs: LTerm[] = [];
  while (true) {
    xs.push(parse_term(p));
    parse_skip(p);
    if (parse_take(p, ":")) {
      return xs;
    }
    parse_take(p, ",");
  }
}

// Tele
// ----

export function parse_tele(p: Parse, close: string, tk: Name[] = []): Array<[Quant, Name, number, LTerm, Span]> {
  const tele: Array<[Quant, Name, number, LTerm, Span]> = [];
  while (true) {
    parse_skip(p);
    if (parse_take(p, close)) {
      return tele;
    }
    if (close === ")" && parse_at(p, "~") && tk.length < tele.length) {
      parse_fail(p, "a plain binder (only leading binders take ~)");
    }
    const ct  = close === ")" && parse_take(p, "~");
    const q   = ct ? None() : parse_quant(p);
    const beg = p.pos;
    const k   = parse_name(p);
    if (close === "}" && tele.some((cell) => cell[1] === k)) {
      parse_fail(p, "a fresh field name (duplicate declaration: " + k + ")", p.pos - k.length);
    }
    const s   = parse_span(p, beg);
    parse_skip(p);
    const bare = q.$ === "Lone" && !parse_at(p, ":");
    if (!bare) {
      parse_eat(p, ":");
    }
    const T: LTerm = bare ? Qnt(s) : parse_term(p);
    if (ct) {
      tk.push(k);
    }
    tele.push([bare ? None() : q, k, parse_open(p, k), T, s]);
    parse_skip(p);
    parse_take(p, ",");
  }
}

// Book
// ----

export function parse_fresh(p: Parse, nm: Name, tab: Record<Name, unknown> = p.book.tlds, what = "a fresh name"): Name {
  const k = parse_qual(p, nm);
  const a = nm.includes(".") ? nm.slice(0, nm.indexOf(".")) : "";
  if (k in tab || nm in tab || a in p.al) {
    parse_fail(p, what + " (" + (a in p.al ? a + " is an import's alias" : "duplicate declaration: " + nm) + ")", p.pos - nm.length);
  }
  return k;
}

export function parse_def(p: Parse, u: Bool): void {
  const book = p.book;
  const nm  = parse_name(p);
  const q   = parse_reso(p, nm);
  const tld = book.tlds[q];
  const law = tld?.$ === "Def" && tld.v === null && tld.b !== true && !tld.i ? tld : undefined;
  const k   = law ? q : parse_fresh(p, nm);
  const un  = parse_take(p, "?") || u;
  parse_eat(p, "(");
  parse_skip(p);
  if (law && parse_at(p, "~")) {
    parse_fail(p, "a name");
  }
  const tk: Name[] = [];
  const tele = parse_tele(p, ")", tk);
  parse_skip(p);
  let def: Def;
  if (law) {
    if (tele.some((cell) => cell[3].$ !== "Qnt")) {
      parse_fail(p, "a name");
    }
    if (tele.length < law.x) {
      parse_fail(p, "a name for each ~ clause of the law (" + String(law.x) + ")");
    }
    def = law;
    def.n = tele.length;
  } else {
    if (!parse_take(p, "->")) {
      parse_fail(p, "'->' (a def with no return type fills a law; no law named " + nm + " is in scope)");
    }
    def = book.tlds[k] = { $: "Def", n: tele.length, x: tk.length, T: term_higher(tele_bind(tele, parse_term(p))), v: null, m: p.ns };
  }
  def.u ||= un;
  parse_eat(p, ":");
  if (parse_at_word(p, "import")) {
    if (def.x > 0) {
      parse_fail(p, "a body (a template is not foreign)");
    }
    def.i = [];
    while (parse_word(p, "import")) {
      parse_eat(p, "\"");
      let eff = "";
      while (parse_peek(p) !== "\"" && parse_peek(p) !== "") {
        eff += parse_bump(p);
      }
      parse_eat(p, "\"");
      if (!/\.(c|js)$/.test(eff)) {
        parse_fail(p, "a .c or .js path");
      }
      def.i.push(p.dir + eff);
    }
  } else {
    const vars = tele.map((cell): PVar => ({ $: "PVar", k: cell[1], i: cell[2], q: Lone(), s: cell[4] }));
    def.v = term_higher(term_lower(term_higher(body_flatten(parse_body(p), vars, () => p.frs++))));
  }
  book.order.push(k);
}

export function parse_book(book: Book, dir: string, src: string, ns: string, al: Record<Name, Name>): void {
  const p: Parse = { book, dir, str: src, pos: 0, stk: [], frs: 0, ns, al };
  while (true) {
    parse_skip(p);
    if (p.pos >= p.str.length) {
      return;
    }
    p.frs = 0;
    parse_close(p, 0);
    if (parse_take(p, "@")) {
      if (!parse_word(p, "unsafe")) {
        parse_fail(p, "'unsafe' (the one decorator)");
      }
      if (!parse_word(p, "def")) {
        parse_fail(p, "'def' (@unsafe marks the def below it)");
      }
      parse_def(p, true);
      continue;
    }
    if (parse_word(p, "def")) {
      parse_def(p, false);
      continue;
    }
    if (parse_word(p, "type")) {
      const k = parse_fresh(p, parse_name(p));
      parse_skip(p);
      const params = parse_take(p, "<") ? parse_tele(p, ">") : [];
      if (!parse_word(p, "is")) {
        parse_fail(p, "'is'");
      }
      const K = parse_term(p);
      parse_eat(p, ":");
      const cs: Ctrs = [];
      const g  = params.findIndex((cell) => cell[3].$ !== "Qnt");
      book.tlds[k] = { $: "ADT", n: params.length, g: g < 0 ? params.length : g, T: term_higher(tele_bind(params, K)), c: cs };
      while (true) {
        parse_skip(p);
        if (!char_is_head(parse_peek(p)) || ["def", "type", "law"].some((w) => parse_at_word(p, w))) {
          break;
        }
        const c = parse_fresh(p, parse_name(p), book.ctrs, "a fresh constructor name");
        parse_eat(p, "{");
        const n1 = p.stk.length;
        const fs = parse_tele(p, "}");
        const tip: LTerm = ADT(k, params.map((cell) => Var(cell[1], cell[2])));
        const ctr = { k: c, n: fs.length, T: term_higher(tele_bind(params.concat(fs), tip)) };
        parse_close(p, n1);
        cs.push(ctr);
        book.ctrs[c] = ctr;
      }
      book.order.push(k);
      continue;
    }
    if (parse_word(p, "law")) {
      const k = parse_fresh(p, parse_name(p));
      parse_eat(p, ":");
      const cls: Array<[Bool, Quant, Name, number, LTerm, Span]> = [];
      let tc = 0;
      while (parse_at_word(p, "for") || parse_at_word(p, "exs")) {
        const all = parse_word(p, "for");
        if (!all) {
          parse_word(p, "exs");
        }
        parse_skip(p);
        if (all && parse_at(p, "~") && tc < cls.length) {
          parse_fail(p, "a plain clause (only leading clauses take ~)");
        }
        const ct = all && parse_take(p, "~");
        const q  = ct ? None() : all ? parse_quant(p) : Lone();
        const beg = p.pos;
        const c = parse_name(p);
        const s = parse_span(p, beg);
        if (ct) {
          tc += 1;
        }
        parse_eat(p, ":");
        let A = parse_term(p);
        if (parse_at_word(p, "where")) {
          const beg = p.pos;
          parse_word(p, "where");
          const ws = parse_span(p, beg);
          const n1 = p.stk.length;
          const i  = parse_open(p, c);
          const w  = parse_term(p);
          parse_close(p, n1);
          A = App(App(Ref("Exists", ws), A, s), Lam(c, i, w, s), s);
        }
        cls.push([all, q, c, parse_open(p, c), A, s]);
      }
      const T = cls.reduceRight((T, [all, q, c, i, A, s]) =>
        all ? All(q, c, i, A, T, s) : App(App(Ref("Exists", s), A, s), Lam(c, i, T, s), s), parse_block(p));
      const n = cls.findIndex((c) => !c[0]);
      book.tlds[k] = { $: "Def", n: n < 0 ? cls.length : n, T: term_higher(T), v: null, x: tc, m: p.ns };
      book.order.push(k);
      continue;
    }
    parse_fail(p, "'def', 'type' or 'law'");
  }
}

// Flatten
// =======
// https://gist.github.com/VictorTaelin/8b00f91d11c6f4ee5714de8adaf02fe6
// match_flatten compiles nested ctr/var patterns into a tree of lambda-
// matches and binders: first row wins, uncovered cases become \{}; guards,
// or/as and column heuristics are out by design. binders are
// identities and ctrs structural, so substitution cannot capture; a ctr
// row makes its column strict even under an earlier catch-all. scus (the
// vars, duplicate-free, in order, one pattern each) is the precondition.
// a match scrutinizes a parameter or a bound field, nothing else: a
// lambda-match applied to a value is a banned form, so a computed or
// let-bound scrutinee is an error here - give it its own def, where it
// is a parameter. a ctr column decides at its binder's own position
// (binder order: the binders before it close as lambdas first); a var
// column is a substitution, binding any pending binder and leaving it
// open, so a field bound by a case or a destructure is matchable
// later. a parallel let becomes one Let node binding its names to its
// values. the + marks of a column's rows join into its binder (a +
// lambda), or into its fields when a constructor row splits it.

export function body_sub(b: Body, i: number, v: Patt): Body {
  function scrut(e: LTerm): LTerm {
    if (e.$ === "Var") {
      return e.i !== i ? e : patt_term(v, e.s);
    } else {
      return Sub(i, v, e);
    }
  }
  switch (b.$) {
    case "Match": {
      const es = b.e.map(scrut);
      const rs = b.r.map((row): Case => ({ p: row.p, f: body_sub(row.f, i, v) }));
      return { $: "Match", e: es, r: rs, s: b.s };
    }
    case "Local": {
      const w = b.v.map(scrut);
      const f = body_sub(b.f, i, v);
      return { $: "Local", k: b.k, q: b.q, v: w, f };
    }
    default: {
      return Sub(i, v, b);
    }
  }
}

export function match_flatten(m: Match, vars: PVar[], fr: () => number): LTerm {
  if (m.e.length === 0 && m.r.length > 0) {
    return body_flatten(m.r[0].f, vars, fr);
  } else if (m.e.length === 0) {
    throw Err(book_nil(), ctx_nil(), "a case (this match has no row to return)", undefined, m.s);
  } else if (vars.length === 0) {
    let e = m.e[0];
    while (e.$ === "Sub") {
      e = e.f;
    }
    switch (e.$) {
      case "Var": {
        const x = e.s === undefined ? e.k : e.s.file.str.slice(e.s.beg, e.s.end);
        throw Err(book_nil(), ctx_nil(), "'" + x + "' can't be matched in this position"
          + " (it is matched after a local statement or after a match on a later binder,"
          + " it was already matched, or it is a def)", undefined, e.s);
      }
      case "Ctr":
      case "Lit": {
        const x = e.s === undefined ? e.k : e.s.file.str.slice(e.s.beg, e.s.end);
        if (char_is_head(x) && [...x].every(char_is_name)) {
          throw Err(book_nil(), ctx_nil(), "'" + x + "' can't be matched here"
            + " (match it in the same match as the pattern that introduced it)", undefined, m.s);
        } else {
          throw Err(book_nil(), ctx_nil(), "'" + x + "' can't be matched"
            + " (this value is already a constructor: bind its fields directly)", undefined, m.s);
        }
      }
      default: {
        throw Err(book_nil(), ctx_nil(), "a parameter or field scrutinee (a match cannot scrutinize a computed value: give it its own def)", undefined, e.s ?? m.s);
      }
    }
  } else {
    const x   = vars[0];
    const scu = m.e[0];
    const c   = m.r.map((row) => row.p[0]).find((q): q is PCtr => q.$ === "PCtr") ?? null;
    const v   = vars.find((w) => scu.$ === "Var" && w.i === scu.i);
    if (v !== undefined && c === null && m.r.length > 0) {
      const w  = { ...v, q: patt_mark(v, m.r) };
      const rs = m.r.map((row): Case => {
        const p0 = row.p[0];
        if (p0.$ !== "PVar") {
          throw Err(book_nil(), ctx_nil(), "a variable pattern (this column has no constructor row)", undefined, p0.s);
        }
        return { p: row.p.slice(1), f: body_sub(row.f, p0.i, w) };
      });
      return match_flatten({ $: "Match", e: m.e.slice(1), r: rs, s: m.s }, vars.map((u) => u === v ? w : u), fr);
    } else if (v === x) {
      if (c === null) {
        return Efq(m.s);
      } else {
        const xq = patt_mark(x, m.r);
        const xs = c.x.map((q): PVar => {
          if (q.$ === "PVar") {
            return { ...q, q: quant_join(q.q, xq) };
          }
          const i = fr();
          return { $: "PVar", k: "_" + String(i), i, q: xq, s: q.s };
        });
        const kx: Patt = { $: "PCtr", k: c.k, x: xs, s: x.s };
        const ps = m.r.flatMap((row): Rows => {
          const p0 = row.p[0];
          switch (p0.$) {
            case "PCtr": {
              if (p0.k !== c.k) {
                return [];
              }
              const f = body_sub(row.f, x.i, kx);
              return [{ p: [...p0.x, ...row.p.slice(1)], f }];
            }
            case "PVar": {
              const g = body_sub(row.f, p0.i, x);
              const f = body_sub(g, x.i, kx);
              return [{ p: [...xs, ...row.p.slice(1)], f }];
            }
          }
        });
        const pe = xs.map((q) => patt_term(q)).concat(m.e.slice(1));
        const pv = xs.concat(vars.slice(1));
        const pt = match_flatten({ $: "Match", e: pe, r: ps, s: m.s }, pv, fr);
        const ds = m.r.filter((row) => row.p[0].$ !== "PCtr" || row.p[0].k !== c.k);
        const dt = match_flatten({ $: "Match", e: m.e, r: ds, s: m.s }, vars, fr);
        return Mat(c.k, pt, dt, c.s);
      }
    } else {
      const t = match_flatten(m, vars.slice(1), fr);
      return Lam(x.k, x.i, t, x.s, x.q);
    }
  }
}

export function patt_mark(x: PVar, rows: Rows): Quant {
  return rows.map((row) => row.p[0]).reduce((q, p) => p.$ === "PVar" ? quant_join(q, p.q) : q, x.q);
}

export function patt_term(q: Patt, s?: Span): LTerm {
  switch (q.$) {
    case "PVar": {
      return Var(q.k, q.i, s ?? q.s);
    }
    case "PCtr": {
      const xs = q.x.map((x) => patt_term(x, s));
      return Ctr(q.k, xs, s ?? q.s);
    }
  }
}

export function body_flatten(b: Body, vars: PVar[], fr: () => number): LTerm {
  switch (b.$) {
    case "Local": {
      if (b.k.length === 1 && b.k[0].$ === "PCtr") {
        const r: Case = { p: [b.k[0]], f: b.f };
        return match_flatten({ $: "Match", e: [b.v[0]], r: [r], s: b.v[0].s }, vars, fr);
      }
      const ws = b.k as PVar[];
      let g = body_flatten(b.f, ws, fr);
      for (const w of ws) {
        if (g.$ !== "Lam") {
          throw Err(book_nil(), ctx_nil(), "a parameter or field scrutinee (a match cannot scrutinize a local binder: give it its own def)", undefined, w.s);
        }
        g = g.f;
      }
      const x = Let(ws.map((w) => w.k), ws.map((w) => w.i), b.v, g, ws[0].s, ws.map((w) => quant_dem(b.q, w.q)));
      return body_flatten(x, vars, fr);
    }
    case "Match": {
      return match_flatten(b, vars, fr);
    }
    default: {
      if (vars.length === 0) {
        return b;
      } else {
        const v = vars[0];
        const f = body_flatten(b, vars.slice(1), fr);
        return Lam(v.k, v.i, f, v.s, v.q);
      }
    }
  }
}

// WNF
// ===
// term_wnf gives weak head normal form: sound, weak (arguments, fields,
// arms raw), idempotent, partial exactly on terms with no whnf. a
// saturated def unfolds raw, a stuck one returns its ref applied, an
// underapplied one stays. a family Ref never unfolds: a nullary one
// steps to its canonical ADT node, a parameterized one is stuck (the
// one spelling is D<..>, and infer-ref rejects a bare family head).
// a term entering a spine (an argument, a let value, a demanded match
// field) becomes a share Var: index -1, its value the raw term; the
// machine overwrites the value with its whnf, restores the term's Ann,
// and marks the var -2, so shared work runs once and a type survives
// the fill; a forcer outside the machine calls term_wnf on the var; a
// var steps into its value; tree nodes inside a leaf are plain values.
// a cell lives in the machine's frames and output, NEVER in an input
// node: the machine does not write into the term it was given, so the
// checker walks and counts unevaluated source syntax, and a cell it
// meets (in a goal computed by evaluation) opens by term_force alone.
// a rewrite demands its evidence and steps to its body on {==}, else
// sticks as a value.

export function term_wnf(book: Book, term: HTerm): HTerm {
  const frs: Frame[] = [];
  let tm: HTerm = term;
  let lhs: { t: () => HTerm; n: number } | null = null;
  main: while (true) {
    focus: switch (tm.$) {
      case "Var": {
        if (tm.v === undefined) {
          break focus;
        } else {
          if (tm.i === -1) {
            frs.push({ $: "VAR", l: tm, a: tm.v.$ === "Ann" ? tm.v : undefined });
          }
          lhs = null;
          tm = tm.v;
          continue main;
        }
      }
      case "Ann": {
        tm = tm.x;
        continue main;
      }
      case "Min": {
        frs.push({ $: "MNA", b: tm.b, s: tm.s });
        tm = tm.a;
        continue main;
      }
      case "Let": {
        const l = tm;
        tm = l.f(l.v.map((v, j) => term_cell(v, l.k[j])));
        continue main;
      }
      case "App": {
        frs.push({ $: "APP", x: term_cell(tm.x), s: tm.s });
        lhs = null;
        tm = tm.f;
        continue main;
      }
      case "Lam": {
        if (frs.length === 0 || frs[frs.length - 1].$ !== "APP") {
          break focus;
        } else {
          const fr = frs.pop() as Extract<Frame, { $: "APP" }>;
          if (lhs !== null && lhs.n === 0) {
            lhs = null;
          } else if (lhs !== null) {
            const pt: () => HTerm = lhs.t;
            const pn: number = lhs.n;
            lhs = { t: () => term_apply(pt(), fr.x, fr.s), n: pn - 1 };
          }
          tm = tm.f(fr.x);
          continue main;
        }
      }
      case "Mat": {
        if (frs.length === 0 || frs[frs.length - 1].$ !== "APP") {
          break focus;
        } else {
          const fr = frs.pop() as Extract<Frame, { $: "APP" }>;
          frs.push({ $: "MAT", t: tm, e: fr.x, lhs, s: fr.s });
          tm = fr.x;
          lhs = null;
          continue main;
        }
      }
      case "Efq": {
        if (lhs !== null && lhs.n > 0 && frs.length > 0 && frs[frs.length - 1].$ === "APP") {
          tm = lhs.t();
        }
        break focus;
      }
      case "Rwt": {
        const e = term_wnf(book, tm.e);
        if (e.$ === "Rfl") {
          tm = tm.f;
          continue main;
        }
        break focus;
      }
      case "Ref": {
        const tld = book.tlds[tm.k];
        if (tld === undefined) {
          break focus;
        }
        if (tld.$ === "ADT") {
          if (tld.n === 0) {
            tm = ADT(tm.k, [], tm.s);
          }
          break focus;
        }
        let run = 0;
        while (run < tld.n && run < frs.length && frs[frs.length - 1 - run].$ === "APP") {
          run += 1;
        }
        if (run < tld.n || tld.v === null) {
          break focus;
        }
        const rf: HTerm = tm;
        lhs = { t: () => rf, n: tld.n };
        tm = tld.v;
        continue main;
      }
      default: {
        break focus;
      }
    }
    lhs = null;
    back: while (true) {
      const fr = frs.pop();
      if (fr === undefined) {
        return tm;
      } else {
        switch (fr.$) {
          case "VAR": {
            switch (tm.$) {
              case "Ctr": tm = Ctr(tm.k, tm.x.map((x) => term_cell(x)), tm.s); break;
              case "ADT": tm = ADT(tm.k, tm.x.map((x) => term_cell(x)), tm.s, tm.r); break;
              case "All": tm = All(tm.q, tm.k, tm.i, term_cell(tm.A), tm.B, tm.s); break;
              case "Mat": tm = Mat(tm.k, term_cell(tm.h), term_cell(tm.m), tm.s); break;
              case "Eql": tm = Eql(term_cell(tm.a), term_cell(tm.b), term_cell(tm.T), tm.s); break;
              case "Min": tm = Min(term_cell(tm.a), term_cell(tm.b), tm.s); break;
              case "Typ": tm = Typ(term_cell(tm.g), tm.s); break;
              default: break;
            }
            fr.l.v = fr.a === undefined ? tm
              : Ann(tm, term_cell(fr.a.T), fr.a.s);
            fr.l.i = -2;
            continue main;
          }
          case "APP": {
            tm = term_apply(tm, fr.x, fr.s);
            continue back;
          }
          case "MNA": {
            if (tm.$ === "Qua" && tm.q.$ === "Many") {
              tm = fr.b;
              continue main;
            }
            if (tm.$ === "Qua" && tm.q.$ === "None") {
              continue back;
            }
            frs.push({ $: "MNB", a: tm, s: fr.s });
            tm = fr.b;
            continue main;
          }
          case "MNB": {
            if (tm.$ === "Qua" && tm.q.$ === "Many") {
              tm = fr.a;
            } else if (tm.$ !== "Qua" || (tm.q.$ === "Lone" && fr.a.$ !== "Qua")) {
              tm = Min(fr.a, tm, fr.s);
            }
            continue back;
          }
          case "MAT": {
            if (tm.$ === "Lit") {
              tm = lit_step(tm);
            }
            if (tm.$ === "Ctr") {
              const ctr = tm;
              let t: HTerm = fr.t;
              walk: while (true) {
                switch (t.$) {
                  case "Ann": {
                    t = t.x;
                    continue walk;
                  }
                  case "Mat": {
                    if (t.k === ctr.k) {
                      const fl = fr.lhs;
                      if (fl === null) {
                        lhs = null;
                      } else {
                        lhs = { t: () => lhs_ext(fl.t(), ctr.k, ctr.x.length), n: fl.n - 1 + ctr.x.length };
                      }
                      for (let j = ctr.x.length - 1; j >= 0; j--) {
                        frs.push({ $: "APP", x: term_cell(ctr.x[j]) });
                      }
                      tm = t.h;
                      continue main;
                    } else {
                      t = t.m;
                      continue walk;
                    }
                  }
                  case "Efq": {
                    break walk;
                  }
                  default: {
                    lhs = fr.lhs;
                    frs.push({ $: "APP", x: ctr });
                    tm = t;
                    continue main;
                  }
                }
              }
            }
            tm = term_apply(fr.lhs === null ? fr.t : fr.lhs.t(), fr.e, fr.s);
            continue back;
          }
        }
      }
    }
  }
}

// SNF
// ===

export function term_snf(book: Book, term: HTerm): HTerm {
  const tm = term_wnf(book, term);
  switch (tm.$) {
    case "Typ": {
      return Typ(term_snf(book, tm.g), tm.s);
    }
    case "Min": {
      return Min(term_snf(book, tm.a), term_snf(book, tm.b), tm.s);
    }
    case "All": {
      return All(tm.q, tm.k, tm.i, term_snf(book, tm.A), (x: HTerm) => {
        return term_snf(book, tm.B(x));
      }, tm.s);
    }
    case "Lam": {
      return Lam(tm.k, tm.i, (x: HTerm) => {
        return term_snf(book, tm.f(x));
      }, tm.s);
    }
    case "App": {
      return App(tm.f.$ === "Ref" ? tm.f : term_snf(book, tm.f), term_snf(book, tm.x), tm.s);
    }
    case "ADT": {
      return ADT(tm.k, tm.x.map((x) => term_snf(book, x)), tm.s, tm.r);
    }
    case "Ctr": {
      return Ctr(tm.k, tm.x.map((x) => term_snf(book, x)), tm.s);
    }
    case "Mat": {
      return Mat(tm.k, term_snf(book, tm.h), term_snf(book, tm.m), tm.s);
    }
    case "Eql": {
      return Eql(term_snf(book, tm.a), term_snf(book, tm.b), term_snf(book, tm.T), tm.s);
    }
    case "Rwt": {
      return Rwt(term_snf(book, tm.e), term_snf(book, tm.p), term_snf(book, tm.f), tm.s);
    }
    default: {
      return tm;
    }
  }
}

// Compare
// =======
// term_compare(mode, a, b): a fits b. the relation is a preorder, not an
// equality: mode LE is conversion (check-any's goal meet), directional
// only at kinds (the quantity order) and ADT residuals (removing more
// constructors fits removing fewer); a function type compares its
// domains SWAPPED (the slot's owner picks the argument, so the wanted
// domain must fit the given one) and its codomain along; every part
// that flows both ways -- an argument, a parameter, a field, an
// equation endpoint -- compares EQ, and EQ stays EQ all the way down,
// so no position is ever visited twice. mode EQ is the symmetric core:
// same walk, kinds exact, no swap (a swap under EQ is harmless, so
// the All case swaps unconditionally).

const RIGID: Book = book_nil();

// two copies of one term are equal: a conversion first compares both
// sides with every def rigid (the empty book unfolds none), then as usual
export function term_compare(mode: "EQ" | "LE", book: Book, lhs: HTerm, rhs: HTerm, dep: number = 0): boolean {
  return compare_go(mode, RIGID, lhs, rhs, dep) || compare_go(mode, book, lhs, rhs, dep);
}

function compare_go(mode: "EQ" | "LE", book: Book, lhs: HTerm, rhs: HTerm, dep: number): boolean {
  if (lhs === rhs) {
    return true;
  }
  let a = term_wnf(book, lhs);
  let b = term_wnf(book, rhs);
  if (a === b) {
    return true;
  }
  // two share cells found equal become one: rhs points at lhs, so a
  // shared graph is compared once, not walked as a tree
  if (mode === "EQ" && lhs.$ === "Var" && lhs.i === -2 && rhs.$ === "Var" && rhs.i === -2) {
    const same = compare_go(mode, book, a, b, dep);
    if (same) {
      rhs.v = lhs.v;
    }
    return same;
  }
  if (a.$ === "Lam" || b.$ === "Lam") {
    const x: HTerm = Var("_", dep);
    return compare_go(mode, book, term_apply(a, x), term_apply(b, x), dep + 1);
  }
  if (a.$ === "Lit" && b.$ === "Ctr") {
    a = lit_step(a);
  }
  if (b.$ === "Lit" && a.$ === "Ctr") {
    b = lit_step(b);
  }
  switch (a.$) {
    case "Var": {
      return b.$ === "Var" && a.i === b.i;
    }
    // a bare def is under-applied, so a function: two defs convert when
    // they agree on fresh arguments (eta, as for a lambda)
    case "Ref": {
      if (b.$ !== "Ref" || a.k === b.k) {
        return b.$ === "Ref";
      }
      const n = Math.max(book.tlds[a.k]?.n ?? 0, book.tlds[b.k]?.n ?? 0);
      let [f, g]: HTerm[] = [a, b];
      for (let j = 0; j < n; j++) {
        [f, g] = [App(f, Var("_", dep + j)), App(g, Var("_", dep + j))];
      }
      return n > 0 && compare_go(mode, book, f, g, dep + n);
    }
    case "Typ": {
      if (b.$ !== "Typ") {
        return false;
      }
      if (mode === "EQ") {
        return compare_go("EQ", book, a.g, b.g, dep);
      }
      const g = term_wnf(book, a.g);
      const h = term_wnf(book, b.g);
      if ((g.$ === "Qua" && g.q.$ === "Many") || (h.$ === "Qua" && h.q.$ !== "Many")) {
        return true;
      }
      if (g.$ === "Min") {
        const fa = compare_go("LE", book, Typ(g.a), b, dep);
        const fb = compare_go("LE", book, Typ(g.b), b, dep);
        return fa && fb;
      }
      if (h.$ === "Min") {
        const fa = compare_go("LE", book, a, Typ(h.a), dep);
        const fb = compare_go("LE", book, a, Typ(h.b), dep);
        return fa || fb;
      }
      return compare_go("LE", book, g, h, dep);
    }
    case "Qnt":
    case "Efq":
    case "Rfl": {
      return b.$ === a.$;
    }
    case "Qua": {
      return b.$ === "Qua" && a.q.$ === b.q.$;
    }
    case "Min": {
      return b.$ === "Min"
          && compare_go("EQ", book, a.a, b.a, dep)
          && compare_go("EQ", book, a.b, b.b, dep);
    }
    case "All": {
      const x: HTerm = Var(a.k, dep);
      return b.$ === "All" && a.q.$ === b.q.$
          && compare_go(mode, book, b.A, a.A, dep)
          && compare_go(mode, book, a.B(x), b.B(x), dep + 1);
    }
    // a stuck call is canonical: its head def compares by name, not by eta
    case "App": {
      if (b.$ !== "App") {
        return false;
      }
      const head = a.f.$ === "Ref" && b.f.$ === "Ref" ? a.f.k === b.f.k
        : compare_go("EQ", book, a.f, b.f, dep);
      return head && compare_go("EQ", book, a.x, b.x, dep);
    }
    case "ADT": {
      if (b.$ !== "ADT" || a.k !== b.k || a.x.length !== b.x.length) {
        return false;
      }
      if (mode === "EQ" && a.r.length !== b.r.length) {
        return false;
      }
      return b.r.every((c) => a.r.includes(c))
          && a.x.every((x, j) => compare_go("EQ", book, x, b.x[j], dep));
    }
    case "Ctr": {
      return b.$ === "Ctr" && a.k === b.k && a.x.length === b.x.length
          && a.x.every((x, j) => compare_go("EQ", book, x, b.x[j], dep));
    }
    case "Lit": {
      return b.$ === "Lit" && a.k === b.k && a.v === b.v;
    }
    case "Mat": {
      return b.$ === "Mat" && a.k === b.k
          && compare_go("EQ", book, a.h, b.h, dep)
          && compare_go("EQ", book, a.m, b.m, dep);
    }
    case "Eql": {
      return b.$ === "Eql"
          && compare_go("EQ", book, a.a, b.a, dep)
          && compare_go("EQ", book, a.b, b.b, dep)
          && compare_go("EQ", book, a.T, b.T, dep);
    }
    case "Hol": {
      return b.$ === "Hol" && a.k === b.k;
    }
    case "Rwt": {
      return b.$ === "Rwt"
          && compare_go("EQ", book, a.e, b.e, dep)
          && compare_go("EQ", book, a.p, b.p, dep)
          && compare_go("EQ", book, a.f, b.f, dep);
    }
    default: {
      return false;
    }
  }
}

// Check
// =====
// one bidirectional pass. qt is the demand: None checks dead, Lone live;
// there is no third demand, since nothing licenses a second live use. us
// is the measured usage: a binder validates measured <= declared on close,
// and a Lone binder measured Many (consumed more than once) fails there.
// a dead check measures only None, so a rule drops the measure of a
// premise it itself checks dead; only demanded premises add, once,
// unscaled (certify-once: a + position copies the value it receives).
// ordinary typing never consults the usage measure: resource accounting
// rides alongside the type judgment, it does not steer it. a checked term
// returns first-order: an LTerm built bottom-up, every node Ann-wrapped
// with its type in a share cell (Infer, Check), so no type is lowered
// and no closure survives; a consumer that needs HOAS calls
// term_higher, which passes a cell through; goals always compute on
// source terms. lhs is the def's
// own equation, rebuilt as the tree walks; lhs.qs holds the def's
// parameter quantities, read off its type once, so descent can skip
// erased columns. quantities: a binder q x: A checks A against Kind(q),
// so its type's quantity is at least q under term_compare's order. a
// template (Def.x) checks once, each ~ binder an opaque bodiless constant
// of its domain, so a mention costs no usage and the body holds for every
// closed instance. a call's ~ arguments check dead in the empty context;
// only infer-var observes a variable, so a miss that observes one of Γ
// names it as open. term_key of the arguments names the instance, and
// the first call mints it and checks it as a def one level deeper.

export function term_descend(q: Quant, arg: HTerm, col: HTerm): Cmp {
  if (q.$ === "None") {
    return "EQ";
  }
  const s = term_strip(arg);
  const p = term_strip(col);
  const a: HTerm = s.$ === "Lit" && p.$ === "Ctr" ? lit_step(s) : s;
  switch (p.$) {
    case "Var": {
      if (a.$ === "Var" && a.i === p.i) {
        return "EQ";
      } else {
        return "GT";
      }
    }
    case "Ctr": {
      let bad = -1;
      if (a.$ === "Ctr" && a.k === p.k && a.x.length === p.x.length) {
        let ord: Cmp = "EQ";
        for (let j = 0; j < a.x.length && bad < 0; j++) {
          const fld = term_descend(Lone(), a.x[j], p.x[j]);
          ord = fld === "EQ" ? ord : fld;
          bad = fld === "GT" ? j : bad;
        }
        if (bad < 0) {
          return ord;
        }
      }
      for (let j = 0; j < p.x.length; j++) {
        if (j !== bad && term_descend(Lone(), a, p.x[j]) !== "GT") {
          return "LT";
        }
      }
      return "GT";
    }
    default: {
      return "GT";
    }
  }
}

export function term_infer(book: Book, lhs: LHS, tm: HTerm, qt: Quant, ctx: Ctx, d: number, sp: HTerm[] = []): Infer {
  switch (tm.$) {
    // Γ[x] = q A
    // where x is bound in Γ (a ~ argument checks in the empty context,
    //       so a variable of the caller's is unbound there)
    // ----------------- infer-var
    // Γ ⊢ x : A ~ {x:q}
    case "Var": {
      if (tm.i < 0 && tm.v !== undefined) {
        return term_infer(book, lhs, term_force(tm), qt, ctx, d, sp);
      }
      const ann = pmap_get(ctx, tm.i);
      if (ann === null) {
        throw Err(book, ctx, "a bound variable", tm, tm.s, lhs.def);
      } else {
        return Infer(Var(tm.k, tm.i, tm.s), ann.T, pmap_set(uses_nil(), tm.i, qt));
      }
    }
    // Book(k) : T
    // where a live k that is the lhs head descends: its pending
    //       arguments sp (the spine above it) compare EQ against the
    //       lhs columns left to right until one is LT; an erased (-)
    //       column is skipped; a bare or non-shrinking self-reference
    //       is an error, so a self-reference never escapes as a value
    //       k has a body in a live region, unless base declared it, it
    //       is a template's ~ parameter, or the def is @unsafe (an
    //       unfilled law is a dead claim; base's are native, a ~
    //       parameter an opaque constant, and an unsafe body trusts it)
    //       a template k in a live region outside a template's own text
    //       takes its x ~ arguments here: the call is k~n, the instance
    //       at them (def_inst), and the x applications above pass
    //       (infer-app)
    //       k is not a parameterized family: D<..> is the one
    //       spelling, a bare family head is an error
    // -------------------------------------------------------- infer-ref
    // Γ ⊢ k : T ~ {}
    case "Ref": {
      const tld = book.tlds[tm.k];
      if (tld === undefined) {
        throw Err(book, ctx, "a defined name", tm, tm.s, lhs.def);
      }
      let k = tm.k;
      let x = 0;
      if (qt.$ !== "None") {
        const gen = tld.$ === "Def" && tld.x > 0 && !(book.tlds[lhs.def] as Def).x ? tld : null;
        if (tld.$ === "Def" && tld.v === null && !tld.i && (tld.b !== true && lhs.u !== true || gen !== null) && k !== lhs.def) {
          throw Err(book, ctx, "a filled definition (an unfilled law is a dead claim: live code cannot use it)", tm, tm.s, lhs.def);
        }
        if (gen !== null) {
          k  = def_inst(book, lhs, tm, gen, sp, ctx, d);
          x  = gen.x;
          sp = sp.slice(x);
        }
        if (k === lhs.def && lhs.u !== true) {
          const cols = term_unapply(lhs.t)[1];
          let ord: Cmp = "EQ";
          for (let j = 0; j < cols.length && j < sp.length && ord === "EQ"; j++) {
            ord = term_descend(lhs.qs[j], sp[j], cols[j]);
          }
          if (ord !== "LT") {
            throw Err(book, ctx, "a decreasing self-call (arguments are read left to right: each passed unchanged until one shrinks)", tm, tm.s, lhs.def);
          }
        }
      }
      if (tld.$ === "ADT" && tld.n > 0) {
        throw Err(book, ctx, "a family instance (write " + name_key(tm.k) + "<..>)", tm, tm.s, lhs.def);
      }
      return Infer(Ref(k, tm.s, tm.b), book.tlds[k].T, uses_nil(), x);
    }
    // Γ ⊢ q : Quant
    // where q is dead
    // ------------------- infer-typ
    // Γ ⊢ Kind(q) : Type
    case "Typ": {
      const g_chk = term_check(book, lhs, tm.g, None(), Qnt(tm.s), ctx, d);
      return Infer(Typ(g_chk.tm, tm.s), Typ(Qua(Lone()), tm.s), uses_nil());
    }
    // ∅
    // ------------------------------------ infer-qnt
    // Γ ⊢ Quant : Type    Γ ⊢ &1, &2 : Quant
    case "Qnt": {
      return Infer(tm, Typ(Qua(Lone()), tm.s), uses_nil());
    }
    case "Qua": {
      return Infer(tm, Qnt(tm.s), uses_nil());
    }
    // Γ ⊢ a : Quant ~ au    Γ ⊢ b : Quant ~ bu
    // ------------------------------------- infer-min
    // Γ ⊢ a <&> b : Quant ~ au + bu
    case "Min": {
      const a_chk = term_check(book, lhs, tm.a, qt, Qnt(tm.s), ctx, d);
      const b_chk = term_check(book, lhs, tm.b, qt, Qnt(tm.s), ctx, d);
      return Infer(Min(a_chk.tm, b_chk.tm, tm.s), Qnt(tm.s), uses_add(a_chk.us, b_chk.us));
    }
    // Γ ⊢ A : Kind(q)
    // Γ , x : qA ⊢ B(x) : Type
    // ----------------------------------------------- infer-all
    // Γ ⊢ @q x:A -> B : Type
    case "All": {
      const B_ctx = ctx_bind(ctx, d, tm.q, tm.k, tm.A);
      const A_chk = term_check_kind(book, lhs, tm.A, tm.q, tm.k, ctx, d, tm.A.s ?? tm.s);
      const B_chk = term_check(book, lhs, tm.B(Var(tm.k, d)), None(), Typ(Qua(Lone()), tm.s), B_ctx, d+1);
      return Infer(All(tm.q, tm.k, d, A_chk.tm, B_chk.tm, tm.s), Typ(Qua(Lone()), tm.s), uses_nil());
    }
    // Γ ⊢ f : @q x:A -> B ~ fu
    // Γ ⊢ a : A ~ au
    // where a is dead if q is -, and consumed once otherwise: its
    //       measure adds unscaled (certify-once), a + callee copies it
    //       f infers with a on its pending spine, for infer-ref's descent
    //       a family head is not a function: infer-ref rejects it,
    //       so D(x) is an error and D<x> the one spelling
    //       (x => f)(a) never arrives: term_higher builds it as f(a) (a
    //       substituted lambda, a Sigma field type B(fst) say, makes one)
    //       a template head took a as a ~ argument (infer-ref, x of
    //       them): the application passes, its instance stands for both
    // --------------------------------------------------------------- infer-app
    // Γ ⊢ f(a) : B(a) ~ fu + au
    case "App": {
      const f_inf = term_infer(book, lhs, tm.f, qt, ctx, d, [tm.x, ...sp]);
      if (f_inf.x) {
        return { ...f_inf, x: f_inf.x - 1 };
      }
      const f_wnf = term_wnf(book, f_inf.ty);
      if (f_wnf.$ !== "All") {
        throw Err(book, ctx, "a function type", f_inf.ty, tm.s, lhs.def);
      }
      const x_chk = term_check(book, lhs, tm.x, quant_dem(f_wnf.q, qt), f_wnf.A, ctx, d);
      return Infer(App(f_inf.tm, x_chk.tm, tm.s), f_wnf.B(tm.x), uses_add(f_inf.us, x_chk.us));
    }
    // book[k].T = @q1 p1:K1 -> .. -> Kind(G)
    // Γ ⊢ xi : Ki ~ ui
    // where xi is dead if qi is -
    // ------------------------------------------------- infer-adt
    // Γ ⊢ k<x1, .., xn> : Kind(G[x..]) ~ u1 + .. + un
    case "ADT": {
      const adt = book_adt(book, tm, ctx, lhs.def);
      if (tm.x.length !== adt.n) {
        throw Err(book, ctx, name_key(tm.k) + " with " + String(adt.n) + (adt.n === 1 ? " parameter" : " parameters"), tm, tm.s, lhs.def);
      }
      const { xs, us, tel } = tele_check(book, lhs, adt.T, tm.x, qt, ctx, d, tm.s);
      return Infer(ADT(tm.k, xs, tm.s, tm.r), tel, us);
    }
    // Γ ⊢ A : Type    Γ ⊢ a : A    Γ ⊢ b : A
    // where A, a and b are dead; evidence is erased, so Data
    // ------------------------------------------------------ infer-eql
    // Γ ⊢ {a == b : A} : Data ~ {}
    case "Eql": {
      const T_chk = term_check(book, lhs, tm.T, None(), Typ(Qua(Lone()), tm.s), ctx, d);
      const a_chk = term_check(book, lhs, tm.a, None(), tm.T, ctx, d);
      const b_chk = term_check(book, lhs, tm.b, None(), tm.T, ctx, d);
      return Infer(Eql(a_chk.tm, b_chk.tm, T_chk.tm, tm.s), Typ(Qua(Many()), tm.s), uses_nil());
    }
    // Γ ⊢ T : Type
    // Γ ⊢ x : T ~ u
    // where T is dead
    // ------------------- infer-ann
    // Γ ⊢ {x : T} : T ~ u
    case "Ann": {
      term_check(book, lhs, tm.T, None(), Typ(Qua(Lone()), tm.s), ctx, d);
      const x_chk = term_check(book, lhs, tm.x, qt, tm.T, ctx, d);
      return { tm: x_chk.tm, ty: tm.T, us: x_chk.us };
    }
    // x is a Lam, Let, Ctr, Lit, Mat, Efq, Rfl, Rwt or Hol
    // ------------------------------------------- infer-err
    // Γ ⊢ x : ⊥ (a goal is needed)
    default: {
      const t = tm.$ === "Lit" ? lit_step(tm) : tm;
      if (t.$ === "Ctr" && book_ctr(book, t.k) === null) {
        throw Err(book, ctx, "a declared constructor", tm, tm.s, lhs.def);
      }
      throw Err(book, ctx, "an annotated term (cannot infer)", tm, tm.s, lhs.def);
    }
  }
}

export function tele_check(book: Book, lhs: LHS, tel: HTerm, xs: HTerm[], qt: Quant, ctx: Ctx, d: number, s?: Span): { xs: LTerm[]; us: Uses; tel: HTerm } {
  const out: LTerm[] = [];
  let us = uses_nil();
  for (const x of xs) {
    const t_all = tele_head(book, tel, ctx, lhs.def, s);
    const t_dem = quant_dem(t_all.q, qt);
    const x_chk = term_check(book, lhs, x, t_dem, t_all.A, ctx, d);
    out.push(x_chk.tm);
    us  = uses_add(us, x_chk.us);
    tel = t_all.B(x);
  }
  return { xs: out, us, tel };
}

// the type T of a binder q k fits Kind(q); a miss is reported at s, and
// a + binder's miss says why it needs Data
export function term_check_kind(book: Book, lhs: LHS, T: HTerm, q: Quant, k: Name, ctx: Ctx, d: number, s?: Span): Check {
  const kind = Typ<HBody>(Qua(lhs_kind(lhs, q)), s);
  try {
    return term_check(book, lhs, T, None(), kind, ctx, d);
  } catch (e) {
    const err = e as Err;
    const nte = q.$ === "Many" ? "Note: +" + k + " can be used many times, so its type must be Data." : undefined;
    throw err?.$ === "Err" && err.exp === kind ? { ...err, spn: s ?? err.spn, nte } : e;
  }
}

export function uses_close(book: Book, ctx: Ctx, us: Uses, i: U32, k: Name, q: Quant, s: Span | undefined, def?: Name): Uses {
  const u = pmap_get(us, i) ?? None();
  if (quant_join(u, q).$ !== q.$) {
    let obs = quant_show(u) + k;
    if (u.$ === "Many") {
      obs = k + " (consumed more than once)";
    }
    throw Err(book, ctx, quant_show(q) + k, obs, s, def);
  }
  return pmap_set(us, i, None());
}

export function term_check(book: Book, lhs: LHS, tm: HTerm, qt: Quant, ty: HTerm, ctx: Ctx, d: number): Check {
  switch (tm.$) {
    case "Var": {
      if (tm.i < 0 && tm.v !== undefined) {
        return term_check(book, lhs, term_force(tm), qt, ty, ctx, d);
      }
      break;
    }
    // T == @q x:A -> B
    // Γ , x : q'A ⊢ f(x) : B(x) ~ u
    // where q' is q, or + at a + binder when q is 1 and A : Kind(+)
    //       u[x] <= q'
    //       lhs steps by x while a parameter remains
    // ---------------------------------------------- check-lam
    // Γ ⊢ x => f : T ~ u - x
    case "Lam": {
      const t_wnf = term_wnf(book, ty);
      if (t_wnf.$ !== "All") {
        throw Err(book, ctx, ty, "non-inferrable term", tm.s, lhs.def);
      }
      const x: HTerm = Var(tm.k, d);
      let f_lhs = lhs;
      if (lhs.n > 0) {
        f_lhs = { ...lhs, t: term_apply(lhs.t, x), n: lhs.n - 1 };
      }
      let q = t_wnf.q;
      if (tm.q?.$ === "Many" && q.$ === "Lone") {
        q = tm.q;
        term_check_kind(book, lhs, t_wnf.A, q, tm.k, ctx, d, tm.s);
      }
      const f_ctx = ctx_bind(ctx, d, q, tm.k, t_wnf.A);
      const f_chk = term_check(book, f_lhs, tm.f(x), qt, t_wnf.B(x), f_ctx, d+1);
      return Check(Lam(tm.k, d, f_chk.tm, tm.s), ty, uses_close(book, ctx, f_chk.us, d, tm.k, q, tm.s, lhs.def));
    }
    // Γ ⊢ vj : Aj ~ vuj  (each value in Γ: the binders are parallel)
    // Γ ⊢ Aj : Kind(qj)
    // Γ , x1 : q1A1 , .. , xn : qnAn ⊢ f(x1, .., xn) : T ~ fu
    // where vj is dead if qj is -
    //       fu[xj] <= qj
    // ------------------------------------------------------------ check-let
    // Γ ⊢ q1 x1 .. qn xn = v1 .. vn; f : T ~ vu1 + .. + vun + fu - x⃗
    case "Let": {
      const n = tm.k.length;
      const vx: LTerm[] = [];
      let us = uses_nil();
      let f_ctx = ctx;
      for (let j = 0; j < n; j++) {
        const v_dem = quant_dem(tm.q[j], qt);
        const v_inf = term_infer(book, lhs, tm.v[j], v_dem, ctx, d);
        term_check_kind(book, lhs, v_inf.ty, tm.q[j], tm.k[j], ctx, d, tm.s);
        vx.push(v_inf.tm);
        us = uses_add(us, v_inf.us);
        f_ctx = ctx_bind(f_ctx, d + j, tm.q[j], tm.k[j], v_inf.ty);
      }
      const xs = tm.k.map((k, j): HTerm => Var(k, d + j, undefined, tm.v[j]));
      const f_chk = term_check(book, lhs, tm.f(xs), qt, ty, f_ctx, d + n);
      let fu = f_chk.us;
      for (let j = 0; j < n; j++) {
        fu = uses_close(book, ctx, fu, d + j, tm.k[j], tm.q[j], tm.s, lhs.def);
      }
      return Check(Let(tm.k, xs.map((_, j) => d + j), vx, f_chk.tm, tm.s, tm.q), ty, uses_add(us, fu));
    }
    // T == D<p1, .., pm>
    // D.c[k] = @q1 x1:F1 -> .. -> D<p1, .., pm>
    // Γ ⊢ xi : Fi ~ ui
    // where xi is dead if qi is -
    // ------------------------------------------ check-ctr
    // Γ ⊢ k{x1, .., xn} : T ~ u1 + .. + un
    case "Ctr": {
      const t_wnf = term_wnf(book, ty);
      if (t_wnf.$ !== "ADT") {
        const fam = book_ctr(book, tm.k) === null ? null : book_fam(book, tm.k);
        const nte = fam === null && book.tlds[tm.k]?.$ === "ADT" ? "Note: " + tm.k + " is a datatype: write its arguments as <>" : undefined;
        throw Err(book, ctx, ty, fam === null ? "non-inferrable term" : Ref(fam, tm.s), tm.s, lhs.def, nte);
      }
      const adt = book_adt(book, t_wnf, ctx, lhs.def);
      const ctr = adt.c.find((c) => c.k === tm.k);
      if (ctr === undefined) {
        if (book_ctr(book, tm.k) === null) {
          throw Err(book, ctx, "a declared constructor (" + name_key(t_wnf.k) + " declares " + adt.c.map((c) => name_key(c.k)).join(", ") + ")", tm, tm.s, lhs.def);
        }
        throw Err(book, ctx, ty, Ref(book_fam(book, tm.k), tm.s), tm.s, lhs.def);
      }
      if (tm.x.length !== ctr.n) {
        throw Err(book, ctx, name_key(tm.k) + " with " + String(ctr.n) + (ctr.n === 1 ? " field" : " fields"), tm, tm.s, lhs.def);
      }
      const tel = tele_fill(book, ctr.T, t_wnf.x, ctx, lhs.def, tm.s);
      const { xs, us } = tele_check(book, lhs, tel, tm.x, qt, ctx, d, tm.s);
      return Check(Ctr(tm.k, xs, tm.s), ty, us);
    }
    // T == Base's type k of the literal, no constructor removed
    // where any other T checks the literal's first step, which reports
    //       as the constructor it is
    // ----------------------------------------------------------- check-lit
    // Γ ⊢ "text" : T ~ {}    Γ ⊢ 3n : T ~ {}
    case "Lit": {
      const t_wnf = term_wnf(book, ty);
      if (t_wnf.$ === "ADT" && t_wnf.k === tm.k && t_wnf.r.length === 0 && book.tlds[tm.k]?.b === true) {
        return Check(tm, ty, uses_nil());
      }
      return term_check(book, lhs, lit_step(tm), qt, ty, ctx, d);
    }
    // T == @q s:D<p..> -> P
    // D.c[k] = @r1 x1:F1 -> .. -> D<p..>
    // Γ ⊢ h : @s1 x1:F1 -> .. -> P(k{x1, .., xn}) ~ hu
    // Γ ⊢ m : @q s:(D - k)<p..> -> P ~ mu
    // where q is not - in a live region
    //       si = ri · q (a field's quantity times its scrutinee's)
    //       h's lhs steps by k{x1, .., xn} while a parameter remains
    //       m is dead past D's last constructor k: a default there (a
    //       flattened fallback) is \{}, unvisited; a case still checks
    // -------------------------------------------------------------- check-mat
    // Γ ⊢ \ {k: h; m} : T ~ hu | mu
    //
    // T == @q s:D<p..> -> P    D.c = [] or live x:E ∈ Γ with E.c = []
    // where q is not - in a live region; a LIVE binder at an emptied
    //       ADT makes the region unreachable (a flattened match's
    //       default chain past its last constructor) - an erased one
    //       proves nothing, dead code inhabits it - so residue is dead
    // ------------------------------------------------------------- check-efq
    // Γ ⊢ \ {} : T ~ {}
    case "Mat":
    case "Efq": {
      const t_wnf = term_wnf(book, ty);
      if (t_wnf.$ !== "All") {
        throw Err(book, ctx, ty, "non-inferrable term", tm.s, lhs.def);
      }
      if (qt.$ !== "None" && t_wnf.q.$ === "None") {
        throw Err(book, ctx, "a live scrutinee (a - scrutinee matches only in a dead region)", undefined, tm.s, lhs.def);
      }
      const a_wnf = term_wnf(book, t_wnf.A);
      if (a_wnf.$ !== "ADT") {
        throw Err(book, ctx, "a datatype", t_wnf.A, tm.s, lhs.def);
      }
      const rem = book_adt(book, a_wnf, ctx, lhs.def).c;
      switch (tm.$) {
        case "Efq": {
          if (rem.length !== 0 && !ctx_dead(book, ctx)) {
            throw Err(book, ctx, "cases for " + rem.map((c) => name_key(c.k)).join(", "), tm, tm.s, lhs.def);
          }
          return Check(tm, ty, uses_nil());
        }
        case "Mat": {
          const b     = tm;
          const t_all = t_wnf;
          const ctr   = rem.find((c) => c.k === tm.k);
          if (ctr === undefined) {
            throw Err(book, ctx, "a constructor of " + name_key(a_wnf.k) + " (missing, or already matched)", tm, tm.s, lhs.def);
          }
          const tel = tele_fill(book, ctr.T, a_wnf.x, ctx, lhs.def, tm.s);
          function term_check_mat_goal(cur: HTerm, n: number, xs: HTerm[]): HTerm {
            if (n === 0) {
              return t_all.B(Ctr(b.k, xs, b.s));
            } else {
              const c_all = tele_head(book, cur, ctx, lhs.def, b.s);
              const c_dem = c_all.q.$ === "None" ? None() : c_all.q.$ === "Lone" ? t_all.q : quant_add(t_all.q, t_all.q);
              return All(c_dem, c_all.k, c_all.i, c_all.A, (x: HTerm) => {
                return term_check_mat_goal(c_all.B(x), n - 1, xs.concat([x]));
              }, b.s);
            }
          }
          let h_lhs = lhs;
          if (lhs.n > 0) {
            h_lhs = { ...lhs, t: lhs_ext(lhs.t, tm.k, ctr.n), n: lhs.n - 1 + ctr.n };
          }
          const h_chk = term_check(book, h_lhs, tm.h, qt, term_check_mat_goal(tel, ctr.n, []), ctx, d);
          const m_gol = All(t_wnf.q, t_wnf.k, t_wnf.i, ADT(a_wnf.k, a_wnf.x, tm.s, a_wnf.r.concat([ctr.k])), t_wnf.B, tm.s);
          const m_chk = rem.length === 1 && tm.m.$ !== "Mat" ? Check(Efq(tm.s), m_gol, uses_nil()) : term_check(book, lhs, tm.m, qt, m_gol, ctx, d);
          return Check(Mat(tm.k, h_chk.tm, m_chk.tm, tm.s), ty, pmap_union(h_chk.us, m_chk.us, quant_join));
        }
      }
    }
    // T == {a == b : A}    a == b
    // ---------------------------- check-rfl
    // Γ ⊢ {==} : T ~ {}
    case "Rfl": {
      const t_wnf = term_wnf(book, ty);
      if (t_wnf.$ !== "Eql") {
        throw Err(book, ctx, ty, "non-inferrable term", tm.s, lhs.def);
      }
      if (!term_compare("EQ", book, t_wnf.a, t_wnf.b, d)) {
        throw Err(book, ctx, t_wnf.a, t_wnf.b, tm.s, lhs.def);
      }
      return Check(tm, ty, uses_nil());
    }
    // T
    // ------------------- check-hol
    // Γ ⊢ ?TODO : T ~ {}
    case "Hol": {
      if (tm.k === "TODO") {
        return Check(tm, ty, uses_nil());
      }
      throw Err(book, ctx, ty, tm, tm.s, lhs.def);
    }
    // Γ ⊢ E : {a == b : A} ~ eu
    // Γ ⊢ P : @x:A -> @e:{a == x : A} -> Type    P(b, E) <= T
    // Γ ⊢ f : P(a, {==}) ~ fu
    // where P is dead; this is the J axiom: elimination
    //       specializes both the equation and its second endpoint
    // ------------------------------------------------------------ check-rwt
    // Γ ⊢ %e@E : P; f : T ~ eu + fu
    case "Rwt": {
      const e_inf = term_infer(book, lhs, tm.e, qt, ctx, d);
      const e_wnf = term_wnf(book, e_inf.ty);
      if (e_wnf.$ !== "Eql") {
        throw Err(book, ctx, "an equation {a == b : T}", e_inf.ty, tm.e.s ?? tm.s, lhs.def);
      }
      const p_typ = All<HBody>(Lone(), "_", 0, e_wnf.T, (x: HTerm) => All<HBody>(Lone(), "e", 0, Eql(e_wnf.a, x, e_wnf.T), () => Typ(Qua(Lone())), tm.s), tm.s);
      const p_chk = term_check(book, lhs, tm.p, None(), p_typ, ctx, d);
      const b_gol = term_apply(term_apply(tm.p, e_wnf.b), tm.e);
      if (!term_compare("LE", book, b_gol, ty, d)) {
        throw Err(book, ctx, ty, b_gol, tm.s, lhs.def);
      }
      const a_gol = term_apply(term_apply(tm.p, e_wnf.a), Rfl<HBody>(tm.s));
      const f_chk = term_check(book, lhs, tm.f, qt, a_gol, ctx, d);
      return Check(Rwt(e_inf.tm, p_chk.tm, f_chk.tm, tm.s), ty, uses_add(e_inf.us, f_chk.us));
    }
    // Γ ⊢ x : A ~ u    A <= T
    // ---------------------- check-any
    // Γ ⊢ x : T ~ u
    default: {
      break;
    }
  }
  const x_inf = term_infer(book, lhs, tm, qt, ctx, d);
  if (term_compare("LE", book, x_inf.ty, ty, d)) {
    return x_inf;
  }
  throw Err(book, ctx, ty, x_inf.ty, tm.s, lhs.def);
}

export function def_check(book: Book, k: Name, def: Def, z?: number): LTerm {
  const qs = tele_unbind(book, def.T).doms.map((dom) => dom[0]);
  const gen = def.x === 0 ? book : { ...book, tlds: Object.create(book.tlds) };
  let [t, v, T]: HTerm[] = [Ref(k), def.v as HTerm, def.T];
  for (let j = 0; j < def.x; j++) {
    const h = tele_head(gen, T, ctx_nil(), k);
    const o = k + "~" + h.k;
    if (o in gen.tlds) {
      throw Err(book, ctx_nil(), "a fresh ~ binder name", h.k, h.s);
    }
    gen.tlds[o] = { $: "Def", n: 0, x: 0, T: h.A, v: null, b: true };
    t = App(t, Ref(o));
    v = term_apply(v, Ref(o));
    T = h.B(Ref(o));
  }
  return term_check(gen, { t, n: def.n - def.x, def: k, qs, u: def.u, z }, v, Lone(), T, ctx_nil(), 0).tm;
}

export function def_inst(book: Book, lhs: LHS, tm: Extract<HTerm, { $: "Ref" }>, def: Def, sp: HTerm[], ctx: Ctx, d: number): Name {
  const xs = sp.slice(0, def.x);
  if (xs.length < def.x) {
    throw Err(book, ctx, "a template applied to closed ~ arguments (a def parameter is not comptime)", tm, tm.s, lhs.def);
  }
  let T: HTerm;
  try {
    T = tele_check(book, lhs, def.T, xs, None(), ctx_nil(), d, tm.s).tel;
  } catch (e) {
    const v = (e as Err)?.obs;
    if (typeof v === "object" && v.$ === "Var" && v.i >= 0 && v.i < d) {
      throw Err(book, ctx, "a template applied to closed ~ arguments (" + v.k + " is a variable here, not comptime: pass it at run time)", tm, tm.s, lhs.def);
    }
    throw e;
  }
  const key = xs.map((a) => term_key(term_lower(a))).join("\n");
  if (key.length > 32768) {
    throw Err(book, ctx, "a ~ argument that stops growing", tm, tm.s, lhs.def);
  }
  const is = book.tmps[tm.k] ??= new Map();
  let o = is.get(key);
  if (o === undefined) {
    const z = (lhs.z ?? 0) + 1;
    if (z > 64) {
      throw Err(book, ctx, "a template that stops instantiating itself (64 levels at most)", tm, tm.s, lhs.def);
    }
    o = tm.k + "~" + String(is.size);
    is.set(key, o);
    const inst: Def = { $: "Def", n: def.n - def.x, x: 0, T, v: xs.reduce((v, a) => term_apply(v, a), def.v as HTerm), u: def.u };
    book.tlds[o] = { ...inst, v: null };
    inst.e = def_check(book, o, inst, z);
    book.tlds[o] = inst;
  } else if (book.tlds[o].v === null && o !== lhs.def) {
    throw Err(book, ctx, "a decreasing self-call (arguments are read left to right: each passed unchanged until one shrinks)", tm, tm.s, lhs.def);
  }
  return o;
}

// Valid
// =====
// book_valid throws the first Err (its first done entries are taken
// as validated: a harness resumes past a seeded base); an order entry
// is an event. every def declares (bodiless) up front and defines at
// its event (a law: type checked at its law, defined at its fill), so
// it is visible and stuck before, and unfolds after; an ADT declares
// whole (constructors too) up front and its event only checks it: a
// declaration is no computation, so seeing it early runs nothing, and
// families name each other in any order. the book checks in place,
// each event against every declaration and the bodies so far: a
// forward reference is a live reference to a bodiless def, which errs
// (infer-ref) unless the def is @unsafe, so mutual recursion cannot
// bypass the wall. an unchecked body never unfolds, a declared ref is
// stuck. a def checks its type against Type, then its
// tree against it (def_check); an ADT checks its signature against Type
// and reads its declared kind Kind(G) off the tip, then checks every
// constructor telescope domain (parameters, then fields) in the real
// context against one goal: Kind(G) for a live field, Kind(q) for a
// binder of quantity q otherwise (term_compare's order does the
// fitting); the tip must be the family applied to its own parameters,
// in order. one telescope per declaration: there is no second face, no
// substitution and no speculative pass. an instance k~n, k's body at
// closed ~ arguments, is minted and checked by its first live call
// (infer-ref) while the caller is declared; it stays outside the order
// (a seeded book keeps it), so it is no claim.

export function book_valid(book: Book, done: number = 0): void {
  const tlds = book.tlds;
  const last = new Map<Name, number>();
  for (let i = 0; i < book.order.length; i++) {
    last.set(book.order[i], i);
  }
  book.tlds = Object.create(null);
  for (const k in tlds) {
    const t = tlds[k];
    book.tlds[k] = last.has(k) && t.$ === "Def" ? { ...t, v: null } : t;
  }
  for (let i = 0; i < book.order.length; i++) {
    const k   = book.order[i];
    const tld = tlds[k];
    const fin = last.get(k) === i;
    if (tld.$ === "ADT") {
      if (i >= done) {
        term_check(book, { t: Ref(k), n: 0, def: k, qs: [] }, tld.T, None(), Typ(Qua(Lone())), ctx_nil(), 0);
        const { doms, ret: kind } = tele_unbind(book, tld.T);
        if (kind.$ !== "Typ") {
          let ctx = ctx_nil();
          for (const [d, [q, x, A]] of doms.entries()) {
            ctx = ctx_bind(ctx, d, q, x, A);
          }
          throw Err(book, ctx, "a kind (type " + name_key(k) + "<..> is Kind(g))", kind, kind.s ?? tld.T.s, k);
        }
        for (const ctr of tld.c) {
          let tel: HTerm = ctr.T;
          let ctx = ctx_nil();
          for (let d = 0; d < tld.n + ctr.n; d++) {
            const t_all = tele_head(book, tel, ctx, ctr.k);
            let goal: HTerm = Typ(Qua(t_all.q));
            if (d >= tld.n && t_all.q.$ === "Lone") {
              goal = kind;
            }
            term_check(book, { t: Ref(ctr.k), n: 0, def: ctr.k, qs: [] }, t_all.A, None(), goal, ctx, d);
            ctx = ctx_bind(ctx, d, t_all.q, t_all.k, t_all.A);
            tel = t_all.B(Var(t_all.k, d));
          }
          const exp = "a telescope tipped at " + name_key(k) + " applied to its own parameters";
          const tip = term_wnf(book, tel);
          if (tip.$ !== "ADT" || tip.k !== k || tip.x.length !== tld.n || tip.r.length !== 0) {
            throw Err(book, ctx, exp, tip, undefined, ctr.k);
          }
          for (let d = 0; d < tld.n; d++) {
            const x = term_wnf(book, tip.x[d]);
            if (x.$ !== "Var" || x.i !== d) {
              throw Err(book, ctx, exp, tip, undefined, ctr.k);
            }
          }
        }
      }
      continue;
    }
    if (i >= done) {
      if (fin && tld.v === null && tld.b !== true && !tld.i) {
        book.hols += 1;
      }
      term_check(book, { t: Ref(k), n: 0, def: k, qs: [], u: tld.u }, tld.T, None(), Typ(Qua(Lone())), ctx_nil(), 0);
      if (tld.i) {
        let tel = term_strip(tld.T);
        for (let d = 0; tel.$ === "All"; d++) {
          tel = term_strip(tel.B(Var(tel.k, d)));
        }
        const [h] = term_unapply(tel);
        const io  = book.tlds["IO"];
        if (h.$ !== "Ref" || h.k !== "IO" || io === undefined || io.$ !== "Def" || io.b !== true) {
          throw Err(book, ctx_nil(), "a foreign definition returning base IO(...) directly (return type aliases are not unfolded)", k, tel.s, k);
        }
      }
      if (fin && tld.v !== null) {
        tld.e = def_check(book, k, tld);
      }
    }
    if (fin) {
      book.tlds[k] = tld;
    }
  }
}
