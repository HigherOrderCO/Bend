// BendTT: An Affine Dependent Type Theory
// Build: typst compile --root .. main.typ ../../../paper/BendTT.pdf
//
// Solarized-light theme for the site build. Set solarized = false for a
// plain black-on-white document: colors revert and NOTHING else changes.
#let solarized = true

#let solbg   = if solarized { rgb("#FDF6E3") } else { white }
#let solhi   = if solarized { rgb("#EEE8D5") } else { luma(235) }
#let solfg   = if solarized { rgb("#073642") } else { black }
#let solblue = if solarized { rgb("#268BD2") } else { rgb("#1A45A8") }
#let solcyan = if solarized { rgb("#2AA198") } else { rgb("#1A45A8") }
#let solgreen = if solarized { rgb("#859900") } else { rgb("#1A6B27") }

// Page and text. Two columns; the title block spans both via a parent-
// scoped float.
#set page(
  paper: "us-letter",
  margin: (x: 54pt, top: 66pt, bottom: 60pt),
  columns: 2,
  fill: solbg,
  header: context {
    let p = counter(page).get().first()
    if p > 1 {
      set text(size: 8pt)
      if calc.even(p) [#p #h(1fr) Victor Taelin et al.] else [BendTT: An Affine Dependent Type Theory #h(1fr) #p]
    }
  },
)
#set columns(gutter: 20pt)
#set text(font: "Libertinus Serif", size: 10pt, fill: solfg)
#set par(justify: true, leading: 0.52em, spacing: 0.52em, first-line-indent: 1em)
#show link: set text(fill: solcyan)
#show ref: set text(fill: solblue)
#show cite: set text(fill: solgreen)

// Headings, ACM-flavored.
#set heading(numbering: "1.1")
#show heading: it => {
  set text(fill: solfg)
  let big = it.level == 1
  block(above: if big { 1.4em } else { 1.2em }, below: if big { 0.7em } else { 0.6em },
    text(size: if big { 12pt } else { 10pt }, weight: "bold", {
      if it.numbering != none {
        counter(heading).display(it.numbering)
        h(if big { 0.9em } else { 0.7em })
      }
      it.body
    }))
}

// Monospace names (theorems, file names).
#let co(body) = text(font: "DejaVu Sans Mono", size: 0.82em, body)

// Code blocks: shaded, monospace, unbreakable, Bend-highlighted.
#set raw(syntaxes: "../bend.sublime-syntax")
#show raw.where(block: true): it => block(
  breakable: false,
  fill: solhi, inset: 6pt, radius: 2pt, width: 100%,
  text(font: "DejaVu Sans Mono", size: 7.5pt, it))
#show raw.where(block: false): it => box(
  fill: solhi, inset: (x: 2pt), outset: (y: 2pt), radius: 1pt,
  text(font: "DejaVu Sans Mono", size: 0.82em, it))

#set figure(placement: top, gap: 1em)
#show figure.where(placement: none): set block(above: 1.2em, below: 1.2em)
#show figure.caption: it => {
  set text(size: 9pt)
  set par(first-line-indent: 0em)
  align(left)[*#it.supplement #context it.counter.display(it.numbering).* #it.body]
}
#show figure.where(kind: table): set figure.caption(position: top)

// Theorems: a bold run-in label, an italic statement.
#let thm(kind, name, body) = block(above: 0.9em, below: 0.9em, {
  set par(first-line-indent: 0em)
  [*#kind* (#name). #emph(body)]
})

// Inference rules: premises over a line over a conclusion, with a name.
#let rule(name, concl, ..prems) = context {
  let ps = prems.pos()
  let top = if ps.len() == 0 { [] } else { ps.join(h(1.1em)) }
  let w = calc.max(measure(top).width, measure(concl).width)
  box(inset: (y: 3pt), grid(columns: 2, column-gutter: 3pt, align: (center, horizon),
    stack(dir: ttb, spacing: 2.5pt,
      box(width: w, align(center, top)),
      line(length: w, stroke: 0.45pt + solfg),
      box(width: w, align(center, concl))),
    text(size: 7pt, smallcaps(name))))
}

// Notation.
#let Ty = $sans("Type")$
#let Da = $sans("Data")$
#let rfl = $sans("rfl")$
#let kind = $sans("kind")$
#let Qs = $sans("Q")$
#let fld = $sans("fld")$
#let live = $sans("live")$
#let emp = $chevron.l chevron.r$

// ---------------------------------------------------------------------
// Title block, full width.

#place(top + center, scope: "parent", float: true, {
  set par(first-line-indent: 0em)
  v(10pt)
  text(size: 17.3pt, weight: "bold")[BendTT: An Affine Dependent Type Theory]
  v(2pt)
  text(size: 11pt)[Victor Taelin, Lorenzo W Battistela, Paulo J Cavalcanti]
  linebreak()
  text(size: 11pt)[Nicolas Abril, Vitor Chiarelli Neves, Vanessa Ostroski]
  linebreak()
  text(size: 10pt)[Higher Order Company]
  linebreak()
  text(size: 10pt)[Rio de Janeiro, Brazil]
  linebreak()
  text(size: 10pt, link("mailto:taelin@higherorderco.com", "taelin@higherorderco.com"))
  v(8pt)
  align(left, block(stroke: 0.5pt + solfg, inset: 6pt, width: 100%, {
    set text(size: 8pt)
    set par(justify: true)
    align(left)[#smallcaps[AI Disclosure.] Bend and BendTT were designed by
    the human author. This paper was written by Claude Opus 5.5 from the
    author's code and design choices, and reviewed by the author. The Lean
    mechanization was human-specified, AI-proven, and verified by a computer.
    Human paper soon™.]
  }))
  v(2pt)
})

// ---------------------------------------------------------------------

#heading(numbering: none, outlined: false)[Abstract]

BendTT is the core type theory of Bend, a programming language in which
programs and their proofs are written together. It has #Ty : #Ty,
impredicative function types, and datatypes whose fields may be
functions over the datatype itself. Proof assistants such as Coq, Agda
and Lean forbid #Ty : #Ty and such datatypes, because each one leads
to a proof of false. BendTT allows both and is consistent. The reason
is that the code that runs is affine. It may copy first-order data, and
it may not copy a function. Girard's paradox and Curry's paradox both
copy a function, so both fail to check. Types never run, so they are
free to use a variable many times. We prove confluence, subject
reduction, progress, termination of running code and consistency in a
single Lean 4 file. The theorems are about the checker in that file,
and Bend can run that checker on its programs.

= Introduction <sec:intro>

Proof assistants based on dependent types, like Coq, Agda and Lean,
restrict two things. First, they stack their types in a hierarchy of
universes, because a type of all types, #Ty : #Ty, makes the logic
inconsistent @girard1972 @hurkens1995. Second, they reject a datatype
that occurs to the left of an arrow in its own constructors, because
such a type lets a program loop without recursion @mendler1991
@coquandpaulin1988. In a logic, a program that loops can have any type,
including the empty type, so it proves anything. Both restrictions cost
the user something. Universe levels must be managed, and some useful
types, like terms in higher-order abstract syntax, cannot be declared.

BendTT is the core type theory of the Bend programming language. It has
no universe levels and no positivity check. Here is the usual first
step towards a contradiction, in Bend:

```bend
type R is Type:
  Fold{f: R -> Empty}

def app(r: R) -> Empty:
  match r:
    case Fold{f}:
      f(Fold{f})
```
```
Error:
- expected : f
- observed : f (consumed more than once)
Location: app
```

The type `R` occurs to the left of an arrow in its own field, and Bend
accepts it. If Bend also accepted `app`, then `app(Fold{app})` would
have type `Empty`, and it would reduce to itself forever. This is
Curry's paradox @curry1942, in the form that positivity checks exist to
stop. Bend rejects `app` for another reason. Its body uses `f` twice,
once to call it and once to rebuild the argument. In Bend, a variable
of a function type may be used at most once.

That is the main idea of this paper. The classical paradoxes of
#Ty : #Ty and of negative datatypes copy a function (@sec:paradox). So a language whose
running code cannot copy functions can keep both features. The catch is
that types must mention variables many times. The statement
`{add(a, b) == add(b, a) : Nat}` mentions `a` and `b` twice each. So
BendTT splits every term in two parts. The _live_ part is the code that
runs. The _dead_ part is the code that is only type-checked: types,
annotations, and arguments that are erased before the program runs.
Live code must follow three rules. Dead code need not follow any of
them.

+ A variable is used at most once. This is _affinity_.
+ A variable may be used more than once only if its type has the kind
  #Da. At run time, a value of a #Da type is made of labels, pairs and
  equality proofs, so it holds no function.
+ A definition may call earlier definitions. It may call itself only
  on smaller arguments.

With these rules, the proof of consistency is short. Suppose that a
definition $k$ had the empty type. The name $k$ is live code, so its
evaluation ends. Evaluation keeps the type and does not get stuck, so
it ends in a value of the empty type. There is no such value. The
termination argument counts calls and ignores types. It works because
affine code cannot copy work that is still to be done. No step of the
proof asks a type to normalize, so #Ty : #Ty does no harm. The dead
part of BendTT can even hold all of Girard's paradox, and the checker
accepts it there (@sec:girard).

= A Tour of Bend <sec:bend>

Every Bend example in this paper imports Bend's base library, which
defines `Nat`, `List` and `Empty`. We ran each example through the
Bend checker, and each accepted example also through the BendTT kernel
(@sec:mech). We show each error message up to its `Location` line.

== Programs and Proofs

Here is addition on natural numbers:

```bend
def add(a: Nat, b: Nat) -> Nat:
  match a:
    case 0n:
      b
    case 1n+p:
      1n+add(p, b)
```

The patterns `0n` and `1n+p` stand for `Zero{}` and `Succ{p}`. A proof
is a program too. This one shows that `add` is commutative:

```bend
law comm:
  for +a: Nat
  for +b: Nat
  {add(a, b) == add(b, a) : Nat}

def comm(a, b):
  match a:
    case 0n:
      zero(b)
    case 1n+p:
      %succ(b, p) : {1n+add(p, b) == _ : Nat}
      %comm(p, b) : {1n+add(p, b) == 1n+_ : Nat}
      {==}
```

A `law` states a type, and the `def` with the same name is a term of
that type. The type `{x == y : T}` says that `x` and `y` are equal
values of type `T`. Its proof `{==}` checks when both sides compute to
the same term. The lemmas `zero(a)`, of type `{a == add(a, 0n) : Nat}`,
and `succ(a, b)`, of type `{1n+add(a, b) == add(a, 1n+b) : Nat}`, have
proofs of the same shape.

The proof is by induction, and induction is recursion. The `match`
splits the goal in two cases, and in each case the checker replaces `a`
by its pattern. In the first case, `add(0n, b)` computes to `b`, and
`zero(b)` proves the goal. In the second case, the recursive call
`comm(p, b)` is the induction hypothesis. Bend accepts it because `p`
is a piece of `a`. A rewrite `%e : P` uses an equation
`e : {x == y : T}`. The motive `P` is a type with a hole `_`. With `y`
in the hole, `P` must be the goal. With `x` in the hole, `P` is the new
goal. Bend has no tactics, and a proof is an ordinary term.

== Quantities

Every binder has a _quantity_. A plain binder, like `b` in `add`, is
_affine_. The running code may use it at most once. A binder marked
`-`, like `-A: Type`, is _erased_. It may appear only in dead code, and
the compiler removes it. A binder marked `+`, like `+a` in `comm`, is
_copyable_. The running code may use it any number of times. In the
second case of `comm`, the proof uses `p` twice and `b` twice, so the
law marks `a` and `b` with `+`. A match on a copyable value gives
copyable fields.

== Kinds

Every type has a _kind_ `Kind(q)`, for a quantity `q`: #Ty is
`Kind(&1)` and #Da is `Kind(&2)`. A copyable binder needs a type of
kind #Da. A datatype declares its kind, and the checker verifies the
kind at each field. A #Da type may only have #Da fields. `Nat` is #Da.
A function type is always #Ty, so a function cannot be copyable:

```bend
law dupf:
  for +f: Nat -> Nat
  Nat
```
```
Error:
- expected : Data
- observed : Type
Location: dupf
```

For the same reason, the checker rejects the type `R` of the
introduction if it is declared `is Data`. It gives the same error, at
the field `f`.

== Dead Code

These rules apply to live code only. A law is a type, so it is dead,
and it may mention a function as often as it needs. This law uses `f`
four times, and `{==}` proves it:

```bend
law twice:
  for f: Nat -> Nat
  for x: Nat
  {f(f(x)) == f(f(x)) : Nat}

def twice(f, x):
  {==}
```

#Ty : #Ty is also free to use. A function may return a type, and a
polymorphic function may be applied to its own type:

```bend
def Pred(A: Type) -> Type:
  A -> Type

def id(-A: Type, x: A) -> A:
  x

law idid:
  @-A: Type -> @_: A -> A

def idid():
  id(@-A: Type -> @_: A -> A, id)
```

The type `@-A: Type -> @_: A -> A` quantifies over all types, itself
included.

= Why the Paradoxes Copy <sec:paradox>

This section looks at the two classical paradoxes. Each one copies a
function, and BendTT rejects that copy. We do not claim that every
possible paradox must copy a function. The consistency theorem of
@sec:meta covers every term, whatever its shape.

== Curry's Paradox

The untyped term $(lambda x. x space x)(lambda x. x space x)$ reduces
to itself. Simple types reject it, because $x space x$ needs $x$ to be
a function whose domain is the type of $x$ itself. The negative type
`R` of the introduction provides such a type, and `app(Fold{app})` is
the self-application. No definition calls itself, so a termination checker
that inspects recursive calls finds nothing to reject. Coq and Agda
reject the declaration of `R`. BendTT accepts `R` and rejects the
second use of `f`. The field `f` cannot be made copyable, because its
type is not #Da.

== Girard's Paradox <sec:girard>

Girard showed that #Ty : #Ty makes Martin-Löf's first type theory
@martinlof1971 inconsistent @girard1972, and Hurkens gave a short proof
@hurkens1995. Let $cal(P) X = X -> #Ty$. Hurkens' proof uses the type
$U = forall X : #Ty. (cal(P) cal(P) X -> X) -> cal(P) cal(P) X$ and the
map $tau : cal(P) cal(P) U -> U$ defined by
$ tau space t = lambda X. lambda f. lambda p. space t space (lambda x. space p space (f space (x space X space f))). $
The variable $f$ occurs twice in the body of $tau$. It is applied, and
it is passed to $x$. Written as a BendTT definition, $tau$ fails the
live check for this reason.

@fig:girard shows the whole proof in BendTT's text syntax
(@sec:calculus). This time every step is an erased let, `!-x = v; t`,
whose value `v` is dead. The kernel type-checks every step, and it
accepts the file. So the definition `girard` holds a closed term,
`false`, of the empty type `<>`. As a logic, the dead part of BendTT is
inconsistent. To make sure that the kernel checks dead code, we broke
one step of `lemma1`, and the kernel rejected the file with a type
mismatch.

#figure(kind: image, supplement: [Figure], placement: top, scope: "parent",
caption: [Hurkens' paradox in BendTT. `*1` is #Ty, `<>` is the empty
type, and `<()>` is the unit type. Each `!-` binds an erased let. The
kernel prints `ALL PROOFS CHECK`. When the lets are live (`!` in place
of `!-`) and `girard` returns `false` at type `<>`, the kernel prints
`SOME PROOFS FAIL` and `affine live code, calls that descend`.],
```
P : ∀S : *1 -> *1 =
  λS => ∀x : S -> *1

Bot : *1 =
  ∀A : *1 -> A

Not : ∀A : *1 -> *1 =
  λA => ∀h : A -> Bot

U : *1 =
  ∀X : *1 -> ∀f : (∀g : (P (P X)) -> X) -> (P (P X))

girard : <()> =
  !-tau = {λt => λX => λf => λp => (t λx => (p (f (x X f)))) : ∀t : (P (P U)) -> U};
  !-sigma = {λs => (s U tau) : ∀s : U -> (P (P U))};
  !-Delta = {λy => (Not ∀p : (P U) -> ∀h : (sigma y p) -> (p (tau (sigma y)))) : (P U)};
  !-Omega = {(tau λp => ∀x : U -> ∀h : (sigma x p) -> (p x)) : U};
  !-lemma1 = {λp => λH1 => (H1 Omega λx => (H1 (tau (sigma x))))
    : ∀p : (P U) -> ∀H1 : (∀x : U -> ∀h : (sigma x p) -> (p x)) -> (p Omega)};
  !-lemma2 = {λH0 => (H0 Delta (λx => λH2 => λH3 => (H3 Delta H2 λp => (H3 λy => (p (tau (sigma y))))))
                              λp => (H0 λy => (p (tau (sigma y)))))
    : (Not ∀p : (P U) -> ∀H1 : (∀x : U -> ∀h : (sigma x p) -> (p x)) -> (p Omega))};
  !-false = {(lemma2 lemma1 <>) : <>};
  ()
```) <fig:girard>

This does no harm, because live code cannot reach `false`. The let
binds it at quantity 0, so a live use of it breaks affinity. A match on
it fails too, because the typing rules require a match to take a live
argument (@sec:typing). The same holds in Bend:

```bend
def bad(-e: Empty) -> Nat:
  match e:
```
```
Error:
- message  : a live scrutinee (a - scrutinee matches only in a dead region)
Location: bad
```

A dead term may also loop. The term `false` is closed and has the
empty type, so by Theorems 2 and 3 and Lemma 4 of @sec:meta, its
evaluation never ends. This does not contradict Theorem 5, because
`false` fails the live check. Evaluation does not enter dead code, so
this loop does not run.

= The Calculus <sec:calculus>

BendTT is the small core language that Bend compiles to. It has no
built-in datatypes and no fixed-point operator. A datatype is a pair of
a label and the fields that the label needs. Recursion goes through the
names of definitions.

== Terms

@fig:terms lists the terms. We use two notations for them. The kernel
reads a text syntax, which we use in examples. The rules use math
notation. Every binder, application and pair carries a quantity 0, 1
or 2, which the text writes as `-`, nothing, or `+`. These quantities
are fixed. A kind carries a quantity too, but there it is a term: a
label of the enumeration $Qs = chevron.l Q_0, Q_1, Q_2 chevron.r$, or a
variable, a call or a meet that computes one. A _book_ is an
ordered list of definitions `k : T = t`, where `t` is closed. A name
`k` in a term refers to a definition of the book.

#figure(kind: image, supplement: [Figure], placement: none, caption: [The
terms of BendTT, in text syntax and in math notation. The quantity $q$
is 0 (erased), 1 (affine) or 2 (copyable).], {
  set text(size: 8.5pt)
  set par(justify: false)
  table(
    columns: (auto, auto, auto),
    stroke: none,
    align: left,
    inset: (x: 3pt, y: 2.2pt),
    table.hline(stroke: 0.5pt),
    [*Text*], [*Math*], [*Term*],
    table.hline(stroke: 0.3pt),
    [`x`, `k`], [$x$, $k$], [variable, name],
    [`{t : T}`], [$(t : T)$], [annotation],
    [`!q x = v; t`], [$sans("let")^q x = v; t$], [let],
    [`*(g)`], [$star(g)$], [kind of quantity $g$],
    [`*1`, `*2`], [#Ty, #Da], [$star(Q_1)$, $star(Q_2)$],
    [`(g <&> h)`], [$g ⊓ h$], [meet of quantities],
    [`∀q x : A -> B`], [$forall^q (x : A). B$], [function type],
    [`λq x => t`], [$lambda^q x. t$], [function],
    [`(f q a)`], [$f ∘^q a$], [application],
    [`Σq x : A -> B`], [$Sigma^q (x : A). B$], [pair type],
    [`(q a, b)`], [$(a ,^q b)$], [pair],
    [`<k1, k2>`, `.k`], [$chevron.l k_1, k_2 chevron.r$, $k$], [enum, label],
    [`λ{(,): h}`], [$lambda{(,) : h}$], [pair match],
    [`λ{.k: h; m}`], [$lambda{k : h; m}$], [label match],
    [`λ{}`], [$lambda{}$], [empty match],
    [`{a == b : A}`], [$a scripts(=)_A b$], [equality],
    [`{==}`], [#rfl], [reflexivity],
    [`%e : x, h => P; f`], [$e ▹_(x,h. P) f$], [rewrite (J)],
    table.hline(stroke: 0.5pt),
  )
}) <fig:terms>

== Datatypes

Here is `Nat`, as Bend compiles it (we renamed the binders):

```
Nat.arms : ∀t : <Zero, Succ> -> *2 =
  λ{.Zero: <()>;
  λ{.Succ: Σp : Nat -> <()>;
  λ{}}}

Nat : *2 =
  Σt : <Zero, Succ> -> (Nat.arms t)
```

A `Nat` is a pair. Its first field is a label, `.Zero` or `.Succ`. The
type of its second field depends on that label, and `Nat.arms` gives
it. `Zero` has no fields, so it gets the unit type `<()>`. `Succ` has
one field of type `Nat`, followed by a unit. The number 2 is
`(.Succ, ((.Succ, ((.Zero, ()), ())), ()))`.

The names `Nat` and `Nat.arms` refer to each other, with no
fixed-point operator. Both references are in types, which are dead. The live
check ignores them, so they need no order and no positivity condition.

== Kinds

The kind of a type says whether its values may be copied. A kind is
$star(g)$ for a quantity $g : Qs$, and a type of kind $star(g)$ is #Da
exactly where $g$ reduces to $Q_2$. Function types have kind #Ty.
Enumerations and equality types have kind #Da. A pair type
$Sigma^q (x : A). B$ has any kind $K = star(g)$ that $B$ has, and $A$
must have the kind $kind(q, K)$:
$ kind(0, K) = #Ty, quad kind(1, K) = K, quad kind(2, K) = #Da. $
An erased field may have any type, since it is gone at run time. An
affine field of a #Da pair must be #Da, and a copyable field is always
#Da. So the live part of a value of a #Da type holds only labels,
pairs and #rfl. In the
same way, the domain of a function type of quantity 2 must be #Da.

A kind $star(g)$ may be used where $star(h)$ is expected when $g$ is
$Q_2$ wherever $h$ is. So #Da fits every kind, and every kind fits #Ty
and $star(Q_0)$. The meet $g ⊓ h$ is the least of two quantities. It
reduces as Bend's `<&>` does: $Q_2$ is its identity and $Q_0$ absorbs
it, on either side, even when the other side is stuck. So Bend's
`Either<a, b, A, B>` has the kind $star(a ⊓ b)$, and it is #Da where
both sides are. Function types fit by their parts, as in Bend:
$forall^q (x : A). B$ fits $forall^q (x : C). D$ when $C$ fits $A$ and
$B$ fits $D$. So a `Nat -> Data` fits where a `Nat -> Type` is
expected, and a `Type -> Nat` where a `Data -> Nat` is.

== Matches and Case Trees

A match is a function that consumes its argument. The match
$lambda{(,) : h}$ takes a pair $(a, b)$ and applies $h$ to $a$ and $b$.
The match $lambda{k : h; m}$ takes a label. On $k$ it returns $h$. On
any other label it passes the label to $m$, whose domain lacks $k$. The
match $lambda{}$ takes a value of the empty enumeration, and there is
none. So a chain of label matches that ends in $lambda{}$ covers every
case. Here is `add`, as Bend compiles it (again with renamed binders):

```
add : ∀a : Nat -> ∀b : Nat -> Nat =
  λ{(,):
    λ{.Zero: λ{(): λb => b; λ{}};
    λ{.Succ: λ{(,): λp => λ{(): λb =>
      (.Succ, ((add p b), ())); λ{}}};
    λ{}}}}
```

The parameter `b` is bound inside each case. The reason is that the
live check adds the uses of a variable in the two arms of a match
(@sec:affinity).

A definition calls itself, or another definition, by name. The _case
tree_ of a definition is its leading lambdas and matches. A call
$k space a_1 ... a_n$ unfolds only when its arguments lead through the
whole case tree to a leaf. Otherwise the call stays as it is. For
example, `(add p b)` with a variable `p` does not unfold, since the
first match needs a pair. This rule has two uses. In proofs, the goal
shows `add(p, b)` and not a half-evaluated match, so the induction
hypothesis can match it. In the termination proof, a call that unfolds
becomes one leaf in one step, and the calls in that leaf can be
compared with the call that unfolded.

Here is Curry's paradox of the introduction, written directly in
BendTT:

```
R.arms : ∀t : <Fold> -> *1 =
  λ{.Fold: Σf : ∀r : R -> <> -> <()>; λ{}}

R : *1 =
  Σt : <Fold> -> (R.arms t)

app : ∀r : R -> <> =
  λ{(,): λ{.Fold: λ{(,): λf => λ{():
    (f (.Fold, (f, ()))); λ{}}}; λ{}}}
```
```
In app:
affine live code, calls that descend
```

The kernel accepts `R` and rejects `app`, whose body uses `f` twice.

== Equality

The type $a scripts(=)_A b$ has kind #Da. Its only value is #rfl, which
proves $a scripts(=)_A b$ when $a$ and $b$ are convertible. The rewrite
$e ▹_(x,h. P) f$ is the J eliminator of Martin-Löf type theory, with
an explicit motive $P$. The motive may depend on both the right
endpoint $x$ and the proof $h : a scripts(=)_A x$. The proof $e$ is
live. At run time it evaluates to #rfl, and then the rewrite steps to
$f$. If $e$ could be dead, a dead proof of a false equation could
change the type of live code, and evaluation could get stuck.

== Typing <sec:typing>

#figure(kind: image, supplement: [Figure], placement: top,
scope: "parent", caption: [Selected typing rules. $U <= T$ holds when
$U$ and $T$ are convertible, when they are kinds $star(g)$ and
$star(h)$ and $g$ is $Q_2$ wherever $h$ is, under every substitution,
when both are enumerations and each label of $U$ is in $T$, when they
are $forall^q (x : A). B$ and $forall^q (x : C). D$ with $C <= A$ and
$B <= D$, or through a type between them. The checker decides
the kind case by rules: $Q_2$ is the top, $Q_0$ and $Q_1$ the bottom, a
meet on the left needs both sides, and a meet on the right needs
either. The rules for
variables, names, annotations, pairs, labels and the types of pairs
and equalities are standard.],
{
  set text(size: 9pt)
  let row(..xs) = align(center, xs.pos().join(h(1.6em)))
  stack(dir: ttb, spacing: 0.9em,
    row(
      rule[sort][$Gamma tack star(g) : #Ty$][$Gamma tack g : Qs$],
      rule[meet][$Gamma tack g ⊓ h : Qs$][$Gamma tack g : Qs$][$Gamma tack h : Qs$],
      rule[enum][$Gamma tack chevron.l k_1, ..., k_n chevron.r : #Da$],
      rule[conv][$Gamma tack t : T$][$Gamma tack t : U$][$U <= T$],
      rule[refl][$Gamma tack rfl : a scripts(=)_A b$][$a equiv b$],
    ),
    row(
      rule[pi][$Gamma tack forall^q (x : A). B : #Ty$][$Gamma tack A : kind(q, #Ty)$][$Gamma, x : A tack B : #Ty$],
      rule[sigma][$Gamma tack Sigma^q (x : A). B : K$][$Gamma tack A : kind(q, K)$][$Gamma, x : A tack B : K$],
    ),
    row(
      rule[lam][$Gamma tack lambda^q x. t : forall^p (x : A). B$][$live(p) = live(q)$][$q = 2 => Gamma tack A : #Da$][$Gamma, x : A tack t : B$],
      rule[app][$Gamma tack f ∘^q a : B[a slash x]$][$Gamma tack f : forall^q (x : A). B$][$Gamma tack a : A$],
    ),
    row(
      rule[let][$Gamma tack sans("let")^q x = v; t : T$][$Gamma tack v : V$][$q = 2 => Gamma tack V : #Da$][$Gamma tack t[v slash x] : T$],
      rule[absurd][$Gamma tack lambda{} : forall^q (s : emp). P$][$live(q)$],
    ),
    row(
      rule[split][$Gamma tack lambda{(,) : h} : forall^q (s : Sigma^r (x : A). B). P$][$live(q)$][$Gamma tack h : forall^(fld(r, q)) (x : A). forall^q (y : B). P[(x ,^r y) slash s]$],
    ),
    row(
      rule[case][$Gamma tack lambda{k : h; m} : forall^q (s : chevron.l L chevron.r). P$][$live(q)$][$k in L$][$Gamma tack h : P[k slash s]$][$Gamma tack m : forall^q (s : chevron.l L without k chevron.r). P$],
    ),
    row(
      rule[J][$Gamma tack e ▹_(x,h. P) f : T$][$Gamma tack e : a scripts(=)_A b$][$Gamma, x : A, h : a scripts(=)_A x tack P : #Ty$][$Gamma tack f : P[a, rfl slash x, h]$][$P[b, e slash x, h] <= T$],
    ),
  )
}) <fig:typing>

@fig:typing gives the typing rules. Here $K$ is a kind $star(g)$, and
$live(q)$ holds when $q$ is 1 or 2. Most rules are standard. We explain
the unusual ones.

The rule #smallcaps[sort] gives every kind the kind #Ty, so #Ty : #Ty
and #Da : #Ty. There is one level, as in the `idid` example of
@sec:bend.

The rules #smallcaps[pi] and #smallcaps[sigma] assign kinds. A function
type always has kind #Ty. No rule gives it kind #Da, and confluence
(@sec:meta) shows that #Ty and #Da are not convertible, since $Q_1$ and
$Q_2$ are distinct labels. So no function is ever bound at quantity 2.

In the rule #smallcaps[lam], a lambda may bind its variable at
quantity 2 under a function type of quantity 1, if the domain is #Da.
The caller passes one value, and the callee copies it. In
#smallcaps[app], the application carries the quantity of the binder.
So the live check can tell a dead argument from a live one without
reading types.

The rules #smallcaps[split], #smallcaps[case] and #smallcaps[absurd]
give matches their dependent types. The motive $P$ depends on the
argument $s$, and each case sees $P$ at its pattern. A pair match
passes its first field at quantity $fld(r, q)$, which is 0 when
$r = 0$, $q$ when $r = 1$, and 2 when $r = 2$. So a match on a
copyable pair gives copyable fields. All three rules require a live
quantity $q$, so a match cannot take a dead argument.

The rule #smallcaps[J] checks the body $f$ with $a$ and #rfl in the
motive, and it gives the rewrite the motive at $b$ and $e$.

The checker is bidirectional @dunfieldkrishnaswami2021. When it checks
a match applied to a variable $x$ against a goal $T$, it takes the
motive to be $T$ with $x$ abstracted. So each case sees the goal with
$x$ replaced by its pattern, as in the proof of `comm`. The kernel has
no unification, so Bend matches only on variables.

With #Ty : #Ty, conversion is undecidable @meyerreinhold1986 @howe1987,
and a type may loop. The checker runs with a step budget, and it
rejects a book when the budget runs out. A loop in a type can make the
checker reject a correct book. It cannot make the checker accept a
wrong one.

= The Live Check <sec:live>

The typing rules do not count uses. A second pass, the _live check_,
counts them, and it reads no types. These positions are _dead_: the
type of an annotation, both parts of a $forall$ or $Sigma$ type, the
motive of a rewrite, the argument of $f ∘^0 a$, the first field of
$(a ,^0 b)$, and the value of $sans("let")^0 x = v; t$. All other
positions are _live_, and the body of a definition is live. The live
check has two rules, affinity and descent.

== Affinity <sec:affinity>

A variable of quantity 0 has no live use. A variable of quantity 1 has
at most one live use. A variable of quantity 2 may have any number of
live uses. The uses in the two arms of a match add up. Until the match
fires, both arms are part of the term. If a variable were substituted
into both, its value would be copied, and the value may hold calls.
This is why the compiler binds `b` inside each arm of `add`. A meet is
live code: its two sides are live, and their uses add up.

The quantity is written on the binder. The kind of its type only
confirms that the quantity is allowed. That way the live check needs
no types.

== Descent

Here is the Ackermann function. Bend accepts it:

```bend
def ack(+m: Nat, n: Nat) -> Nat:
  match m:
    case 0n:
      1n+n
    case 1n+p:
      match n:
        case 0n:
          ack(p, 1n)
        case 1n+q:
          ack(p, ack(1n+p, q))
```

A live reference to a name $k$ is allowed in two cases. Either $k$
comes earlier in the book than the definition being checked, or $k$ is
that definition and its arguments _descend_. To define descent, we read
the case tree. Each leading lambda binds a _column_, which is one
parameter. A match on a column splits it into _pieces_. A pair match
gives its two fields, and a label match records the label it hit. At a
self-call, the checker compares the arguments with the columns, from
left to right, and skips erased columns. Each argument must _rebuild_
its column, until one argument is a strict piece of its column. The
arguments after that one may be anything. An argument rebuilds its
column when it is the variable of the column, the label that a match
on the column hit, or a pair of the two pieces of one split.

In `ack`, the two outer calls pass `p`, a piece of `m`, in the first
column. The inner call rebuilds `m` as `1n+p` and passes `q`, a piece
of `n`, in the second column. The last case uses `p` twice, so `m` must
be copyable.

Dead positions have no such rule. A type may refer to any name, as
`Nat` and `Nat.arms` do. The body of a definition is live, so the
kernel rejects `loop : <> = loop`.

= Metatheory <sec:meta>

This section states the theorems and sketches each proof. All of them
are proved in Lean (@sec:mech). They are about a declarative version of
the theory, with three relations:

- _Parallel reduction_ $t => u$ reduces any set of redexes in $t$, in
  live and dead positions. It includes the rules of @fig:eval with any
  terms in place of the values, and the unfolding of a name to its
  body. It also steps $lambda^2 x. t$ to $lambda^1 x. t$, and never
  back: the quantity of a lambda says how it runs, so two lambdas of one
  liveness are convertible when their bodies are. _Conversion_
  $a equiv b$ holds when $a$ and $b$ have a common reduct.
- _Typing_ $Gamma tack t : T$ has the rules of @fig:typing.
- _Evaluation_ $t |-> u$ is call-by-value evaluation of live code
  (@fig:eval). Its values are lambdas, matches, labels, #rfl, types,
  pairs of values, and calls that wait for more arguments.

#figure(kind: image, supplement: [Figure], placement: none, caption: [Evaluation
of live code. $v$ is a value, and it must be #Da when $p = 2$. In the
last rule, the values $v_i$ lead through the case tree of $k$ to the
leaf $t$. Evaluation does not enter dead positions.], {
  set text(size: 9pt)
  set align(left)
  grid(columns: (auto, auto, auto), column-gutter: 0.6em, row-gutter: 0.6em, align: (right, center, left),
    $(lambda^p x. t) ∘^q v$, $|->$, $t[v slash x]$,
    $lambda{(,) : h} ∘^q (a ,^r b)$, $|->$, $(h ∘^(fld(r, q)) a) ∘^q b$,
    $lambda{k : h; m} ∘^q k$, $|->$, $h$,
    $lambda{k : h; m} ∘^q j$, $|->$, $m ∘^q j quad (j != k)$,
    $sans("let")^q x = v; t$, $|->$, $t[v slash x]$,
    $(t : T)$, $|->$, $t$,
    $Q_i ⊓ Q_j$, $|->$, $Q_(min(i, j))$,
    $rfl ▹_(x,h. P) f$, $|->$, $f$,
    $k space v_1 ... v_n$, $|->$, $t[v_1 ... v_n]$,
  )
}) <fig:eval>

Evaluation checks two facts at run time that the types promise: a
quantity-2 lambda receives a #Da value, and a match receives a live
argument. Progress shows that typed terms pass these checks. A book is
_well typed_ when each definition $k : T = t$ has $tack T : #Ty$ and
$tack t : T$. It is _live_ when each body passes the live check.

== Confluence

#thm[Theorem 1][Confluence][If $t scripts(=>)^* u_1$ and
$t scripts(=>)^* u_2$, then $u_1 scripts(=>)^* v$ and
$u_2 scripts(=>)^* v$ for some $v$.]

We follow Takahashi @takahashi1995. The complete development $t^*$
reduces every redex of $t$ at once, and steps every $lambda^2$ to
$lambda^1$. If $t => u$, then $u => t^*$, and confluence follows. The
proof does not use types, so #Ty : #Ty plays no part in it.

Confluence gives the two facts about conversion that the rest of the
proof needs. First, type formers are injective. If
$forall^p (x : A). B equiv forall^q (x : A'). B'$, then $p = q$,
$A equiv A'$ and $B equiv B'$, the same holds for $Sigma$, and
$star(g) equiv star(h)$ only when $g equiv h$. Second, terms with
different head formers are not convertible. So #Ty is not #Da, since
$Q_1$ and $Q_2$ are distinct labels, and a function type is not an
enumeration. Many type theories get
these facts from the normalization of types. BendTT gets them from
confluence alone.

== Subject Reduction <sec:sr>

#thm[Theorem 2][Subject reduction][If the book is well typed,
$Gamma tack t : T$ and $t => u$, then $Gamma tack u : T$.]

The proof is by induction on the typing derivation, with a
substitution lemma and inversion through injectivity. A fit is read the
same way: where $U <= T$ and $T$ converts to a type former, $U$
converts to the same former, and their parts fit. So a $beta$ step
casts the argument along the fit of the domains, and the result along
the fit of the codomains. It holds for
reduction anywhere, dead positions included, and it does not need the
live check.

Parallel reduction does not preserve the live check. On a closed
term, the step from $lambda^2 x. (x, x)$ to $lambda^1 x. (x, x)$ leaves
a quantity-1 variable used twice. This is harmless, since the live
claims are about evaluation, and $|->$ never takes that step. Reduction
of open terms loses the live check in a second way. Take
$(lambda^2 x. (x, x)) space y$, where $y$ is an affine variable of type
`Nat`. It passes the live check, since it uses $y$ once. One step later
it is $(y, y)$, which uses $y$ twice. Quantitative type theory
@mcbride2016 @atkey2018 handles this case by scaling the uses of an
argument by the quantity of its binder. BendTT counts the argument once
and evaluates closed terms only. When a quantity-2 lambda fires in a
closed term, its argument is a closed #Da value. Its copies hold no
variable and no call.

== Progress

#thm[Theorem 3][Progress][If the book is well typed and live, and
$tack t : T$ with $t$ closed, then $t$ is a value or $t |-> u$ for
some $u$.]

The proof is by induction on the typing derivation, with a canonical
forms lemma for each type former. Two side conditions of the typing
rules matter here. The rules for matches require a live quantity, so
evaluation computes the argument of every match, and a match meets a
value. A copyable binder requires a #Da type, and a closed value of a
#Da type holds no lambda and no call.

#thm[Lemma 4][Empty][No value has the type #emp.]

A value of an enumeration type is one of its labels, and #emp has
none. Other values have other head formers, which are not convertible
to an enumeration.

== Termination

#thm[Theorem 5][Termination][If the book is live, and $t$ is closed and
passes the live check, then there is no infinite sequence
$t |-> t_1 |-> t_2 |-> ...$]

The theorem has no typing hypothesis. The proof gives each term a
measure that decreases at every step.

Each live call $k space a_1 ... a_n$ gets a _label_. The label is the
index of $k$ in the book, followed by one size per column. The size of
a column is the size of the live part of its argument when that
argument is a closed value. It is 0 when the column is erased, and
$omega$ when the argument is not yet a value. Labels are ordered by
index first, and then by sizes, from left to right, with every number
below $omega$. Each lambda, match, let, annotation, rewrite and meet in
a live position gets the least label. The _measure_ of a term is the multiset
of the labels of its live calls and nodes. Multisets are ordered as
Dershowitz and Manna define @dershowitzmanna1979: one element may be
replaced by any finite number of smaller ones. This order is well
founded because the label order is.

A step that does not fire a call removes a node with the least label.
It must not copy anything that has a label, and affinity ensures this.
When a quantity-1 lambda fires, its argument goes to at most one live
position, so its labels move and are not copied. When a quantity-2
lambda fires, its argument is a #Da value, which holds no labels. When
a quantity-0 lambda fires, its argument goes to dead positions, which
the measure ignores. The substitution may also turn the size $omega$
of some column into a number, which makes a label smaller.

When a call fires, its label is replaced by the labels in the leaf that
it reaches. These are calls to earlier definitions, which have a
smaller index, and self-calls. The arguments of a self-call rebuild
their columns, which keeps those sizes, until one is a strict piece,
which has a smaller size. So the label of the self-call is smaller,
whatever its later columns hold. The other labels in the leaf belong to
nodes and are the least.

The proof uses one property of affine code: a $beta$ step never
copies a call. Without affinity, a step could copy a call that has not
fired, and the multiset could grow. That is what the paradoxes of
@sec:paradox do.

== Consistency

#thm[Theorem 6][Soundness of the checker][If the checker accepts a
book, the book is well typed and live.]

The checker reduces types by lazy weak-head evaluation, and it compares
them one head at a time. Each of its steps is a step of
$scripts(=>)^*$, so a conversion that it finds is a conversion of the
declarative theory.

#thm[Theorem 7][Consistency][If the checker accepts a book, then no
definition $k : T$ of the book has a type $T equiv #emp$.]

Suppose that $k : T$ with $T equiv #emp$. By Theorem 6, the book is
well typed and live, so $tack k : #emp$, and $k$ passes the live check.
By Theorem 5, every evaluation sequence from $k$ is finite, so we
follow one to its end. By Theorem 2, every term on the way has type
#emp. By Theorem 3, the last term is a value, since it takes no step.
By Lemma 4, no value has type #emp.

Every step of this proof is syntactic. In systems such as the calculus
of constructions, consistency follows from the normalization of all
typed terms, types included. That proof interprets each type as a set
of terms, and it cannot work with #Ty : #Ty, where some typed terms
loop. BendTT gets termination from an untyped measure on live terms,
and it uses types only to rule out stuck terms.

= What Affinity Costs <sec:costs>

A closure may be called at most once, even when everything it captures
is #Da. So the usual `map` is rejected:

```bend
def map(f: Nat -> Nat, l: List<Nat>) -> List<Nat>:
  match l:
    case Nil{}:
      Nil{}
    case Con{h, t}:
      Con{f(h), map(f, t)}
```
```
Error:
- expected : f
- observed : f (consumed more than once)
Location: map
```

The function `f` is applied once and passed to the recursive call once.
There are three ways around this. First, the name of a definition is
not a variable, so a program may call a definition any number of
times. Second, a _template_ parameter, written `~f`, is replaced by its
argument at compile time. The base library's `List.map` takes its
function this way. A template argument must be closed, so the checker
rejects `List.map(~(x => (x + k : U32)), xs)` when `k` is a parameter.
Third, the loop can be its own definition, with the captured value as a
copyable parameter:

```bend
def addk(+k: U32, xs: List<U32>) -> List<U32>:
  match xs:
    case Nil{}:
      Nil{}
    case Con{h, t}:
      Con{(h + k : U32), addk(k, t)}
```

The same rule limits impredicative encodings. The Church numeral 2
uses its function twice:

```bend
def two(-A: Type, f: A -> A, x: A) -> A:
  f(f(x))
```
```
Error:
- expected : f
- observed : f (consumed more than once)
Context:
- A : Type
Location: two
```

So BendTT has impredicative function types, but live code cannot use
them to encode data that iterates a function. Bend uses the datatypes
of @sec:calculus instead.

To copy a structure, its type must be #Da, and a generic type is #Da
only when its elements are. The base library declares lists with a
quantity parameter:

```bend
type List<a, -A: Kind(a)> is Kind(a):
  Nil{}
  Con{head: A, tail: List<a, A>}
```

Here `Kind(&1)` is #Ty and `Kind(&2)` is #Da. So `List<&2, Nat>`,
written `+List<Nat>`, is #Da, and a list of functions is #Ty. In
BendTT, `a` is a variable of type $Qs$, and `List` has the kind
$star(a)$. So each generic definition goes to the kernel once, as
written. A kind may depend on any value, even one computed at run time.

Negative datatypes are fine, and so is an evaluator that applies each
closure once:

```bend
type Trm is Type:
  Lam{f: Trm -> Trm}
  Num{n: U32}

def app(t: Trm, x: Trm) -> Trm:
  match t:
    case Lam{f}:
      f(x)
    case Num{n}:
      Num{n}
```

A full normalizer for `Trm` must go on with the result of `f(x)`, which
is not a piece of `t`. So it needs an extra `Nat` argument that counts
down. The recursion rule has two more costs. Mutual recursion is not
allowed, so two functions that call each other become one definition
with an extra argument that selects which one runs. And a loop whose
arguments do not shrink, like the main loop of a server, also needs a
counter.

= Mechanization <sec:mech>

The kernel of BendTT, the statements of the theorems and their proofs
are one Lean 4 file @demoura2021, `bendtt.lean`, of about 4,000 lines. Every
proof is complete, and the file declares no axioms of its own, so the
proofs rest only on Lean's standard axioms. The file has three parts.
Part 1 is the kernel: terms, evaluation, conversion, the checker, the
live check, a parser for the text syntax, and a `main` function. Part 2
defines the declarative theory of @sec:meta and states the claims of
@tab:claims. Part 3 proves them.

#figure(kind: table, supplement: [Table], placement: none,
caption: [The claims of `bendtt.lean`.], {
  set text(size: 8.5pt)
  set par(justify: false)
  table(
    columns: (auto, 1fr),
    stroke: none,
    align: left,
    inset: (x: 3pt, y: 2.5pt),
    table.hline(stroke: 0.5pt),
    [*Claim*], [*Statement*],
    table.hline(stroke: 0.3pt),
    [#co[confluent]], [Theorem 1: parallel reduction is confluent.],
    [#co[sr]], [Theorem 2: parallel reduction preserves types.],
    [#co[progress]], [Theorem 3: a closed typed term is a value or steps.],
    [#co[empty]], [Lemma 4: no value has the empty type.],
    [#co[halts]], [Theorem 5: live evaluation of closed terms ends.],
    [#co[sound]], [Theorem 6: an accepted book is well typed and live.],
    [#co[consistent]], [Theorem 7: no accepted definition has the empty type.],
    table.hline(stroke: 0.5pt),
  )
}) <tab:claims>

The theorem `consistent` is about `Book.check`, the function that
`main` runs. So the theorem covers the checker that runs, and no model
stands between the two. Two parts sit outside the proof. The parser is
not verified. And Bend reaches BendTT through a translation, written in
TypeScript, that has no proof. The command `bend --verdict` runs this
translation and then the kernel. The command `bend -o F.bendtt` writes
the translated book to a file, where a reader can check what a law
states.

= Related Work <sec:related>

*Type in type.* Martin-Löf's first type theory had a type of all types
@martinlof1971, and Girard showed it inconsistent @girard1972. Coquand
studied the paradox in the calculus of constructions @coquand1986, and
Hurkens simplified it @hurkens1995. Howe showed that the paradox gives
a looping term @howe1987, and Meyer and Reinhold studied what it means
for type checking @meyerreinhold1986. Cardelli proposed #Ty : #Ty for a
programming language, where the lack of a logic is acceptable
@cardelli1986. BendTT keeps #Ty : #Ty and gets a logic back by
restricting the code that runs.

*Contraction.* In sequent calculus, the rule that uses a hypothesis
twice is contraction. Grishin showed that naive set theory is
consistent in a logic without contraction @grishin1982, and Terui built
a naive set theory on light affine logic @terui2004. Light, elementary
and soft logics restrict contraction to bound the cost of normalization
@girard1998 @asperti1998 @danosjoinet2003 @lafont2004 @baillotterui2009.
BendTT forbids contraction on functions, allows it on first-order data,
and adds structural recursion. It gets termination, but no bound on
cost, since the Ackermann function checks.

*Quantities.* Quantitative type theory @mcbride2016 @atkey2018, used in
Idris 2 @brady2021, and graded type theories @moon2021 @abel2023graded
put quantities on binders, as BendTT does. In these theories the
unrestricted quantity is available at every type, and an argument
passed to it has its uses scaled. In BendTT the copyable quantity is
available only at #Da, and an argument passed to it counts once.
Linear Haskell adds linear arrows to a language without dependent types
@bernardy2018. Linear dependent type theories go back to LLF
@cervesatopfenning2002 and to Krishnaswami et al. @krishnaswami2015.

*Two fragments.* Zombie @casinghino2014, from the Trellys project
@sjoberg2012, has a logical fragment that is consistent and a
programmatic fragment with general recursion. A classifier on each
judgment says which fragment a term is in. BendTT also has two parts,
with two differences. The position of a term decides its part, so the
programmer writes no classifier. And the unrestricted part of BendTT
does not run. The partial types of NuPRL also separate total from
partial computation @constablesmith1987.

*Erasure and impredicativity.* The implicit calculus of constructions
@miquel2001 and Cedille @stump2017 erase parts of terms and have
impredicative quantification. Cedille derives induction for
$lambda$-encoded data and proves consistency with a semantic model.
BendTT builds its datatypes from pairs and labels, affinity limits
its encodings, and its consistency proof is syntactic.

*Termination and positivity.* Coq and Agda require strict positivity
@coquandpaulin1988, since negative types break normalization
@mendler1991. Their termination checkers descend from guarded
definitions @gimenez1994 and from the foetus checker
@abelaltenkirch2002 @norell2007. Size-change termination @lee2001 and
sized types @hughes1996 @barthe2004 @abel2008 accept more programs.
BendTT's descent check is lexicographic, in the style of foetus. The
reason it is sound differs. In Coq and Agda, a termination check is
sound because the typed calculus normalizes, and that relies on
positivity and universes. In BendTT, the termination argument is
untyped, and it relies on affinity.

*Verified kernels.* MetaCoq @sozeau2020 and Lean4Lean @carneiro2024
verify parts of the kernels of large proof assistants, and Abel et al.
prove conversion decidable for a type theory in Agda @abel2018. These
projects must handle universe hierarchies and the normalization of
types. BendTT has neither.

= Conclusion <sec:conclusion>

BendTT keeps #Ty : #Ty, impredicative function types and negative
datatypes, and it is consistent. The code that runs must be affine,
copy only first-order data, and recurse on smaller arguments. Curry's and Girard's paradoxes both copy a function, and
affinity rejects that copy. Types are exempt from these rules because
they never run, and a match cannot look inside a dead value. Termination
then follows from a multiset measure on calls that ignores types, and
consistency follows from subject reduction and progress.

The main cost is that a closure may be called only once. Several
questions are open. BendTT has no semantic model yet. Its equality is
intensional. We do not know how far the descent check can be relaxed,
for example to size-change termination, while the termination argument
stays untyped.

#{
  show heading: set text(size: 12pt)
  set text(size: 7.5pt)
  set par(leading: 0.48em, spacing: 0.48em)
  bibliography("refs.bib",
    title: [References],
    style: "association-for-computing-machinery")
}
