# Passes

|  flag          | Default  | What it does? |
|----------------|----------|---------------|
| `-Oall`        | Disabled | Enables all compiler passes |
| `-Ono-all`     | Disabled | Disables all compiler passes |
| `-Oeta` `-Ono-eta` | Disabled | [eta-reduction](#eta-reduction) |
| `-Oref-to-ref` `-Ono-ref-to-ref` | Disabled | [ref-to-ref](#ref-to-ref) |
| `-Oprune` `-Ono-prune` | Disabled | [definition-pruning](#definition-pruning) |
| `-Opre-reduce` `-Ono-pre-reduce` | Disabled | [pre-reduce](#pre-reduce) |
| `-Osupercombinators` `-Ono-supercombinators` | Enabled  | [supercombinators](#supercombinators) |
| `-Osimplify-main` `-Ono-simplify-main` | Disabled | [simplify-main](#simplify-main) |
| `-Ocross-refs` `-Ono-cross-refs` | Disabled | [cross-refs](#cross-refs) | 

## Eta-reduction

Enables or disables Eta Reduction for defined functions.

Eta reduction simplifies lambda expressions, removing redundant parameters. [See also](https://wiki.haskell.org/Eta_conversion).

Example:
```rs
// program
id = λx x
id_id = λx (id x)

// -Oeta
id_id = id

// -Ono-eta
id_id = λz (id z)
```

## Ref-to-ref

If enabled, When a function that is simply directly calling another function, substitutes all occurrences of that function to the one being called.

Example:
```rs
// program
Foo = λ* 0

Bar = Foo

Main = (Bar *)

// -Oref-to-ref
Foo = λ* 0

Bar = Foo

Main = (Foo *) // Call `Bar` is resolved to a call to `Foo`.

// -Ono-ref-to-ref
(Foo) = λ* 0

(Bar) = Foo

(Main) = (Bar *)
```

## Definition-pruning

If enabled, removes all unused definitions.

Example:
```rs
// program
Id = λx x

Id2 = Id

Main = (Id 42)

// -Oprune
Id = λx x

Main = (Id 42)

// -Ono-prune
Id = λx x

Id2 = Id

Main = (Id 42)
```

## Pre-reduce

Normalizes all functions, solving annihilations and commutations, except main.

Example:
```rs
// program
id = λx x
foo = (id 42)
main = foo

// -Opre-reduce, compilation output
@foo = #42
@id = (a a)
@main = @foo

// -Ono-pre-reduce, compilation output
@foo = a
& @id ~ (#42 a)
@id = (a a)
@main = @foo
```

## Supercombinators

Extracts closed terms to new definitions. See [lazy definitions](lazy-definitions#automatic-optimization).
Since HVM-Core is an eager runtime, this pass is enabled by default to prevent infinite expansions.

Example:
```rs
True  =    λt λf λm t
False =    λt λf λm f
Maybe = λx λt λf λm (m x)

getVal = λb (b 1 0 (λx (== x 1)))
// `(λx (== x 1))` can be extracted, since there is no free variables.

Cons = λh λt λc λn (c h t)
Nil  = λc λn n

fold = λinit λf λxs (xs λh λt (fold (f init h) f t) init)
// Here we need to extract `λh λt (fold (f init h) f t)` to not expand `fold` infinitely, but it will not be extracted because of the free variable `init`.
```

## Simplify-main

If enabled, when directly calling another function, substitute the ref with a copy of its body.

Example:
```
// program
id = λx x
main = id

// -Osimplify-main
@id = (a a)
main = (a a)

// -Ono-simplify-main
@id = (a a)
@main = @id
```

## Cross-refs

Is triggered only if the [pre-reduce](#pre-reduce) option is enabled.

If enabled, will dereference and try to find the smallest net.

Example:
```rs
foo = (+ 2 2)

bar = (+ foo foo)

main = (bar)

// -Ocross-refs, compilation output
@bar = #8
@foo = #4
@main = @bar

// -Ono-cross-refs, compilation output
@bar = a
& @foo ~ <+ @foo a>
@foo = #4
@main = @bar
```
