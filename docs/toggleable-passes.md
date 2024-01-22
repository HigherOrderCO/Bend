# Passes

|  flag          | Default  | What it does? |
|----------------|----------|---------------|
| `-Oall`        | Disabled | Enables all compiler passes |
| `-Ono-all`     | Disabled | Disables all compiler passes |
| `-Oeta` `-Ono-eta` | Disabled | [eta-reduction](#eta-reduction) |
| `-Oref-to-ref` `-Ono-ref-to-ref` | Disabled | [ref-to-ref](#ref-to-ref) |
| `-Oprune` `-Ono-prune` | Disabled | [definition-prununing](#definition-pruning) |
| `-Opre-reduce` `-Ono-pre-reduce` | Disabled | [pre-reduce](#pre-reduce) |
| `-Osupercombinators` `-Ono-supercombinators` | Enabled  | [supercombinators](#supercombinators) |
| `-Osimplify-main` `-Ono-simplify-main` | Disabled | [simplify-main](#simplify-main) |

## Eta-reduction

Enables or disables Eta Reduction for compiler generated functions.

Example:
```rs
// -Oeta
some$generated$fun = other_fun

// -Ono-eta
some$generated$fun = λx (other_fun x)
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

Main = (Foo *)

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

Reduces compiled inets, solving annihilations and commutations.

Example:
```
// program
id = λx x
foo = (id 42)
main = foo

// -Opre-reduce
@foo = #42
@id = (a a)
@main = @foo

// -Ono-pre-reduce
@foo = a
& @id ~ (#42 a)
@id = (a a)
@main = @foo
```

## Supercombinators

Extracts closed terms to new definitions. See [lazy definitions](lazy-definitions#automatic-optimization).
Since HVM-Core is an eager runtime, this pass is enabled by default to prevent infinite expansions.

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
