# Options

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
| `-Omerge` `-Ono-merge` | Disabled | [definition-merging](#definition-merging) |
| `-Oinline` `-Ono-inline` | Disabled | [inline](#inline) |
| `-e` `--entrypoint` | `Main \| main` | [entrypoint](#entrypoint) |

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

## Definition-merging

If enabled, merges definitions that are identical at the term level.

Since it runs on hvm-lang terms, it does not merge inets that become identical after [compile-time reduction](#pre-reduce).

Example:
```rs
// Original program
id = λx x
also_id = λx x
main = (id also_id)

// After definition merging
id_$_also_id = λx x
main = (id also_id)

// -Ono-merge, compilation output
@also_id = (a a)
@id = (a a)
@main = a
& @id ~ (@also_id a)

// -Omerge, compilation output
@a = (a a)
@main = a
& @a ~ (@a a)
```

## Pre-reduce

Normalizes all functions except main, dereferencing definitions in active positions, and solving annihilations and commutations. It does not reduce [builtin definitions](builtin-defs.md), such as `HVM.log`.

Example:
```rs
foo = (+ 2 2)

bar = (+ foo foo)

main = (bar)

// -Opre-reduce, compilation output
@bar = #8
@foo = #4
@main = @bar
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
```rs
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

# Inline

If enabled, inlines terms that compiles to 0 or 1 inet nodes at lambda level, before pre reduction.

Example:
```rs
foo = (2, 3)
id = λx x

main = (id foo)

// -Oinline, compilation output
@foo = [#2 #3]
@id = (a a)

@main = a
& (b b) ~ ([#2 #3] a)
```

## Entrypoint

If given the option, use another definition as entrypoint rather than `main` or `Main`.

> By default, HVM-Lang searches for a function named either `main` or `Main` to use as entrypoint to a program, but it is possible to use a different entrypoint with the `-e --entrypoint` option.

Example:
```
// program
Main = (λx x λx x)

// compilation output, both `Main` and `main` compile to a `@main` definition
@main = a
& (b b) ~ ((c c) a)

// program
// Normally would fail as the program does not have a `main` definition.
run = (λx x λx x)

// compilation output using `--entrypoint run`.
// No `@main` definition is created.
@run = a
& (b b) ~ ((c c) a)
```
