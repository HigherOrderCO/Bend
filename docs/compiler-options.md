# Options

|  flag          | Default  | What it does? |
|----------------|----------|---------------|
| `-Oall`        | Disabled | Enables all compiler passes |
| `-Ono-all`     | Disabled | Disables all compiler passes |
| `-Oeta` `-Ono-eta` | Disabled | [eta-reduction](#eta-reduction) |
| `-Oprune` `-Ono-prune` | Disabled | [definition-pruning](#definition-pruning) |
| `-Olinearize-matches` `-Olinearize-matches-alt` `-Ono-linearize-matches` | Enabled  | [linearize-matches](#linearize-matches) |
| `-Ofloat_combinators` `-Ono-float_combinators` | Enabled  | [float-combinators](#float-combinators) |
| `-Omerge` `-Ono-merge` | Disabled | [definition-merging](#definition-merging) |
| `-Oinline` `-Ono-inline` | Disabled | [inline](#inline) |

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

Normalizes all functions except main, dereferencing definitions in active positions, and solving annihilations and commutations.

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

## linearize-matches

Linearizes the variables between match cases, transforming them into combinators when possible.
```rs
// Linearization means going from this
@a @b switch a {
  0: (Foo b)
  _: (Bar a-1 b)
}
// To this
@a @b (switch a {
  0: @b (Foo b)
  _: @b (Bar a-1 b)
} b)
```

When the `linearize-matches` option is used, only linearizes variables that would generate an eta-reducible application.

Example:
```rs
λa λb switch a { 0: b; _: b }

// Is transformed to
λa switch a { 0: λb b; _: λb b }

// But this stays the same
λa λb switch b { 0: a; _: a }
```

When the `linearize-matches-extra` option is used, it linearizes all vars used in the arms.

example:
```rs
λa λb λc switch b { 0: a; _: c }

// Is transformed to (without eta-reducing 'c')
λa λb λc (switch b { 0: λa λc a; _: λa λc c } a c)
```

These automatic linearization passes are done before the manual linearization from `with` and doesn't duplicate manually linearized variables.

```rs
// These variables are only linearized once
λa λb λc switch a with b c { 0: (b c); _: (a-1 b c) }

// With -Olinearize-matches-extra becomes
λa λb λc (switch a { 0: λb λc (b c); _: λb λc (a-1 b c) } b c)

// And not
λa λb λc (switch a { 0: λb λc λb λc (b c); _: λb λc λb λc  (a-1 b c) } b c b c)
```

## float-combinators

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
```rust
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
