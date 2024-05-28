# Options

| flag                                                                     | Default       | What it does?                             |
| ------------------------------------------------------------------------ | ------------- | ----------------------------------------- | --- |
| `-Oall`                                                                  | Disabled      | Enables all compiler passes               |
| `-Ono-all`                                                               | Disabled      | Disables all compiler passes              |
| `-Oeta` `-Ono-eta`                                                       | Disabled      | [eta-reduction](#eta-reduction)           |
| `-Oprune` `-Ono-prune`                                                   | Disabled      | [definition-pruning](#definition-pruning) |
| `-Olinearize-matches` `-Olinearize-matches-alt` `-Ono-linearize-matches` | Enabled       | [linearize-matches](#linearize-matches)   |
| `-Ofloat_combinators` `-Ono-float_combinators`                           | Enabled       | [float-combinators](#float-combinators)   |
| `-Omerge` `-Ono-merge`                                                   | Disabled      | [definition-merging](#definition-merging) |
| `-Oinline` `-Ono-inline`                                                 | Disabled      | [inline](#inline)                         |
| `-Ocheck-net-size` `-Ono-check-net-size`                                 | Disabled      | [check-net-size](#check-net-size)         |
| `-Oadt-scott` `-Oadt-num-scott`                                          | adt-num-scott | [adt-encoding](#adt-encoding)             |     |

## Eta-reduction

Enables or disables Eta Reduction for defined functions.

Eta reduction simplifies lambda expressions, removing redundant parameters. [See also](https://wiki.haskell.org/Eta_conversion).

Example:

```py
# program
id = λx x
id_id = λx (id x)

# -Oeta
id_id = id

# -Ono-eta
id_id = λz (id z)
```

## Definition-pruning

If enabled, removes all unused definitions.

Example:

```py
# program
Id = λx x

Id2 = Id

Main = (Id 42)

# -Oprune
Id = λx x

Main = (Id 42)

# -Ono-prune
Id = λx x

Id2 = Id

Main = (Id 42)
```

## Definition-merging

If enabled, merges definitions that are identical at the term level.

Example:

```py
# Original program
id = λx x
also_id = λx x
main = (id also_id)

# After definition merging
id_$_also_id = λx x
main = (id also_id)

# -Ono-merge, compilation output
@also_id = (a a)
@id = (a a)
@main = a
& @id ~ (@also_id a)

# -Omerge, compilation output
@a = (a a)
@main = a
& @a ~ (@a a)
```

## linearize-matches

Linearizes the variables between match cases, transforming them into combinators when possible.

```py
# Linearization means going from this
@a @b switch a {
  0: (Foo b)
  _: (Bar a-1 b)
}
# To this
@a @b (switch a {
  0: @b (Foo b)
  _: @b (Bar a-1 b)
} b)
```

When the `linearize-matches` option is used, only linearizes variables that would generate an eta-reducible application.

Example:

```py
λa λb switch a { 0: b; _: b }

# Is transformed to
λa switch a { 0: λb b; _: λb b }

# But this stays the same
λa λb switch b { 0: a; _: a }
```

When the `linearize-matches-extra` option is used, it linearizes all variables used in the arms.

example:

```py
λa λb λc switch b { 0: a; _: c }

# Is transformed to (without eta-reducing 'c')
λa λb λc (switch b { 0: λa λc a; _: λa λc c } a c)
```

These automatic linearization passes are done before the manual linearization from `with` and doesn't duplicate manually linearized variables.

```py
# These variables are only linearized once
λa λb λc switch a with b c { 0: (b c); _: (a-1 b c) }

# With -Olinearize-matches becomes
λa λb λc (switch a { 0: λb λc (b c); _: λb λc (a-1 b c) } b c)

# And not
λa λb λc (switch a { 0: λb λc λb λc (b c); _: λb λc λb λc  (a-1 b c) } b c b c)
```

## float-combinators

Extracts closed terms to new definitions. See [lazy definitions](lazy-definitions.md#automatic-optimization).
Since HVM-Core is an eager runtime, this pass is enabled by default to prevent infinite expansions.

Example:

```py
True  =    λt λf λm t
False =    λt λf λm f
Maybe = λx λt λf λm (m x)

getVal = λb (b 1 0 (λx (== x 1)))
# `(λx (== x 1))` can be extracted, since there is no free variables.

Cons = λh λt λc λn (c h t)
Nil  = λc λn n

fold = λinit λf λxs (xs λh λt (fold (f init h) f t) init)
# Here we need to extract `λh λt (fold (f init h) f t)` to not expand `fold` infinitely, but it will not be extracted because of the free variable `init`.
```

# Inline

If enabled, inlines terms that compile to nullary inet nodes (refs, numbers, erasures).

Example:

```py
# program
foo = 2
id = λx x
main = (id foo)

# -Ono-inline, compilation output
@foo = 2
@id = (a a)
@main = a
& @id ~ (@foo a)

# -Oinline, compilation output
@foo = 2
@id = (a a)
@main = a
& @id ~ (2 a)
```

## Check-net-size

If enabled, checks that the size of each function after compilation has at most 64 HVM nodes.
This is a memory restriction of the CUDA runtime, if you're not using the `*-cu` you can disable it.

Example:

```py
# Without -Ocheck-net-size compiles normally.
# But with -Ocheck-net-size it fails with
# `Definition is too large for hvm`
(Radix n) =
  let r = Map_/Used
  let r = (Swap (& n 1) r Map_/Free)
  let r = (Swap (& n 2) r Map_/Free)
  let r = (Swap (& n 4) r Map_/Free)
  let r = (Swap (& n 8) r Map_/Free)
  let r = (Swap (& n 16) r Map_/Free)
  let r = (Swap (& n 32) r Map_/Free)
  let r = (Swap (& n 64) r Map_/Free)
  let r = (Swap (& n 128) r Map_/Free)
  let r = (Swap (& n 256) r Map_/Free)
  let r = (Swap (& n 512) r Map_/Free)
  let r = (Swap (& n 1024) r Map_/Free)
  let r = (Swap (& n 2048) r Map_/Free)
  let r = (Swap (& n 4096) r Map_/Free)
  let r = (Swap (& n 8192) r Map_/Free)
  let r = (Swap (& n 16384) r Map_/Free)
  let r = (Swap (& n 32768) r Map_/Free)
  let r = (Swap (& n 65536) r Map_/Free)
  let r = (Swap (& n 131072) r Map_/Free)
  let r = (Swap (& n 262144) r Map_/Free)
  let r = (Swap (& n 524288) r Map_/Free)
  let r = (Swap (& n 1048576) r Map_/Free)
  let r = (Swap (& n 2097152) r Map_/Free)
  let r = (Swap (& n 4194304) r Map_/Free)
  let r = (Swap (& n 8388608) r Map_/Free)
  r
```

## ADT Encoding

Selects the lambda encoding for types defined with `type` and `object`.

`-Oadt-scott` uses Scott encoding.
`-Oadt-num-scott` uses a variation of Scott encoding where instead of one lambda per constructor, we use a numeric tag to indicate which constructor it is. The numeric tag is assigned to the constructors in the order they are defined and each tag is accessible as a definition by `<type>/<ctr>/tag`.

```py
# Generates functions Option/Some and Option/None
type Option:
  Some { value }
  None

# With -Oadt-scott they become:
Option/Some = λvalue λSome λNone (Some value)
Option/None = λSome λNone (None)

# With -Oadt-num-scott they become:
Option/Some = λvalue λx (x Option/Some/tag value)
Option/None = λx (x Option/None/tag)

# Generated -Oadt-num-scott tags:
Option/Some/tag = 0
Option/None/tag = 1
```

Pattern-matching with `match` and `fold` is generated according to the encoding.

Note: IO is **only** available with `-Oadt-num-scott`.
