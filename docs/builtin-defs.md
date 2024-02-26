# Builtin Definitions

HVM-lang includes some built-in definitions that can make certain tasks easier, such as debugging, benchmarking, etc. These definitions are "special", and can't be implemented using regular HVM code.

## `HVM.log`

Reads back its first argument and displays it on stdout, then returns its second argument. 

```rs
main = (HVM.log "Hi" "Hello world")

// Program output:
"Hi"
"Hello world"
```

This function can be very useful for debugging, however, it's somewhat limited. For example, `λx (HVM.log x 0)` won't readback correctly

```rs
main = λx(HVM.log x "Hello world")

// Program output:
Reached Root
<Invalid>
λ* 0
```

This will happen whenever there are free variables inside the logged term, or [scopeless lambdas](using-scopeless-lambdas.md) which bind variables that are used outside the logged term.

## `HVM.print`

This is very similar to `HVM.log`. However, it can only log strings. It prints these strings directly to stdout, without wrapping them in quotes or escaping them.

With `HVM.log`:
```rs
main = (HVM.log "Hi" "Hello world")

// Program output:
"Hi"
"Hello world"
```
However, with `HVM.print`:
```rs
main = (HVM.print "Hi" "Hello world")

// Program output:
Hi
"Hello world"
```

This can be very useful, for example, to print strings with newlines in multiple lines.

When the first argument is not a string `HVM.print` returns the second argument and no side effects are produced (In other words, it fails silently).

## `HVM.black_box`

`HVM.black_box` is simply the identity function, but it does not get [pre-reduced](compiler-options.md#pre-reduce). This makes it possible to prevent some redexes from getting pre-reduced. 

Example without `HVM.black_box`
```rs
foo = (* 30 40)
// Compilation output
@foo = #1200
```
Example with `HVM.black-box`

```rs
foo = (* (HVM.black_box 30) 40) // This is normal form
// Compilation output
@foo = a & @HVM.black_box ~ (#30 <* #40 a>)
```