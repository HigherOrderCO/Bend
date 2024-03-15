# Builtin Definitions

HVM-lang includes some built-in definitions that can make certain tasks easier, such as debugging, benchmarking, etc. These definitions are "special", and can't be implemented using regular HVM code.

### On errors.

Some of the functions may error. `hvm-lang` defines a builtin `Result` ADT which looks like this:

```
data Result = (Ok val) | (Err val)
```

This allows hvm-lang to do basic error handling. Right now, `val` is meant to be opaque; `val`'s type is not stable and might change from one update to another.

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

## `HVM.query`

`HVM.query` reads a line from standard input, and calls its argument with a `Result` that might contain a string containing the user's input. It expects the standard input to be valid utf-8.

Here's an example program using `HVM.query`
```rs
Join (String.nil) x = x
Join (String.cons head tail) x = (String.cons head (Join tail x))
main = (((HVM.print "What's your name?") HVM.query) λresult match result {
	(Result.ok name): (HVM.print (Join "Hi, " (Join name "!")) *)
	(Result.err err): err
})
```
This program also shows using the return value of `HVM.print` (which is the identity function) to block `HVM.query` from reducing too early. If we used a naive version of the program, which is this:
```rs
main = (HVM.print "What's your name?" (HVM.query λresult match result {
	(Result.ok name): (HVM.print (Join "Hi, " (Join name "!")) *)
	(Result.err err): err
}))
```
We would get asked our name after typing it in.

## `HVM.store`

`HVM.store` writes a string to a file, and calls its return value with a `Result` (the `Result`, if it's `Result.ok`, contains `ERA`).

Example:

```rs
main = (HVM.store "file.txt" "These are the file contents" λres match res {
	(Ok *): "Return value of program"
	(Err err): err
})
```

## `HVM.load`

`HVM.load`, given a filename, reads the file and passes a `Result` that might contain the contents of the file to its argument

```rs
Join (String.nil) x = x
Join (String.cons head tail) x = (String.cons head (Join tail x))
main = (HVM.load "file.txt" λres match res {
	(Result.ok contents): (HVM.print (Join "File contents: " contents) "Program return value")
	(Result.err err): err
})
```

## `HVM.exit`

`HVM.exit` terminates the processes with the status code determined by its argument.

```rs
main = (HVM.exit #42)
// Outputs nothing, but `hvml`'s status code will be 42.
```

On failure, it terminates the process with a -1 exit status code.

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