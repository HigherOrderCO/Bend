# CLI arguments

It's possible to pass arguments to a program executed with `hvml run`:

```sh
hvml run <Path to program> [Arguments in expression form]...
```

It accepts any expression that would also be valid inside an hvm-lang function.

Arguments are passed to programs by applying them to the entrypoint function:

```js
main x1 x2 x3 = (MainBody x1 x2 x3)

// Calling with `hvml run <file> arg1 arg2 arg3 argN`, it becomes:

main = (λx1 λx2 λx3 (MainBody x1 x2 x3) arg1 arg2 arg3 argN)
```

There are no restrictions on the number of arguments passed to the program.

```rust
// Can receive 2 CLI arguments
main x y = (+ x y)

// Can't receive CLI arguments
main = λx λy (+ x y)

// Calling with just one argument
hvml run <path> 5
λa (+ a 5)
```
