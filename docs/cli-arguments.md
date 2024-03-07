# CLI arguments

It's possible to pass arguments to a program executed with `hvml run`:
```sh
hvml run <Path to program> [Arguments in expression form]...
```
It accepts any expression that would also be valid inside an hvm-lang function.

Arguments are passed to programs by applying them to the entrypoint function:
```js
main x1 x2 x3 = (MainBody x1 x2 x3)

// Calling with `hvml run <file> arg1 arg2 arg3`, it becomes:

main = (λx1 λx2 λx3 (MainBody x1 x2 x3) arg1 arg2 arg3)
```

The entrypoint function must receive exactly the number of arguments specified in the left-hand side of its definition.
```
// Must pass exactly 3 arguments when running
main x y z = (MainBody x y z)

// Can't receive CLI arguments
main = λx λy λz (MainBody x y z)
```