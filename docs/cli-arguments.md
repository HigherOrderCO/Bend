# CLI arguments

It's possible to pass arguments to a program executed with `bend run`:

```sh
bend run <Path to program> [Arguments in expression form]...
```

It accepts any expression that would also be valid inside a bend function.

Arguments are passed to programs by applying them to the entrypoint function:

```py
# Imp syntax
def main(x1, x2, x3):
  return MainBody(x1 x2 x3)

# Calling with `bend run <file> arg1 arg2 arg3 argN`, it becomes (in the "fun" syntax):
main = (x1 λx2 λx3 (MainBody x1 x2 x3) arg1 arg2 arg3 argN)
```

There are no restrictions on the number of arguments passed to the program.
You can even pass more arguments than the function expects, although that can lead to unexpected results.
```py
# Expects 2 CLI arguments
def main(x, y):
  return {x - y, y - x}
```
```sh
# Calling with just one argument
> bend run <path> +5
λa {(- a 5) (- a +5)}

# Calling with two argument
> bend run <path> +5 +3
{+2 -2}

# Calling with three argument
# In this case, the third argument doesn't change anything
# due to the underlying interaction rules.
# If this were a variant of simply-typed lambda-calculus
# it wouldn't be well-typed.
> bend run <path> +5 +3 +1
{+2 -2}
```
