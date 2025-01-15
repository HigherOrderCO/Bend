# Using scopeless lambdas

Scopeless lambdas are very powerful lambdas that are a side-effect of HVM's internal representation for lambda terms.

Scopeless lambdas are lambdas that have no scope. The variables bound by them can be used outside the lambda's body. They can be created by prefixing a dollar symbol (`$`) to a lambda's variable name.

```py
λ$x $x # The identity function as a scopeless lambda
```

Of course, using scopeless lambdas as a replacement for regular lambdas is kind of pointless. Their real power comes from being able to use the bound variable outside the body:

```py
main = (((λ$x 1) 2), $x)
# $x gets replaced by 2 and the application ((λ$x 1) 2) gets replaced by 1
# Outputs (1, 2)
```
In the imp syntax, scopeless lambdas can be written in the following way:
```py
def main() -> _:
  # This is the equivalent code to the above example
  # Notice that in the imp syntax, you scopeless lambdas are written as `lambda $x: 1` instead of `λ$x 1`.
  f = lambda $x: 1
  return (f(2), $x)
```

Take some time to think about the program above. It is valid, despite `$x` being used outside the lambda's body.

However, scopeless lambdas don't bind across definitions.
```py
def = $x

main = (((λ$x 1) 2), def)
```

The bound variables are local to each term.

## Duplicating scopeless lambdas

We have seen that the variable bound to a scopeless lambda gets set when the lambda is called. But, what happens if we never call `λ$x 1`? What will `$x` get set to then? Here is a program that does that:

```py
main =
	let _ = λ$x 1 # Discard and erase the scopeless lambda
	(2, $x)

# Outputs (2, *)
```

The program outputs `2` as the first item of the tuple, as expected. But the second item is `*`! What is `*`?

`*` (called ERA or eraser) is a special term HVM uses when a value was erased. This is what happened to `$x`. We erased `λ$x 1` when we discarded it, which led to `$x` being erased.

What happens if we call `λ$x 1` with two different values instead? 

Try to answer this with your knowledge of HVM. Will it throw a runtime error? Will it return something unexpected?

```py
main =
	let f = λ$x 1 # Assign the lambda to a variable
	((f 2), ((f 3), $x)) # Return a tuple of (f 2) and another tuple.

# Outputs (1, (1, {2 3}))
```

What? This is even more confusing. The first two values are `1`, as expected. But what about the last term?

The last term in the tuple is a **superposition** of two values. A [superposition](dups-and-sups.md) is the "other side" of a duplication. It is created here because we implicitly duplicated `f` when we used it twice, and duplicating lambdas creates superpositions.

When implicitly duplicating a lambda, the order of the arguments is left to the compiler's discretion. So it's possible that depending on the context of your program, the order of the arguments on the superposition might be different than expected. If you want to make sure that your duplications come out in a specific order, you need to explicitly duplicate the lambda.
## Usage

Now that we know how scopeless lambdas work, we can make programs using them. An example of a function that is usually thought as "primitive", but can be implemented using scopeless lambdas is [call/cc](http://www.madore.org/~david/computers/callcc.html)

Call/cc is a function that takes a function that takes a parameter `k`. When `k` is called with an argument, `callcc` returns it.

```py
# Function that discards its second argument
Seq a b = a

# Create a program capable of using `callcc`
CC.lang = λprogram
  let callcc  = λcallback (λ$garbage($hole) (callback λ$hole(0)));
  let result  = (program callcc);
  (Seq result $garbage)

Main = (CC.lang λcallcc 
  # This code calls `callcc`, then calls `k` to fill the hole with `42`. This means that the call to callcc returns `42`, and the program returns `52`. (+ (k 42) 1729) is garbage and is erased.
  (+ 10 (callcc λk(+ (k 42) 1729)))
)
```