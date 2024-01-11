# Writing fusing functions
## Church encoding
Church Encoding is a way to encode common datatypes as λ-calculus terms. For example, here is a [Church-encoded](https://en.wikipedia.org/wiki/Church_encoding) boolean type in HVM
```rs
true = λt λf t
false = λt λf f
```
Matching on values of this representation is simply calling the boolean value with what the function should return if the boolean is true and if the boolean is false.
```rs
if boolean case_true case_false = (boolean case_true case_false)
main = (if true 42 37) 
// Outputs 42

// Or alternatively:
// if boolean = boolean
// Each boolean is represented by its own matching function
// so (true 42 37) will do the same thing.
```

This is how  a`Not` function that acts on this encoding can be defined
```rs
not = λboolean (boolean false true)
main = (not true) // Outputs λtλf f.
main = (not false) // Outputs λtλf t.
```
If the boolean is `true`, then the function will return `false`. If it is `false`, it will return `true`.

## Self-application

What happens if we self-compose the `not` function? It is a well known fact that`(not (not x)) == x`, so we should expect something that behaves like the identity function.
```rs
main = λx (not (not x))
// Output:
// λa (a λ* λb b λc λ* c λ* λd d λe λ* e)
```
The self-application of `not` outputs a large term. Testing will show that the term does indeed behave like an identity function. However, since the self-application of `not` is larger than `not` itself, if we self-compose this function many times, our program will get really slow and eat up a lot of memory, despite all functions being equivalent to the identity function:
```rs
main = λx (not (not x)) // Long
main = λx (not (not (not (not x)))) // Longer
main = λx (not (not (not (not (not (not (not (not x)))))))) // Longer
// etc...
```
The self-application of not a large number of times, such as 65536 or 4294967296, will be large enough to slow down our computer by a significant amount.

Luckily, there's a trick we can do to make the self-application of `not` much shorter. The trick is to rewrite `not` in another way that makes the self-composition of `not` much smaller. This trick is called "fusing". Here is how it's done.

### Fusing functions
Let's first take our initial `not` implementation.
```rs
not = λboolean (boolean false true)
```
We begin by replacing `false` and `true` by their values.
```rs
not = λboolean (boolean λtλf(f) λtλf(t))
```
After doing this, it's easy to notice that there's something that both terms have in common. Both of them are lambdas that take in two arguments. We can **lift** the lambda arguments up and make them **shared** between both cases.
```rs
not = λbooleanλtλf (boolean f t)
```
Let's see how the self-application of `not` gets reduced now. Each line will be a step in the reduction.
```rs
main = λx (not (not x))
main = λx (not (λbooleanλtλf (boolean f t) x))
main = λx (not (λtλf (boolean x f t)))
main = λx (λboolean1λt1λf1 (boolean1 f1 t1) (λtλf (boolean x f t)))
main = λxλt1λf1 (λtλf (x f t) f1 t1)
main = λxλt1λf1 (λf (x f f1) t1))
main = λxλt1λf1 (x f t1)
```
Wow! Simply by replacing lambda arguments with the values applied to them, we were able to make `(not (not x))` not grow in size. This is what fusing means, and it's a really powerful tool to make programs faster.

Fusing isn't only for Church-encoded `not`. Fusing can be done anywhere where efficient composition is important. Broadly speaking, a good rule of thumb in HVM is "push lambdas to the top and duplications to the bottom".

To show the power of fusing, here is a program that self-composes `fusing_not` 2^512 times and prints the result. `2^512` is larger than amount of atoms in the observable universe, and yet HVM is still able to work with it due to its optimal sharing capabilities. 

This program uses [native numbers, which are described here](native-numbers.md).
```rs
true = λt λf t
false = λt λf f
not = λboolean (boolean false true)
fusing_not = λboolean λt λf (boolean f t)
// Creates a Church numeral out of a native number
to_church 0 = λf λx x
to_church +p = λf λx (f (to_church p f x))
main = 
	let two = λf λx (f (f x))
	let two_pow_512 = ((to_church 512) two) // Composition of church-encoded numbers is equivalent to exponentiation.
	// Self-composes `not` 2^512 times and prints the result.
	(two_pow_512 fusing_not)  // try replacing this by not. Will it still work?
```
Here is the program's output:
```bash
$ hvml run -s fuse_magic.hvm 
λa λb λc (a b c)

RWTS   : 15374
- ANNI : 8193
- COMM : 5116
- ERAS : 521
- DREF : 1031
- OPER : 513
TIME   : 0.002 s
RPS    : 9.537 m
```
Only 15374 rewrites! Fusing is really powerful.
