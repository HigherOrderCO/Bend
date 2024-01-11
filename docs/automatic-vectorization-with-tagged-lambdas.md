# Automatic vectorization with tagged lambdas

We have seen in [Dups and Sups](dups-and-sups.md) that duplications and superpositions can have labels. In HVM, lambdas and applications can have labels too.  

Tagged applications will only annihilate lambdas with the same tag.
```rs
(#A λ#A x(body) arg)
// Reduces to
x = arg; body
```

For example, data types can be encoded as tagged lambdas:

```rs
// data Bool = T | F
T = λ#Bool t λ#Bool f t
F = λ#Bool t λ#Bool f f

// data List = (Cons x xs) | Nil
Cons = λx λxs λ#List c λ#List n (#List.Cons.xs (#List.Cons.x c x) xs)
Nil  =        λ#List c λ#List n n
```

When encoding the pattern matching, the application can then use the same label:

```rs
// not = λbool match bool { T: (F) F: (T) } 
not = λbool (#Bool bool F T)
```

In fact, `match` is syntax sugar for a tagged application like the one above. This means that it is not possible to match without using tagged applications.

When an application and a lambda with different tags interact, the application "commutes" through the lambda. Here is how it roughly works:

```rs
(#A λ#B x (b x) a)
// Reduces to
λ#B c (#A (b λ#A $d c) (#B a $d))
```

This reduction can be hard to grasp, but an accurate way to understand it is that "the application goes through the lambda".

This allows, in some limited scenarios, automatic vectorization. See "limitations" for a description of the limitations.
```rs
// vectorizes to: (Cons F (Cons T (Cons F Nil)))
main = (not (Cons T (Cons F (Cons T Nil))))
```
This works because the `Bool`-tagged application in `not` passes through the `List`-tagged lambdas in `Cons` until it gets to `T` and `F`.

The tagged lambda and applications are compiled to `inet` nodes with different tag values for each data type. This allows them to commute, read [HVM-Core](https://github.com/HigherOrderCO/hvm-core/tree/main#language) to learn more about it.

### Limitations
- The function must not be recursive
- There must not be labels in common between the function and what you want to vectorize over