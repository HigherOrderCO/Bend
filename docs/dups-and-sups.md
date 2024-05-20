# Dups and sups

Term duplication is done automatically when a variable is used more than once. But it's possible to manually duplicate a term using `let`. This type of statement is called `dup` or `duplication`.
```py
# the number 2 in church encoding using let.
ch2 = λf λx let {f1 f2} = f; (f1 (f2 x))

# the number 3 in church encoding using let.
ch3 = λf λx let {f0 f1} = f; let {f2 f3} = f0; (f1 (f2 (f3 x)))
```

A `sup` is a superposition of two values, it is defined using curly brackets with two terms inside. A superposition is the opposite of a duplication. 
```py
sup = {3 7}
```

Sups can be used anywhere a value is expected, if anything interacts with the superposition, the result is the superposition of that interaction on both the possible values:

```py
mul = λa λb (* a b)
result     = (mul 2 5)         # returns 10
result_sup = (mul 2 {5 7})     # returns {10 14}
multi_sup  = (mul {2 3} {5 7}) # returns {{10 14} {15 21}}
```

If we pair a superposition with a duplication, the result is that they behave like constructing and destructing a pair:

```py
# each dup variable now has a copy of the {1 2} superposition
let {x1 x2} = {1 2}
```

Due to how duplications are compiled, when two dups interact, they destructively interfere with each other.
In this case the result doesn't follow the expected behavior (it's well defined at the HVM level, but doesn't is incorrect at a lambda-calculus level).

That imposes a strong restriction on correct Bend programs: a variable should not duplicate another variable that itself duplicates some variables.

The program below is an example where this can go wrong when using higher-order functions.
```py
def List/map(xs, f):
  fold xs:
    case List/Nil:
      return List/Nil
    case List/Cons:
      # 'f' is duplicated here
      return List/Cons(f(xs.head), List/map(xs.tail, f))
      # This line above is converted by the compiler to an explicit duplication of 'f'
      # {f1 f2} = f
      # return List/Cons(f1(xs.head), List/map(xs.tail, f2))

def main:
  # This lambda duplicates `x` and is itself duplicated by the map function.
  # This will result in wrong behavior.
  # In this specific case, the runtime will catch it and generate an error,
  # but at the moment that is not always the case.
  return List/map([1, 2, 3], lambda x: (+ x x))
```

In this case, we can only have one source of duplication, or our results will be incorrect.
Either List/map is linear (doesn't duplicate `f`) or the passed function is linear (doesn't duplicate `x` or any other variable inside it).