# Example of some things you can do with the 'fun' syntax

# Define functions like this:
# By default they accept and return any type
(Def1) = ((λa a) (λb b))

# You can call a definition by just referencing its name.
# It will be substituted in place of the reference.
(Def2) = ((λa a) Def1)

# Definitions and variables can have names in upper and lower case and
#  contain numbers, '.', '-', '_' and '/'.
# Names defined in a more inner position shadow names in an outer position.
(def3) = ((λDef1 Def1) (λx λx x))

# You can annotate a function with the expected types and Bend will check them.
# The parentheses on the left side of the definition are optional.
const (a: A) (b: B) : A = a

# There are three types of native numbers available: u24, i24 and f24.
# u24: Unsigned 24-bit integers
# i24: Signed 24-bit integers
# f24: Floating point numbers with 24-bit precision
(unsigneds (x1: u24) (x2: u24)) : u24 = (* (+ x1 1) (/ (- x2 2) 1))

# '+' or '-' are mandatory for i24.
(signeds (x1: i24) (x2: i24)) : i24 = (* (+ x1 +1) (/ (- x2 -2) +1))

# The '.' is mandatory for f24.
(floats (x1: f24) (x2: f24)) : f24 = (* (+ x1 1.0) (/ (- x2 -2.0) +1.0))

# Numeric operations are only meaningful on native numbers.
# You can force the compiler to do it anyway by using untyped values.
id = λx x  # 'id' wasn't given a type so it's inferred as 'Any'.
bad_nums : Any = (+ id 1)

# You can use numbers on the native match expression
# The `+` arm binds the `scrutinee`-1 variable to the value of the number -1
(Num.pred) = λn
  switch n {
    0: 0
    _: n-1
  }

# Write new data types like this
type Option = (Some val) | None
type Bool = True | False

# You can have pattern matching on definitions
# Use `*` to ignore a pattern
(Option.unwrap_or (Option/Some val) *) = val
(Option.unwrap_or  Option/None     or) = or

(Bool.or Bool/True  *) = Bool/True
(Bool.or * Bool/True)  = Bool/True
(Bool.or * *)     = Bool/False

# Or using a match expression
(Bool.not) = λbool
  match bool {
    Bool/True:  Bool/False
    Bool/False: Bool/True
  } 

# Data types can store values
type Boxed T = (Box (val: T))

# Types with only one constructor can be destructured using `open`,
# a single matching definition or a 'match'.
(Box.map (Boxed/Box val) f) = (Boxed/Box (f val))

(Box.unbox (box: (Boxed T))): T = open Boxed box; box.val

# Use tuples to store two values together without needing to create a new data type
(Tuple.new fst snd) =
  let pair = (fst, snd)
  pair

# Then you can destructure it inside the definition or using `let`
(Tuple.fst (fst, snd)) = fst

(Tuple.snd) = λpair
  let (fst, snd) = pair
  snd

# You can give type annotations to type definitions as well.
# The recursive fields should be annotated with a '~'.
type ListList T =
  (Cons (head: (List T)) ~(tail: (ListList T))) |
  Nil

# Bend functions usually center around recursion
sum (List/Nil)       = 0
sum (List/Cons x xs) = (+ x (sum xs))

sum_list (ListList/Nil)       = List/Nil
sum_list (ListList/Cons x xs) = (List/Cons (sum x) (sum_list xs))

# To create local recursive functions that consume a recursive type, you can use 'fold'.
sum_list2 ll = fold ll {
  # The fold is implicitly called for fields marked with '~' in their definition.
  # In this case, 'll.tail' is replaced with a recursive call to the fold.
  ListList/Cons: (List/Cons (sum ll.head) ll.tail)
  ListList/Nil: List/Nil
}

# To do the opposite and create a recursive structure, you can use 'bend'.
# 'bend' can be seen as a tree-like generalization of a while loop.
new_list = bend x = 0 {
  when (< x 10):
    # 'fork' calls the bend recursively with the provided values.
    (List/Cons x (fork (+ x 1)))
  else:
    List/Nil
}

# To make programs that are more parallelizable, you generally want to
# avoid lists and use tree-like structures with multiple recursion instead.
sum_nums from to =
  if (< from to) {
    (+ 
      (sum_nums from (/ 2 (+ from to)))
      (sum_nums (+ 1 (/ 2 (+ from to))) to))
  } else {
    from
  }

# Internally, each variable is only used once. If you use a variable
# is used more than once, the compiler will insert duplications for you.
#
# You can also write them manually with 'let {a b} = ...', but then
# your function needs to be unchecked, either by not annotating it 
# with types or by marking it as unchecked.
unchecked (def4) = λz let {z1 z2} = z; (z1 ((λx (x x x x x)) z2))

# Duplicating a term that duplicates a variable is not allowed and will break the program.
map f (List/Cons x xs) = (List/Cons (f x) (map f xs)) # 'f' is duplicated here
map f (List/Nil)       = List/Nil 

# 'map' duplicated the lambda which duplicates 'a'.
# Although this is well defined, it does not produce a sound lambda-calculus result.
VeryBad (a: u24) (xs: (List u24)) : (List u24) =
(map 
  λ x (+ (* a x) a) # 'a' is duplicated here
  [1, 2, 3, 4, 5])


# All files must have a main definition to run.
# You can execute a program in Bend with "bend run <path to file>"
# Other options are "check" to just see if the file is well formed
# and "gen-hvm" to output hvm code.
(main) = 
  let tup = (Tuple.new Option/None (Num.pred 5))

  let fst = (Tuple.fst tup)
  let snd = (Tuple.snd tup)

  let box =     (Boxed/Box fst)
  let map =     (Box.map box Option.unwrap_or)
  let unboxed = ((Box.unbox map) snd)

  (unsigneds 3 unboxed)
