# Testing linearization in different situations
type ConsList = (Cons h t) | Nil

# Auto linearized on strict mode
A = @a @b @c switch a {
  0: (b c)
  _: (a-1 b c)
}

# Manually linearized
B = @a @b @c switch c with a b {
  0: (a b)
  _: (a b c-1)
}

# Not linearized
C = @a @b @c switch c {
  0: (a b)
  _: (a b c-1)
}

# Auto linearized, one arg not used
D = @a @b @c switch a {
  0: c
  _: (a-1 c)
}

# Pattern matching defs linearize all arguments
E (ConsList/Cons a b) (ConsList/Cons c d) = (a b c d)
E a b = (a b)

A2 = @a @b @c match a {
  ConsList/Cons: (a.h a.t b c)
  ConsList/Nil: (b c)
}

B2 = @a @b @c match c with a b {
  ConsList/Cons: (a b c.h c.t)
  ConsList/Nil: (a b)
}

C2 = @a @b @c match c {
  ConsList/Cons: (a b c.h c.t)
  ConsList/Nil: (a b)
}

D2 = @a @b @c match a {
  ConsList/Cons: (a.h a.t b c)
  ConsList/Nil: (b c)
}

main = *
