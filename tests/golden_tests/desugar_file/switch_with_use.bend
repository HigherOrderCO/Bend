# Test manual linearization of a use term
main =
  @x @y use z = (x y); @a @b @c switch a with z {
    0: z
    _: (a-1 z)
  }
