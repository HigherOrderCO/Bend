# A net that expands inifinitely like church exponentiation
# Compiles to this net:
# a
# & ({(b c) (d b)} (c d)) ~ {(e a) e}
main =
  let {a1 a2} =
    @b
    let {b1 b2} = b
    let (c1, c2) = b2
    ((b1 c2), c1)
  (a1 a2)