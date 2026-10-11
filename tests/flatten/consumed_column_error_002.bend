# a later row's pattern splits a column too: the [p, q] row splits W, so
# the first row's inner `match W` is refused like a destructed value;
# its rows belong in the outer match, next to the Con{h, W} that
# introduced W (#1125)
import Base

def g(n: Nat, xs: List<&2, U32>) -> U32:
  match n, xs:
    case 0n, Con{h, W}:
      match W:
        case Nil{}:
          h
        case Con{a, b}:
          a
    case k, [p, q]:
      p
    case k, _:
      0

def main() -> U32:
  g(0n, [1, 2])

#|SOME PROOFS FAIL
#|Error:
#|- message  : 'W' can't be matched here (match it in the same match as the pattern that introduced it)
#|Location:
#| 9 |     case 0n, Con{h, W}:
#|10>|       match W:
#|   |       ^^^^^^^^
#|11 |         case Nil{}:
#|exit 1
