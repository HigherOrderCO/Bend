# Repeated ~ binder labels once collided as the opaque constant cast~T.
# Parameters now have distinct positional identities, so shadowing is
# harmless but x still has the first T: returning it at the second T
# is rejected by ordinary type comparison (#905).
type Unit is Data:
  Unit{}

def cast(~T: Type, ~x: T, ~T: Type) -> T:
  x

def main() -> Unit:
  Unit{}

#|SOME PROOFS FAIL
#|Error:
#|- expected : cast~param2
#|- observed : cast~param0
#|Location: cast
#| 8 | def cast(~T: Type, ~x: T, ~T: Type) -> T:
#| 9>|   x
#|   |   ^
#|10 |
#|exit 1
