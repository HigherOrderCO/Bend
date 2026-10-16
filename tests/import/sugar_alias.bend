import Base
import ./sugar_alias_lib.bend as String
import ./sugar_alias_lib.bend as Array
import ./sugar_alias_lib.bend as Nat

def put(xs: Array<U32>, i: U32) -> Array<U32>:
  xs[i] <- 7
  xs

def big(n: Nat) -> Nat:
  300n+n

def main() -> String:
  "a" ++ "b"

#|"ab"
