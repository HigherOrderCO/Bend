type Pair = (new a b)

type State:
  new { a, b }

def with_state(x, s):
  open State: s
  return Pair/new(s, x(s.a))

main =
  let x = (with_state @x x (State/new 1 2))
  open Pair x;
  {x.a x.b}
