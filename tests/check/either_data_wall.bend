# bend3's family wall, both halves carried: an either of Data components
# contracted freely while a closure-holding either refused multi-use. bend is
# affine for every type, so the free half becomes twice's explicit +e binder
# over the reusable Either<&2, &2, ..> -- and the wall half lands on the SAME
# def: leak's bare e spends twice and refuses at leak, closure or not.
# ported from bend3: U32 | U32 -> Either<&2, &2, U32, U32> (base Inl/Inr; no |
# type sugar); leak's let-then-match routes through leak.go
import Base

law side:
  for e: Either<&2, &2, U32, U32>
  U32

def side(e):
  match e:
    case Inl{value}:
      value
    case Inr{value}:
      (value + 1 : U32)

law twice:
  for +e: Either<&2, &2, U32, U32>
  U32

def twice(e):
  (side(e) + side(e) : U32)

law la:
  for e: Either<&1, &1, U32, U32 -> U32>
  U32

def la(e):
  match e:
    case Inl{value}:
      value
    case Inr{f}:
      f(0)

law leak.go:
  for e: Either<&1, &1, U32, U32 -> U32>
  for x: U32
  U32

def leak.go(e, x):
  match e:
    case Inl{value}:
      value
    case Inr{f}:
      f(x)

law leak:
  for e: Either<&1, &1, U32, U32 -> U32>
  U32

def leak(e):
  leak.go(e, la(e))

law main:
  U32

def main(): twice(Inr{2})

#|SOME PROOFS FAIL
#|Error:
#|- message  : e appears 2 times (at 56:11, 56:17), but a plain binding is used at most once
#|Location: leak
#|55 | def leak(e):
#|56>|   leak.go(e, la(e))
#|   |                 ^
#|57 |
#|Note: a +e binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
