# bend3's pair wall, both halves carried: a pair of Data components
# contracted freely while a closure-holding pair refused multi-use.
# bend grades the pair: the free half is twice's +p binder over the
# reusable pair Sigma<&2, &2, U32, _ => U32> (a Data type), and the
# wall half refuses at leak -- an affine pair holding a closure is
# Type, so leak's bare p spends twice.
# ported from bend3: leak's let-then-destructure routes through
# leak.go; unused destructure components simply drop
import Base

law pa:
  for p: Sigma<&2, &2, U32, _ => U32>
  U32

def pa(p):
  (a, b) = p
  a

law pb:
  for p: Sigma<&2, &2, U32, _ => U32>
  U32

def pb(p):
  (a, b) = p
  b

law twice:
  for +p: Sigma<&2, &2, U32, _ => U32>
  U32

def twice(p):
  (pa(p) + pb(p) : U32)

law la:
  for p: U32 & (U32 -> U32)
  U32

def la(p):
  (a, f) = p
  a

law leak.go:
  for p: U32 & (U32 -> U32)
  for x: U32
  U32

def leak.go(p, x):
  (a, f) = p
  f(x)

law leak:
  for p: U32 & (U32 -> U32)
  U32

def leak(p):
  leak.go(p, la(p))

law main:
  U32

def main(): twice((2, 3))

#|SOME PROOFS FAIL
#|Error:
#|- message  : p is used 2 times (at 56:11, 56:17), but a plain binding is used at most once
#|Location: leak
#|55 | def leak(p):
#|56>|   leak.go(p, la(p))
#|   |                 ^
#|57 |
#|Note: a +p binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
