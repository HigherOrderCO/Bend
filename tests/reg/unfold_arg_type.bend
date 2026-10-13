# regression witness: emit_unfold let-bound an unfolded call's argument
# as a bare value, and a Let carries no type, so the value reached
# emit_open untyped; an intrinsic emitted with no expected type took
# the BOX layout, and the fused Bool match then read a raw 0/1 word
# through the boxed protocol, term_aux against CID_False, matching no
# arm: C fail-stopped with error 3 where the interpreter and JS print
# 0. The unfold now annotates the let with the callee's parameter type.
import Base

def ch12(c: Char) -> U32:
  Chr{n} = c
  n

def bu14(b: Bool) -> U32:
  match b:
    case False{}:
      0
    case True{}:
      1

def rs16() -> U32:
  v10 = {True{} && U32.is_eq(ch12('B'), 8) : Bool}
  bu14(v10)

def main() -> IO(Unit):
  IO.print(U32.show(rs16()))

#|0
