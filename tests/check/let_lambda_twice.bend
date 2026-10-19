# a let-bound lambda applied twice: term_higher's beta step reads a bare
# lambda head, never under a binding (a let's name is a cell that
# term_strip would open), so each k0(..) is a use of k0, and the second
# is refused
import Base

type Pr12<Elem: Type> is Type:
  P12{x: Elem, y: Elem}

law main:
  Pr12<U32>

def main():
  k0 = {(x => x) : U32 -> U32}
  P12{k0(1), k0(2)}

#|SOME PROOFS FAIL
#|Error:
#|- message  : k0 is used 2 times (at 15:7, 15:14), but a plain binding is used at most once
#|Location: main
#|14 |   k0 = {(x => x) : U32 -> U32}
#|15>|   P12{k0(1), k0(2)}
#|   |              ^^
#|16 |
#|Note: a +k0 binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
