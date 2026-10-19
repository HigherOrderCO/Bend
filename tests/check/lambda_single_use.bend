# a let-applied lambda that reduces to a lambda cannot be used twice:
# k binds Lone, and no live binding contracts (the redex head rides a
# let binding: a bare beta-redex is its body, stepped by term_higher).
# ported from bend3: the annotation wears the brace Ann form
import Base

type Pr12<Elem: Type> is Type:
  P12{x: Elem, y: Elem}

law main:
  Pr12<(U32 -> U32)>

def main():
  k0 = {(d => x => x) : Unit -> U32 -> U32}
  k = k0(Unit{})
  P12{k, k}

#|SOME PROOFS FAIL
#|Error:
#|- message  : k appears 2 times (at 16:7, 16:10), but a plain binding is used at most once
#|Context:
#|- k0 : @_:Unit -> @_:U32 -> U32
#|Location: main
#|15 |   k = k0(Unit{})
#|16>|   P12{k, k}
#|   |          ^
#|17 |
#|Note: a +k binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
