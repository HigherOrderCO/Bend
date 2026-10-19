# audit 286: same-def binder-key capture soundness
# (check_binder_name_capture's sibling). Capture-free beta gives
# s(b,a) = a, so the second argument lambda's {==} must be rejected
# with expected b, observed a -- a key collision would accept it and
# close main : Empty.
# ported from bend3: the annotation wears the brace Ann form, riding
# a let binding (a bare beta-redex is its body: term_higher steps it)
type Empty6 is Data:

type Unit6 is Data:
  U6{}

type Bit is Data:
  Zb{}
  Ob{}

law family:
  for x: Bit
  Type

def family(x):
  match x:
    case Zb{}:
      Empty6
    case Ob{}:
      Unit6

law bad:
  {Zb{} == Ob{} : Bit}

def bad():
  f = {s => e => e(Ob{}, Zb{}) :
    @s:(@a:Bit -> @b:Bit -> Bit) ->
    @e:(@a:Bit -> @b:Bit -> {b == s(b, a) : Bit}) ->
    {Zb{} == s(Zb{}, Ob{}) : Bit}
  }
  f(a => b => b, a => b => {==})

law main:
  Empty6

def main():
  %h@bad : family(_); U6{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Context:
#|- f : @s:(@a:Bit -> @b:Bit -> Bit) -> @e:(@a:Bit -> @b:Bit -> {b == s(b, a) : Bit}) -> {Zb{} == s(Zb{}, Ob{}) : Bit}
#|- a : Bit
#|- b : Bit
#|Location: bad
#|36 |   }
#|37>|   f(a => b => b, a => b => {==})
#|   |                            ^^^^
#|38 |
#|- left     : b  =>  b
#|- right    : a  =>  a
#|exit 1
