# a Base-free module with its own Exists: a where clause builds THIS
# Exists, alone and imported alike (#1331)
type Two is Data:
  X{}
  Y{}

def f(t: Two) -> Two:
  Y{}

def g(t: Two) -> Two:
  X{}

type Sig<a, b, -A: Kind(a), -B: @-x: A -> Kind(b)> is Kind(a <&> b):
  Tup{fst: A, snd: B(fst)}

def Exists(-A: Type, -B: @-x: A -> Type) -> Type:
  Sig<&1, &1, A, B>

law holds_when_gx:
  for w: Two where {g(w) == X{} : Two}
  {f(X{}) == X{} : Two}

#|SOME PROOFS FAIL
#|Error: 1 TODO found.
#|The code is incomplete, and not a valid proof yet.
#|exit 1
