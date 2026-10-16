type Nat is Data:
  Zero{}
  Succ{pred: Nat}

type Empty is Data:

type Sigma<a, b, -A: Kind(a), -B: @-x: A -> Kind(b)> is Kind(a <&> b):
  Tuple{fst: A, snd: B(fst)}

type List<a, -A: Kind(a)> is Kind(a):
  Nil{}
  Con{head: A, tail: List<a, A>}

# reusable Data values: a + binder on Nat, on a Data list, on evidence,
# on a reusable pair, a phantom-affine tag, an empty type; an affine
# list of functions and a mixed-grade pair exist; Data weakens to Type
type Tag<-T: Type> is Data:
  MkTag{n: Nat}

law dup:
  for +n: Nat
  Sigma<&2, &2, Nat, _ => Nat>

def dup(n):
  Tuple{n, n}

law dupl:
  for +xs: List<&2, Nat>
  Sigma<&2, &2, List<&2, Nat>, _ => List<&2, Nat>>

def dupl(xs):
  Tuple{xs, xs}

law dupe:
  for -a: Nat
  for -b: Nat
  for +e: {a == b : Nat}
  Sigma<&2, &2, {a == b : Nat}, _ => {a == b : Nat}>

def dupe(a, b, e):
  Tuple{e, e}

law dupp:
  for +p: Sigma<&2, &2, Nat, _ => Nat>
  Sigma<&2, &2, Sigma<&2, &2, Nat, _ => Nat>, _ => Sigma<&2, &2, Nat, _ => Nat>>

def dupp(p):
  Tuple{p, p}

law dupt:
  for +t: Tag<(Nat -> Nat)>
  Sigma<&2, &2, Tag<(Nat -> Nat)>, _ => Tag<(Nat -> Nat)>>

def dupt(t):
  Tuple{t, t}

law absurd:
  for -A: Type
  for +e: Empty
  A

def absurd(A, e):
  match e:

law fns:
  List<&1, Nat -> Nat>

def fns():
  Con{x => x, Con{x => Succ{x}, Nil{}}}

law mixed:
  Sigma<&2, &1, Nat, _ => (Nat -> Nat)>

def mixed():
  Tuple{Zero{}, x => x}

law weak:
  for -A: Type
  for  x: A
  List<&1, A>

def weak(A, x):
  Con{x, Nil{}}

law main:
  Sigma<&2, &2, Nat, _ => Nat>

def main():
  dup(Succ{Zero{}})

#|Tuple{Succ{Zero{}}, Succ{Zero{}}}
