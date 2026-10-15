# a Base-free module with its own List and Tuple: its list, <> and tuple
# sugar build THIS module's constructors, alone and imported alike
type List<A: Data> is Data:
  Nil{}
  Con{h: A, t: List<A>}

type T is Data:
  A{}
  B{}

type Pair is Data:
  Tuple{a: T, b: T}

def xs() -> List<T>:
  [A{}, B{}]

def ys() -> List<T>:
  A{} <> [B{}]

def pr() -> Pair:
  (A{}, B{})

def main() -> List<T>:
  ys()

#|[A{}, B{}]
