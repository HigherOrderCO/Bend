# a Base-free module with its own Empty: its != law negates into THIS
# Empty, alone and imported alike. Before #1331 the sugar named a bare
# Empty, so whichever file was the entry supplied the law's codomain
type T is Data:
  A{}
  B{}

def f(x: T) -> T:
  A{}

def g(x: T) -> T:
  A{}

type Empty is Data:

law f_ne_g:
  for x: T
  {f(x) != g(x) : T}

#|SOME PROOFS FAIL
#|Error: 1 TODO found.
#|The code is incomplete, and not a valid proof yet.
#|exit 1
