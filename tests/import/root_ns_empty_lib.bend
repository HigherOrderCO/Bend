# a Base-free module whose != law names Empty: sugar names Base's, which
# is not loaded, so the law has no codomain to borrow from the entry
type T is Data:
  A{}
  B{}

def f(x: T) -> T:
  A{}

def g(x: T) -> T:
  A{}

law f_ne_g:
  for x: T
  {f(x) != g(x) : T}

#|SOME PROOFS FAIL
#|Error:
#|- expected : a defined name
#|- observed : Empty
#|Context:
#|- x : T
#|- _ : {A{} == A{} : T}
#|Location: f_ne_g
#|14 |   for x: T
#|15>|   {f(x) != g(x) : T}
#|   |         ^^
#|16 | 
#|exit 1
