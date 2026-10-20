# --verdict must retain the dependent type of a constructor field.
type Unit is Data:
  Unit{}
type Dep<-A: Type, -P: @x: A -> Type> is Type:
  Pack{first: A, second: P(first)}

def proof() -> {Pack{Unit{}, f => f(Unit{})} == Pack{Unit{}, f => f(Unit{})} : Dep<Unit, u => @-f: (@-x: Unit -> {u == u : Unit}) -> {u == u : Unit}>}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
