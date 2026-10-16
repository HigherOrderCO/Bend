# --verdict must use the carrier to type a generic constructor field.
type Box<-A: Type> is Type:
  Box{value: A}

def proof() -> {Box{x => Type} == Box{x => Type} : Box<@-x: Type -> Type>}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
