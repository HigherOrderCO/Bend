# the type tests/show/imported_adt.bend prints from another file
import Base

type Shape is Data:
  Circ{r: U32}
  Sq{s: U32, t: Shape}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
