type Box is Type:
  Box{value: Type -> Type}

def reflexive(-x: Box) -> {x == x : Box}:
  {==}

def rebuilt(h: Box) -> {h == h : Box}:
  match h:
    case Box{f}:
      reflexive(Box{f})

def original(h: Box) -> {h == h : Box}:
  reflexive(h)

def direct(h: Box) -> {h == h : Box}:
  match h:
    case Box{f}:
      {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
