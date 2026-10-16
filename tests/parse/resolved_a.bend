# Operators distinguish this module from the colliding alias's module.
type Box<-T: Type> is Type:
  Wrap{x: T}

type Bit is Data:
  Off{}

type Handle<q> is Kind(q):
  Hold{}

def Q() -> Type:
  Quant

def Q.add(x: Quant, y: Quant) -> Quant:
  x

def Box.add(x: Quant, y: Quant) -> Quant:
  x

def value() -> Quant:
  &1

type Id is Type:
  MkId{x: Quant}

def Id.pure(x: Quant) -> Id:
  MkId{x}

def Id.bind(-A: Type, v: Id, f: A -> Id) -> Id:
  v

#|All terms check.
