# These same-spelled operators expose a second lookup of resolved names.
type Box<-T: Type> is Type:
  Wrap{x: T}

type Bit is Data:
  Off{}

type Handle<q> is Kind(q):
  Hold{}

def Q() -> Type:
  Quant

def Q.add(x: Quant, y: Quant) -> Quant:
  y

def Box.add(x: Quant, y: Quant) -> Quant:
  y

def value() -> Quant:
  &2

type Id is Type:
  MkId{x: Quant}

def Id.pure(x: Quant) -> Id:
  MkId{x}

def Id.bind(-A: Type, v: Id, f: A -> Id) -> Id:
  v

def F(x: Quant) -> Type:
  Quant

def F.add(x: Quant, y: Quant) -> Quant:
  y

def calc(~T: Type) -> Quant:
  (&1 + &2 : T)

def add(x: Quant, y: Quant) -> Quant:
  y

#|All terms check.
