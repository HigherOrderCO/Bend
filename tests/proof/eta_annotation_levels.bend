# Eta expansion must rebind the annotations below an inserted lambda.
import Base

type Fact<-P: Type> is Data:
  Fact{-proof: P}

type Box<-P: Type> is Data:
  Box{}

def Cells() -> Type:
  @+b: Bool -> {b == b : Bool}

def cells(-keeps: @+n: Nat -> {n == n : Nat}) -> Fact<Cells()>:
  Fact{b => {==}}

def take(-p: Cells()) -> Box<Cells()>:
  Box{}

def passed(-kf: @+n: Nat -> Fact<{n == n : Nat}>) -> Box<Cells()>:
  take({\{Fact: q => q} : Fact<Cells()> -> Cells()}(
    cells(n => {\{Fact: q => q} : Fact<{n == n : Nat}> -> {n == n : Nat}}(kf(n)))))

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
