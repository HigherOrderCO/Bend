# Bare heads select lexical members; alias-only heads resolve their member.
import resolved_b.bend as Q
import resolved_b.bend as Box
import resolved_b.bend as M

def Q() -> Type:
  Quant

def Q.add(x: Quant, y: Quant) -> Quant:
  x

type Box<-T: Type> is Type:
  Wrap{x: T}

def Box.add(x: Quant, y: Quant) -> Quant:
  x

def plain() -> {(&1 + &2 : Q) == &1 : Quant}:
  {==}

def family() -> {(&1 + &2 : Box<Quant>) == &1 : Quant}:
  {==}

def alias_only() -> {(&1 + &2 : M) == &2 : Quant}:
  {==}

#|All terms check.
