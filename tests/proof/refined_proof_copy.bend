# A copyable view of a match-refined hypothesis must retain its annotation.
# The original affine hypothesis is consumed once, including non-equality Data.
import Base

type Spine is Data:
  SpineEnd{}
  SpineCon{head: Nat, tail: Spine}

def AtEnd(rest: Spine, b: Nat) -> Type:
  match rest:
    case SpineEnd{}: {Nat.is_le(b, 7n) == True{} : Bool}
    case SpineCon{x, r}: Unit

def PairAtEnd(rest: Spine, +b: Nat) -> Type:
  match rest:
    case SpineEnd{}:
      {Nat.is_le(b, 7n) == True{} : Bool} & {Nat.is_le(b, 7n) == True{} : Bool}
    case SpineCon{x, r}: Unit

def pairProof(+b: Nat, +proof: {Nat.is_le(b, 7n) == True{} : Bool}) -> {Nat.is_le(b, 7n) == True{} : Bool} & {Nat.is_le(b, 7n) == True{} : Bool}:
  (proof, proof)

def duplicate(rest: Spine, +b: Nat, proof: AtEnd(rest, b)) -> PairAtEnd(rest, b):
  match rest:
    case SpineEnd{}:
      +copy = {proof : {Nat.is_le(b, 7n) == True{} : Bool}}
      (copy, copy)
    case SpineCon{x, r}: Unit{}

def viaHelper(rest: Spine, +b: Nat, proof: AtEnd(rest, b)) -> PairAtEnd(rest, b):
  match rest:
    case SpineEnd{}: pairProof(b, {proof : {Nat.is_le(b, 7n) == True{} : Bool}})
    case SpineCon{x, r}: Unit{}

def consumedOnce(rest: Spine, +b: Nat, proof: AtEnd(rest, b)) -> AtEnd(rest, b):
  match rest:
    case SpineEnd{}: {proof : {Nat.is_le(b, 7n) == True{} : Bool}}
    case SpineCon{x, r}: Unit{}

def seen(+b: Nat, proof: {Nat.is_le(b, 7n) == True{} : Bool}) -> Nat:
  1n

# a copy a self-call takes
def walk(n: Nat, rest: Spine, +b: Nat, proof: AtEnd(rest, b)) -> Nat:
  match n:
    case Zero{}: 0n
    case Succ{p}:
      match rest:
        case SpineEnd{}:
          +copy = {proof : {Nat.is_le(b, 7n) == True{} : Bool}}
          Nat.add(walk(p, SpineEnd{}, b, copy), seen(b, copy))
        case SpineCon{x, r}: 0n

def EndValue(rest: Spine) -> Type:
  match rest:
    case SpineEnd{}: Bool
    case SpineCon{x, r}: Unit

def EndPair(rest: Spine) -> Type:
  match rest:
    case SpineEnd{}: Bool & Bool
    case SpineCon{x, r}: Unit

def duplicateValue(rest: Spine, value: EndValue(rest)) -> EndPair(rest):
  match rest:
    case SpineEnd{}:
      +copy = {value : Bool}
      (copy, copy)
    case SpineCon{x, r}: Unit{}

def value_result() -> {duplicateValue(SpineEnd{}, False{}) == (False{}, False{}) : Bool & Bool}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
