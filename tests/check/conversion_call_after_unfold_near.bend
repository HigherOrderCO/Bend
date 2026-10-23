# copies that agree only after a few steps convert without evaluating
# them: each step of the conversion asks the rigid pass first, so once
# both sides evaluate to the same text, the walk stops there. mid() is
# 2^16, a chain of Succ the full pass would walk on both sides (a stack
# overflow)
import Base

type Box is Data:
  Box{n: Nat}

def mid() -> Nat:
  Nat.pow(2n, 16n)

def pow2(+n: Nat) -> Nat:
  Nat.pow(2n, n)

def boxed() -> Box:
  Box{mid()}

def boxed_at(+n: Nat) -> Box:
  Box{pow2(n)}

def pow_box(b: Box) -> Nat:
  match b:
    case Box{n}: Nat.pow(2n, n)

def wrapped() -> Box:
  Box{pow_box(Box{16n})}

def mid2() -> Nat:
  mid()

def mid3() -> Nat:
  Nat.pow(2n, 16n)

# a def that names another
def alias_eq() -> {boxed() == Box{mid2()} : Box}:
  {==}

# another def with the same body
def same_body_eq() -> {boxed() == Box{mid3()} : Box}:
  {==}

# equal arguments, written differently
def equal_args_eq() -> {boxed_at(16n) == Box{pow2(Nat.add(8n, 8n))} : Box}:
  {==}

# a call as the argument
def call_arg_eq() -> {boxed_at(Nat.add(8n, 8n)) == Box{pow2(Nat.add(8n, 8n))} : Box}:
  {==}

# a constructor as the argument
def ctor_arg_eq() -> {wrapped() == Box{pow_box(Box{16n})} : Box}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
