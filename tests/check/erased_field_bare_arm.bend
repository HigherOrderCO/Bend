# a match inside a law's type is checked by bend2 but not wrapped in its
# types, so --verdict gives each arm the goal bend2 checks it at: an
# erased field binds dead (λ-), and a field of a function type keeps that
# type, so an arm may apply it. Before, the first went out live and the
# kernel rejected it, and the second was out of scope (#1157). The arm's
# type is the constructor's own, so a field may depend on one before it
import Base

type Wrap is Data:
  Mk{-f: Unit -> Unit}

type Three is Data:
  ArmFn{-f: Unit -> Unit}
  ArmPair{x: Unit, -g: Unit -> Unit}
  ArmNone{}

# a parameterized constructor whose erased field's type names the field
# before it
def Same(-A: Data, x: A) -> Data:
  {x == x : A}

type Dep<-A: Data> is Data:
  MkDep{x: A, -f: Same(A, x) -> A}

def SelfEqF(-f: Wrap -> (Unit -> Unit)) -> Data:
  {f == f : Wrap -> (Unit -> Unit)}

def SelfEq(-u: Unit) -> Data:
  {u == u : Unit}

law erased_field_identity:
  SelfEqF({\{Mk: f => f} : Wrap -> (Unit -> Unit)})

def erased_field_identity():
  {==}

law erased_field_call:
  SelfEq({({\{Mk: f => f(Unit{})} : Wrap -> Unit})(Mk{u => u}) : Unit})

def erased_field_call():
  {==}

law erased_field_arms:
  SelfEq({({\{ArmFn: f => f(Unit{}); ArmPair: x => g => g(x); ArmNone: Unit{}} : Three -> Unit})(ArmPair{Unit{}, u => u}) : Unit})

def erased_field_arms():
  {==}

law erased_field_default:
  SelfEq({({\{ArmFn: f => f(Unit{}); t => Unit{}} : Three -> Unit})(ArmNone{}) : Unit})

def erased_field_default():
  {==}

law dependent_field:
  SelfEq({({\{MkDep: x => f => f({==})} : Dep<Unit> -> Unit})(MkDep{Unit{}, e => Unit{}}) : Unit})

def dependent_field():
  {==}

def main() -> U32:
  3

#|3
