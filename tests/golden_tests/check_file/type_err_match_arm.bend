type Nat_:
  Zero
  Succ {x: Unit, pred: Nat_}

type Unit:
  Unit

Test1: Nat_ -> (Nat_, Nat_)
Test1 = λx match x {
  Nat_/Zero: (Nat_/Zero, Nat_/Zero)
  Nat_/Succ: (x.x, x.pred)
}

main = *
