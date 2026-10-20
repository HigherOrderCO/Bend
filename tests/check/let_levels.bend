# a let-bound λ passed to a self-call goes into the call, built at the
# call's depth. bend2's annotations name a variable by the level bend2
# checked it at, so the type of the λ-match under n still named n at the
# let's depth, which at the call is g itself: building that type built g
# again, and --verdict ran until the stack overflowed. A def's body now
# goes through its lowered form once, before --verdict builds it, so its
# annotations move with its binders. The law checks main's value in the
# emitted book too
import Base

type Fact<-P: Type> is Type:
  Fact{proof: P}

def refl(+n: Nat) -> Fact<{n == n : Nat}>:
  Fact{{==}}

def walk(c: Nat, keeps: @+n: Nat -> {n == n : Nat}) -> Nat:
  match c:
    case Zero{}:
      0n
    case Succ{p}:
      g : @+n: Nat -> {n == n : Nat} = n => {\{Fact: q => q} : Fact<{n == n : Nat}> -> {n == n : Nat}}(refl(n))
      walk(p, g)

def main() -> Nat:
  walk(3n, n => {==})

law let_levels_result:
  {main() == 0n : Nat}

def let_levels_result():
  {==}

#|0n
