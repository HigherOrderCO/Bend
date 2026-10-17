# a term of a function type goes out η-long, λy => t(y), and the new λ
# puts t's binders a level deeper. bend2's annotations name a variable by
# the level bend2 checked it at, so the type of the λ-match under n still
# named n at the level y took: the kernel saw y's Bool where n's Nat was
# expected (#1665). t now goes under the λ through its lowered form, so
# its annotations move with its binders. A term goes η-long as an argument
# (arg), as a def's body (leaf), as a match arm (arm) and as a default arm
# (rest). The law checks main's value in the emitted book too
import Base

type Fact<-P: Type> is Data:
  Fact{-proof: P}

type Box<-P: Type> is Data:
  Box{n: Nat}

def Cells() -> Type:
  @+b: Bool -> {b == b : Bool}

def mk(-keeps: @+n: Nat -> {n == n : Nat}) -> Cells():
  b => {==}

def mk2(-keeps: @+n: Nat -> {n == n : Nat}) -> @c: Bool -> Box<Cells()>:
  c => Box{4n}

def take(-p: Cells(), n: Nat) -> Box<Cells()>:
  Box{n}

def refl(+n: Nat) -> Fact<{n == n : Nat}>:
  Fact{{==}}

def arg(-k: @+n: Nat -> Fact<{n == n : Nat}>) -> Box<Cells()>:
  take(mk(n => {\{Fact: q => q} : Fact<{n == n : Nat}> -> {n == n : Nat}}(k(n))), 1n)

def leaf(-k: @+n: Nat -> Fact<{n == n : Nat}>) -> Cells():
  mk(n => {\{Fact: q => q} : Fact<{n == n : Nat}> -> {n == n : Nat}}(k(n)))

def arm(-k: @+n: Nat -> Fact<{n == n : Nat}>, flag: Bool) -> Cells():
  match flag:
    case True{}:
      b => {==}
    case False{}:
      mk(n => {\{Fact: q => q} : Fact<{n == n : Nat}> -> {n == n : Nat}}(k(n)))

def rest(-k: @+n: Nat -> Fact<{n == n : Nat}>) -> @c: Bool -> Box<Cells()>:
  \{True: Box{8n}; mk2(n => {\{Fact: q => q} : Fact<{n == n : Nat}> -> {n == n : Nat}}(k(n)))}

def num(b: Box<Cells()>) -> Nat:
  match b:
    case Box{n}:
      n

def main() -> Nat:
  Nat.add(num(arg(refl)),
  Nat.add(num(take(leaf(refl), 2n)),
  Nat.add(num(take(arm(refl, False{}), 3n)),
    num(rest(refl, False{})))))

law eta_levels_result:
  {main() == 10n : Nat}

def eta_levels_result():
  {==}

#|10n
