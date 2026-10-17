# a λ in an erased argument may use its variable twice: it goes out as a
# λ-match on the variable, whose arms call its body on each constructor,
# under a binder per field. bend2's annotations name a variable by the
# level bend2 checked it at, so the type Tag<p> -> Nat of a λ-match in
# the body still named p's level, now the first field's: the kernel saw
# a Nat where a P was expected, and with no fields --verdict met a free
# variable (#1669). The body now goes under the fields through its
# lowered form, so its annotations move with its binders. The annotation
# names the variable, of a type with two constructors (pair) or with no
# fields (unit), or the binder of a λ inside the body (inner). The law
# checks main's value in the emitted book too
import Base

type Fact<-P: Type> is Data:
  Fact{-proof: P}

type P is Type:
  P{a: Nat, b: Nat}
  Q{c: Nat}

type Tag<-x: P> is Data:
  Tag{}

type U is Type:
  U1{}
  U2{}

type UTag<-x: U> is Data:
  UTag{}

def first(p: P) -> Nat:
  match p:
    case P{a, b}:
      a
    case Q{c}:
      c

def tag(-p: P) -> Tag<p>:
  Tag{}

def code(u: U) -> Nat:
  match u:
    case U1{}:
      1n
    case U2{}:
      2n

def utag(-u: U) -> UTag<u>:
  UTag{}

def use(-f: P -> Nat, n: Nat) -> Nat:
  n

def use_u(-f: U -> Nat, n: Nat) -> Nat:
  n

def keep(-g: @+n: Nat -> {n == n : Nat}, x: Nat) -> Nat:
  x

def refl(+n: Nat) -> Fact<{n == n : Nat}>:
  Fact{{==}}

def pair() -> Nat:
  use(p => Nat.add(first(p), {\{Tag: first(p)} : Tag<p> -> Nat}(tag(p))), 1n)

def unit() -> Nat:
  use_u(u => Nat.add(code(u), {\{UTag: code(u)} : UTag<u> -> Nat}(utag(u))), 2n)

def inner(-k: @+n: Nat -> Fact<{n == n : Nat}>) -> Nat:
  use(p => keep(n => {\{Fact: q => q} : Fact<{n == n : Nat}> -> {n == n : Nat}}(k(n)), Nat.add(first(p), first(p))), 3n)

def main() -> Nat:
  Nat.add(pair(), Nat.add(unit(), inner(refl)))

law rebuild_levels_result:
  {main() == 6n : Nat}

def rebuild_levels_result():
  {==}

#|6n
