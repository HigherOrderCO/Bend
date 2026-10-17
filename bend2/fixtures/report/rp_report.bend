import Base

@unsafe
def Rp.unsafe_leaf(x: Nat) -> Nat:
  x

def Rp.mid(x: Nat) -> Nat:
  Nat.add(Rp.unsafe_leaf(x), 1n)

def Rp.top() -> Nat:
  Rp.mid(1n)

def Rp.cyc(n: Nat) -> Nat:
  match n:
    case 0n:
      0n
    case 1n+p:
      Rp.cyc(p)

def Rp.ext(x: Nat) -> IO(Unit):
  import "./rp_ext.c"

def Rp.uses_ext() -> IO(Unit):
  Rp.ext(3n)

def Rp.step(u: Unit) -> IO(Unit):
  Rp.ext(4n)

def Rp.clean(x: Nat) -> Nat:
  Nat.double(x)

def main() -> IO(Unit):
  IO.bind(Unit, Unit, Rp.uses_ext(), Rp.step)
