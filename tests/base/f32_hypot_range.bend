# Hypot must retain representable lengths across the F32 exponent range.
# Keep the existing NaN policy and positive results for signed zeros.
# In the ordinary range, keep the plain formula's correctly rounded results.
import Base

def axis(+x: F32) -> Bool:
  Bool.and(F32.is_eq(F32.hypot(x, 0.0), F32.abs(x)),
    Bool.and(F32.is_eq(F32.hypot(0.0, x), F32.abs(x)),
      Bool.and(F32.is_eq(F32.hypot(F32.neg(x), 0.0), F32.abs(x)),
        F32.is_eq(F32.hypot(0.0, F32.neg(x)), F32.abs(x)))))

def near(+x: F32, +y: F32) -> Bool:
  +a = F32.bits(x)
  +b = F32.bits(y)
  Bool.and(F32.is_ge(x, 0.0),
    U32.is_le(U32.sub(U32.max(a, b), U32.min(a, b)), 2))

def triangle(+s: F32) -> Bool:
  +a = F32.mul(3.0, s)
  +b = F32.mul(4.0, s)
  +c = F32.mul(5.0, s)
  Bool.and(near(F32.hypot(a, b), c),
    Bool.and(near(F32.hypot(b, a), c),
      Bool.and(near(F32.hypot(F32.neg(a), b), c),
        near(F32.hypot(a, F32.neg(b)), c))))

def up(n: Nat, +x: F32) -> Bool:
  match n:
    case 0n:
      axis(x)
    case 1n+p:
      Bool.and(axis(x), up(p, F32.mul(x, 2.0)))

def down(n: Nat, +x: F32) -> Bool:
  match n:
    case 0n:
      Bool.and(axis(x), triangle(x))
    case 1n+p:
      Bool.and(Bool.and(axis(x), triangle(x)), down(p, F32.div(x, 2.0)))

def large(n: Nat, +x: F32) -> Bool:
  match n:
    case 0n:
      triangle(x)
    case 1n+p:
      Bool.and(triangle(x), large(p, F32.mul(x, 2.0)))

def exact(+a: F32, +b: F32, +c: F32) -> Bool:
  Bool.and(F32.is_eq(F32.hypot(a, b), c), F32.is_eq(F32.hypot(b, a), c))

def same(+x: F32, +y: F32) -> Bool:
  +r = F32.sqrt(F32.add(F32.mul(x, x), F32.mul(y, y)))
  U32.is_eq(F32.bits(F32.hypot(x, y)), F32.bits(r))

def plain(n: Nat, +x: F32, +y: F32) -> Bool:
  match n:
    case 0n:
      same(x, y)
    case 1n+p:
      Bool.and(same(x, y), plain(p, F32.add(x, 1.0), y))

def nan(+x: F32) -> Bool:
  F32.is_ne(x, x)

def special() -> Bool:
  +inf = F32.div(1.0, 0.0)
  +bad = F32.div(0.0, 0.0)
  +zero = F32.neg(0.0)
  Bool.and(U32.is_eq(F32.bits(F32.hypot(zero, zero)), 0),
    Bool.and(F32.is_eq(F32.hypot(inf, inf), inf),
      Bool.and(F32.is_eq(F32.hypot(F32.neg(inf), 3.0), inf),
        Bool.and(nan(F32.hypot(bad, 0.0)),
          Bool.and(nan(F32.hypot(0.0, bad)),
            Bool.and(nan(F32.hypot(bad, inf)), nan(F32.hypot(inf, bad))))))))

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.print("axes: " ++ Bool.show(up(127n, 1.0)))
    Unit <- IO.print("small lengths: " ++ Bool.show(down(149n, 1.0)))
    Unit <- IO.print("large lengths: " ++ Bool.show(large(125n, 1.0)))
    Unit <- IO.print("decimal lengths: " ++ Bool.show(
      Bool.and(triangle(1.0e30), triangle(1.0e-30))))
    Unit <- IO.print("integer lengths: " ++ Bool.show(
      Bool.and(exact(13.0, 84.0, 85.0), exact(27.0, 364.0, 365.0))))
    Unit <- IO.print("plain lengths: " ++ Bool.show(
      Bool.and(plain(199n, 1.0, 29.0), plain(199n, 0.1, 0.37))))
    Unit <- IO.print("largest axis: " ++ Bool.show(axis(3.4028234663852886e38)))
    IO.print("specials: " ++ Bool.show(special()))

#|axes: True
#|small lengths: True
#|large lengths: True
#|decimal lengths: True
#|integer lengths: True
#|plain lengths: True
#|largest axis: True
#|specials: True
