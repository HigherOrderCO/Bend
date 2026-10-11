import Base

def make(-T: Type, -x: T, -y: T, +p: {x == y : T}) -> Array<{x == y : T}>:
  Array.new({x == y : T}, 2n, p)

def count(r: Array<{0n == 0n : Nat}> & U32) -> U32:
  (a, n) = r
  n

def halves(a: Array<{0n == 0n : Nat}>, b: Array<{0n == 0n : Nat}>) -> U32:
  match a:
    case ALeaf{p}:
      count(Array.size({0n == 0n : Nat}, b))
    case ANode{xs, ys}:
      U32.add(U32.mul(100, count(Array.size({0n == 0n : Nat}, xs))),
        count(Array.size({0n == 0n : Nat}, b)))

def after_swap(b: Array<{0n == 0n : Nat}>,
  r: Array<{0n == 0n : Nat}> & {0n == 0n : Nat}) -> U32:
  (a, q) = r
  halves(Array.set({0n == 0n : Nat}, a, 1, q), b)

def after_get(b: Array<{0n == 0n : Nat}>,
  r: Array<{0n == 0n : Nat}> & {0n == 0n : Nat}) -> U32:
  (a, p) = r
  after_swap(b, Array.swap({0n == 0n : Nat}, a, 3, p))

def sizes(r: Array<{0n == 0n : Nat}> & Array<{0n == 0n : Nat}>) -> U32:
  (a, b) = r
  after_get(b, Array.get({0n == 0n : Nat}, a, 6))

def main() -> IO(Unit):
  IO.print(U32.show(sizes(Array.clone({0n == 0n : Nat}, make(Nat, 0n, 0n, {==})))))

#|204
