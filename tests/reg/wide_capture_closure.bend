# Four tagged 67-word captures exceed the segment limit. Boxing must
# preserve both constructor arms and the parent's shared heap fields.
import Base

type R0 is Data:
  R0{a: U32, b: U32, c: U32, d: U32, e: U32, f: U32, g: U32, h: U32}

type R1 is Data:
  R1{a: R0, b: R0, c: R0, d: R0, e: R0, f: R0, g: R0, h: R0}

type Wide is Data:
  Large{words: R1, text: String, nums: List<&2, U32>}
  Small{text: String}

def r0(+x: U32) -> R0:
  R0{x, x, x, x, x, x, x, x}

def r1(+x: U32) -> R1:
  R1{r0(x), r0(x), r0(x), r0(x), r0(x), r0(x), r0(x), r0(x)}

def sum0(x: R0) -> U32:
  match x:
    case R0{a, b, c, d, e, f, g, h}:
      U32.add(U32.add(U32.add(a, b), U32.add(c, d)), U32.add(U32.add(e, f), U32.add(g, h)))

def sum1(x: R1) -> U32:
  match x:
    case R1{a, b, c, d, e, f, g, h}:
      U32.add(U32.add(U32.add(sum0(a), sum0(b)), U32.add(sum0(c), sum0(d))),
        U32.add(U32.add(sum0(e), sum0(f)), U32.add(sum0(g), sum0(h))))

def make(large: Bool, +n: U32) -> Wide:
  match large:
    case True{}:
      Large{r1(n), U32.show(n), [n, U32.add(n, 1)]}
    case False{}:
      Small{U32.show(n)}

def list_sum(xs: List<&2, U32>) -> U32:
  match xs:
    case Nil{}:
      0
    case Con{h, t}:
      U32.add(h, list_sum(t))

def score(v: Wide) -> U32:
  match v:
    case Large{r, s, xs}:
      U32.add(U32.add(sum1(r), U32.from_nat(String.length(s))), list_sum(xs))
    case Small{s}:
      U32.from_nat(String.length(s))

def score4(a: Wide, b: Wide, c: Wide, d: Wide) -> U32:
  U32.add(U32.add(score(a), score(b)), U32.add(score(c), score(d)))

def apply(f: U32 -> U32, x: U32) -> U32:
  f(x)

def check(+a: Wide, +b: Wide, +c: Wide, +d: Wide) -> Bool:
  f : U32 -> U32 = x => U32.add(x, score4(a, b, c, d))
  got = apply(f, 7)
  Bool.and(U32.is_eq(got, 277), U32.is_eq(score4(a, b, c, d), 270))

def run(n: Nat, +x: U32, ok: Bool) -> Bool:
  match n:
    case 0n:
      ok
    case 1n+p:
      run(p, x, Bool.and(ok, check(make(True{}, x), make(False{}, U32.add(x, 1)), make(True{}, U32.add(x, 2)), make(False{}, U32.add(x, 3)))))

def main() -> IO(Unit):
  do IO<Unit>:
    args : List<String> <- IO.args()
    x : U32 = U32.from_nat(List.length(&1, String, args))
    IO.print(Bool.show(run(30n, x, True{})))
#|True
