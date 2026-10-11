# regression witness: a datatype hidden under a rewrite stuck on an open
# proof, shared and then read as itself. The C facts saw the rewrite and
# not the datatype under it, so the share sealed the node and the cold
# reader took the seal as the record: a wrong sum, or a fault on the tree.
# Each def shares one shape: a record, a sum, a rewrite of a rewrite, a
# type argument, a field of a generic record and a recursive tree.
import Base

def unhide(-T: Data, e: {0 == 0 : U32}) -> ((%q@e : Data; T) -> T):
  %h@e : ((%q@h : Data; T) -> T)
  x => x

def hide(-T: Data, e: {0 == 0 : U32}) -> (T -> (%q@e : Data; T)):
  %h@e : (T -> (%q@h : Data; T))
  x => x

def hold(x: U32) -> U32:
  x

def opaque(u: Unit) -> U32:
  a b = hold(1) hold(0)
  (a + b : U32)

type Rec is Data:
  Rec{a: U32, b: U32, c: U32}

def mk(+x: U32) -> Rec:
  Rec{x, (x + 1 : U32), (x + 7 : U32)}

def use(r: Rec) -> U32:
  Rec{a, b, c} = r
  (a + c : U32)

def rec(+e: {0 == 0 : U32}, +x: (%h@e : Data; Rec)) -> U32:
  (use(unhide(Rec, e, x)) + use(unhide(Rec, e, x)) : U32)

type S is Data:
  SA{x: U32}
  SB{y: U32, z: U32}

def get(s: S) -> U32:
  match s:
    case SA{x}:
      x
    case SB{y, z}:
      (y + z : U32)

def sum(+e: {0 == 0 : U32}, +s: (%h@e : Data; S)) -> U32:
  (get(unhide(S, e, s)) + get(unhide(S, e, s)) : U32)

def nest(+e: {0 == 0 : U32}, +x: (%h@e : Data; (%k@e : Data; Rec))) -> U32:
  (use(unhide(Rec, e, unhide((%k@e : Data; Rec), e, x)))
    + use(unhide(Rec, e, unhide((%k@e : Data; Rec), e, x))) : U32)

def gen(-A: Data, +e: {0 == 0 : U32}, +x: (%h@e : Data; A), f: A -> U32,
  g: A -> U32) -> U32:
  (f(unhide(A, e, x)) + g(unhide(A, e, x)) : U32)

type Holder<-A: Data> is Data:
  H{a: A, n: U32}

def peek(+e: {0 == 0 : U32}, h: Holder<(%h@e : Data; Rec)>) -> U32:
  H{a, n} = h
  (use(unhide(Rec, e, a)) + n : U32)

def held(+e: {0 == 0 : U32}, +h: Holder<(%h@e : Data; Rec)>) -> U32:
  (peek(e, h) + peek(e, h) : U32)

type Tree is Data:
  Leaf{}
  Node{l: Tree, v: U32, r: Tree}

def total(t: Tree) -> U32:
  match t:
    case Leaf{}:
      0
    case Node{l, v, r}:
      (total(l) + v + total(r) : U32)

def tree(+e: {0 == 0 : U32}, +t: (%h@e : Data; Tree)) -> U32:
  (total(unhide(Tree, e, t)) + total(unhide(Tree, e, t)) : U32)

def main() -> U32 & U32 & U32 & U32 & U32 & U32:
  +x = opaque(Unit{})
  +r = mk(x)
  (rec({==}, r), sum({==}, SB{x, (x + 5 : U32)}), nest({==}, r),
    gen(Rec, {==}, r, use, use), held({==}, H{hide(Rec, {==}, r), 100}),
    tree({==}, Node{Node{Leaf{}, x, Leaf{}}, (x + 1 : U32), Leaf{}}))

#|(18, 14, 18, 18, 218, 6)
