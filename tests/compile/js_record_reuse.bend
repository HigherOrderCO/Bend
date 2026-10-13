# Record reuse preserves fields, constructor tags and closure captures.
# Runtime input prevents constant folding of these checks.
import Base

type Point is Data:
  Point{x: U32, y: U32}

type Box is Data:
  Box{point: Point, label: U32}

type Choice is Data:
  Left{point: Point}
  Right{point: Point}

type Indexed<-n: Nat> is Data:
  Indexed{-tag: Nat, value: U32}

type Item<-A: Data> is Data:
  Item{value: A}

type EmptyRecord is Data:
  EmptyRecord{}

type Holder is Type:
  Holder{f: U32 -> Point}

type Holder2 is Type:
  Holder2{f: U32 -> Holder}

type BoxHolder is Type:
  BoxHolder{f: U32 -> Box}

def keep(+p: Point) -> Point:
  Point{x, y} = p
  p

def swapped(p: Point) -> Point:
  Point{x, y} = p
  Point{y, x}

def changed(p: Point) -> Point:
  Point{x, y} = p
  Point{U32.inc(x), y}

def cross(p: Point, q: Point) -> Point:
  Point{x, y} = p
  Point{u, v} = q
  Point{x, v}

def nested(+b: Box) -> Box:
  Box{point, label} = b
  Point{x, y} = point
  b

def nested_change(b: Box) -> Box:
  Box{point, label} = b
  Point{x, y} = point
  Box{Point{x, U32.inc(y)}, label}

def choice(+c: Choice) -> Choice:
  match c:
    case Left{p}:
      Point{x, y} = p
      c
    case Right{p}:
      Right{changed(p)}

def retag(c: Choice) -> Choice:
  match c:
    case Left{p}: Right{p}
    case Right{p}: Left{p}

def hold(+p: Point) -> Holder:
  Point{x, y} = p
  Holder{n => p}

def hold2(+p: Point) -> Holder2:
  Point{x, y} = p
  Holder2{n => Holder{m => p}}

def hold_box(+b: Box) -> BoxHolder:
  Box{point, label} = b
  Point{x, y} = point
  BoxHolder{n => b}

def closure_changed(p: Point) -> Holder:
  Point{x, y} = p
  Holder{n => Point{U32.add(x, n), y}}

def indexed(-n: Nat, +r: Indexed<n>) -> Indexed<n>:
  Indexed{tag, value} = r
  r

def item(-A: Data, +r: Item<A>) -> Item<A>:
  Item{value} = r
  r

def empty(+e: EmptyRecord) -> EmptyRecord:
  EmptyRecord{} = e
  e

def score(p: Point) -> U32:
  Point{x, y} = p
  U32.add(U32.mul(x, 100), y)

def score_box(b: Box) -> U32:
  Box{p, label} = b
  U32.add(score(p), label)

def score_choice(c: Choice) -> U32:
  match c:
    case Left{p}: score(p)
    case Right{p}: U32.add(10000, score(p))

def use(h: Holder) -> U32:
  Holder{f} = h
  score(f(7))

def use2(h: Holder2) -> U32:
  Holder2{f} = h
  use(f(0))

def use_box(h: BoxHolder) -> U32:
  BoxHolder{f} = h
  score_box(f(0))

def value(-n: Nat, r: Indexed<n>) -> U32:
  Indexed{tag, value} = r
  value

def sum(fs: List<(U32 -> Point)>) -> U32:
  match fs:
    case Nil{}: 0
    case f <> rest: U32.add(score(f(0)), sum(rest))

def saved(+n: Nat, +p: Point, fs: List<(U32 -> Point)>) -> U32:
  match n:
    case 0n:
      sum(fs)
    case 1n+k:
      Point{x, y} = p
      saved(k, Point{U32.inc(x), y}, (m => p) <> fs)

def main() -> IO(Unit):
  do IO<Unit>:
    +t : Nat <- IO.now()
    +x : U32 = U32.add(3, U32.from_nat(Nat.mod(t, 1n)))
    +p : Point = Point{x, 9}
    IO.print(U32.show(score(keep(p))))
    IO.print(U32.show(score(swapped(p))))
    IO.print(U32.show(score(changed(p))))
    IO.print(U32.show(score(cross(p, Point{10, 20}))))
    IO.print(U32.show(score_box(nested(Box{p, 57}))))
    IO.print(U32.show(score_box(nested_change(Box{p, 57}))))
    IO.print(U32.show(score_choice(choice(Left{p}))))
    IO.print(U32.show(score_choice(choice(Right{p}))))
    IO.print(U32.show(score_choice(retag(Left{p}))))
    IO.print(U32.show(score_choice(retag(Right{p}))))
    IO.print(U32.show(use(hold(p))))
    IO.print(U32.show(use2(hold2(p))))
    IO.print(U32.show(use_box(hold_box(Box{p, 57}))))
    IO.print(U32.show(use(closure_changed(p))))
    IO.print(U32.show(value(5n, indexed(5n, Indexed{7n, x}))))
    IO.print(U32.show(saved(Nat.add(4n, Nat.mod(t, 1n)), p, Nil{})))

#|309
#|903
#|409
#|320
#|366
#|367
#|309
#|10409
#|10309
#|309
#|309
#|309
#|366
#|1009
#|3
#|1836
