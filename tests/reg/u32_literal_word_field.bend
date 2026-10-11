# regression witness (#1100): the C emitter folds a call on a U32
# literal, so `case U32{w}` on 5 hands the body the literal's Word(32n),
# a WCon chain no annotation covers. emit_ctr emitted every field with
# no type, so the chain's inner WCon, a constructor of the parametrized
# Word.Con<p>, died with "a constructor outside a datatype"; the
# evaluator and the JS backend answered. emit_ctr now gives each field
# its domain from the constructor's telescope, filled with the datatype's
# arguments. The shapes: the word alone, in a box, in a pair behind a
# parametrized datatype (as #1100 reported it), in a field whose type
# names an earlier field, and stored into an array by Array.new and
# Array.set.
import Base

type Df<-T: Type> is Type:
  Df{n: U32, xs: T}

type Box is Data:
  Box{w: Word(32n)}

type Sized is Data:
  Sized{n: Nat, w: Word(n)}

def word_of(u: U32) -> Word(32n):
  match u:
    case U32{w}:
      w

def unbox(b: Box) -> Word(32n):
  match b:
    case Box{w}:
      w

def chunk(df: Df<U32>, u: U32) -> Df<U32> & Word(32n):
  (df, word_of(u))

def second(p: Df<U32> & Word(32n)) -> Word(32n):
  match p:
    case (df, w):
      w

def size(s: Sized) -> Nat:
  match s:
    case Sized{n, w}:
      Word.to_nat(n, w)

def fill(u: U32) -> Array<Word(32n)>:
  match u:
    case U32{w}:
      [w : Word(32n) * 2n]

def put(u: U32, xs: Array<Word(32n)>) -> Array<Word(32n)>:
  match u:
    case U32{w}:
      Array.set(Word(32n), xs, 1, w)

def got(p: Array<Word(32n)> & Word(32n)) -> Word(32n):
  match p:
    case (xs, w):
      w

def show(w: Word(32n)) -> String:
  Nat.show(Word.to_nat(32n, w))

def main() -> IO(Unit):
  xs = put(10, fill(9))
  do IO<Unit>:
    IO.print(show(word_of(5)))
    IO.print(show(unbox(Box{word_of(6)})))
    IO.print(show(second(chunk(Df{1, 2}, 7))))
    IO.print(Nat.show(size(Sized{32n, word_of(8)})))
    IO.print(show(got(Array.get(Word(32n), xs, 1))))

#|5
#|6
#|7
#|8
#|10
