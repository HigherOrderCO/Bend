# borrow witness: a lookup that returns a field of a shared list's element.
# find passes value, a box field of its borrowed parameter, to an owned
# position, so it keeps value there and es stays borrowed (term_peek, no
# ctr_take): finds keeps nothing. late keeps value two calls past its match,
# in a continuation. A found value outlives its list (lookup sinks es before
# main prints it), and pair forks two lookups over one list.
import Base

type Entry is Data:
  Entry{key: U32, value: String}

def entries(n: Nat, +i: U32) -> List<&2, Entry>:
  match n:
    case 0n:
      []
    case 1n+p:
      Entry{i, U32.show((i * 7 : U32))} <> entries(p, (i + 1 : U32))

def hit(same: Bool, value: String) -> Maybe<&2, String>:
  match same:
    case True{}:
      Some{value}
    case False{}:
      None{}

def entry_hit(e: Entry, +q: U32) -> Maybe<&2, String>:
  match e:
    case Entry{key, value}:
      hit(U32.is_eq(key, q), value)

def find(es: List<&2, Entry>, +q: U32, found: Maybe<&2, String>) ->
    Maybe<&2, String>:
  match es found:
    case Nil{} f:
      f
    case e <> tail Some{v}:
      Some{v}
    case e <> tail None{}:
      find(tail, q, entry_hit(e, q))

def tag(same: Bool, value: String, +n: Nat) -> Maybe<&2, String>:
  match same:
    case True{}:
      Some{String.concat([value, "/", Nat.show(n)])}
    case False{}:
      None{}

def late_hit(e: Entry, +q: U32, +n: Nat) -> Maybe<&2, String>:
  match e:
    case Entry{key, value}:
      tag(U32.is_eq(key, q), value, n)

def late(es: List<&2, Entry>, +q: U32, found: Maybe<&2, String>) ->
    Maybe<&2, String>:
  match es found:
    case Nil{} f:
      f
    case e <> tail Some{v}:
      Some{v}
    case e <> tail None{}:
      +n = String.length(U32.show(q))
      late(tail, q, late_hit(e, q, n))

def show(m: Maybe<&2, String>) -> String:
  match m:
    case Some{s}:
      s
    case None{}:
      "none"

def lookup(q: U32) -> Maybe<&2, String>:
  +es = entries(50n, 0)
  find(es, q, None{})

def pair(+es: List<&2, Entry>) -> String:
  a b = find(es, 3, None{}) find(es, 49, None{})
  String.concat([show(a), " ", show(b)])

def lates(+es: List<&2, Entry>) -> String:
  String.concat([show(late(es, 12, None{})), " ",
    show(late(es, 50, None{}))])

def main() -> IO(Unit):
  do IO<Unit>:
    +es : List<&2, Entry> = entries(50n, 0)
    IO.print(show(lookup(7)))
    IO.print(show(lookup(99)))
    IO.print(pair(es))
    IO.print(lates(es))

#|49
#|none
#|21 343
#|84/2 none
