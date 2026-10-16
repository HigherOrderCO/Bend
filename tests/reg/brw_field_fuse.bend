# borrow witness: a field kept in a body fused into its one caller. first
# and tag run inside outer, so value, kept for tag's Some, is rooted in
# outer~2 while the scope is tag's: the keep heats outer's List<&2, Entry>
# (Entry builds seal), not tag's parameters. Heating tag's Bool and Nat left
# value raw, and lookup2 printed freed memory.
import Base

type Entry is Data:
  Entry{key: U32, value: String}

def entries(n: Nat, +i: U32) -> List<&2, Entry>:
  match n:
    case 0n:
      []
    case 1n+p:
      Entry{i, U32.show((i * 7 : U32))} <> entries(p, (i + 1 : U32))

def len(es: List<&2, Entry>) -> Nat:
  match es:
    case Nil{}:
      0n
    case e <> tail:
      (1n + len(tail) : Nat)

def tag(same: Bool, value: String, +n: Nat) -> Maybe<&2, String>:
  match same:
    case True{}:
      Some{String.concat([value, "/", Nat.show(n)])}
    case False{}:
      None{}

def first(es: List<&2, Entry>, +q: U32) -> Maybe<&2, String>:
  match es:
    case Nil{}:
      None{}
    case e <> tail:
      match e:
        case Entry{key, value}:
          tag(U32.is_eq(key, q), value, len(tail))

def outer(+q: U32, n: Nat, es: List<&2, Entry>) -> Maybe<&2, String>:
  match n:
    case 0n:
      None{}
    case 1n+p:
      first(es, q)

def show(m: Maybe<&2, String>) -> String:
  match m:
    case Some{s}:
      s
    case None{}:
      "none"

def lookup(q: U32) -> String:
  +es = entries(5n, 0)
  a = outer(q, 1n, es)
  String.concat([show(a), " ", Nat.show(len(es))])

def lookup2(q: U32) -> String:
  +es = entries(9n, 3)
  a = outer(q, 1n, es)
  String.concat([show(a), " ", Nat.show(len(es))])

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(lookup(0))
    IO.print(lookup2(3))

#|0/4 5
#|21/8 9
