# a Base-free module with its own Bool.or: its || means THIS Bool.or,
# alone and imported alike. Before, the sugar named a bare Bool.or, so
# whichever file was the entry supplied the law's meaning
type B is Data:
  T{}
  F{}

type Empty is Data:

def Bool.or(a: B, b: B) -> Type:
  Empty

law l:
  for x: B
  T{} || x

#|SOME PROOFS FAIL
#|Error: 1 TODO found.
#|The code is incomplete, and not a valid proof yet.
#|exit 1
