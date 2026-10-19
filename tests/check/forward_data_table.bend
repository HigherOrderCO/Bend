# refusal pin: bend3's --no-halt door let use2 duplicate a value of the
# FORWARD datatype D, and the pin was that an unchecked datatype must
# never certify Data-ness. bend declares every datatype up front, so the
# forward D3 in Two's field is legal, and the refusal is the duplication
# itself: D3 is Type, so use2 consumes x twice.
# ported from bend3: the Pair.Pair redefinition half is pinned by
# check_data_table_checked_000 (the duplicate-declaration wall)
import Base

type Two is Type:
  MkTwo{a: D3<U32>, b: D3<U32>}

law use2:
  for x: D3<U32>
  Two

def use2(x):
  MkTwo{x, x}

type D3<-result: Type> is Type:
  MkD3{fun: D3<result> -> result}

law main:
  U32

def main(): 0

#|SOME PROOFS FAIL
#|Error:
#|- message  : x appears 2 times (at 18:9, 18:12), but a plain binding is used at most once
#|Location: use2
#|17 | def use2(x):
#|18>|   MkTwo{x, x}
#|   |            ^
#|19 |
#|Note: a +x binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
