# the classic negative-datatype loop needs the function field twice
# (omega(L{f}) = f(L{f})), which linearity rejects: f is Lone.
# ported from bend3
import Base

type Tn is Type:
  Ln{f: Tn -> Tn}
  Vn{n: U32}

law omega:
  for t: Tn
  Tn

def omega(t):
  match t:
    case Ln{f}:
      f(Ln{f})
    case Vn{n}:
      Vn{n}

law main:
  Tn

def main(): omega(Ln{t => t})

#|SOME PROOFS FAIL
#|Error:
#|- message  : f appears 2 times (at 17:7, 17:12), but a plain binding is used at most once
#|Location: omega
#|16 |     case Ln{f}:
#|17>|       f(Ln{f})
#|   |            ^
#|18 |     case Vn{n}:
#|Note: a +f binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|Note: f is bound by a pattern: put the + on the matched parameter, +t.
#|exit 1
