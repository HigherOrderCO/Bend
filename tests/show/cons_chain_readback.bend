# a cons chain off a non-list tail prints as a <> b <> t, at the parser's
# precedence: the interpreter shows a stuck tail (a parameter); main is a
# function, so no compiled lane
import Base

def chain(t: List<List<U32>>) -> List<List<U32>>:
  Con{Nil{}, Con{Nil{}, t}}

def main() -> List<List<U32>> -> List<List<U32>>:
  t => chain(t)

#|t => [] <> [] <> t
