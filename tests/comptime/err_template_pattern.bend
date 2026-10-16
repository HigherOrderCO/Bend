# Substitution must not turn a reference inside a constructor into a binder.
type Box is Type:
  Mk{x: Quant -> Quant}

def id(x: Quant) -> Quant:
  x

def bad(~p: Box, box: Box) -> Quant:
  match box:
    case p: &1

def main() -> Quant:
  bad(~Mk{id}, Mk{id})

#|Error:
#|- expected : a pattern (a binder or a constructor)
#|- observed : id
#|Location:
#| 9 |   match box:
#|10>|     case p: &1
#|11 |
#|exit 1
