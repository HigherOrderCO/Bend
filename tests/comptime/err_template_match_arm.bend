# A function name cannot be a constructor arm; report its use in the template.
type Bit is Data:
  Off{}

def id(x: Quant) -> Quant:
  x

def bad(~f: Quant -> Quant) -> Bit -> Bit:
  \{f: Off{}}

def main() -> Bit -> Bit:
  bad(~id)

#|Error:
#|- expected : a constructor of Bit (missing, or already matched)
#|- observed : \{id: Off{}}
#|Context:
#|- f : @_:Quant -> Quant
#|Location: bad~0
#| 8 | def bad(~f: Quant -> Quant) -> Bit -> Bit:
#| 9>|   \{f: Off{}}
#|10 |
#|exit 1
