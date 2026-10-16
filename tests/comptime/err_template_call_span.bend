# The extra application belongs to the template, not this supplied name.
import call_template.bend as T

def id(x: Quant) -> Quant:
  x

def main() -> Quant:
  T.call(~id)

#|Error:
#|- expected : a function type
#|- observed : Quant
#|Context:
#|- f : @_:Quant -> Quant
#|Location: call_template.call~0
#|2 | def call(~f: Quant -> Quant) -> Quant:
#|3>|   f(&1, &2)
#|4 |
#|exit 1
