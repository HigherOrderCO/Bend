# a module's defs name its dotted datatypes before declaring them
import Base
import ./forward_dotted_lib.bend as L

def main() -> U32:
  L.get(3n, L.Cell.Mk{5})

#|5
