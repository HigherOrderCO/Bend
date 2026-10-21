# a pure main prints a value of an imported type, alone and in an Array:
# its printer names the constructors and the type through an alias, so
# the source it writes parses (was `case lib:Circ{f0}:`, a parse error)
import Base
import ./imported_adt_lib.bend as L

def main() -> Array<L.Shape> & L.Shape:
  (Array.new(L.Shape, 1n, L.Circ{1}), L.Sq{4, L.Circ{5}})

#|([imported_adt_lib.Circ{1}, imported_adt_lib.Circ{1}], imported_adt_lib.Sq{4, imported_adt_lib.Circ{5}})
