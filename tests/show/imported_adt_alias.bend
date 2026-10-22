# main-file defs named like the printer's import aliases (`_ns1.Circ`)
# leave those aliases fresh: the printer of an imported type still builds
# (was "the alias _ns1 shadows _ns1.Circ")
import Base
import ./imported_adt_lib.bend as L

def _ns0.Circ() -> U32:
  0

def _ns1.Circ() -> U32:
  1

def _ns2.Circ() -> U32:
  2

def main() -> L.Shape & U32 & U32 & U32:
  (L.Circ{1}, _ns0.Circ(), _ns1.Circ(), _ns2.Circ())

#|(imported_adt_lib.Circ{1}, 0, 1, 2)
