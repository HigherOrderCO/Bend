# the entry declares its own Tuple, Con and Nil; the module's sugar still
# builds the module's constructors
import ./list_own_lib.bend as L

type Tuple is Data:
  Bad{}

type List is Data:
  Nil{}
  Con{}

def main() -> L.List<L.T>:
  L.ys()

#|list_own_lib.Con{list_own_lib.A{}, list_own_lib.Con{list_own_lib.B{}, list_own_lib.Nil{}}}
