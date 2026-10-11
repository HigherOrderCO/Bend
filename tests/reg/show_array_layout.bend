# regression witness: an array element and a flat value of the same
# type need different display layouts; sharing one printed garbage.
import Base

type D is Data:
  Db{}

def main() -> (Array<Sigma<&2, &2, Sigma<&2, &2, U32, _ => D>, _ => Unit>> & Sigma<&2, &2, Sigma<&2, &2, U32, _ => D>, _ => Unit>):
  ([((1, Db{}), Unit{}) : Sigma<&2, &2, Sigma<&2, &2, U32, _ => D>, _ => Unit>*1n], ((2, Db{}), Unit{}))

#|([((1, Db{}), Unit{})], (2, Db{}), Unit{})
