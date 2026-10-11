# regression witness: visiting the flat value first must not reuse its
# display layout for array elements of the same type.
import Base

type D is Data:
  Da{}
  Db{}

def main() -> (Sigma<&2, &2, Sigma<&2, &2, U32, _ => D>, _ => Unit> & Array<Sigma<&2, &2, Sigma<&2, &2, U32, _ => D>, _ => Unit>>):
  (((2, Db{}), Unit{}), [((1, Db{}), Unit{}) : Sigma<&2, &2, Sigma<&2, &2, U32, _ => D>, _ => Unit>*1n])

#|(((2, Db{}), Unit{}), [((1, Db{}), Unit{})])
