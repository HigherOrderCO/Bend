# only a statement a[i] <- v is the write let: an explicit Array.set(..)
# call or a parenthesized write followed by ";" is a term, so an inline
# match's next row stays a row (#1126)
import Base

def main() -> Array<U32> & Array<U32>:
  f = {(\{False: w => Array.set(U32, w, 0, 9); sx => v => v}) : (Bool) -> (Array<U32> -> Array<U32>)}(True{})
  g = {(\{False: w => (w[0] <- 9); sx => v => v}) : (Bool) -> (Array<U32> -> Array<U32>)}(False{})
  (f([1 : U32^1n]), g([1 : U32^1n]))

#|([1, 1], [9, 1])
