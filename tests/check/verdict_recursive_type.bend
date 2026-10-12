# The frontend accepts this recursive type.
# The survey regression checks its translator failure classification.
import Base

def G() -> Type:
  (G -> U32) -> U32

def run(g: G, k: G -> U32) -> U32:
  g(k)

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
