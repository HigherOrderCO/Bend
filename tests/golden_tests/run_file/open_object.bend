object Pair { fst, snd }

def main:
  x = Pair(1, 2)
  open Pair: x
  return x.fst