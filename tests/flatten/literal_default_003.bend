# literal ladder with a default: each literal splits the miss chain,
# the default closes it
# ported from bend3: 0/1/2 become Z/S{Z}/S{S{Z}} nested ctor patterns
# and the arm values become ctors (bend has no number patterns)
type W is Data:
  Z{}
  S{p: W}

type R is Data:
  K100{}
  K101{}
  K102{}
  KD{k: W}

law main:
  for x: W
  R

def main(x):
  match x:
    case Z{}:
      K100{}
    case S{Z{}}:
      K101{}
    case S{S{Z{}}}:
      K102{}
    case k:
      KD{k}

#|\{Z: K100{}; S: \{Z: K101{}; S: \{Z: K102{}; (_4 => KD{S{S{_4}}})}}}
