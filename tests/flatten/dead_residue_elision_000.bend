type T is Data:
  A{}
  C{}
  B{}

type R is Data:
  XA{}
  XC{}
  XB{}
  XD{w: T}

law main:
  for a: T
  R

def main(a):
  match a:
    case A{}:
      XA{}
    case C{}:
      XC{}
    case B{}:
      XB{}
    case a:
      XD{a}

#|\{A: XA{}; C: XC{}; B: XB{}}
