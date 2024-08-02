type Bool:
  T
  F

def main:
  with IO:
    match _ = Bool/T:
      case Bool/T:
        x <- wrap(0)
      case Bool/F:
        x = wrap(0)
    return wrap(x)
