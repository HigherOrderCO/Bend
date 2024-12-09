type Bool = True | False
def main:
  with IO:
    match _ = Bool/True:
      case Bool/True:
        x <- wrap(0)
      case Bool/False:
        x <- wrap(0)
    return wrap(x)
