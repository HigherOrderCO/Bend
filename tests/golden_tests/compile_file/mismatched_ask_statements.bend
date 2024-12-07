def main:
  with IO:
    match _ = Bool/True:
      case Bool/False:
        x <- wrap(0)
      case Bool/False:
        x = wrap(0)
    return wrap(x)
