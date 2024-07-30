type MyOption = (Some val) | (None)

def MyOption/bind(val, nxt):
  match val:
    case MyOption/Some:
      nxt = undefer(nxt)
      return nxt(val.val)
    case MyOption/None:
      return MyOption/None

def MyOption/wrap(val):
  return MyOption/Some(val)
