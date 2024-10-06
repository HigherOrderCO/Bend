type MyOption(A):
  Some { val: A }
  None

def MyOption/bind(val: MyOption(A), nxt: (Id -> Id) -> A -> MyOption(B)) -> MyOption(B):
  match val:
    case MyOption/Some:
      nxt = undefer(nxt)
      return nxt(val.val)
    case MyOption/None:
      return MyOption/None

def MyOption/wrap(val: A) -> MyOption(A):
  return MyOption/Some(val)
