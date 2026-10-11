# THE ORDER BAN, locals: the head-binder window is one statement deep,
# so a constructor destructure must sit adjacent to the statement that
# binds its scrutinee -- any local between them takes the head and the
# destructure dies at the ban, like every out-of-order match.
type Nw is Data:
  Zw{}
  Sw{p: Nw}

law f:
  for x: Nw
  Nw

def f(x):
  y = Sw{x}
  z = Zw{}
  Sw{p} = y
  p

law main:
  Nw

def main():
  Zw{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : 'y' can't be matched in this position (it is matched after a local statement or after a match on a later binder, it was already matched, or it is a def)
#|Location:
#|15 |   z = Zw{}
#|16>|   Sw{p} = y
#|   |           ^
#|17 |   p
#|exit 1
