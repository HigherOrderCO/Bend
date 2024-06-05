fun0 = ![!0 ![!1 ![!2, !3]]]
fun1 = !(![!*!*])
fun2 = !!!*
fun3 = !1
fun4 = @n switch n {
  0: !0
  _: ![(fun4 n-1) (fun4 n-1)]
}

def imp0:
  return ![!0, ![!1, ![!2, !3]]]
def imp1:
  return !(![!*,!*])
def imp2:
  return !!!*
def imp3:
  return !1
def imp4(n):
  switch n:
    case 0:
      return !0
    case _:
      return ![imp4(n-1), imp4(n-1)]

main = *