l = 1001

v1 = match l {
  x: x
}

v2 = match l {
  x: l
}

v3 = match [] {
  x: 2002
}

v4 = match l = 3003 {
  x: x
  y: 0
}

v5 = match p = 4004 with b = 5005 {
  x: b
}

main = (v1, v2, v3, v4, v5)
