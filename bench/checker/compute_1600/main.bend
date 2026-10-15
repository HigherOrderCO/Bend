law cong:
  for -A: Type
  for -B: Type
  for -f: A -> B
  for -a: A
  for -b: A
  for  e: {a == b : A}
  {f(a) == f(b) : B}

def cong(A, B, f, a, b, e):
  %e : {f(a) == f(_) : B}
  {==}

type Nat is Data:
  Z{}
  S{pred: Nat}

type Sigma<-A: Type, -B: @-x: A -> Type> is Type:
  Tuple{fst: A, snd: B(fst)}

law Pair:
  for -A: Type
  for -B: Type
  Type

def Pair(A, B):
  Sigma<A, _ => B>

law Cpy:
  for -A: Type
  for -x: A
  Type

def Cpy(A, x):
  Sigma<A, y => {x == y : A}>

law Cpyable:
  for -T: Type
  Type

def Cpyable(T):
  @x: T -> Pair(Cpy(T, x), Cpy(T, x))

law Equal.cong2:
  for -A: Type
  for -B: Type
  for -C: Type
  for -f: A -> @y: B -> C
  for -a: A
  for -c: A
  for -b: B
  for -d: B
  for ea: {a == c : A}
  for eb: {b == d : B}
  {f(a)(b) == f(c)(d) : C}

def Equal.cong2(A, B, C, f, a, c, b, d, ea, eb):
  %eb : {f(a)(b) == f(c)(_) : C}
  %ea : {f(a)(b) == f(_)(b) : C}
  {==}

law add:
  for a: Nat
  for b: Nat
  Nat

def add(a, b):
  match a:
    case Z{}:
      b
    case S{a}:
      S{add(a, b)}

law Nat.cpy:
  for n: Nat
  Pair(Cpy(Nat, n), Cpy(Nat, n))

law Nat.cpy.succ:
  for -p: Nat
  for c: Pair(Cpy(Nat, p), Cpy(Nat, p))
  Pair(Cpy(Nat, S{p}), Cpy(Nat, S{p}))

def Nat.cpy.succ(p, c):
  Tuple{Tuple{a, ea}, Tuple{b, eb}} = c
  Tuple{Tuple{S{a}, cong(Nat, Nat, x => S{x}, p, a, ea)}, Tuple{S{b}, cong(Nat, Nat, x => S{x}, p, b, eb)}}

def Nat.cpy(n):
  match n:
    case Z{}:
      Tuple{Tuple{Z{}, {==}}, Tuple{Z{}, {==}}}
    case S{p}:
      Nat.cpy.succ(p, Nat.cpy(p))

law mul:
  for a: Nat
  for +b: Nat
  Nat

def mul(a, b):
  match a:
    case Z{}:
      Z{}
    case S{a}:
      add(b, mul(a, b))

law n0:
  Nat

def n0():
  Z{}

law n1:
  Nat

def n1():
  S{n0()}

law n2:
  Nat

def n2():
  S{n1()}

law n3:
  Nat

def n3():
  S{n2()}

law n4:
  Nat

def n4():
  S{n3()}

law n5:
  Nat

def n5():
  S{n4()}

law n6:
  Nat

def n6():
  S{n5()}

law n7:
  Nat

def n7():
  S{n6()}

law n8:
  Nat

def n8():
  S{n7()}

law n9:
  Nat

def n9():
  S{n8()}

law n10:
  Nat

def n10():
  S{n9()}

law n11:
  Nat

def n11():
  S{n10()}

law n12:
  Nat

def n12():
  S{n11()}

law n13:
  Nat

def n13():
  S{n12()}

law n14:
  Nat

def n14():
  S{n13()}

law n15:
  Nat

def n15():
  S{n14()}

law n16:
  Nat

def n16():
  S{n15()}

law n17:
  Nat

def n17():
  S{n16()}

law n18:
  Nat

def n18():
  S{n17()}

law n19:
  Nat

def n19():
  S{n18()}

law n20:
  Nat

def n20():
  S{n19()}

law n21:
  Nat

def n21():
  S{n20()}

law n22:
  Nat

def n22():
  S{n21()}

law n23:
  Nat

def n23():
  S{n22()}

law n24:
  Nat

def n24():
  S{n23()}

law n25:
  Nat

def n25():
  S{n24()}

law n26:
  Nat

def n26():
  S{n25()}

law n27:
  Nat

def n27():
  S{n26()}

law n28:
  Nat

def n28():
  S{n27()}

law n29:
  Nat

def n29():
  S{n28()}

law n30:
  Nat

def n30():
  S{n29()}

law n31:
  Nat

def n31():
  S{n30()}

law n32:
  Nat

def n32():
  S{n31()}

type Duo<-A: Type, -B: Type> is Type:
  Duo{fst: A, snd: B}

law Tup:
  for n: Nat
  Type

def Tup(n):
  match n:
    case Z{}:
      Nat
    case S{p}:
      Duo<Nat, Tup(p)>

law cm0:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm0():
  {==}

law tv0:
  Tup(n4())

def tv0():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1():
  {==}

law tv1:
  Tup(n7())

def tv1():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm2:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm2():
  {==}

law tv2:
  Tup(n10())

def tv2():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm3:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm3():
  {==}

law tv3:
  Tup(n13())

def tv3():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm4:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm4():
  {==}

law tv4:
  Tup(n16())

def tv4():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm5:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm5():
  {==}

law tv5:
  Tup(n19())

def tv5():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm6:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm6():
  {==}

law tv6:
  Tup(n22())

def tv6():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm7:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm7():
  {==}

law tv7:
  Tup(n25())

def tv7():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm8:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm8():
  {==}

law tv8:
  Tup(n28())

def tv8():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm9:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm9():
  {==}

law tv9:
  Tup(n31())

def tv9():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm10:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm10():
  {==}

law tv10:
  Tup(n6())

def tv10():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm11:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm11():
  {==}

law tv11:
  Tup(n9())

def tv11():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm12:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm12():
  {==}

law tv12:
  Tup(n12())

def tv12():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm13:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm13():
  {==}

law tv13:
  Tup(n15())

def tv13():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm14:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm14():
  {==}

law tv14:
  Tup(n18())

def tv14():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm15:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm15():
  {==}

law tv15:
  Tup(n21())

def tv15():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm16:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm16():
  {==}

law tv16:
  Tup(n24())

def tv16():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm17:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm17():
  {==}

law tv17:
  Tup(n27())

def tv17():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm18:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm18():
  {==}

law tv18:
  Tup(n30())

def tv18():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm19:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm19():
  {==}

law tv19:
  Tup(n5())

def tv19():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm20:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm20():
  {==}

law tv20:
  Tup(n8())

def tv20():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm21:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm21():
  {==}

law tv21:
  Tup(n11())

def tv21():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm22:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm22():
  {==}

law tv22:
  Tup(n14())

def tv22():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm23:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm23():
  {==}

law tv23:
  Tup(n17())

def tv23():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm24:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm24():
  {==}

law tv24:
  Tup(n20())

def tv24():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm25:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm25():
  {==}

law tv25:
  Tup(n23())

def tv25():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm26:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm26():
  {==}

law tv26:
  Tup(n26())

def tv26():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm27:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm27():
  {==}

law tv27:
  Tup(n29())

def tv27():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm28:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm28():
  {==}

law tv28:
  Tup(n4())

def tv28():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm29:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm29():
  {==}

law tv29:
  Tup(n7())

def tv29():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm30:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm30():
  {==}

law tv30:
  Tup(n10())

def tv30():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm31:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm31():
  {==}

law tv31:
  Tup(n13())

def tv31():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm32:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm32():
  {==}

law tv32:
  Tup(n16())

def tv32():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm33:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm33():
  {==}

law tv33:
  Tup(n19())

def tv33():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm34:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm34():
  {==}

law tv34:
  Tup(n22())

def tv34():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm35:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm35():
  {==}

law tv35:
  Tup(n25())

def tv35():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm36:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm36():
  {==}

law tv36:
  Tup(n28())

def tv36():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm37:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm37():
  {==}

law tv37:
  Tup(n31())

def tv37():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm38:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm38():
  {==}

law tv38:
  Tup(n6())

def tv38():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm39:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm39():
  {==}

law tv39:
  Tup(n9())

def tv39():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm40:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm40():
  {==}

law tv40:
  Tup(n12())

def tv40():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm41:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm41():
  {==}

law tv41:
  Tup(n15())

def tv41():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm42:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm42():
  {==}

law tv42:
  Tup(n18())

def tv42():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm43:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm43():
  {==}

law tv43:
  Tup(n21())

def tv43():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm44:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm44():
  {==}

law tv44:
  Tup(n24())

def tv44():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm45:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm45():
  {==}

law tv45:
  Tup(n27())

def tv45():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm46:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm46():
  {==}

law tv46:
  Tup(n30())

def tv46():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm47:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm47():
  {==}

law tv47:
  Tup(n5())

def tv47():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm48:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm48():
  {==}

law tv48:
  Tup(n8())

def tv48():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm49:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm49():
  {==}

law tv49:
  Tup(n11())

def tv49():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm50:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm50():
  {==}

law tv50:
  Tup(n14())

def tv50():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm51:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm51():
  {==}

law tv51:
  Tup(n17())

def tv51():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm52:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm52():
  {==}

law tv52:
  Tup(n20())

def tv52():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm53:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm53():
  {==}

law tv53:
  Tup(n23())

def tv53():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm54:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm54():
  {==}

law tv54:
  Tup(n26())

def tv54():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm55:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm55():
  {==}

law tv55:
  Tup(n29())

def tv55():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm56:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm56():
  {==}

law tv56:
  Tup(n4())

def tv56():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm57:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm57():
  {==}

law tv57:
  Tup(n7())

def tv57():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm58:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm58():
  {==}

law tv58:
  Tup(n10())

def tv58():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm59:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm59():
  {==}

law tv59:
  Tup(n13())

def tv59():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm60:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm60():
  {==}

law tv60:
  Tup(n16())

def tv60():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm61:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm61():
  {==}

law tv61:
  Tup(n19())

def tv61():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm62:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm62():
  {==}

law tv62:
  Tup(n22())

def tv62():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm63:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm63():
  {==}

law tv63:
  Tup(n25())

def tv63():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm64:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm64():
  {==}

law tv64:
  Tup(n28())

def tv64():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm65:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm65():
  {==}

law tv65:
  Tup(n31())

def tv65():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm66:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm66():
  {==}

law tv66:
  Tup(n6())

def tv66():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm67:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm67():
  {==}

law tv67:
  Tup(n9())

def tv67():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm68:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm68():
  {==}

law tv68:
  Tup(n12())

def tv68():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm69:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm69():
  {==}

law tv69:
  Tup(n15())

def tv69():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm70:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm70():
  {==}

law tv70:
  Tup(n18())

def tv70():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm71:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm71():
  {==}

law tv71:
  Tup(n21())

def tv71():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm72:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm72():
  {==}

law tv72:
  Tup(n24())

def tv72():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm73:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm73():
  {==}

law tv73:
  Tup(n27())

def tv73():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm74:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm74():
  {==}

law tv74:
  Tup(n30())

def tv74():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm75:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm75():
  {==}

law tv75:
  Tup(n5())

def tv75():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm76:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm76():
  {==}

law tv76:
  Tup(n8())

def tv76():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm77:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm77():
  {==}

law tv77:
  Tup(n11())

def tv77():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm78:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm78():
  {==}

law tv78:
  Tup(n14())

def tv78():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm79:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm79():
  {==}

law tv79:
  Tup(n17())

def tv79():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm80:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm80():
  {==}

law tv80:
  Tup(n20())

def tv80():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm81:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm81():
  {==}

law tv81:
  Tup(n23())

def tv81():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm82:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm82():
  {==}

law tv82:
  Tup(n26())

def tv82():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm83:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm83():
  {==}

law tv83:
  Tup(n29())

def tv83():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm84:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm84():
  {==}

law tv84:
  Tup(n4())

def tv84():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm85:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm85():
  {==}

law tv85:
  Tup(n7())

def tv85():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm86:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm86():
  {==}

law tv86:
  Tup(n10())

def tv86():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm87:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm87():
  {==}

law tv87:
  Tup(n13())

def tv87():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm88:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm88():
  {==}

law tv88:
  Tup(n16())

def tv88():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm89:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm89():
  {==}

law tv89:
  Tup(n19())

def tv89():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm90:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm90():
  {==}

law tv90:
  Tup(n22())

def tv90():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm91:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm91():
  {==}

law tv91:
  Tup(n25())

def tv91():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm92:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm92():
  {==}

law tv92:
  Tup(n28())

def tv92():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm93:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm93():
  {==}

law tv93:
  Tup(n31())

def tv93():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm94:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm94():
  {==}

law tv94:
  Tup(n6())

def tv94():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm95:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm95():
  {==}

law tv95:
  Tup(n9())

def tv95():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm96:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm96():
  {==}

law tv96:
  Tup(n12())

def tv96():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm97:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm97():
  {==}

law tv97:
  Tup(n15())

def tv97():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm98:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm98():
  {==}

law tv98:
  Tup(n18())

def tv98():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm99:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm99():
  {==}

law tv99:
  Tup(n21())

def tv99():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm100:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm100():
  {==}

law tv100:
  Tup(n24())

def tv100():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm101:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm101():
  {==}

law tv101:
  Tup(n27())

def tv101():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm102:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm102():
  {==}

law tv102:
  Tup(n30())

def tv102():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm103:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm103():
  {==}

law tv103:
  Tup(n5())

def tv103():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm104:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm104():
  {==}

law tv104:
  Tup(n8())

def tv104():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm105:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm105():
  {==}

law tv105:
  Tup(n11())

def tv105():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm106:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm106():
  {==}

law tv106:
  Tup(n14())

def tv106():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm107:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm107():
  {==}

law tv107:
  Tup(n17())

def tv107():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm108:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm108():
  {==}

law tv108:
  Tup(n20())

def tv108():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm109:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm109():
  {==}

law tv109:
  Tup(n23())

def tv109():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm110:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm110():
  {==}

law tv110:
  Tup(n26())

def tv110():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm111:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm111():
  {==}

law tv111:
  Tup(n29())

def tv111():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm112:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm112():
  {==}

law tv112:
  Tup(n4())

def tv112():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm113:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm113():
  {==}

law tv113:
  Tup(n7())

def tv113():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm114:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm114():
  {==}

law tv114:
  Tup(n10())

def tv114():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm115:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm115():
  {==}

law tv115:
  Tup(n13())

def tv115():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm116:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm116():
  {==}

law tv116:
  Tup(n16())

def tv116():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm117:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm117():
  {==}

law tv117:
  Tup(n19())

def tv117():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm118:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm118():
  {==}

law tv118:
  Tup(n22())

def tv118():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm119:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm119():
  {==}

law tv119:
  Tup(n25())

def tv119():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm120:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm120():
  {==}

law tv120:
  Tup(n28())

def tv120():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm121:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm121():
  {==}

law tv121:
  Tup(n31())

def tv121():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm122:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm122():
  {==}

law tv122:
  Tup(n6())

def tv122():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm123:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm123():
  {==}

law tv123:
  Tup(n9())

def tv123():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm124:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm124():
  {==}

law tv124:
  Tup(n12())

def tv124():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm125:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm125():
  {==}

law tv125:
  Tup(n15())

def tv125():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm126:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm126():
  {==}

law tv126:
  Tup(n18())

def tv126():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm127:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm127():
  {==}

law tv127:
  Tup(n21())

def tv127():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm128:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm128():
  {==}

law tv128:
  Tup(n24())

def tv128():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm129:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm129():
  {==}

law tv129:
  Tup(n27())

def tv129():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm130:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm130():
  {==}

law tv130:
  Tup(n30())

def tv130():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm131:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm131():
  {==}

law tv131:
  Tup(n5())

def tv131():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm132:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm132():
  {==}

law tv132:
  Tup(n8())

def tv132():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm133:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm133():
  {==}

law tv133:
  Tup(n11())

def tv133():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm134:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm134():
  {==}

law tv134:
  Tup(n14())

def tv134():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm135:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm135():
  {==}

law tv135:
  Tup(n17())

def tv135():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm136:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm136():
  {==}

law tv136:
  Tup(n20())

def tv136():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm137:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm137():
  {==}

law tv137:
  Tup(n23())

def tv137():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm138:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm138():
  {==}

law tv138:
  Tup(n26())

def tv138():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm139:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm139():
  {==}

law tv139:
  Tup(n29())

def tv139():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm140:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm140():
  {==}

law tv140:
  Tup(n4())

def tv140():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm141:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm141():
  {==}

law tv141:
  Tup(n7())

def tv141():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm142:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm142():
  {==}

law tv142:
  Tup(n10())

def tv142():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm143:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm143():
  {==}

law tv143:
  Tup(n13())

def tv143():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm144:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm144():
  {==}

law tv144:
  Tup(n16())

def tv144():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm145:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm145():
  {==}

law tv145:
  Tup(n19())

def tv145():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm146:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm146():
  {==}

law tv146:
  Tup(n22())

def tv146():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm147:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm147():
  {==}

law tv147:
  Tup(n25())

def tv147():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm148:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm148():
  {==}

law tv148:
  Tup(n28())

def tv148():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm149:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm149():
  {==}

law tv149:
  Tup(n31())

def tv149():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm150:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm150():
  {==}

law tv150:
  Tup(n6())

def tv150():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm151:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm151():
  {==}

law tv151:
  Tup(n9())

def tv151():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm152:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm152():
  {==}

law tv152:
  Tup(n12())

def tv152():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm153:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm153():
  {==}

law tv153:
  Tup(n15())

def tv153():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm154:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm154():
  {==}

law tv154:
  Tup(n18())

def tv154():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm155:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm155():
  {==}

law tv155:
  Tup(n21())

def tv155():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm156:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm156():
  {==}

law tv156:
  Tup(n24())

def tv156():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm157:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm157():
  {==}

law tv157:
  Tup(n27())

def tv157():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm158:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm158():
  {==}

law tv158:
  Tup(n30())

def tv158():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm159:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm159():
  {==}

law tv159:
  Tup(n5())

def tv159():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm160:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm160():
  {==}

law tv160:
  Tup(n8())

def tv160():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm161:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm161():
  {==}

law tv161:
  Tup(n11())

def tv161():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm162:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm162():
  {==}

law tv162:
  Tup(n14())

def tv162():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm163:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm163():
  {==}

law tv163:
  Tup(n17())

def tv163():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm164:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm164():
  {==}

law tv164:
  Tup(n20())

def tv164():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm165:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm165():
  {==}

law tv165:
  Tup(n23())

def tv165():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm166:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm166():
  {==}

law tv166:
  Tup(n26())

def tv166():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm167:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm167():
  {==}

law tv167:
  Tup(n29())

def tv167():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm168:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm168():
  {==}

law tv168:
  Tup(n4())

def tv168():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm169:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm169():
  {==}

law tv169:
  Tup(n7())

def tv169():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm170:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm170():
  {==}

law tv170:
  Tup(n10())

def tv170():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm171:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm171():
  {==}

law tv171:
  Tup(n13())

def tv171():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm172:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm172():
  {==}

law tv172:
  Tup(n16())

def tv172():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm173:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm173():
  {==}

law tv173:
  Tup(n19())

def tv173():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm174:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm174():
  {==}

law tv174:
  Tup(n22())

def tv174():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm175:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm175():
  {==}

law tv175:
  Tup(n25())

def tv175():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm176:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm176():
  {==}

law tv176:
  Tup(n28())

def tv176():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm177:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm177():
  {==}

law tv177:
  Tup(n31())

def tv177():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm178:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm178():
  {==}

law tv178:
  Tup(n6())

def tv178():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm179:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm179():
  {==}

law tv179:
  Tup(n9())

def tv179():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm180:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm180():
  {==}

law tv180:
  Tup(n12())

def tv180():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm181:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm181():
  {==}

law tv181:
  Tup(n15())

def tv181():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm182:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm182():
  {==}

law tv182:
  Tup(n18())

def tv182():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm183:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm183():
  {==}

law tv183:
  Tup(n21())

def tv183():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm184:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm184():
  {==}

law tv184:
  Tup(n24())

def tv184():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm185:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm185():
  {==}

law tv185:
  Tup(n27())

def tv185():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm186:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm186():
  {==}

law tv186:
  Tup(n30())

def tv186():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm187:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm187():
  {==}

law tv187:
  Tup(n5())

def tv187():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm188:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm188():
  {==}

law tv188:
  Tup(n8())

def tv188():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm189:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm189():
  {==}

law tv189:
  Tup(n11())

def tv189():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm190:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm190():
  {==}

law tv190:
  Tup(n14())

def tv190():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm191:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm191():
  {==}

law tv191:
  Tup(n17())

def tv191():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm192:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm192():
  {==}

law tv192:
  Tup(n20())

def tv192():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm193:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm193():
  {==}

law tv193:
  Tup(n23())

def tv193():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm194:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm194():
  {==}

law tv194:
  Tup(n26())

def tv194():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm195:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm195():
  {==}

law tv195:
  Tup(n29())

def tv195():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm196:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm196():
  {==}

law tv196:
  Tup(n4())

def tv196():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm197:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm197():
  {==}

law tv197:
  Tup(n7())

def tv197():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm198:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm198():
  {==}

law tv198:
  Tup(n10())

def tv198():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm199:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm199():
  {==}

law tv199:
  Tup(n13())

def tv199():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm200:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm200():
  {==}

law tv200:
  Tup(n16())

def tv200():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm201:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm201():
  {==}

law tv201:
  Tup(n19())

def tv201():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm202:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm202():
  {==}

law tv202:
  Tup(n22())

def tv202():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm203:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm203():
  {==}

law tv203:
  Tup(n25())

def tv203():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm204:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm204():
  {==}

law tv204:
  Tup(n28())

def tv204():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm205:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm205():
  {==}

law tv205:
  Tup(n31())

def tv205():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm206:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm206():
  {==}

law tv206:
  Tup(n6())

def tv206():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm207:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm207():
  {==}

law tv207:
  Tup(n9())

def tv207():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm208:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm208():
  {==}

law tv208:
  Tup(n12())

def tv208():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm209:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm209():
  {==}

law tv209:
  Tup(n15())

def tv209():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm210:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm210():
  {==}

law tv210:
  Tup(n18())

def tv210():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm211:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm211():
  {==}

law tv211:
  Tup(n21())

def tv211():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm212:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm212():
  {==}

law tv212:
  Tup(n24())

def tv212():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm213:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm213():
  {==}

law tv213:
  Tup(n27())

def tv213():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm214:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm214():
  {==}

law tv214:
  Tup(n30())

def tv214():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm215:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm215():
  {==}

law tv215:
  Tup(n5())

def tv215():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm216:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm216():
  {==}

law tv216:
  Tup(n8())

def tv216():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm217:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm217():
  {==}

law tv217:
  Tup(n11())

def tv217():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm218:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm218():
  {==}

law tv218:
  Tup(n14())

def tv218():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm219:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm219():
  {==}

law tv219:
  Tup(n17())

def tv219():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm220:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm220():
  {==}

law tv220:
  Tup(n20())

def tv220():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm221:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm221():
  {==}

law tv221:
  Tup(n23())

def tv221():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm222:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm222():
  {==}

law tv222:
  Tup(n26())

def tv222():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm223:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm223():
  {==}

law tv223:
  Tup(n29())

def tv223():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm224:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm224():
  {==}

law tv224:
  Tup(n4())

def tv224():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm225:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm225():
  {==}

law tv225:
  Tup(n7())

def tv225():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm226:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm226():
  {==}

law tv226:
  Tup(n10())

def tv226():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm227:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm227():
  {==}

law tv227:
  Tup(n13())

def tv227():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm228:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm228():
  {==}

law tv228:
  Tup(n16())

def tv228():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm229:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm229():
  {==}

law tv229:
  Tup(n19())

def tv229():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm230:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm230():
  {==}

law tv230:
  Tup(n22())

def tv230():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm231:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm231():
  {==}

law tv231:
  Tup(n25())

def tv231():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm232:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm232():
  {==}

law tv232:
  Tup(n28())

def tv232():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm233:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm233():
  {==}

law tv233:
  Tup(n31())

def tv233():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm234:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm234():
  {==}

law tv234:
  Tup(n6())

def tv234():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm235:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm235():
  {==}

law tv235:
  Tup(n9())

def tv235():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm236:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm236():
  {==}

law tv236:
  Tup(n12())

def tv236():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm237:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm237():
  {==}

law tv237:
  Tup(n15())

def tv237():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm238:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm238():
  {==}

law tv238:
  Tup(n18())

def tv238():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm239:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm239():
  {==}

law tv239:
  Tup(n21())

def tv239():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm240:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm240():
  {==}

law tv240:
  Tup(n24())

def tv240():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm241:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm241():
  {==}

law tv241:
  Tup(n27())

def tv241():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm242:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm242():
  {==}

law tv242:
  Tup(n30())

def tv242():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm243:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm243():
  {==}

law tv243:
  Tup(n5())

def tv243():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm244:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm244():
  {==}

law tv244:
  Tup(n8())

def tv244():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm245:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm245():
  {==}

law tv245:
  Tup(n11())

def tv245():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm246:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm246():
  {==}

law tv246:
  Tup(n14())

def tv246():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm247:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm247():
  {==}

law tv247:
  Tup(n17())

def tv247():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm248:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm248():
  {==}

law tv248:
  Tup(n20())

def tv248():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm249:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm249():
  {==}

law tv249:
  Tup(n23())

def tv249():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm250:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm250():
  {==}

law tv250:
  Tup(n26())

def tv250():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm251:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm251():
  {==}

law tv251:
  Tup(n29())

def tv251():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm252:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm252():
  {==}

law tv252:
  Tup(n4())

def tv252():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm253:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm253():
  {==}

law tv253:
  Tup(n7())

def tv253():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm254:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm254():
  {==}

law tv254:
  Tup(n10())

def tv254():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm255:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm255():
  {==}

law tv255:
  Tup(n13())

def tv255():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm256:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm256():
  {==}

law tv256:
  Tup(n16())

def tv256():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm257:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm257():
  {==}

law tv257:
  Tup(n19())

def tv257():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm258:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm258():
  {==}

law tv258:
  Tup(n22())

def tv258():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm259:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm259():
  {==}

law tv259:
  Tup(n25())

def tv259():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm260:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm260():
  {==}

law tv260:
  Tup(n28())

def tv260():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm261:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm261():
  {==}

law tv261:
  Tup(n31())

def tv261():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm262:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm262():
  {==}

law tv262:
  Tup(n6())

def tv262():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm263:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm263():
  {==}

law tv263:
  Tup(n9())

def tv263():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm264:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm264():
  {==}

law tv264:
  Tup(n12())

def tv264():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm265:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm265():
  {==}

law tv265:
  Tup(n15())

def tv265():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm266:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm266():
  {==}

law tv266:
  Tup(n18())

def tv266():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm267:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm267():
  {==}

law tv267:
  Tup(n21())

def tv267():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm268:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm268():
  {==}

law tv268:
  Tup(n24())

def tv268():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm269:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm269():
  {==}

law tv269:
  Tup(n27())

def tv269():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm270:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm270():
  {==}

law tv270:
  Tup(n30())

def tv270():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm271:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm271():
  {==}

law tv271:
  Tup(n5())

def tv271():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm272:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm272():
  {==}

law tv272:
  Tup(n8())

def tv272():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm273:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm273():
  {==}

law tv273:
  Tup(n11())

def tv273():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm274:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm274():
  {==}

law tv274:
  Tup(n14())

def tv274():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm275:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm275():
  {==}

law tv275:
  Tup(n17())

def tv275():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm276:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm276():
  {==}

law tv276:
  Tup(n20())

def tv276():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm277:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm277():
  {==}

law tv277:
  Tup(n23())

def tv277():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm278:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm278():
  {==}

law tv278:
  Tup(n26())

def tv278():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm279:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm279():
  {==}

law tv279:
  Tup(n29())

def tv279():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm280:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm280():
  {==}

law tv280:
  Tup(n4())

def tv280():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm281:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm281():
  {==}

law tv281:
  Tup(n7())

def tv281():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm282:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm282():
  {==}

law tv282:
  Tup(n10())

def tv282():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm283:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm283():
  {==}

law tv283:
  Tup(n13())

def tv283():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm284:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm284():
  {==}

law tv284:
  Tup(n16())

def tv284():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm285:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm285():
  {==}

law tv285:
  Tup(n19())

def tv285():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm286:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm286():
  {==}

law tv286:
  Tup(n22())

def tv286():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm287:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm287():
  {==}

law tv287:
  Tup(n25())

def tv287():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm288:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm288():
  {==}

law tv288:
  Tup(n28())

def tv288():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm289:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm289():
  {==}

law tv289:
  Tup(n31())

def tv289():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm290:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm290():
  {==}

law tv290:
  Tup(n6())

def tv290():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm291:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm291():
  {==}

law tv291:
  Tup(n9())

def tv291():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm292:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm292():
  {==}

law tv292:
  Tup(n12())

def tv292():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm293:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm293():
  {==}

law tv293:
  Tup(n15())

def tv293():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm294:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm294():
  {==}

law tv294:
  Tup(n18())

def tv294():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm295:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm295():
  {==}

law tv295:
  Tup(n21())

def tv295():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm296:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm296():
  {==}

law tv296:
  Tup(n24())

def tv296():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm297:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm297():
  {==}

law tv297:
  Tup(n27())

def tv297():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm298:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm298():
  {==}

law tv298:
  Tup(n30())

def tv298():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm299:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm299():
  {==}

law tv299:
  Tup(n5())

def tv299():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm300:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm300():
  {==}

law tv300:
  Tup(n8())

def tv300():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm301:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm301():
  {==}

law tv301:
  Tup(n11())

def tv301():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm302:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm302():
  {==}

law tv302:
  Tup(n14())

def tv302():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm303:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm303():
  {==}

law tv303:
  Tup(n17())

def tv303():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm304:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm304():
  {==}

law tv304:
  Tup(n20())

def tv304():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm305:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm305():
  {==}

law tv305:
  Tup(n23())

def tv305():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm306:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm306():
  {==}

law tv306:
  Tup(n26())

def tv306():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm307:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm307():
  {==}

law tv307:
  Tup(n29())

def tv307():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm308:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm308():
  {==}

law tv308:
  Tup(n4())

def tv308():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm309:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm309():
  {==}

law tv309:
  Tup(n7())

def tv309():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm310:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm310():
  {==}

law tv310:
  Tup(n10())

def tv310():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm311:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm311():
  {==}

law tv311:
  Tup(n13())

def tv311():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm312:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm312():
  {==}

law tv312:
  Tup(n16())

def tv312():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm313:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm313():
  {==}

law tv313:
  Tup(n19())

def tv313():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm314:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm314():
  {==}

law tv314:
  Tup(n22())

def tv314():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm315:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm315():
  {==}

law tv315:
  Tup(n25())

def tv315():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm316:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm316():
  {==}

law tv316:
  Tup(n28())

def tv316():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm317:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm317():
  {==}

law tv317:
  Tup(n31())

def tv317():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm318:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm318():
  {==}

law tv318:
  Tup(n6())

def tv318():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm319:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm319():
  {==}

law tv319:
  Tup(n9())

def tv319():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm320:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm320():
  {==}

law tv320:
  Tup(n12())

def tv320():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm321:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm321():
  {==}

law tv321:
  Tup(n15())

def tv321():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm322:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm322():
  {==}

law tv322:
  Tup(n18())

def tv322():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm323:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm323():
  {==}

law tv323:
  Tup(n21())

def tv323():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm324:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm324():
  {==}

law tv324:
  Tup(n24())

def tv324():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm325:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm325():
  {==}

law tv325:
  Tup(n27())

def tv325():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm326:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm326():
  {==}

law tv326:
  Tup(n30())

def tv326():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm327:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm327():
  {==}

law tv327:
  Tup(n5())

def tv327():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm328:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm328():
  {==}

law tv328:
  Tup(n8())

def tv328():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm329:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm329():
  {==}

law tv329:
  Tup(n11())

def tv329():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm330:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm330():
  {==}

law tv330:
  Tup(n14())

def tv330():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm331:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm331():
  {==}

law tv331:
  Tup(n17())

def tv331():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm332:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm332():
  {==}

law tv332:
  Tup(n20())

def tv332():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm333:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm333():
  {==}

law tv333:
  Tup(n23())

def tv333():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm334:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm334():
  {==}

law tv334:
  Tup(n26())

def tv334():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm335:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm335():
  {==}

law tv335:
  Tup(n29())

def tv335():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm336:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm336():
  {==}

law tv336:
  Tup(n4())

def tv336():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm337:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm337():
  {==}

law tv337:
  Tup(n7())

def tv337():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm338:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm338():
  {==}

law tv338:
  Tup(n10())

def tv338():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm339:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm339():
  {==}

law tv339:
  Tup(n13())

def tv339():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm340:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm340():
  {==}

law tv340:
  Tup(n16())

def tv340():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm341:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm341():
  {==}

law tv341:
  Tup(n19())

def tv341():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm342:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm342():
  {==}

law tv342:
  Tup(n22())

def tv342():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm343:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm343():
  {==}

law tv343:
  Tup(n25())

def tv343():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm344:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm344():
  {==}

law tv344:
  Tup(n28())

def tv344():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm345:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm345():
  {==}

law tv345:
  Tup(n31())

def tv345():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm346:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm346():
  {==}

law tv346:
  Tup(n6())

def tv346():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm347:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm347():
  {==}

law tv347:
  Tup(n9())

def tv347():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm348:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm348():
  {==}

law tv348:
  Tup(n12())

def tv348():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm349:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm349():
  {==}

law tv349:
  Tup(n15())

def tv349():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm350:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm350():
  {==}

law tv350:
  Tup(n18())

def tv350():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm351:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm351():
  {==}

law tv351:
  Tup(n21())

def tv351():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm352:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm352():
  {==}

law tv352:
  Tup(n24())

def tv352():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm353:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm353():
  {==}

law tv353:
  Tup(n27())

def tv353():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm354:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm354():
  {==}

law tv354:
  Tup(n30())

def tv354():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm355:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm355():
  {==}

law tv355:
  Tup(n5())

def tv355():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm356:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm356():
  {==}

law tv356:
  Tup(n8())

def tv356():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm357:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm357():
  {==}

law tv357:
  Tup(n11())

def tv357():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm358:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm358():
  {==}

law tv358:
  Tup(n14())

def tv358():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm359:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm359():
  {==}

law tv359:
  Tup(n17())

def tv359():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm360:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm360():
  {==}

law tv360:
  Tup(n20())

def tv360():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm361:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm361():
  {==}

law tv361:
  Tup(n23())

def tv361():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm362:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm362():
  {==}

law tv362:
  Tup(n26())

def tv362():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm363:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm363():
  {==}

law tv363:
  Tup(n29())

def tv363():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm364:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm364():
  {==}

law tv364:
  Tup(n4())

def tv364():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm365:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm365():
  {==}

law tv365:
  Tup(n7())

def tv365():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm366:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm366():
  {==}

law tv366:
  Tup(n10())

def tv366():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm367:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm367():
  {==}

law tv367:
  Tup(n13())

def tv367():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm368:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm368():
  {==}

law tv368:
  Tup(n16())

def tv368():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm369:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm369():
  {==}

law tv369:
  Tup(n19())

def tv369():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm370:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm370():
  {==}

law tv370:
  Tup(n22())

def tv370():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm371:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm371():
  {==}

law tv371:
  Tup(n25())

def tv371():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm372:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm372():
  {==}

law tv372:
  Tup(n28())

def tv372():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm373:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm373():
  {==}

law tv373:
  Tup(n31())

def tv373():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm374:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm374():
  {==}

law tv374:
  Tup(n6())

def tv374():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm375:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm375():
  {==}

law tv375:
  Tup(n9())

def tv375():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm376:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm376():
  {==}

law tv376:
  Tup(n12())

def tv376():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm377:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm377():
  {==}

law tv377:
  Tup(n15())

def tv377():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm378:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm378():
  {==}

law tv378:
  Tup(n18())

def tv378():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm379:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm379():
  {==}

law tv379:
  Tup(n21())

def tv379():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm380:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm380():
  {==}

law tv380:
  Tup(n24())

def tv380():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm381:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm381():
  {==}

law tv381:
  Tup(n27())

def tv381():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm382:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm382():
  {==}

law tv382:
  Tup(n30())

def tv382():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm383:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm383():
  {==}

law tv383:
  Tup(n5())

def tv383():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm384:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm384():
  {==}

law tv384:
  Tup(n8())

def tv384():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm385:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm385():
  {==}

law tv385:
  Tup(n11())

def tv385():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm386:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm386():
  {==}

law tv386:
  Tup(n14())

def tv386():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm387:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm387():
  {==}

law tv387:
  Tup(n17())

def tv387():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm388:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm388():
  {==}

law tv388:
  Tup(n20())

def tv388():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm389:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm389():
  {==}

law tv389:
  Tup(n23())

def tv389():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm390:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm390():
  {==}

law tv390:
  Tup(n26())

def tv390():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm391:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm391():
  {==}

law tv391:
  Tup(n29())

def tv391():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm392:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm392():
  {==}

law tv392:
  Tup(n4())

def tv392():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm393:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm393():
  {==}

law tv393:
  Tup(n7())

def tv393():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm394:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm394():
  {==}

law tv394:
  Tup(n10())

def tv394():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm395:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm395():
  {==}

law tv395:
  Tup(n13())

def tv395():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm396:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm396():
  {==}

law tv396:
  Tup(n16())

def tv396():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm397:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm397():
  {==}

law tv397:
  Tup(n19())

def tv397():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm398:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm398():
  {==}

law tv398:
  Tup(n22())

def tv398():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm399:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm399():
  {==}

law tv399:
  Tup(n25())

def tv399():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm400:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm400():
  {==}

law tv400:
  Tup(n28())

def tv400():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm401:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm401():
  {==}

law tv401:
  Tup(n31())

def tv401():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm402:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm402():
  {==}

law tv402:
  Tup(n6())

def tv402():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm403:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm403():
  {==}

law tv403:
  Tup(n9())

def tv403():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm404:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm404():
  {==}

law tv404:
  Tup(n12())

def tv404():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm405:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm405():
  {==}

law tv405:
  Tup(n15())

def tv405():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm406:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm406():
  {==}

law tv406:
  Tup(n18())

def tv406():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm407:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm407():
  {==}

law tv407:
  Tup(n21())

def tv407():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm408:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm408():
  {==}

law tv408:
  Tup(n24())

def tv408():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm409:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm409():
  {==}

law tv409:
  Tup(n27())

def tv409():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm410:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm410():
  {==}

law tv410:
  Tup(n30())

def tv410():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm411:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm411():
  {==}

law tv411:
  Tup(n5())

def tv411():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm412:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm412():
  {==}

law tv412:
  Tup(n8())

def tv412():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm413:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm413():
  {==}

law tv413:
  Tup(n11())

def tv413():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm414:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm414():
  {==}

law tv414:
  Tup(n14())

def tv414():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm415:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm415():
  {==}

law tv415:
  Tup(n17())

def tv415():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm416:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm416():
  {==}

law tv416:
  Tup(n20())

def tv416():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm417:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm417():
  {==}

law tv417:
  Tup(n23())

def tv417():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm418:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm418():
  {==}

law tv418:
  Tup(n26())

def tv418():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm419:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm419():
  {==}

law tv419:
  Tup(n29())

def tv419():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm420:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm420():
  {==}

law tv420:
  Tup(n4())

def tv420():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm421:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm421():
  {==}

law tv421:
  Tup(n7())

def tv421():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm422:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm422():
  {==}

law tv422:
  Tup(n10())

def tv422():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm423:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm423():
  {==}

law tv423:
  Tup(n13())

def tv423():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm424:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm424():
  {==}

law tv424:
  Tup(n16())

def tv424():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm425:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm425():
  {==}

law tv425:
  Tup(n19())

def tv425():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm426:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm426():
  {==}

law tv426:
  Tup(n22())

def tv426():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm427:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm427():
  {==}

law tv427:
  Tup(n25())

def tv427():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm428:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm428():
  {==}

law tv428:
  Tup(n28())

def tv428():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm429:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm429():
  {==}

law tv429:
  Tup(n31())

def tv429():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm430:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm430():
  {==}

law tv430:
  Tup(n6())

def tv430():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm431:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm431():
  {==}

law tv431:
  Tup(n9())

def tv431():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm432:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm432():
  {==}

law tv432:
  Tup(n12())

def tv432():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm433:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm433():
  {==}

law tv433:
  Tup(n15())

def tv433():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm434:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm434():
  {==}

law tv434:
  Tup(n18())

def tv434():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm435:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm435():
  {==}

law tv435:
  Tup(n21())

def tv435():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm436:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm436():
  {==}

law tv436:
  Tup(n24())

def tv436():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm437:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm437():
  {==}

law tv437:
  Tup(n27())

def tv437():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm438:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm438():
  {==}

law tv438:
  Tup(n30())

def tv438():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm439:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm439():
  {==}

law tv439:
  Tup(n5())

def tv439():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm440:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm440():
  {==}

law tv440:
  Tup(n8())

def tv440():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm441:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm441():
  {==}

law tv441:
  Tup(n11())

def tv441():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm442:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm442():
  {==}

law tv442:
  Tup(n14())

def tv442():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm443:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm443():
  {==}

law tv443:
  Tup(n17())

def tv443():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm444:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm444():
  {==}

law tv444:
  Tup(n20())

def tv444():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm445:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm445():
  {==}

law tv445:
  Tup(n23())

def tv445():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm446:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm446():
  {==}

law tv446:
  Tup(n26())

def tv446():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm447:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm447():
  {==}

law tv447:
  Tup(n29())

def tv447():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm448:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm448():
  {==}

law tv448:
  Tup(n4())

def tv448():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm449:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm449():
  {==}

law tv449:
  Tup(n7())

def tv449():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm450:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm450():
  {==}

law tv450:
  Tup(n10())

def tv450():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm451:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm451():
  {==}

law tv451:
  Tup(n13())

def tv451():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm452:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm452():
  {==}

law tv452:
  Tup(n16())

def tv452():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm453:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm453():
  {==}

law tv453:
  Tup(n19())

def tv453():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm454:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm454():
  {==}

law tv454:
  Tup(n22())

def tv454():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm455:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm455():
  {==}

law tv455:
  Tup(n25())

def tv455():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm456:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm456():
  {==}

law tv456:
  Tup(n28())

def tv456():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm457:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm457():
  {==}

law tv457:
  Tup(n31())

def tv457():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm458:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm458():
  {==}

law tv458:
  Tup(n6())

def tv458():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm459:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm459():
  {==}

law tv459:
  Tup(n9())

def tv459():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm460:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm460():
  {==}

law tv460:
  Tup(n12())

def tv460():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm461:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm461():
  {==}

law tv461:
  Tup(n15())

def tv461():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm462:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm462():
  {==}

law tv462:
  Tup(n18())

def tv462():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm463:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm463():
  {==}

law tv463:
  Tup(n21())

def tv463():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm464:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm464():
  {==}

law tv464:
  Tup(n24())

def tv464():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm465:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm465():
  {==}

law tv465:
  Tup(n27())

def tv465():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm466:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm466():
  {==}

law tv466:
  Tup(n30())

def tv466():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm467:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm467():
  {==}

law tv467:
  Tup(n5())

def tv467():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm468:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm468():
  {==}

law tv468:
  Tup(n8())

def tv468():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm469:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm469():
  {==}

law tv469:
  Tup(n11())

def tv469():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm470:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm470():
  {==}

law tv470:
  Tup(n14())

def tv470():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm471:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm471():
  {==}

law tv471:
  Tup(n17())

def tv471():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm472:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm472():
  {==}

law tv472:
  Tup(n20())

def tv472():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm473:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm473():
  {==}

law tv473:
  Tup(n23())

def tv473():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm474:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm474():
  {==}

law tv474:
  Tup(n26())

def tv474():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm475:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm475():
  {==}

law tv475:
  Tup(n29())

def tv475():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm476:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm476():
  {==}

law tv476:
  Tup(n4())

def tv476():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm477:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm477():
  {==}

law tv477:
  Tup(n7())

def tv477():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm478:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm478():
  {==}

law tv478:
  Tup(n10())

def tv478():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm479:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm479():
  {==}

law tv479:
  Tup(n13())

def tv479():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm480:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm480():
  {==}

law tv480:
  Tup(n16())

def tv480():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm481:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm481():
  {==}

law tv481:
  Tup(n19())

def tv481():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm482:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm482():
  {==}

law tv482:
  Tup(n22())

def tv482():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm483:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm483():
  {==}

law tv483:
  Tup(n25())

def tv483():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm484:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm484():
  {==}

law tv484:
  Tup(n28())

def tv484():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm485:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm485():
  {==}

law tv485:
  Tup(n31())

def tv485():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm486:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm486():
  {==}

law tv486:
  Tup(n6())

def tv486():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm487:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm487():
  {==}

law tv487:
  Tup(n9())

def tv487():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm488:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm488():
  {==}

law tv488:
  Tup(n12())

def tv488():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm489:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm489():
  {==}

law tv489:
  Tup(n15())

def tv489():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm490:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm490():
  {==}

law tv490:
  Tup(n18())

def tv490():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm491:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm491():
  {==}

law tv491:
  Tup(n21())

def tv491():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm492:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm492():
  {==}

law tv492:
  Tup(n24())

def tv492():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm493:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm493():
  {==}

law tv493:
  Tup(n27())

def tv493():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm494:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm494():
  {==}

law tv494:
  Tup(n30())

def tv494():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm495:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm495():
  {==}

law tv495:
  Tup(n5())

def tv495():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm496:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm496():
  {==}

law tv496:
  Tup(n8())

def tv496():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm497:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm497():
  {==}

law tv497:
  Tup(n11())

def tv497():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm498:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm498():
  {==}

law tv498:
  Tup(n14())

def tv498():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm499:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm499():
  {==}

law tv499:
  Tup(n17())

def tv499():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm500:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm500():
  {==}

law tv500:
  Tup(n20())

def tv500():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm501:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm501():
  {==}

law tv501:
  Tup(n23())

def tv501():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm502:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm502():
  {==}

law tv502:
  Tup(n26())

def tv502():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm503:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm503():
  {==}

law tv503:
  Tup(n29())

def tv503():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm504:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm504():
  {==}

law tv504:
  Tup(n4())

def tv504():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm505:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm505():
  {==}

law tv505:
  Tup(n7())

def tv505():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm506:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm506():
  {==}

law tv506:
  Tup(n10())

def tv506():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm507:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm507():
  {==}

law tv507:
  Tup(n13())

def tv507():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm508:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm508():
  {==}

law tv508:
  Tup(n16())

def tv508():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm509:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm509():
  {==}

law tv509:
  Tup(n19())

def tv509():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm510:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm510():
  {==}

law tv510:
  Tup(n22())

def tv510():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm511:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm511():
  {==}

law tv511:
  Tup(n25())

def tv511():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm512:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm512():
  {==}

law tv512:
  Tup(n28())

def tv512():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm513:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm513():
  {==}

law tv513:
  Tup(n31())

def tv513():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm514:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm514():
  {==}

law tv514:
  Tup(n6())

def tv514():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm515:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm515():
  {==}

law tv515:
  Tup(n9())

def tv515():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm516:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm516():
  {==}

law tv516:
  Tup(n12())

def tv516():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm517:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm517():
  {==}

law tv517:
  Tup(n15())

def tv517():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm518:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm518():
  {==}

law tv518:
  Tup(n18())

def tv518():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm519:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm519():
  {==}

law tv519:
  Tup(n21())

def tv519():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm520:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm520():
  {==}

law tv520:
  Tup(n24())

def tv520():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm521:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm521():
  {==}

law tv521:
  Tup(n27())

def tv521():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm522:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm522():
  {==}

law tv522:
  Tup(n30())

def tv522():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm523:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm523():
  {==}

law tv523:
  Tup(n5())

def tv523():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm524:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm524():
  {==}

law tv524:
  Tup(n8())

def tv524():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm525:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm525():
  {==}

law tv525:
  Tup(n11())

def tv525():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm526:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm526():
  {==}

law tv526:
  Tup(n14())

def tv526():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm527:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm527():
  {==}

law tv527:
  Tup(n17())

def tv527():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm528:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm528():
  {==}

law tv528:
  Tup(n20())

def tv528():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm529:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm529():
  {==}

law tv529:
  Tup(n23())

def tv529():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm530:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm530():
  {==}

law tv530:
  Tup(n26())

def tv530():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm531:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm531():
  {==}

law tv531:
  Tup(n29())

def tv531():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm532:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm532():
  {==}

law tv532:
  Tup(n4())

def tv532():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm533:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm533():
  {==}

law tv533:
  Tup(n7())

def tv533():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm534:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm534():
  {==}

law tv534:
  Tup(n10())

def tv534():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm535:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm535():
  {==}

law tv535:
  Tup(n13())

def tv535():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm536:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm536():
  {==}

law tv536:
  Tup(n16())

def tv536():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm537:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm537():
  {==}

law tv537:
  Tup(n19())

def tv537():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm538:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm538():
  {==}

law tv538:
  Tup(n22())

def tv538():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm539:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm539():
  {==}

law tv539:
  Tup(n25())

def tv539():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm540:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm540():
  {==}

law tv540:
  Tup(n28())

def tv540():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm541:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm541():
  {==}

law tv541:
  Tup(n31())

def tv541():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm542:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm542():
  {==}

law tv542:
  Tup(n6())

def tv542():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm543:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm543():
  {==}

law tv543:
  Tup(n9())

def tv543():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm544:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm544():
  {==}

law tv544:
  Tup(n12())

def tv544():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm545:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm545():
  {==}

law tv545:
  Tup(n15())

def tv545():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm546:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm546():
  {==}

law tv546:
  Tup(n18())

def tv546():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm547:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm547():
  {==}

law tv547:
  Tup(n21())

def tv547():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm548:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm548():
  {==}

law tv548:
  Tup(n24())

def tv548():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm549:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm549():
  {==}

law tv549:
  Tup(n27())

def tv549():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm550:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm550():
  {==}

law tv550:
  Tup(n30())

def tv550():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm551:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm551():
  {==}

law tv551:
  Tup(n5())

def tv551():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm552:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm552():
  {==}

law tv552:
  Tup(n8())

def tv552():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm553:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm553():
  {==}

law tv553:
  Tup(n11())

def tv553():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm554:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm554():
  {==}

law tv554:
  Tup(n14())

def tv554():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm555:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm555():
  {==}

law tv555:
  Tup(n17())

def tv555():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm556:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm556():
  {==}

law tv556:
  Tup(n20())

def tv556():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm557:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm557():
  {==}

law tv557:
  Tup(n23())

def tv557():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm558:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm558():
  {==}

law tv558:
  Tup(n26())

def tv558():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm559:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm559():
  {==}

law tv559:
  Tup(n29())

def tv559():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm560:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm560():
  {==}

law tv560:
  Tup(n4())

def tv560():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm561:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm561():
  {==}

law tv561:
  Tup(n7())

def tv561():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm562:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm562():
  {==}

law tv562:
  Tup(n10())

def tv562():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm563:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm563():
  {==}

law tv563:
  Tup(n13())

def tv563():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm564:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm564():
  {==}

law tv564:
  Tup(n16())

def tv564():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm565:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm565():
  {==}

law tv565:
  Tup(n19())

def tv565():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm566:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm566():
  {==}

law tv566:
  Tup(n22())

def tv566():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm567:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm567():
  {==}

law tv567:
  Tup(n25())

def tv567():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm568:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm568():
  {==}

law tv568:
  Tup(n28())

def tv568():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm569:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm569():
  {==}

law tv569:
  Tup(n31())

def tv569():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm570:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm570():
  {==}

law tv570:
  Tup(n6())

def tv570():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm571:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm571():
  {==}

law tv571:
  Tup(n9())

def tv571():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm572:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm572():
  {==}

law tv572:
  Tup(n12())

def tv572():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm573:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm573():
  {==}

law tv573:
  Tup(n15())

def tv573():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm574:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm574():
  {==}

law tv574:
  Tup(n18())

def tv574():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm575:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm575():
  {==}

law tv575:
  Tup(n21())

def tv575():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm576:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm576():
  {==}

law tv576:
  Tup(n24())

def tv576():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm577:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm577():
  {==}

law tv577:
  Tup(n27())

def tv577():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm578:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm578():
  {==}

law tv578:
  Tup(n30())

def tv578():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm579:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm579():
  {==}

law tv579:
  Tup(n5())

def tv579():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm580:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm580():
  {==}

law tv580:
  Tup(n8())

def tv580():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm581:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm581():
  {==}

law tv581:
  Tup(n11())

def tv581():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm582:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm582():
  {==}

law tv582:
  Tup(n14())

def tv582():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm583:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm583():
  {==}

law tv583:
  Tup(n17())

def tv583():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm584:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm584():
  {==}

law tv584:
  Tup(n20())

def tv584():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm585:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm585():
  {==}

law tv585:
  Tup(n23())

def tv585():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm586:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm586():
  {==}

law tv586:
  Tup(n26())

def tv586():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm587:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm587():
  {==}

law tv587:
  Tup(n29())

def tv587():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm588:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm588():
  {==}

law tv588:
  Tup(n4())

def tv588():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm589:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm589():
  {==}

law tv589:
  Tup(n7())

def tv589():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm590:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm590():
  {==}

law tv590:
  Tup(n10())

def tv590():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm591:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm591():
  {==}

law tv591:
  Tup(n13())

def tv591():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm592:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm592():
  {==}

law tv592:
  Tup(n16())

def tv592():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm593:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm593():
  {==}

law tv593:
  Tup(n19())

def tv593():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm594:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm594():
  {==}

law tv594:
  Tup(n22())

def tv594():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm595:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm595():
  {==}

law tv595:
  Tup(n25())

def tv595():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm596:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm596():
  {==}

law tv596:
  Tup(n28())

def tv596():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm597:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm597():
  {==}

law tv597:
  Tup(n31())

def tv597():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm598:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm598():
  {==}

law tv598:
  Tup(n6())

def tv598():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm599:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm599():
  {==}

law tv599:
  Tup(n9())

def tv599():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm600:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm600():
  {==}

law tv600:
  Tup(n12())

def tv600():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm601:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm601():
  {==}

law tv601:
  Tup(n15())

def tv601():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm602:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm602():
  {==}

law tv602:
  Tup(n18())

def tv602():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm603:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm603():
  {==}

law tv603:
  Tup(n21())

def tv603():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm604:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm604():
  {==}

law tv604:
  Tup(n24())

def tv604():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm605:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm605():
  {==}

law tv605:
  Tup(n27())

def tv605():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm606:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm606():
  {==}

law tv606:
  Tup(n30())

def tv606():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm607:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm607():
  {==}

law tv607:
  Tup(n5())

def tv607():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm608:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm608():
  {==}

law tv608:
  Tup(n8())

def tv608():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm609:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm609():
  {==}

law tv609:
  Tup(n11())

def tv609():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm610:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm610():
  {==}

law tv610:
  Tup(n14())

def tv610():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm611:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm611():
  {==}

law tv611:
  Tup(n17())

def tv611():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm612:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm612():
  {==}

law tv612:
  Tup(n20())

def tv612():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm613:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm613():
  {==}

law tv613:
  Tup(n23())

def tv613():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm614:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm614():
  {==}

law tv614:
  Tup(n26())

def tv614():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm615:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm615():
  {==}

law tv615:
  Tup(n29())

def tv615():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm616:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm616():
  {==}

law tv616:
  Tup(n4())

def tv616():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm617:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm617():
  {==}

law tv617:
  Tup(n7())

def tv617():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm618:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm618():
  {==}

law tv618:
  Tup(n10())

def tv618():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm619:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm619():
  {==}

law tv619:
  Tup(n13())

def tv619():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm620:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm620():
  {==}

law tv620:
  Tup(n16())

def tv620():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm621:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm621():
  {==}

law tv621:
  Tup(n19())

def tv621():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm622:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm622():
  {==}

law tv622:
  Tup(n22())

def tv622():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm623:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm623():
  {==}

law tv623:
  Tup(n25())

def tv623():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm624:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm624():
  {==}

law tv624:
  Tup(n28())

def tv624():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm625:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm625():
  {==}

law tv625:
  Tup(n31())

def tv625():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm626:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm626():
  {==}

law tv626:
  Tup(n6())

def tv626():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm627:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm627():
  {==}

law tv627:
  Tup(n9())

def tv627():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm628:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm628():
  {==}

law tv628:
  Tup(n12())

def tv628():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm629:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm629():
  {==}

law tv629:
  Tup(n15())

def tv629():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm630:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm630():
  {==}

law tv630:
  Tup(n18())

def tv630():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm631:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm631():
  {==}

law tv631:
  Tup(n21())

def tv631():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm632:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm632():
  {==}

law tv632:
  Tup(n24())

def tv632():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm633:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm633():
  {==}

law tv633:
  Tup(n27())

def tv633():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm634:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm634():
  {==}

law tv634:
  Tup(n30())

def tv634():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm635:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm635():
  {==}

law tv635:
  Tup(n5())

def tv635():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm636:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm636():
  {==}

law tv636:
  Tup(n8())

def tv636():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm637:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm637():
  {==}

law tv637:
  Tup(n11())

def tv637():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm638:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm638():
  {==}

law tv638:
  Tup(n14())

def tv638():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm639:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm639():
  {==}

law tv639:
  Tup(n17())

def tv639():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm640:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm640():
  {==}

law tv640:
  Tup(n20())

def tv640():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm641:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm641():
  {==}

law tv641:
  Tup(n23())

def tv641():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm642:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm642():
  {==}

law tv642:
  Tup(n26())

def tv642():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm643:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm643():
  {==}

law tv643:
  Tup(n29())

def tv643():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm644:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm644():
  {==}

law tv644:
  Tup(n4())

def tv644():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm645:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm645():
  {==}

law tv645:
  Tup(n7())

def tv645():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm646:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm646():
  {==}

law tv646:
  Tup(n10())

def tv646():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm647:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm647():
  {==}

law tv647:
  Tup(n13())

def tv647():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm648:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm648():
  {==}

law tv648:
  Tup(n16())

def tv648():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm649:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm649():
  {==}

law tv649:
  Tup(n19())

def tv649():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm650:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm650():
  {==}

law tv650:
  Tup(n22())

def tv650():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm651:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm651():
  {==}

law tv651:
  Tup(n25())

def tv651():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm652:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm652():
  {==}

law tv652:
  Tup(n28())

def tv652():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm653:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm653():
  {==}

law tv653:
  Tup(n31())

def tv653():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm654:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm654():
  {==}

law tv654:
  Tup(n6())

def tv654():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm655:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm655():
  {==}

law tv655:
  Tup(n9())

def tv655():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm656:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm656():
  {==}

law tv656:
  Tup(n12())

def tv656():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm657:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm657():
  {==}

law tv657:
  Tup(n15())

def tv657():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm658:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm658():
  {==}

law tv658:
  Tup(n18())

def tv658():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm659:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm659():
  {==}

law tv659:
  Tup(n21())

def tv659():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm660:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm660():
  {==}

law tv660:
  Tup(n24())

def tv660():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm661:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm661():
  {==}

law tv661:
  Tup(n27())

def tv661():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm662:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm662():
  {==}

law tv662:
  Tup(n30())

def tv662():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm663:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm663():
  {==}

law tv663:
  Tup(n5())

def tv663():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm664:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm664():
  {==}

law tv664:
  Tup(n8())

def tv664():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm665:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm665():
  {==}

law tv665:
  Tup(n11())

def tv665():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm666:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm666():
  {==}

law tv666:
  Tup(n14())

def tv666():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm667:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm667():
  {==}

law tv667:
  Tup(n17())

def tv667():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm668:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm668():
  {==}

law tv668:
  Tup(n20())

def tv668():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm669:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm669():
  {==}

law tv669:
  Tup(n23())

def tv669():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm670:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm670():
  {==}

law tv670:
  Tup(n26())

def tv670():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm671:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm671():
  {==}

law tv671:
  Tup(n29())

def tv671():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm672:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm672():
  {==}

law tv672:
  Tup(n4())

def tv672():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm673:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm673():
  {==}

law tv673:
  Tup(n7())

def tv673():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm674:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm674():
  {==}

law tv674:
  Tup(n10())

def tv674():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm675:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm675():
  {==}

law tv675:
  Tup(n13())

def tv675():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm676:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm676():
  {==}

law tv676:
  Tup(n16())

def tv676():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm677:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm677():
  {==}

law tv677:
  Tup(n19())

def tv677():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm678:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm678():
  {==}

law tv678:
  Tup(n22())

def tv678():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm679:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm679():
  {==}

law tv679:
  Tup(n25())

def tv679():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm680:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm680():
  {==}

law tv680:
  Tup(n28())

def tv680():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm681:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm681():
  {==}

law tv681:
  Tup(n31())

def tv681():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm682:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm682():
  {==}

law tv682:
  Tup(n6())

def tv682():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm683:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm683():
  {==}

law tv683:
  Tup(n9())

def tv683():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm684:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm684():
  {==}

law tv684:
  Tup(n12())

def tv684():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm685:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm685():
  {==}

law tv685:
  Tup(n15())

def tv685():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm686:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm686():
  {==}

law tv686:
  Tup(n18())

def tv686():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm687:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm687():
  {==}

law tv687:
  Tup(n21())

def tv687():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm688:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm688():
  {==}

law tv688:
  Tup(n24())

def tv688():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm689:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm689():
  {==}

law tv689:
  Tup(n27())

def tv689():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm690:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm690():
  {==}

law tv690:
  Tup(n30())

def tv690():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm691:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm691():
  {==}

law tv691:
  Tup(n5())

def tv691():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm692:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm692():
  {==}

law tv692:
  Tup(n8())

def tv692():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm693:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm693():
  {==}

law tv693:
  Tup(n11())

def tv693():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm694:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm694():
  {==}

law tv694:
  Tup(n14())

def tv694():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm695:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm695():
  {==}

law tv695:
  Tup(n17())

def tv695():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm696:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm696():
  {==}

law tv696:
  Tup(n20())

def tv696():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm697:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm697():
  {==}

law tv697:
  Tup(n23())

def tv697():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm698:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm698():
  {==}

law tv698:
  Tup(n26())

def tv698():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm699:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm699():
  {==}

law tv699:
  Tup(n29())

def tv699():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm700:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm700():
  {==}

law tv700:
  Tup(n4())

def tv700():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm701:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm701():
  {==}

law tv701:
  Tup(n7())

def tv701():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm702:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm702():
  {==}

law tv702:
  Tup(n10())

def tv702():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm703:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm703():
  {==}

law tv703:
  Tup(n13())

def tv703():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm704:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm704():
  {==}

law tv704:
  Tup(n16())

def tv704():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm705:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm705():
  {==}

law tv705:
  Tup(n19())

def tv705():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm706:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm706():
  {==}

law tv706:
  Tup(n22())

def tv706():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm707:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm707():
  {==}

law tv707:
  Tup(n25())

def tv707():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm708:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm708():
  {==}

law tv708:
  Tup(n28())

def tv708():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm709:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm709():
  {==}

law tv709:
  Tup(n31())

def tv709():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm710:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm710():
  {==}

law tv710:
  Tup(n6())

def tv710():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm711:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm711():
  {==}

law tv711:
  Tup(n9())

def tv711():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm712:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm712():
  {==}

law tv712:
  Tup(n12())

def tv712():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm713:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm713():
  {==}

law tv713:
  Tup(n15())

def tv713():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm714:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm714():
  {==}

law tv714:
  Tup(n18())

def tv714():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm715:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm715():
  {==}

law tv715:
  Tup(n21())

def tv715():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm716:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm716():
  {==}

law tv716:
  Tup(n24())

def tv716():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm717:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm717():
  {==}

law tv717:
  Tup(n27())

def tv717():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm718:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm718():
  {==}

law tv718:
  Tup(n30())

def tv718():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm719:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm719():
  {==}

law tv719:
  Tup(n5())

def tv719():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm720:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm720():
  {==}

law tv720:
  Tup(n8())

def tv720():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm721:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm721():
  {==}

law tv721:
  Tup(n11())

def tv721():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm722:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm722():
  {==}

law tv722:
  Tup(n14())

def tv722():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm723:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm723():
  {==}

law tv723:
  Tup(n17())

def tv723():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm724:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm724():
  {==}

law tv724:
  Tup(n20())

def tv724():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm725:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm725():
  {==}

law tv725:
  Tup(n23())

def tv725():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm726:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm726():
  {==}

law tv726:
  Tup(n26())

def tv726():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm727:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm727():
  {==}

law tv727:
  Tup(n29())

def tv727():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm728:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm728():
  {==}

law tv728:
  Tup(n4())

def tv728():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm729:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm729():
  {==}

law tv729:
  Tup(n7())

def tv729():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm730:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm730():
  {==}

law tv730:
  Tup(n10())

def tv730():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm731:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm731():
  {==}

law tv731:
  Tup(n13())

def tv731():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm732:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm732():
  {==}

law tv732:
  Tup(n16())

def tv732():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm733:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm733():
  {==}

law tv733:
  Tup(n19())

def tv733():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm734:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm734():
  {==}

law tv734:
  Tup(n22())

def tv734():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm735:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm735():
  {==}

law tv735:
  Tup(n25())

def tv735():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm736:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm736():
  {==}

law tv736:
  Tup(n28())

def tv736():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm737:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm737():
  {==}

law tv737:
  Tup(n31())

def tv737():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm738:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm738():
  {==}

law tv738:
  Tup(n6())

def tv738():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm739:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm739():
  {==}

law tv739:
  Tup(n9())

def tv739():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm740:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm740():
  {==}

law tv740:
  Tup(n12())

def tv740():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm741:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm741():
  {==}

law tv741:
  Tup(n15())

def tv741():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm742:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm742():
  {==}

law tv742:
  Tup(n18())

def tv742():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm743:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm743():
  {==}

law tv743:
  Tup(n21())

def tv743():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm744:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm744():
  {==}

law tv744:
  Tup(n24())

def tv744():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm745:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm745():
  {==}

law tv745:
  Tup(n27())

def tv745():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm746:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm746():
  {==}

law tv746:
  Tup(n30())

def tv746():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm747:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm747():
  {==}

law tv747:
  Tup(n5())

def tv747():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm748:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm748():
  {==}

law tv748:
  Tup(n8())

def tv748():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm749:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm749():
  {==}

law tv749:
  Tup(n11())

def tv749():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm750:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm750():
  {==}

law tv750:
  Tup(n14())

def tv750():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm751:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm751():
  {==}

law tv751:
  Tup(n17())

def tv751():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm752:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm752():
  {==}

law tv752:
  Tup(n20())

def tv752():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm753:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm753():
  {==}

law tv753:
  Tup(n23())

def tv753():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm754:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm754():
  {==}

law tv754:
  Tup(n26())

def tv754():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm755:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm755():
  {==}

law tv755:
  Tup(n29())

def tv755():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm756:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm756():
  {==}

law tv756:
  Tup(n4())

def tv756():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm757:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm757():
  {==}

law tv757:
  Tup(n7())

def tv757():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm758:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm758():
  {==}

law tv758:
  Tup(n10())

def tv758():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm759:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm759():
  {==}

law tv759:
  Tup(n13())

def tv759():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm760:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm760():
  {==}

law tv760:
  Tup(n16())

def tv760():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm761:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm761():
  {==}

law tv761:
  Tup(n19())

def tv761():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm762:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm762():
  {==}

law tv762:
  Tup(n22())

def tv762():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm763:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm763():
  {==}

law tv763:
  Tup(n25())

def tv763():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm764:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm764():
  {==}

law tv764:
  Tup(n28())

def tv764():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm765:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm765():
  {==}

law tv765:
  Tup(n31())

def tv765():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm766:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm766():
  {==}

law tv766:
  Tup(n6())

def tv766():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm767:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm767():
  {==}

law tv767:
  Tup(n9())

def tv767():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm768:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm768():
  {==}

law tv768:
  Tup(n12())

def tv768():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm769:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm769():
  {==}

law tv769:
  Tup(n15())

def tv769():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm770:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm770():
  {==}

law tv770:
  Tup(n18())

def tv770():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm771:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm771():
  {==}

law tv771:
  Tup(n21())

def tv771():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm772:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm772():
  {==}

law tv772:
  Tup(n24())

def tv772():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm773:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm773():
  {==}

law tv773:
  Tup(n27())

def tv773():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm774:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm774():
  {==}

law tv774:
  Tup(n30())

def tv774():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm775:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm775():
  {==}

law tv775:
  Tup(n5())

def tv775():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm776:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm776():
  {==}

law tv776:
  Tup(n8())

def tv776():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm777:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm777():
  {==}

law tv777:
  Tup(n11())

def tv777():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm778:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm778():
  {==}

law tv778:
  Tup(n14())

def tv778():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm779:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm779():
  {==}

law tv779:
  Tup(n17())

def tv779():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm780:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm780():
  {==}

law tv780:
  Tup(n20())

def tv780():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm781:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm781():
  {==}

law tv781:
  Tup(n23())

def tv781():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm782:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm782():
  {==}

law tv782:
  Tup(n26())

def tv782():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm783:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm783():
  {==}

law tv783:
  Tup(n29())

def tv783():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm784:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm784():
  {==}

law tv784:
  Tup(n4())

def tv784():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm785:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm785():
  {==}

law tv785:
  Tup(n7())

def tv785():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm786:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm786():
  {==}

law tv786:
  Tup(n10())

def tv786():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm787:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm787():
  {==}

law tv787:
  Tup(n13())

def tv787():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm788:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm788():
  {==}

law tv788:
  Tup(n16())

def tv788():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm789:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm789():
  {==}

law tv789:
  Tup(n19())

def tv789():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm790:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm790():
  {==}

law tv790:
  Tup(n22())

def tv790():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm791:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm791():
  {==}

law tv791:
  Tup(n25())

def tv791():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm792:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm792():
  {==}

law tv792:
  Tup(n28())

def tv792():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm793:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm793():
  {==}

law tv793:
  Tup(n31())

def tv793():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm794:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm794():
  {==}

law tv794:
  Tup(n6())

def tv794():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm795:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm795():
  {==}

law tv795:
  Tup(n9())

def tv795():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm796:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm796():
  {==}

law tv796:
  Tup(n12())

def tv796():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm797:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm797():
  {==}

law tv797:
  Tup(n15())

def tv797():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm798:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm798():
  {==}

law tv798:
  Tup(n18())

def tv798():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm799:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm799():
  {==}

law tv799:
  Tup(n21())

def tv799():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm800:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm800():
  {==}

law tv800:
  Tup(n24())

def tv800():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm801:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm801():
  {==}

law tv801:
  Tup(n27())

def tv801():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm802:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm802():
  {==}

law tv802:
  Tup(n30())

def tv802():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm803:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm803():
  {==}

law tv803:
  Tup(n5())

def tv803():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm804:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm804():
  {==}

law tv804:
  Tup(n8())

def tv804():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm805:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm805():
  {==}

law tv805:
  Tup(n11())

def tv805():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm806:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm806():
  {==}

law tv806:
  Tup(n14())

def tv806():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm807:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm807():
  {==}

law tv807:
  Tup(n17())

def tv807():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm808:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm808():
  {==}

law tv808:
  Tup(n20())

def tv808():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm809:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm809():
  {==}

law tv809:
  Tup(n23())

def tv809():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm810:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm810():
  {==}

law tv810:
  Tup(n26())

def tv810():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm811:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm811():
  {==}

law tv811:
  Tup(n29())

def tv811():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm812:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm812():
  {==}

law tv812:
  Tup(n4())

def tv812():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm813:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm813():
  {==}

law tv813:
  Tup(n7())

def tv813():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm814:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm814():
  {==}

law tv814:
  Tup(n10())

def tv814():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm815:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm815():
  {==}

law tv815:
  Tup(n13())

def tv815():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm816:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm816():
  {==}

law tv816:
  Tup(n16())

def tv816():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm817:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm817():
  {==}

law tv817:
  Tup(n19())

def tv817():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm818:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm818():
  {==}

law tv818:
  Tup(n22())

def tv818():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm819:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm819():
  {==}

law tv819:
  Tup(n25())

def tv819():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm820:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm820():
  {==}

law tv820:
  Tup(n28())

def tv820():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm821:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm821():
  {==}

law tv821:
  Tup(n31())

def tv821():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm822:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm822():
  {==}

law tv822:
  Tup(n6())

def tv822():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm823:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm823():
  {==}

law tv823:
  Tup(n9())

def tv823():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm824:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm824():
  {==}

law tv824:
  Tup(n12())

def tv824():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm825:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm825():
  {==}

law tv825:
  Tup(n15())

def tv825():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm826:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm826():
  {==}

law tv826:
  Tup(n18())

def tv826():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm827:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm827():
  {==}

law tv827:
  Tup(n21())

def tv827():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm828:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm828():
  {==}

law tv828:
  Tup(n24())

def tv828():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm829:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm829():
  {==}

law tv829:
  Tup(n27())

def tv829():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm830:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm830():
  {==}

law tv830:
  Tup(n30())

def tv830():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm831:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm831():
  {==}

law tv831:
  Tup(n5())

def tv831():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm832:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm832():
  {==}

law tv832:
  Tup(n8())

def tv832():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm833:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm833():
  {==}

law tv833:
  Tup(n11())

def tv833():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm834:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm834():
  {==}

law tv834:
  Tup(n14())

def tv834():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm835:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm835():
  {==}

law tv835:
  Tup(n17())

def tv835():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm836:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm836():
  {==}

law tv836:
  Tup(n20())

def tv836():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm837:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm837():
  {==}

law tv837:
  Tup(n23())

def tv837():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm838:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm838():
  {==}

law tv838:
  Tup(n26())

def tv838():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm839:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm839():
  {==}

law tv839:
  Tup(n29())

def tv839():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm840:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm840():
  {==}

law tv840:
  Tup(n4())

def tv840():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm841:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm841():
  {==}

law tv841:
  Tup(n7())

def tv841():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm842:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm842():
  {==}

law tv842:
  Tup(n10())

def tv842():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm843:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm843():
  {==}

law tv843:
  Tup(n13())

def tv843():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm844:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm844():
  {==}

law tv844:
  Tup(n16())

def tv844():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm845:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm845():
  {==}

law tv845:
  Tup(n19())

def tv845():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm846:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm846():
  {==}

law tv846:
  Tup(n22())

def tv846():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm847:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm847():
  {==}

law tv847:
  Tup(n25())

def tv847():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm848:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm848():
  {==}

law tv848:
  Tup(n28())

def tv848():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm849:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm849():
  {==}

law tv849:
  Tup(n31())

def tv849():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm850:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm850():
  {==}

law tv850:
  Tup(n6())

def tv850():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm851:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm851():
  {==}

law tv851:
  Tup(n9())

def tv851():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm852:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm852():
  {==}

law tv852:
  Tup(n12())

def tv852():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm853:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm853():
  {==}

law tv853:
  Tup(n15())

def tv853():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm854:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm854():
  {==}

law tv854:
  Tup(n18())

def tv854():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm855:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm855():
  {==}

law tv855:
  Tup(n21())

def tv855():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm856:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm856():
  {==}

law tv856:
  Tup(n24())

def tv856():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm857:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm857():
  {==}

law tv857:
  Tup(n27())

def tv857():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm858:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm858():
  {==}

law tv858:
  Tup(n30())

def tv858():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm859:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm859():
  {==}

law tv859:
  Tup(n5())

def tv859():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm860:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm860():
  {==}

law tv860:
  Tup(n8())

def tv860():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm861:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm861():
  {==}

law tv861:
  Tup(n11())

def tv861():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm862:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm862():
  {==}

law tv862:
  Tup(n14())

def tv862():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm863:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm863():
  {==}

law tv863:
  Tup(n17())

def tv863():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm864:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm864():
  {==}

law tv864:
  Tup(n20())

def tv864():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm865:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm865():
  {==}

law tv865:
  Tup(n23())

def tv865():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm866:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm866():
  {==}

law tv866:
  Tup(n26())

def tv866():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm867:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm867():
  {==}

law tv867:
  Tup(n29())

def tv867():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm868:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm868():
  {==}

law tv868:
  Tup(n4())

def tv868():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm869:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm869():
  {==}

law tv869:
  Tup(n7())

def tv869():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm870:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm870():
  {==}

law tv870:
  Tup(n10())

def tv870():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm871:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm871():
  {==}

law tv871:
  Tup(n13())

def tv871():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm872:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm872():
  {==}

law tv872:
  Tup(n16())

def tv872():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm873:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm873():
  {==}

law tv873:
  Tup(n19())

def tv873():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm874:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm874():
  {==}

law tv874:
  Tup(n22())

def tv874():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm875:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm875():
  {==}

law tv875:
  Tup(n25())

def tv875():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm876:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm876():
  {==}

law tv876:
  Tup(n28())

def tv876():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm877:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm877():
  {==}

law tv877:
  Tup(n31())

def tv877():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm878:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm878():
  {==}

law tv878:
  Tup(n6())

def tv878():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm879:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm879():
  {==}

law tv879:
  Tup(n9())

def tv879():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm880:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm880():
  {==}

law tv880:
  Tup(n12())

def tv880():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm881:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm881():
  {==}

law tv881:
  Tup(n15())

def tv881():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm882:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm882():
  {==}

law tv882:
  Tup(n18())

def tv882():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm883:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm883():
  {==}

law tv883:
  Tup(n21())

def tv883():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm884:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm884():
  {==}

law tv884:
  Tup(n24())

def tv884():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm885:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm885():
  {==}

law tv885:
  Tup(n27())

def tv885():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm886:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm886():
  {==}

law tv886:
  Tup(n30())

def tv886():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm887:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm887():
  {==}

law tv887:
  Tup(n5())

def tv887():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm888:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm888():
  {==}

law tv888:
  Tup(n8())

def tv888():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm889:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm889():
  {==}

law tv889:
  Tup(n11())

def tv889():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm890:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm890():
  {==}

law tv890:
  Tup(n14())

def tv890():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm891:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm891():
  {==}

law tv891:
  Tup(n17())

def tv891():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm892:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm892():
  {==}

law tv892:
  Tup(n20())

def tv892():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm893:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm893():
  {==}

law tv893:
  Tup(n23())

def tv893():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm894:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm894():
  {==}

law tv894:
  Tup(n26())

def tv894():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm895:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm895():
  {==}

law tv895:
  Tup(n29())

def tv895():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm896:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm896():
  {==}

law tv896:
  Tup(n4())

def tv896():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm897:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm897():
  {==}

law tv897:
  Tup(n7())

def tv897():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm898:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm898():
  {==}

law tv898:
  Tup(n10())

def tv898():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm899:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm899():
  {==}

law tv899:
  Tup(n13())

def tv899():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm900:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm900():
  {==}

law tv900:
  Tup(n16())

def tv900():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm901:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm901():
  {==}

law tv901:
  Tup(n19())

def tv901():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm902:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm902():
  {==}

law tv902:
  Tup(n22())

def tv902():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm903:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm903():
  {==}

law tv903:
  Tup(n25())

def tv903():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm904:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm904():
  {==}

law tv904:
  Tup(n28())

def tv904():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm905:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm905():
  {==}

law tv905:
  Tup(n31())

def tv905():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm906:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm906():
  {==}

law tv906:
  Tup(n6())

def tv906():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm907:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm907():
  {==}

law tv907:
  Tup(n9())

def tv907():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm908:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm908():
  {==}

law tv908:
  Tup(n12())

def tv908():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm909:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm909():
  {==}

law tv909:
  Tup(n15())

def tv909():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm910:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm910():
  {==}

law tv910:
  Tup(n18())

def tv910():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm911:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm911():
  {==}

law tv911:
  Tup(n21())

def tv911():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm912:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm912():
  {==}

law tv912:
  Tup(n24())

def tv912():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm913:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm913():
  {==}

law tv913:
  Tup(n27())

def tv913():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm914:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm914():
  {==}

law tv914:
  Tup(n30())

def tv914():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm915:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm915():
  {==}

law tv915:
  Tup(n5())

def tv915():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm916:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm916():
  {==}

law tv916:
  Tup(n8())

def tv916():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm917:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm917():
  {==}

law tv917:
  Tup(n11())

def tv917():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm918:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm918():
  {==}

law tv918:
  Tup(n14())

def tv918():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm919:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm919():
  {==}

law tv919:
  Tup(n17())

def tv919():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm920:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm920():
  {==}

law tv920:
  Tup(n20())

def tv920():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm921:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm921():
  {==}

law tv921:
  Tup(n23())

def tv921():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm922:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm922():
  {==}

law tv922:
  Tup(n26())

def tv922():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm923:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm923():
  {==}

law tv923:
  Tup(n29())

def tv923():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm924:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm924():
  {==}

law tv924:
  Tup(n4())

def tv924():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm925:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm925():
  {==}

law tv925:
  Tup(n7())

def tv925():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm926:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm926():
  {==}

law tv926:
  Tup(n10())

def tv926():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm927:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm927():
  {==}

law tv927:
  Tup(n13())

def tv927():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm928:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm928():
  {==}

law tv928:
  Tup(n16())

def tv928():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm929:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm929():
  {==}

law tv929:
  Tup(n19())

def tv929():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm930:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm930():
  {==}

law tv930:
  Tup(n22())

def tv930():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm931:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm931():
  {==}

law tv931:
  Tup(n25())

def tv931():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm932:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm932():
  {==}

law tv932:
  Tup(n28())

def tv932():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm933:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm933():
  {==}

law tv933:
  Tup(n31())

def tv933():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm934:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm934():
  {==}

law tv934:
  Tup(n6())

def tv934():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm935:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm935():
  {==}

law tv935:
  Tup(n9())

def tv935():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm936:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm936():
  {==}

law tv936:
  Tup(n12())

def tv936():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm937:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm937():
  {==}

law tv937:
  Tup(n15())

def tv937():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm938:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm938():
  {==}

law tv938:
  Tup(n18())

def tv938():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm939:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm939():
  {==}

law tv939:
  Tup(n21())

def tv939():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm940:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm940():
  {==}

law tv940:
  Tup(n24())

def tv940():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm941:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm941():
  {==}

law tv941:
  Tup(n27())

def tv941():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm942:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm942():
  {==}

law tv942:
  Tup(n30())

def tv942():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm943:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm943():
  {==}

law tv943:
  Tup(n5())

def tv943():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm944:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm944():
  {==}

law tv944:
  Tup(n8())

def tv944():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm945:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm945():
  {==}

law tv945:
  Tup(n11())

def tv945():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm946:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm946():
  {==}

law tv946:
  Tup(n14())

def tv946():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm947:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm947():
  {==}

law tv947:
  Tup(n17())

def tv947():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm948:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm948():
  {==}

law tv948:
  Tup(n20())

def tv948():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm949:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm949():
  {==}

law tv949:
  Tup(n23())

def tv949():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm950:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm950():
  {==}

law tv950:
  Tup(n26())

def tv950():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm951:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm951():
  {==}

law tv951:
  Tup(n29())

def tv951():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm952:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm952():
  {==}

law tv952:
  Tup(n4())

def tv952():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm953:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm953():
  {==}

law tv953:
  Tup(n7())

def tv953():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm954:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm954():
  {==}

law tv954:
  Tup(n10())

def tv954():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm955:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm955():
  {==}

law tv955:
  Tup(n13())

def tv955():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm956:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm956():
  {==}

law tv956:
  Tup(n16())

def tv956():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm957:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm957():
  {==}

law tv957:
  Tup(n19())

def tv957():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm958:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm958():
  {==}

law tv958:
  Tup(n22())

def tv958():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm959:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm959():
  {==}

law tv959:
  Tup(n25())

def tv959():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm960:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm960():
  {==}

law tv960:
  Tup(n28())

def tv960():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm961:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm961():
  {==}

law tv961:
  Tup(n31())

def tv961():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm962:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm962():
  {==}

law tv962:
  Tup(n6())

def tv962():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm963:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm963():
  {==}

law tv963:
  Tup(n9())

def tv963():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm964:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm964():
  {==}

law tv964:
  Tup(n12())

def tv964():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm965:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm965():
  {==}

law tv965:
  Tup(n15())

def tv965():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm966:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm966():
  {==}

law tv966:
  Tup(n18())

def tv966():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm967:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm967():
  {==}

law tv967:
  Tup(n21())

def tv967():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm968:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm968():
  {==}

law tv968:
  Tup(n24())

def tv968():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm969:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm969():
  {==}

law tv969:
  Tup(n27())

def tv969():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm970:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm970():
  {==}

law tv970:
  Tup(n30())

def tv970():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm971:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm971():
  {==}

law tv971:
  Tup(n5())

def tv971():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm972:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm972():
  {==}

law tv972:
  Tup(n8())

def tv972():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm973:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm973():
  {==}

law tv973:
  Tup(n11())

def tv973():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm974:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm974():
  {==}

law tv974:
  Tup(n14())

def tv974():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm975:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm975():
  {==}

law tv975:
  Tup(n17())

def tv975():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm976:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm976():
  {==}

law tv976:
  Tup(n20())

def tv976():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm977:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm977():
  {==}

law tv977:
  Tup(n23())

def tv977():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm978:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm978():
  {==}

law tv978:
  Tup(n26())

def tv978():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm979:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm979():
  {==}

law tv979:
  Tup(n29())

def tv979():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm980:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm980():
  {==}

law tv980:
  Tup(n4())

def tv980():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm981:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm981():
  {==}

law tv981:
  Tup(n7())

def tv981():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm982:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm982():
  {==}

law tv982:
  Tup(n10())

def tv982():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm983:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm983():
  {==}

law tv983:
  Tup(n13())

def tv983():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm984:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm984():
  {==}

law tv984:
  Tup(n16())

def tv984():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm985:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm985():
  {==}

law tv985:
  Tup(n19())

def tv985():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm986:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm986():
  {==}

law tv986:
  Tup(n22())

def tv986():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm987:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm987():
  {==}

law tv987:
  Tup(n25())

def tv987():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm988:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm988():
  {==}

law tv988:
  Tup(n28())

def tv988():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm989:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm989():
  {==}

law tv989:
  Tup(n31())

def tv989():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm990:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm990():
  {==}

law tv990:
  Tup(n6())

def tv990():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm991:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm991():
  {==}

law tv991:
  Tup(n9())

def tv991():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm992:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm992():
  {==}

law tv992:
  Tup(n12())

def tv992():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm993:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm993():
  {==}

law tv993:
  Tup(n15())

def tv993():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm994:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm994():
  {==}

law tv994:
  Tup(n18())

def tv994():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm995:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm995():
  {==}

law tv995:
  Tup(n21())

def tv995():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm996:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm996():
  {==}

law tv996:
  Tup(n24())

def tv996():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm997:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm997():
  {==}

law tv997:
  Tup(n27())

def tv997():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm998:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm998():
  {==}

law tv998:
  Tup(n30())

def tv998():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm999:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm999():
  {==}

law tv999:
  Tup(n5())

def tv999():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1000:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1000():
  {==}

law tv1000:
  Tup(n8())

def tv1000():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1001:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1001():
  {==}

law tv1001:
  Tup(n11())

def tv1001():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1002:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1002():
  {==}

law tv1002:
  Tup(n14())

def tv1002():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1003:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1003():
  {==}

law tv1003:
  Tup(n17())

def tv1003():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1004:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1004():
  {==}

law tv1004:
  Tup(n20())

def tv1004():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1005:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1005():
  {==}

law tv1005:
  Tup(n23())

def tv1005():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1006:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1006():
  {==}

law tv1006:
  Tup(n26())

def tv1006():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1007:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1007():
  {==}

law tv1007:
  Tup(n29())

def tv1007():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1008:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1008():
  {==}

law tv1008:
  Tup(n4())

def tv1008():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1009:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1009():
  {==}

law tv1009:
  Tup(n7())

def tv1009():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1010:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1010():
  {==}

law tv1010:
  Tup(n10())

def tv1010():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1011:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1011():
  {==}

law tv1011:
  Tup(n13())

def tv1011():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1012:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1012():
  {==}

law tv1012:
  Tup(n16())

def tv1012():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1013:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1013():
  {==}

law tv1013:
  Tup(n19())

def tv1013():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1014:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1014():
  {==}

law tv1014:
  Tup(n22())

def tv1014():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1015:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1015():
  {==}

law tv1015:
  Tup(n25())

def tv1015():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1016:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1016():
  {==}

law tv1016:
  Tup(n28())

def tv1016():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1017:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1017():
  {==}

law tv1017:
  Tup(n31())

def tv1017():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1018:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1018():
  {==}

law tv1018:
  Tup(n6())

def tv1018():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1019:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1019():
  {==}

law tv1019:
  Tup(n9())

def tv1019():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1020:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1020():
  {==}

law tv1020:
  Tup(n12())

def tv1020():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1021:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1021():
  {==}

law tv1021:
  Tup(n15())

def tv1021():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1022:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1022():
  {==}

law tv1022:
  Tup(n18())

def tv1022():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1023:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1023():
  {==}

law tv1023:
  Tup(n21())

def tv1023():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1024:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1024():
  {==}

law tv1024:
  Tup(n24())

def tv1024():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1025:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1025():
  {==}

law tv1025:
  Tup(n27())

def tv1025():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1026:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1026():
  {==}

law tv1026:
  Tup(n30())

def tv1026():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1027:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1027():
  {==}

law tv1027:
  Tup(n5())

def tv1027():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1028:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1028():
  {==}

law tv1028:
  Tup(n8())

def tv1028():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1029:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1029():
  {==}

law tv1029:
  Tup(n11())

def tv1029():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1030:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1030():
  {==}

law tv1030:
  Tup(n14())

def tv1030():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1031:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1031():
  {==}

law tv1031:
  Tup(n17())

def tv1031():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1032:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1032():
  {==}

law tv1032:
  Tup(n20())

def tv1032():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1033:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1033():
  {==}

law tv1033:
  Tup(n23())

def tv1033():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1034:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1034():
  {==}

law tv1034:
  Tup(n26())

def tv1034():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1035:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1035():
  {==}

law tv1035:
  Tup(n29())

def tv1035():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1036:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1036():
  {==}

law tv1036:
  Tup(n4())

def tv1036():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1037:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1037():
  {==}

law tv1037:
  Tup(n7())

def tv1037():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1038:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1038():
  {==}

law tv1038:
  Tup(n10())

def tv1038():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1039:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1039():
  {==}

law tv1039:
  Tup(n13())

def tv1039():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1040:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1040():
  {==}

law tv1040:
  Tup(n16())

def tv1040():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1041:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1041():
  {==}

law tv1041:
  Tup(n19())

def tv1041():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1042:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1042():
  {==}

law tv1042:
  Tup(n22())

def tv1042():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1043:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1043():
  {==}

law tv1043:
  Tup(n25())

def tv1043():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1044:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1044():
  {==}

law tv1044:
  Tup(n28())

def tv1044():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1045:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1045():
  {==}

law tv1045:
  Tup(n31())

def tv1045():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1046:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1046():
  {==}

law tv1046:
  Tup(n6())

def tv1046():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1047:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1047():
  {==}

law tv1047:
  Tup(n9())

def tv1047():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1048:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1048():
  {==}

law tv1048:
  Tup(n12())

def tv1048():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1049:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1049():
  {==}

law tv1049:
  Tup(n15())

def tv1049():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1050:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1050():
  {==}

law tv1050:
  Tup(n18())

def tv1050():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1051:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1051():
  {==}

law tv1051:
  Tup(n21())

def tv1051():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1052:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1052():
  {==}

law tv1052:
  Tup(n24())

def tv1052():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1053:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1053():
  {==}

law tv1053:
  Tup(n27())

def tv1053():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1054:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1054():
  {==}

law tv1054:
  Tup(n30())

def tv1054():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1055:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1055():
  {==}

law tv1055:
  Tup(n5())

def tv1055():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1056:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1056():
  {==}

law tv1056:
  Tup(n8())

def tv1056():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1057:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1057():
  {==}

law tv1057:
  Tup(n11())

def tv1057():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1058:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1058():
  {==}

law tv1058:
  Tup(n14())

def tv1058():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1059:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1059():
  {==}

law tv1059:
  Tup(n17())

def tv1059():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1060:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1060():
  {==}

law tv1060:
  Tup(n20())

def tv1060():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1061:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1061():
  {==}

law tv1061:
  Tup(n23())

def tv1061():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1062:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1062():
  {==}

law tv1062:
  Tup(n26())

def tv1062():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1063:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1063():
  {==}

law tv1063:
  Tup(n29())

def tv1063():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1064:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1064():
  {==}

law tv1064:
  Tup(n4())

def tv1064():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1065:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1065():
  {==}

law tv1065:
  Tup(n7())

def tv1065():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1066:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1066():
  {==}

law tv1066:
  Tup(n10())

def tv1066():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1067:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1067():
  {==}

law tv1067:
  Tup(n13())

def tv1067():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1068:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1068():
  {==}

law tv1068:
  Tup(n16())

def tv1068():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1069:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1069():
  {==}

law tv1069:
  Tup(n19())

def tv1069():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1070:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1070():
  {==}

law tv1070:
  Tup(n22())

def tv1070():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1071:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1071():
  {==}

law tv1071:
  Tup(n25())

def tv1071():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1072:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1072():
  {==}

law tv1072:
  Tup(n28())

def tv1072():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1073:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1073():
  {==}

law tv1073:
  Tup(n31())

def tv1073():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1074:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1074():
  {==}

law tv1074:
  Tup(n6())

def tv1074():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1075:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1075():
  {==}

law tv1075:
  Tup(n9())

def tv1075():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1076:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1076():
  {==}

law tv1076:
  Tup(n12())

def tv1076():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1077:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1077():
  {==}

law tv1077:
  Tup(n15())

def tv1077():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1078:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1078():
  {==}

law tv1078:
  Tup(n18())

def tv1078():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1079:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1079():
  {==}

law tv1079:
  Tup(n21())

def tv1079():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1080:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1080():
  {==}

law tv1080:
  Tup(n24())

def tv1080():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1081:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1081():
  {==}

law tv1081:
  Tup(n27())

def tv1081():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1082:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1082():
  {==}

law tv1082:
  Tup(n30())

def tv1082():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1083:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1083():
  {==}

law tv1083:
  Tup(n5())

def tv1083():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1084:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1084():
  {==}

law tv1084:
  Tup(n8())

def tv1084():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1085:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1085():
  {==}

law tv1085:
  Tup(n11())

def tv1085():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1086:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1086():
  {==}

law tv1086:
  Tup(n14())

def tv1086():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1087:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1087():
  {==}

law tv1087:
  Tup(n17())

def tv1087():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1088:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1088():
  {==}

law tv1088:
  Tup(n20())

def tv1088():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1089:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1089():
  {==}

law tv1089:
  Tup(n23())

def tv1089():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1090:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1090():
  {==}

law tv1090:
  Tup(n26())

def tv1090():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1091:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1091():
  {==}

law tv1091:
  Tup(n29())

def tv1091():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1092:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1092():
  {==}

law tv1092:
  Tup(n4())

def tv1092():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1093:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1093():
  {==}

law tv1093:
  Tup(n7())

def tv1093():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1094:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1094():
  {==}

law tv1094:
  Tup(n10())

def tv1094():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1095:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1095():
  {==}

law tv1095:
  Tup(n13())

def tv1095():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1096:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1096():
  {==}

law tv1096:
  Tup(n16())

def tv1096():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1097:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1097():
  {==}

law tv1097:
  Tup(n19())

def tv1097():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1098:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1098():
  {==}

law tv1098:
  Tup(n22())

def tv1098():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1099:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1099():
  {==}

law tv1099:
  Tup(n25())

def tv1099():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1100:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1100():
  {==}

law tv1100:
  Tup(n28())

def tv1100():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1101:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1101():
  {==}

law tv1101:
  Tup(n31())

def tv1101():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1102:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1102():
  {==}

law tv1102:
  Tup(n6())

def tv1102():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1103:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1103():
  {==}

law tv1103:
  Tup(n9())

def tv1103():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1104:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1104():
  {==}

law tv1104:
  Tup(n12())

def tv1104():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1105:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1105():
  {==}

law tv1105:
  Tup(n15())

def tv1105():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1106:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1106():
  {==}

law tv1106:
  Tup(n18())

def tv1106():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1107:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1107():
  {==}

law tv1107:
  Tup(n21())

def tv1107():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1108:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1108():
  {==}

law tv1108:
  Tup(n24())

def tv1108():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1109:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1109():
  {==}

law tv1109:
  Tup(n27())

def tv1109():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1110:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1110():
  {==}

law tv1110:
  Tup(n30())

def tv1110():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1111:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1111():
  {==}

law tv1111:
  Tup(n5())

def tv1111():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1112:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1112():
  {==}

law tv1112:
  Tup(n8())

def tv1112():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1113:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1113():
  {==}

law tv1113:
  Tup(n11())

def tv1113():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1114:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1114():
  {==}

law tv1114:
  Tup(n14())

def tv1114():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1115:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1115():
  {==}

law tv1115:
  Tup(n17())

def tv1115():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1116:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1116():
  {==}

law tv1116:
  Tup(n20())

def tv1116():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1117:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1117():
  {==}

law tv1117:
  Tup(n23())

def tv1117():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1118:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1118():
  {==}

law tv1118:
  Tup(n26())

def tv1118():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1119:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1119():
  {==}

law tv1119:
  Tup(n29())

def tv1119():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1120:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1120():
  {==}

law tv1120:
  Tup(n4())

def tv1120():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1121:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1121():
  {==}

law tv1121:
  Tup(n7())

def tv1121():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1122:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1122():
  {==}

law tv1122:
  Tup(n10())

def tv1122():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1123:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1123():
  {==}

law tv1123:
  Tup(n13())

def tv1123():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1124:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1124():
  {==}

law tv1124:
  Tup(n16())

def tv1124():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1125:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1125():
  {==}

law tv1125:
  Tup(n19())

def tv1125():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1126:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1126():
  {==}

law tv1126:
  Tup(n22())

def tv1126():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1127:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1127():
  {==}

law tv1127:
  Tup(n25())

def tv1127():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1128:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1128():
  {==}

law tv1128:
  Tup(n28())

def tv1128():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1129:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1129():
  {==}

law tv1129:
  Tup(n31())

def tv1129():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1130:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1130():
  {==}

law tv1130:
  Tup(n6())

def tv1130():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1131:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1131():
  {==}

law tv1131:
  Tup(n9())

def tv1131():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1132:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1132():
  {==}

law tv1132:
  Tup(n12())

def tv1132():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1133:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1133():
  {==}

law tv1133:
  Tup(n15())

def tv1133():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1134:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1134():
  {==}

law tv1134:
  Tup(n18())

def tv1134():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1135:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1135():
  {==}

law tv1135:
  Tup(n21())

def tv1135():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1136:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1136():
  {==}

law tv1136:
  Tup(n24())

def tv1136():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1137:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1137():
  {==}

law tv1137:
  Tup(n27())

def tv1137():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1138:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1138():
  {==}

law tv1138:
  Tup(n30())

def tv1138():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1139:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1139():
  {==}

law tv1139:
  Tup(n5())

def tv1139():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1140:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1140():
  {==}

law tv1140:
  Tup(n8())

def tv1140():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1141:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1141():
  {==}

law tv1141:
  Tup(n11())

def tv1141():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1142:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1142():
  {==}

law tv1142:
  Tup(n14())

def tv1142():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1143:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1143():
  {==}

law tv1143:
  Tup(n17())

def tv1143():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1144:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1144():
  {==}

law tv1144:
  Tup(n20())

def tv1144():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1145:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1145():
  {==}

law tv1145:
  Tup(n23())

def tv1145():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1146:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1146():
  {==}

law tv1146:
  Tup(n26())

def tv1146():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1147:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1147():
  {==}

law tv1147:
  Tup(n29())

def tv1147():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1148:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1148():
  {==}

law tv1148:
  Tup(n4())

def tv1148():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1149:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1149():
  {==}

law tv1149:
  Tup(n7())

def tv1149():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1150:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1150():
  {==}

law tv1150:
  Tup(n10())

def tv1150():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1151:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1151():
  {==}

law tv1151:
  Tup(n13())

def tv1151():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1152:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1152():
  {==}

law tv1152:
  Tup(n16())

def tv1152():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1153:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1153():
  {==}

law tv1153:
  Tup(n19())

def tv1153():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1154:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1154():
  {==}

law tv1154:
  Tup(n22())

def tv1154():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1155:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1155():
  {==}

law tv1155:
  Tup(n25())

def tv1155():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1156:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1156():
  {==}

law tv1156:
  Tup(n28())

def tv1156():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1157:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1157():
  {==}

law tv1157:
  Tup(n31())

def tv1157():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1158:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1158():
  {==}

law tv1158:
  Tup(n6())

def tv1158():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1159:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1159():
  {==}

law tv1159:
  Tup(n9())

def tv1159():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1160:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1160():
  {==}

law tv1160:
  Tup(n12())

def tv1160():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1161:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1161():
  {==}

law tv1161:
  Tup(n15())

def tv1161():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1162:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1162():
  {==}

law tv1162:
  Tup(n18())

def tv1162():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1163:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1163():
  {==}

law tv1163:
  Tup(n21())

def tv1163():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1164:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1164():
  {==}

law tv1164:
  Tup(n24())

def tv1164():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1165:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1165():
  {==}

law tv1165:
  Tup(n27())

def tv1165():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1166:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1166():
  {==}

law tv1166:
  Tup(n30())

def tv1166():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1167:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1167():
  {==}

law tv1167:
  Tup(n5())

def tv1167():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1168:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1168():
  {==}

law tv1168:
  Tup(n8())

def tv1168():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1169:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1169():
  {==}

law tv1169:
  Tup(n11())

def tv1169():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1170:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1170():
  {==}

law tv1170:
  Tup(n14())

def tv1170():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1171:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1171():
  {==}

law tv1171:
  Tup(n17())

def tv1171():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1172:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1172():
  {==}

law tv1172:
  Tup(n20())

def tv1172():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1173:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1173():
  {==}

law tv1173:
  Tup(n23())

def tv1173():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1174:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1174():
  {==}

law tv1174:
  Tup(n26())

def tv1174():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1175:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1175():
  {==}

law tv1175:
  Tup(n29())

def tv1175():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1176:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1176():
  {==}

law tv1176:
  Tup(n4())

def tv1176():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1177:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1177():
  {==}

law tv1177:
  Tup(n7())

def tv1177():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1178:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1178():
  {==}

law tv1178:
  Tup(n10())

def tv1178():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1179:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1179():
  {==}

law tv1179:
  Tup(n13())

def tv1179():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1180:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1180():
  {==}

law tv1180:
  Tup(n16())

def tv1180():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1181:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1181():
  {==}

law tv1181:
  Tup(n19())

def tv1181():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1182:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1182():
  {==}

law tv1182:
  Tup(n22())

def tv1182():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1183:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1183():
  {==}

law tv1183:
  Tup(n25())

def tv1183():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1184:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1184():
  {==}

law tv1184:
  Tup(n28())

def tv1184():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1185:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1185():
  {==}

law tv1185:
  Tup(n31())

def tv1185():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1186:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1186():
  {==}

law tv1186:
  Tup(n6())

def tv1186():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1187:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1187():
  {==}

law tv1187:
  Tup(n9())

def tv1187():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1188:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1188():
  {==}

law tv1188:
  Tup(n12())

def tv1188():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1189:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1189():
  {==}

law tv1189:
  Tup(n15())

def tv1189():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1190:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1190():
  {==}

law tv1190:
  Tup(n18())

def tv1190():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1191:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1191():
  {==}

law tv1191:
  Tup(n21())

def tv1191():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1192:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1192():
  {==}

law tv1192:
  Tup(n24())

def tv1192():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1193:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1193():
  {==}

law tv1193:
  Tup(n27())

def tv1193():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1194:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1194():
  {==}

law tv1194:
  Tup(n30())

def tv1194():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1195:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1195():
  {==}

law tv1195:
  Tup(n5())

def tv1195():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1196:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1196():
  {==}

law tv1196:
  Tup(n8())

def tv1196():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1197:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1197():
  {==}

law tv1197:
  Tup(n11())

def tv1197():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1198:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1198():
  {==}

law tv1198:
  Tup(n14())

def tv1198():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1199:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1199():
  {==}

law tv1199:
  Tup(n17())

def tv1199():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1200:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1200():
  {==}

law tv1200:
  Tup(n20())

def tv1200():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1201:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1201():
  {==}

law tv1201:
  Tup(n23())

def tv1201():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1202:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1202():
  {==}

law tv1202:
  Tup(n26())

def tv1202():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1203:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1203():
  {==}

law tv1203:
  Tup(n29())

def tv1203():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1204:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1204():
  {==}

law tv1204:
  Tup(n4())

def tv1204():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1205:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1205():
  {==}

law tv1205:
  Tup(n7())

def tv1205():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1206:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1206():
  {==}

law tv1206:
  Tup(n10())

def tv1206():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1207:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1207():
  {==}

law tv1207:
  Tup(n13())

def tv1207():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1208:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1208():
  {==}

law tv1208:
  Tup(n16())

def tv1208():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1209:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1209():
  {==}

law tv1209:
  Tup(n19())

def tv1209():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1210:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1210():
  {==}

law tv1210:
  Tup(n22())

def tv1210():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1211:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1211():
  {==}

law tv1211:
  Tup(n25())

def tv1211():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1212:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1212():
  {==}

law tv1212:
  Tup(n28())

def tv1212():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1213:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1213():
  {==}

law tv1213:
  Tup(n31())

def tv1213():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1214:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1214():
  {==}

law tv1214:
  Tup(n6())

def tv1214():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1215:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1215():
  {==}

law tv1215:
  Tup(n9())

def tv1215():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1216:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1216():
  {==}

law tv1216:
  Tup(n12())

def tv1216():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1217:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1217():
  {==}

law tv1217:
  Tup(n15())

def tv1217():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1218:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1218():
  {==}

law tv1218:
  Tup(n18())

def tv1218():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1219:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1219():
  {==}

law tv1219:
  Tup(n21())

def tv1219():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1220:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1220():
  {==}

law tv1220:
  Tup(n24())

def tv1220():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1221:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1221():
  {==}

law tv1221:
  Tup(n27())

def tv1221():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1222:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1222():
  {==}

law tv1222:
  Tup(n30())

def tv1222():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1223:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1223():
  {==}

law tv1223:
  Tup(n5())

def tv1223():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1224:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1224():
  {==}

law tv1224:
  Tup(n8())

def tv1224():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1225:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1225():
  {==}

law tv1225:
  Tup(n11())

def tv1225():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1226:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1226():
  {==}

law tv1226:
  Tup(n14())

def tv1226():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1227:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1227():
  {==}

law tv1227:
  Tup(n17())

def tv1227():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1228:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1228():
  {==}

law tv1228:
  Tup(n20())

def tv1228():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1229:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1229():
  {==}

law tv1229:
  Tup(n23())

def tv1229():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1230:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1230():
  {==}

law tv1230:
  Tup(n26())

def tv1230():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1231:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1231():
  {==}

law tv1231:
  Tup(n29())

def tv1231():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1232:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1232():
  {==}

law tv1232:
  Tup(n4())

def tv1232():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1233:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1233():
  {==}

law tv1233:
  Tup(n7())

def tv1233():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1234:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1234():
  {==}

law tv1234:
  Tup(n10())

def tv1234():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1235:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1235():
  {==}

law tv1235:
  Tup(n13())

def tv1235():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1236:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1236():
  {==}

law tv1236:
  Tup(n16())

def tv1236():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1237:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1237():
  {==}

law tv1237:
  Tup(n19())

def tv1237():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1238:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1238():
  {==}

law tv1238:
  Tup(n22())

def tv1238():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1239:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1239():
  {==}

law tv1239:
  Tup(n25())

def tv1239():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1240:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1240():
  {==}

law tv1240:
  Tup(n28())

def tv1240():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1241:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1241():
  {==}

law tv1241:
  Tup(n31())

def tv1241():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1242:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1242():
  {==}

law tv1242:
  Tup(n6())

def tv1242():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1243:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1243():
  {==}

law tv1243:
  Tup(n9())

def tv1243():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1244:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1244():
  {==}

law tv1244:
  Tup(n12())

def tv1244():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1245:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1245():
  {==}

law tv1245:
  Tup(n15())

def tv1245():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1246:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1246():
  {==}

law tv1246:
  Tup(n18())

def tv1246():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1247:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1247():
  {==}

law tv1247:
  Tup(n21())

def tv1247():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1248:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1248():
  {==}

law tv1248:
  Tup(n24())

def tv1248():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1249:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1249():
  {==}

law tv1249:
  Tup(n27())

def tv1249():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1250:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1250():
  {==}

law tv1250:
  Tup(n30())

def tv1250():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1251:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1251():
  {==}

law tv1251:
  Tup(n5())

def tv1251():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1252:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1252():
  {==}

law tv1252:
  Tup(n8())

def tv1252():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1253:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1253():
  {==}

law tv1253:
  Tup(n11())

def tv1253():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1254:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1254():
  {==}

law tv1254:
  Tup(n14())

def tv1254():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1255:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1255():
  {==}

law tv1255:
  Tup(n17())

def tv1255():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1256:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1256():
  {==}

law tv1256:
  Tup(n20())

def tv1256():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1257:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1257():
  {==}

law tv1257:
  Tup(n23())

def tv1257():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1258:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1258():
  {==}

law tv1258:
  Tup(n26())

def tv1258():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1259:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1259():
  {==}

law tv1259:
  Tup(n29())

def tv1259():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1260:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1260():
  {==}

law tv1260:
  Tup(n4())

def tv1260():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1261:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1261():
  {==}

law tv1261:
  Tup(n7())

def tv1261():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1262:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1262():
  {==}

law tv1262:
  Tup(n10())

def tv1262():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1263:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1263():
  {==}

law tv1263:
  Tup(n13())

def tv1263():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1264:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1264():
  {==}

law tv1264:
  Tup(n16())

def tv1264():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1265:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1265():
  {==}

law tv1265:
  Tup(n19())

def tv1265():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1266:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1266():
  {==}

law tv1266:
  Tup(n22())

def tv1266():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1267:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1267():
  {==}

law tv1267:
  Tup(n25())

def tv1267():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1268:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1268():
  {==}

law tv1268:
  Tup(n28())

def tv1268():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1269:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1269():
  {==}

law tv1269:
  Tup(n31())

def tv1269():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1270:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1270():
  {==}

law tv1270:
  Tup(n6())

def tv1270():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1271:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1271():
  {==}

law tv1271:
  Tup(n9())

def tv1271():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1272:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1272():
  {==}

law tv1272:
  Tup(n12())

def tv1272():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1273:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1273():
  {==}

law tv1273:
  Tup(n15())

def tv1273():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1274:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1274():
  {==}

law tv1274:
  Tup(n18())

def tv1274():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1275:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1275():
  {==}

law tv1275:
  Tup(n21())

def tv1275():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1276:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1276():
  {==}

law tv1276:
  Tup(n24())

def tv1276():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1277:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1277():
  {==}

law tv1277:
  Tup(n27())

def tv1277():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1278:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1278():
  {==}

law tv1278:
  Tup(n30())

def tv1278():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1279:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1279():
  {==}

law tv1279:
  Tup(n5())

def tv1279():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1280:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1280():
  {==}

law tv1280:
  Tup(n8())

def tv1280():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1281:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1281():
  {==}

law tv1281:
  Tup(n11())

def tv1281():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1282:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1282():
  {==}

law tv1282:
  Tup(n14())

def tv1282():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1283:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1283():
  {==}

law tv1283:
  Tup(n17())

def tv1283():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1284:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1284():
  {==}

law tv1284:
  Tup(n20())

def tv1284():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1285:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1285():
  {==}

law tv1285:
  Tup(n23())

def tv1285():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1286:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1286():
  {==}

law tv1286:
  Tup(n26())

def tv1286():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1287:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1287():
  {==}

law tv1287:
  Tup(n29())

def tv1287():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1288:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1288():
  {==}

law tv1288:
  Tup(n4())

def tv1288():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1289:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1289():
  {==}

law tv1289:
  Tup(n7())

def tv1289():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1290:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1290():
  {==}

law tv1290:
  Tup(n10())

def tv1290():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1291:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1291():
  {==}

law tv1291:
  Tup(n13())

def tv1291():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1292:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1292():
  {==}

law tv1292:
  Tup(n16())

def tv1292():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1293:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1293():
  {==}

law tv1293:
  Tup(n19())

def tv1293():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1294:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1294():
  {==}

law tv1294:
  Tup(n22())

def tv1294():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1295:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1295():
  {==}

law tv1295:
  Tup(n25())

def tv1295():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1296:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1296():
  {==}

law tv1296:
  Tup(n28())

def tv1296():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1297:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1297():
  {==}

law tv1297:
  Tup(n31())

def tv1297():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1298:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1298():
  {==}

law tv1298:
  Tup(n6())

def tv1298():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1299:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1299():
  {==}

law tv1299:
  Tup(n9())

def tv1299():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1300:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1300():
  {==}

law tv1300:
  Tup(n12())

def tv1300():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1301:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1301():
  {==}

law tv1301:
  Tup(n15())

def tv1301():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1302:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1302():
  {==}

law tv1302:
  Tup(n18())

def tv1302():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1303:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1303():
  {==}

law tv1303:
  Tup(n21())

def tv1303():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1304:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1304():
  {==}

law tv1304:
  Tup(n24())

def tv1304():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1305:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1305():
  {==}

law tv1305:
  Tup(n27())

def tv1305():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1306:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1306():
  {==}

law tv1306:
  Tup(n30())

def tv1306():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1307:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1307():
  {==}

law tv1307:
  Tup(n5())

def tv1307():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1308:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1308():
  {==}

law tv1308:
  Tup(n8())

def tv1308():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1309:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1309():
  {==}

law tv1309:
  Tup(n11())

def tv1309():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1310:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1310():
  {==}

law tv1310:
  Tup(n14())

def tv1310():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1311:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1311():
  {==}

law tv1311:
  Tup(n17())

def tv1311():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1312:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1312():
  {==}

law tv1312:
  Tup(n20())

def tv1312():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1313:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1313():
  {==}

law tv1313:
  Tup(n23())

def tv1313():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1314:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1314():
  {==}

law tv1314:
  Tup(n26())

def tv1314():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1315:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1315():
  {==}

law tv1315:
  Tup(n29())

def tv1315():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1316:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1316():
  {==}

law tv1316:
  Tup(n4())

def tv1316():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1317:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1317():
  {==}

law tv1317:
  Tup(n7())

def tv1317():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1318:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1318():
  {==}

law tv1318:
  Tup(n10())

def tv1318():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1319:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1319():
  {==}

law tv1319:
  Tup(n13())

def tv1319():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1320:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1320():
  {==}

law tv1320:
  Tup(n16())

def tv1320():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1321:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1321():
  {==}

law tv1321:
  Tup(n19())

def tv1321():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1322:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1322():
  {==}

law tv1322:
  Tup(n22())

def tv1322():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1323:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1323():
  {==}

law tv1323:
  Tup(n25())

def tv1323():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1324:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1324():
  {==}

law tv1324:
  Tup(n28())

def tv1324():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1325:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1325():
  {==}

law tv1325:
  Tup(n31())

def tv1325():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1326:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1326():
  {==}

law tv1326:
  Tup(n6())

def tv1326():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1327:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1327():
  {==}

law tv1327:
  Tup(n9())

def tv1327():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1328:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1328():
  {==}

law tv1328:
  Tup(n12())

def tv1328():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1329:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1329():
  {==}

law tv1329:
  Tup(n15())

def tv1329():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1330:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1330():
  {==}

law tv1330:
  Tup(n18())

def tv1330():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1331:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1331():
  {==}

law tv1331:
  Tup(n21())

def tv1331():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1332:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1332():
  {==}

law tv1332:
  Tup(n24())

def tv1332():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1333:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1333():
  {==}

law tv1333:
  Tup(n27())

def tv1333():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1334:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1334():
  {==}

law tv1334:
  Tup(n30())

def tv1334():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1335:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1335():
  {==}

law tv1335:
  Tup(n5())

def tv1335():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1336:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1336():
  {==}

law tv1336:
  Tup(n8())

def tv1336():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1337:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1337():
  {==}

law tv1337:
  Tup(n11())

def tv1337():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1338:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1338():
  {==}

law tv1338:
  Tup(n14())

def tv1338():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1339:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1339():
  {==}

law tv1339:
  Tup(n17())

def tv1339():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1340:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1340():
  {==}

law tv1340:
  Tup(n20())

def tv1340():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1341:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1341():
  {==}

law tv1341:
  Tup(n23())

def tv1341():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1342:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1342():
  {==}

law tv1342:
  Tup(n26())

def tv1342():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1343:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1343():
  {==}

law tv1343:
  Tup(n29())

def tv1343():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1344:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1344():
  {==}

law tv1344:
  Tup(n4())

def tv1344():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1345:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1345():
  {==}

law tv1345:
  Tup(n7())

def tv1345():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1346:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1346():
  {==}

law tv1346:
  Tup(n10())

def tv1346():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1347:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1347():
  {==}

law tv1347:
  Tup(n13())

def tv1347():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1348:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1348():
  {==}

law tv1348:
  Tup(n16())

def tv1348():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1349:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1349():
  {==}

law tv1349:
  Tup(n19())

def tv1349():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1350:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1350():
  {==}

law tv1350:
  Tup(n22())

def tv1350():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1351:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1351():
  {==}

law tv1351:
  Tup(n25())

def tv1351():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1352:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1352():
  {==}

law tv1352:
  Tup(n28())

def tv1352():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1353:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1353():
  {==}

law tv1353:
  Tup(n31())

def tv1353():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1354:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1354():
  {==}

law tv1354:
  Tup(n6())

def tv1354():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1355:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1355():
  {==}

law tv1355:
  Tup(n9())

def tv1355():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1356:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1356():
  {==}

law tv1356:
  Tup(n12())

def tv1356():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1357:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1357():
  {==}

law tv1357:
  Tup(n15())

def tv1357():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1358:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1358():
  {==}

law tv1358:
  Tup(n18())

def tv1358():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1359:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1359():
  {==}

law tv1359:
  Tup(n21())

def tv1359():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1360:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1360():
  {==}

law tv1360:
  Tup(n24())

def tv1360():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1361:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1361():
  {==}

law tv1361:
  Tup(n27())

def tv1361():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1362:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1362():
  {==}

law tv1362:
  Tup(n30())

def tv1362():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1363:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1363():
  {==}

law tv1363:
  Tup(n5())

def tv1363():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1364:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1364():
  {==}

law tv1364:
  Tup(n8())

def tv1364():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1365:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1365():
  {==}

law tv1365:
  Tup(n11())

def tv1365():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1366:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1366():
  {==}

law tv1366:
  Tup(n14())

def tv1366():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1367:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1367():
  {==}

law tv1367:
  Tup(n17())

def tv1367():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1368:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1368():
  {==}

law tv1368:
  Tup(n20())

def tv1368():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1369:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1369():
  {==}

law tv1369:
  Tup(n23())

def tv1369():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1370:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1370():
  {==}

law tv1370:
  Tup(n26())

def tv1370():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1371:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1371():
  {==}

law tv1371:
  Tup(n29())

def tv1371():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1372:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1372():
  {==}

law tv1372:
  Tup(n4())

def tv1372():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1373:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1373():
  {==}

law tv1373:
  Tup(n7())

def tv1373():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1374:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1374():
  {==}

law tv1374:
  Tup(n10())

def tv1374():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1375:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1375():
  {==}

law tv1375:
  Tup(n13())

def tv1375():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1376:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1376():
  {==}

law tv1376:
  Tup(n16())

def tv1376():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1377:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1377():
  {==}

law tv1377:
  Tup(n19())

def tv1377():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1378:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1378():
  {==}

law tv1378:
  Tup(n22())

def tv1378():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1379:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1379():
  {==}

law tv1379:
  Tup(n25())

def tv1379():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1380:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1380():
  {==}

law tv1380:
  Tup(n28())

def tv1380():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1381:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1381():
  {==}

law tv1381:
  Tup(n31())

def tv1381():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1382:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1382():
  {==}

law tv1382:
  Tup(n6())

def tv1382():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1383:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1383():
  {==}

law tv1383:
  Tup(n9())

def tv1383():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1384:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1384():
  {==}

law tv1384:
  Tup(n12())

def tv1384():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1385:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1385():
  {==}

law tv1385:
  Tup(n15())

def tv1385():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1386:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1386():
  {==}

law tv1386:
  Tup(n18())

def tv1386():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1387:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1387():
  {==}

law tv1387:
  Tup(n21())

def tv1387():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1388:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1388():
  {==}

law tv1388:
  Tup(n24())

def tv1388():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1389:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1389():
  {==}

law tv1389:
  Tup(n27())

def tv1389():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1390:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1390():
  {==}

law tv1390:
  Tup(n30())

def tv1390():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1391:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1391():
  {==}

law tv1391:
  Tup(n5())

def tv1391():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1392:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1392():
  {==}

law tv1392:
  Tup(n8())

def tv1392():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1393:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1393():
  {==}

law tv1393:
  Tup(n11())

def tv1393():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1394:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1394():
  {==}

law tv1394:
  Tup(n14())

def tv1394():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1395:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1395():
  {==}

law tv1395:
  Tup(n17())

def tv1395():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1396:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1396():
  {==}

law tv1396:
  Tup(n20())

def tv1396():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1397:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1397():
  {==}

law tv1397:
  Tup(n23())

def tv1397():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1398:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1398():
  {==}

law tv1398:
  Tup(n26())

def tv1398():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1399:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1399():
  {==}

law tv1399:
  Tup(n29())

def tv1399():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1400:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1400():
  {==}

law tv1400:
  Tup(n4())

def tv1400():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1401:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1401():
  {==}

law tv1401:
  Tup(n7())

def tv1401():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1402:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1402():
  {==}

law tv1402:
  Tup(n10())

def tv1402():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1403:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1403():
  {==}

law tv1403:
  Tup(n13())

def tv1403():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1404:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1404():
  {==}

law tv1404:
  Tup(n16())

def tv1404():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1405:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1405():
  {==}

law tv1405:
  Tup(n19())

def tv1405():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1406:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1406():
  {==}

law tv1406:
  Tup(n22())

def tv1406():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1407:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1407():
  {==}

law tv1407:
  Tup(n25())

def tv1407():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1408:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1408():
  {==}

law tv1408:
  Tup(n28())

def tv1408():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1409:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1409():
  {==}

law tv1409:
  Tup(n31())

def tv1409():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1410:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1410():
  {==}

law tv1410:
  Tup(n6())

def tv1410():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1411:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1411():
  {==}

law tv1411:
  Tup(n9())

def tv1411():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1412:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1412():
  {==}

law tv1412:
  Tup(n12())

def tv1412():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1413:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1413():
  {==}

law tv1413:
  Tup(n15())

def tv1413():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1414:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1414():
  {==}

law tv1414:
  Tup(n18())

def tv1414():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1415:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1415():
  {==}

law tv1415:
  Tup(n21())

def tv1415():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1416:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1416():
  {==}

law tv1416:
  Tup(n24())

def tv1416():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1417:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1417():
  {==}

law tv1417:
  Tup(n27())

def tv1417():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1418:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1418():
  {==}

law tv1418:
  Tup(n30())

def tv1418():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1419:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1419():
  {==}

law tv1419:
  Tup(n5())

def tv1419():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1420:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1420():
  {==}

law tv1420:
  Tup(n8())

def tv1420():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1421:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1421():
  {==}

law tv1421:
  Tup(n11())

def tv1421():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1422:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1422():
  {==}

law tv1422:
  Tup(n14())

def tv1422():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1423:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1423():
  {==}

law tv1423:
  Tup(n17())

def tv1423():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1424:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1424():
  {==}

law tv1424:
  Tup(n20())

def tv1424():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1425:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1425():
  {==}

law tv1425:
  Tup(n23())

def tv1425():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1426:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1426():
  {==}

law tv1426:
  Tup(n26())

def tv1426():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1427:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1427():
  {==}

law tv1427:
  Tup(n29())

def tv1427():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1428:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1428():
  {==}

law tv1428:
  Tup(n4())

def tv1428():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1429:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1429():
  {==}

law tv1429:
  Tup(n7())

def tv1429():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1430:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1430():
  {==}

law tv1430:
  Tup(n10())

def tv1430():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1431:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1431():
  {==}

law tv1431:
  Tup(n13())

def tv1431():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1432:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1432():
  {==}

law tv1432:
  Tup(n16())

def tv1432():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1433:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1433():
  {==}

law tv1433:
  Tup(n19())

def tv1433():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1434:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1434():
  {==}

law tv1434:
  Tup(n22())

def tv1434():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1435:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1435():
  {==}

law tv1435:
  Tup(n25())

def tv1435():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1436:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1436():
  {==}

law tv1436:
  Tup(n28())

def tv1436():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1437:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1437():
  {==}

law tv1437:
  Tup(n31())

def tv1437():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1438:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1438():
  {==}

law tv1438:
  Tup(n6())

def tv1438():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1439:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1439():
  {==}

law tv1439:
  Tup(n9())

def tv1439():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1440:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1440():
  {==}

law tv1440:
  Tup(n12())

def tv1440():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1441:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1441():
  {==}

law tv1441:
  Tup(n15())

def tv1441():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1442:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1442():
  {==}

law tv1442:
  Tup(n18())

def tv1442():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1443:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1443():
  {==}

law tv1443:
  Tup(n21())

def tv1443():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1444:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1444():
  {==}

law tv1444:
  Tup(n24())

def tv1444():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1445:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1445():
  {==}

law tv1445:
  Tup(n27())

def tv1445():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1446:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1446():
  {==}

law tv1446:
  Tup(n30())

def tv1446():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1447:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1447():
  {==}

law tv1447:
  Tup(n5())

def tv1447():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1448:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1448():
  {==}

law tv1448:
  Tup(n8())

def tv1448():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1449:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1449():
  {==}

law tv1449:
  Tup(n11())

def tv1449():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1450:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1450():
  {==}

law tv1450:
  Tup(n14())

def tv1450():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1451:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1451():
  {==}

law tv1451:
  Tup(n17())

def tv1451():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1452:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1452():
  {==}

law tv1452:
  Tup(n20())

def tv1452():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1453:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1453():
  {==}

law tv1453:
  Tup(n23())

def tv1453():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1454:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1454():
  {==}

law tv1454:
  Tup(n26())

def tv1454():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1455:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1455():
  {==}

law tv1455:
  Tup(n29())

def tv1455():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1456:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1456():
  {==}

law tv1456:
  Tup(n4())

def tv1456():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1457:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1457():
  {==}

law tv1457:
  Tup(n7())

def tv1457():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1458:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1458():
  {==}

law tv1458:
  Tup(n10())

def tv1458():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1459:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1459():
  {==}

law tv1459:
  Tup(n13())

def tv1459():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1460:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1460():
  {==}

law tv1460:
  Tup(n16())

def tv1460():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1461:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1461():
  {==}

law tv1461:
  Tup(n19())

def tv1461():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1462:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1462():
  {==}

law tv1462:
  Tup(n22())

def tv1462():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1463:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1463():
  {==}

law tv1463:
  Tup(n25())

def tv1463():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1464:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1464():
  {==}

law tv1464:
  Tup(n28())

def tv1464():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1465:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1465():
  {==}

law tv1465:
  Tup(n31())

def tv1465():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1466:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1466():
  {==}

law tv1466:
  Tup(n6())

def tv1466():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1467:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1467():
  {==}

law tv1467:
  Tup(n9())

def tv1467():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1468:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1468():
  {==}

law tv1468:
  Tup(n12())

def tv1468():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1469:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1469():
  {==}

law tv1469:
  Tup(n15())

def tv1469():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1470:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1470():
  {==}

law tv1470:
  Tup(n18())

def tv1470():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1471:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1471():
  {==}

law tv1471:
  Tup(n21())

def tv1471():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1472:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1472():
  {==}

law tv1472:
  Tup(n24())

def tv1472():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1473:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1473():
  {==}

law tv1473:
  Tup(n27())

def tv1473():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1474:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1474():
  {==}

law tv1474:
  Tup(n30())

def tv1474():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1475:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1475():
  {==}

law tv1475:
  Tup(n5())

def tv1475():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1476:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1476():
  {==}

law tv1476:
  Tup(n8())

def tv1476():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1477:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1477():
  {==}

law tv1477:
  Tup(n11())

def tv1477():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1478:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1478():
  {==}

law tv1478:
  Tup(n14())

def tv1478():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1479:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1479():
  {==}

law tv1479:
  Tup(n17())

def tv1479():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1480:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1480():
  {==}

law tv1480:
  Tup(n20())

def tv1480():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1481:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1481():
  {==}

law tv1481:
  Tup(n23())

def tv1481():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1482:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1482():
  {==}

law tv1482:
  Tup(n26())

def tv1482():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1483:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1483():
  {==}

law tv1483:
  Tup(n29())

def tv1483():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1484:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1484():
  {==}

law tv1484:
  Tup(n4())

def tv1484():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1485:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1485():
  {==}

law tv1485:
  Tup(n7())

def tv1485():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1486:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1486():
  {==}

law tv1486:
  Tup(n10())

def tv1486():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1487:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1487():
  {==}

law tv1487:
  Tup(n13())

def tv1487():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1488:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1488():
  {==}

law tv1488:
  Tup(n16())

def tv1488():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1489:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1489():
  {==}

law tv1489:
  Tup(n19())

def tv1489():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1490:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1490():
  {==}

law tv1490:
  Tup(n22())

def tv1490():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1491:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1491():
  {==}

law tv1491:
  Tup(n25())

def tv1491():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1492:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1492():
  {==}

law tv1492:
  Tup(n28())

def tv1492():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1493:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1493():
  {==}

law tv1493:
  Tup(n31())

def tv1493():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1494:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1494():
  {==}

law tv1494:
  Tup(n6())

def tv1494():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1495:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1495():
  {==}

law tv1495:
  Tup(n9())

def tv1495():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1496:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1496():
  {==}

law tv1496:
  Tup(n12())

def tv1496():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1497:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1497():
  {==}

law tv1497:
  Tup(n15())

def tv1497():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1498:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1498():
  {==}

law tv1498:
  Tup(n18())

def tv1498():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1499:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1499():
  {==}

law tv1499:
  Tup(n21())

def tv1499():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1500:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1500():
  {==}

law tv1500:
  Tup(n24())

def tv1500():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1501:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1501():
  {==}

law tv1501:
  Tup(n27())

def tv1501():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1502:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1502():
  {==}

law tv1502:
  Tup(n30())

def tv1502():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1503:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1503():
  {==}

law tv1503:
  Tup(n5())

def tv1503():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1504:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1504():
  {==}

law tv1504:
  Tup(n8())

def tv1504():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1505:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1505():
  {==}

law tv1505:
  Tup(n11())

def tv1505():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1506:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1506():
  {==}

law tv1506:
  Tup(n14())

def tv1506():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1507:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1507():
  {==}

law tv1507:
  Tup(n17())

def tv1507():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1508:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1508():
  {==}

law tv1508:
  Tup(n20())

def tv1508():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1509:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1509():
  {==}

law tv1509:
  Tup(n23())

def tv1509():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1510:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1510():
  {==}

law tv1510:
  Tup(n26())

def tv1510():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1511:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1511():
  {==}

law tv1511:
  Tup(n29())

def tv1511():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1512:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1512():
  {==}

law tv1512:
  Tup(n4())

def tv1512():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1513:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1513():
  {==}

law tv1513:
  Tup(n7())

def tv1513():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1514:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1514():
  {==}

law tv1514:
  Tup(n10())

def tv1514():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1515:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1515():
  {==}

law tv1515:
  Tup(n13())

def tv1515():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1516:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1516():
  {==}

law tv1516:
  Tup(n16())

def tv1516():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1517:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1517():
  {==}

law tv1517:
  Tup(n19())

def tv1517():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1518:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1518():
  {==}

law tv1518:
  Tup(n22())

def tv1518():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1519:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1519():
  {==}

law tv1519:
  Tup(n25())

def tv1519():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1520:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1520():
  {==}

law tv1520:
  Tup(n28())

def tv1520():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1521:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1521():
  {==}

law tv1521:
  Tup(n31())

def tv1521():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1522:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1522():
  {==}

law tv1522:
  Tup(n6())

def tv1522():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1523:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1523():
  {==}

law tv1523:
  Tup(n9())

def tv1523():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1524:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1524():
  {==}

law tv1524:
  Tup(n12())

def tv1524():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1525:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1525():
  {==}

law tv1525:
  Tup(n15())

def tv1525():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1526:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1526():
  {==}

law tv1526:
  Tup(n18())

def tv1526():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1527:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1527():
  {==}

law tv1527:
  Tup(n21())

def tv1527():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1528:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1528():
  {==}

law tv1528:
  Tup(n24())

def tv1528():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1529:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1529():
  {==}

law tv1529:
  Tup(n27())

def tv1529():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1530:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1530():
  {==}

law tv1530:
  Tup(n30())

def tv1530():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1531:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1531():
  {==}

law tv1531:
  Tup(n5())

def tv1531():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1532:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1532():
  {==}

law tv1532:
  Tup(n8())

def tv1532():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1533:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1533():
  {==}

law tv1533:
  Tup(n11())

def tv1533():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1534:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1534():
  {==}

law tv1534:
  Tup(n14())

def tv1534():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1535:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1535():
  {==}

law tv1535:
  Tup(n17())

def tv1535():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1536:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1536():
  {==}

law tv1536:
  Tup(n20())

def tv1536():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1537:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1537():
  {==}

law tv1537:
  Tup(n23())

def tv1537():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1538:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1538():
  {==}

law tv1538:
  Tup(n26())

def tv1538():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1539:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1539():
  {==}

law tv1539:
  Tup(n29())

def tv1539():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1540:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1540():
  {==}

law tv1540:
  Tup(n4())

def tv1540():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1541:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1541():
  {==}

law tv1541:
  Tup(n7())

def tv1541():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1542:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1542():
  {==}

law tv1542:
  Tup(n10())

def tv1542():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1543:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1543():
  {==}

law tv1543:
  Tup(n13())

def tv1543():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1544:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1544():
  {==}

law tv1544:
  Tup(n16())

def tv1544():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1545:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1545():
  {==}

law tv1545:
  Tup(n19())

def tv1545():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1546:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1546():
  {==}

law tv1546:
  Tup(n22())

def tv1546():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1547:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1547():
  {==}

law tv1547:
  Tup(n25())

def tv1547():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1548:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1548():
  {==}

law tv1548:
  Tup(n28())

def tv1548():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1549:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1549():
  {==}

law tv1549:
  Tup(n31())

def tv1549():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1550:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1550():
  {==}

law tv1550:
  Tup(n6())

def tv1550():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1551:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1551():
  {==}

law tv1551:
  Tup(n9())

def tv1551():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1552:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1552():
  {==}

law tv1552:
  Tup(n12())

def tv1552():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1553:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1553():
  {==}

law tv1553:
  Tup(n15())

def tv1553():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1554:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1554():
  {==}

law tv1554:
  Tup(n18())

def tv1554():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1555:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1555():
  {==}

law tv1555:
  Tup(n21())

def tv1555():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1556:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1556():
  {==}

law tv1556:
  Tup(n24())

def tv1556():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1557:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1557():
  {==}

law tv1557:
  Tup(n27())

def tv1557():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1558:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1558():
  {==}

law tv1558:
  Tup(n30())

def tv1558():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1559:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1559():
  {==}

law tv1559:
  Tup(n5())

def tv1559():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1560:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1560():
  {==}

law tv1560:
  Tup(n8())

def tv1560():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1561:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1561():
  {==}

law tv1561:
  Tup(n11())

def tv1561():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1562:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1562():
  {==}

law tv1562:
  Tup(n14())

def tv1562():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1563:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1563():
  {==}

law tv1563:
  Tup(n17())

def tv1563():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1564:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1564():
  {==}

law tv1564:
  Tup(n20())

def tv1564():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1565:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1565():
  {==}

law tv1565:
  Tup(n23())

def tv1565():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1566:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1566():
  {==}

law tv1566:
  Tup(n26())

def tv1566():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1567:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1567():
  {==}

law tv1567:
  Tup(n29())

def tv1567():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1568:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1568():
  {==}

law tv1568:
  Tup(n4())

def tv1568():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1569:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1569():
  {==}

law tv1569:
  Tup(n7())

def tv1569():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1570:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1570():
  {==}

law tv1570:
  Tup(n10())

def tv1570():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1571:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1571():
  {==}

law tv1571:
  Tup(n13())

def tv1571():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

law cm1572:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1572():
  {==}

law tv1572:
  Tup(n16())

def tv1572():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}

law cm1573:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1573():
  {==}

law tv1573:
  Tup(n19())

def tv1573():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}

law cm1574:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1574():
  {==}

law tv1574:
  Tup(n22())

def tv1574():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}

law cm1575:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1575():
  {==}

law tv1575:
  Tup(n25())

def tv1575():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1576:
  {mul(n16(), n16()) == mul(n16(), n16()) : Nat}

def cm1576():
  {==}

law tv1576:
  Tup(n28())

def tv1576():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1577:
  {mul(n21(), n27()) == mul(n27(), n21()) : Nat}

def cm1577():
  {==}

law tv1577:
  Tup(n31())

def tv1577():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1578:
  {mul(n26(), n14()) == mul(n14(), n26()) : Nat}

def cm1578():
  {==}

law tv1578:
  Tup(n6())

def tv1578():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}

law cm1579:
  {mul(n31(), n25()) == mul(n25(), n31()) : Nat}

def cm1579():
  {==}

law tv1579:
  Tup(n9())

def tv1579():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}

law cm1580:
  {mul(n12(), n12()) == mul(n12(), n12()) : Nat}

def cm1580():
  {==}

law tv1580:
  Tup(n12())

def tv1580():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}

law cm1581:
  {mul(n17(), n23()) == mul(n23(), n17()) : Nat}

def cm1581():
  {==}

law tv1581:
  Tup(n15())

def tv1581():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}

law cm1582:
  {mul(n22(), n10()) == mul(n10(), n22()) : Nat}

def cm1582():
  {==}

law tv1582:
  Tup(n18())

def tv1582():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}

law cm1583:
  {mul(n27(), n21()) == mul(n21(), n27()) : Nat}

def cm1583():
  {==}

law tv1583:
  Tup(n21())

def tv1583():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}

law cm1584:
  {mul(n8(), n8()) == mul(n8(), n8()) : Nat}

def cm1584():
  {==}

law tv1584:
  Tup(n24())

def tv1584():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1585:
  {mul(n13(), n19()) == mul(n19(), n13()) : Nat}

def cm1585():
  {==}

law tv1585:
  Tup(n27())

def tv1585():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1586:
  {mul(n18(), n30()) == mul(n30(), n18()) : Nat}

def cm1586():
  {==}

law tv1586:
  Tup(n30())

def tv1586():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1587:
  {mul(n23(), n17()) == mul(n17(), n23()) : Nat}

def cm1587():
  {==}

law tv1587:
  Tup(n5())

def tv1587():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}

law cm1588:
  {mul(n28(), n28()) == mul(n28(), n28()) : Nat}

def cm1588():
  {==}

law tv1588:
  Tup(n8())

def tv1588():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}

law cm1589:
  {mul(n9(), n15()) == mul(n15(), n9()) : Nat}

def cm1589():
  {==}

law tv1589:
  Tup(n11())

def tv1589():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}

law cm1590:
  {mul(n14(), n26()) == mul(n26(), n14()) : Nat}

def cm1590():
  {==}

law tv1590:
  Tup(n14())

def tv1590():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}

law cm1591:
  {mul(n19(), n13()) == mul(n13(), n19()) : Nat}

def cm1591():
  {==}

law tv1591:
  Tup(n17())

def tv1591():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}

law cm1592:
  {mul(n24(), n24()) == mul(n24(), n24()) : Nat}

def cm1592():
  {==}

law tv1592:
  Tup(n20())

def tv1592():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}

law cm1593:
  {mul(n29(), n11()) == mul(n11(), n29()) : Nat}

def cm1593():
  {==}

law tv1593:
  Tup(n23())

def tv1593():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}

law cm1594:
  {mul(n10(), n22()) == mul(n22(), n10()) : Nat}

def cm1594():
  {==}

law tv1594:
  Tup(n26())

def tv1594():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1595:
  {mul(n15(), n9()) == mul(n9(), n15()) : Nat}

def cm1595():
  {==}

law tv1595:
  Tup(n29())

def tv1595():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

law cm1596:
  {mul(n20(), n20()) == mul(n20(), n20()) : Nat}

def cm1596():
  {==}

law tv1596:
  Tup(n4())

def tv1596():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}

law cm1597:
  {mul(n25(), n31()) == mul(n31(), n25()) : Nat}

def cm1597():
  {==}

law tv1597:
  Tup(n7())

def tv1597():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}

law cm1598:
  {mul(n30(), n18()) == mul(n18(), n30()) : Nat}

def cm1598():
  {==}

law tv1598:
  Tup(n10())

def tv1598():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}

law cm1599:
  {mul(n11(), n29()) == mul(n29(), n11()) : Nat}

def cm1599():
  {==}

law tv1599:
  Tup(n13())

def tv1599():
  Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Duo{Z{}, Z{}}}}}}}}}}}}}}

