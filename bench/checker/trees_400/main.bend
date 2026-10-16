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

type Bool is Data:
  T{}
  F{}

law and:
  for a: Bool
  for b: Bool
  Bool

def and(a, b):
  match a:
    case T{}:
      b
    case F{}:
      F{}

type Tree is Data:
  L{}
  N{lft: Tree, rgt: Tree}

law full:
  for +n: Nat
  Tree

def full(n):
  match n:
    case Z{}:
      L{}
    case S{p}:
      N{full(p), full(p)}

law mirror:
  for t: Tree
  Tree

def mirror(t):
  match t:
    case L{}:
      L{}
    case N{l, r}:
      N{mirror(r), mirror(l)}

law alltrue:
  for t: Tree
  Bool

def alltrue(t):
  match t:
    case L{}:
      T{}
    case N{l, r}:
      and(alltrue(l), alltrue(r))

law ft0:
  {alltrue(full(n7())) == T{} : Bool}

def ft0():
  {==}

law mt0:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt0():
  {==}

law ft1:
  {alltrue(full(n12())) == T{} : Bool}

def ft1():
  {==}

law mt1:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt1():
  {==}

law ft2:
  {alltrue(full(n11())) == T{} : Bool}

def ft2():
  {==}

law mt2:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt2():
  {==}

law ft3:
  {alltrue(full(n10())) == T{} : Bool}

def ft3():
  {==}

law mt3:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt3():
  {==}

law ft4:
  {alltrue(full(n9())) == T{} : Bool}

def ft4():
  {==}

law mt4:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt4():
  {==}

law ft5:
  {alltrue(full(n8())) == T{} : Bool}

def ft5():
  {==}

law mt5:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt5():
  {==}

law ft6:
  {alltrue(full(n7())) == T{} : Bool}

def ft6():
  {==}

law mt6:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt6():
  {==}

law ft7:
  {alltrue(full(n12())) == T{} : Bool}

def ft7():
  {==}

law mt7:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt7():
  {==}

law ft8:
  {alltrue(full(n11())) == T{} : Bool}

def ft8():
  {==}

law mt8:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt8():
  {==}

law ft9:
  {alltrue(full(n10())) == T{} : Bool}

def ft9():
  {==}

law mt9:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt9():
  {==}

law ft10:
  {alltrue(full(n9())) == T{} : Bool}

def ft10():
  {==}

law mt10:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt10():
  {==}

law ft11:
  {alltrue(full(n8())) == T{} : Bool}

def ft11():
  {==}

law mt11:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt11():
  {==}

law ft12:
  {alltrue(full(n7())) == T{} : Bool}

def ft12():
  {==}

law mt12:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt12():
  {==}

law ft13:
  {alltrue(full(n12())) == T{} : Bool}

def ft13():
  {==}

law mt13:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt13():
  {==}

law ft14:
  {alltrue(full(n11())) == T{} : Bool}

def ft14():
  {==}

law mt14:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt14():
  {==}

law ft15:
  {alltrue(full(n10())) == T{} : Bool}

def ft15():
  {==}

law mt15:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt15():
  {==}

law ft16:
  {alltrue(full(n9())) == T{} : Bool}

def ft16():
  {==}

law mt16:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt16():
  {==}

law ft17:
  {alltrue(full(n8())) == T{} : Bool}

def ft17():
  {==}

law mt17:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt17():
  {==}

law ft18:
  {alltrue(full(n7())) == T{} : Bool}

def ft18():
  {==}

law mt18:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt18():
  {==}

law ft19:
  {alltrue(full(n12())) == T{} : Bool}

def ft19():
  {==}

law mt19:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt19():
  {==}

law ft20:
  {alltrue(full(n11())) == T{} : Bool}

def ft20():
  {==}

law mt20:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt20():
  {==}

law ft21:
  {alltrue(full(n10())) == T{} : Bool}

def ft21():
  {==}

law mt21:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt21():
  {==}

law ft22:
  {alltrue(full(n9())) == T{} : Bool}

def ft22():
  {==}

law mt22:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt22():
  {==}

law ft23:
  {alltrue(full(n8())) == T{} : Bool}

def ft23():
  {==}

law mt23:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt23():
  {==}

law ft24:
  {alltrue(full(n7())) == T{} : Bool}

def ft24():
  {==}

law mt24:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt24():
  {==}

law ft25:
  {alltrue(full(n12())) == T{} : Bool}

def ft25():
  {==}

law mt25:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt25():
  {==}

law ft26:
  {alltrue(full(n11())) == T{} : Bool}

def ft26():
  {==}

law mt26:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt26():
  {==}

law ft27:
  {alltrue(full(n10())) == T{} : Bool}

def ft27():
  {==}

law mt27:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt27():
  {==}

law ft28:
  {alltrue(full(n9())) == T{} : Bool}

def ft28():
  {==}

law mt28:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt28():
  {==}

law ft29:
  {alltrue(full(n8())) == T{} : Bool}

def ft29():
  {==}

law mt29:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt29():
  {==}

law ft30:
  {alltrue(full(n7())) == T{} : Bool}

def ft30():
  {==}

law mt30:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt30():
  {==}

law ft31:
  {alltrue(full(n12())) == T{} : Bool}

def ft31():
  {==}

law mt31:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt31():
  {==}

law ft32:
  {alltrue(full(n11())) == T{} : Bool}

def ft32():
  {==}

law mt32:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt32():
  {==}

law ft33:
  {alltrue(full(n10())) == T{} : Bool}

def ft33():
  {==}

law mt33:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt33():
  {==}

law ft34:
  {alltrue(full(n9())) == T{} : Bool}

def ft34():
  {==}

law mt34:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt34():
  {==}

law ft35:
  {alltrue(full(n8())) == T{} : Bool}

def ft35():
  {==}

law mt35:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt35():
  {==}

law ft36:
  {alltrue(full(n7())) == T{} : Bool}

def ft36():
  {==}

law mt36:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt36():
  {==}

law ft37:
  {alltrue(full(n12())) == T{} : Bool}

def ft37():
  {==}

law mt37:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt37():
  {==}

law ft38:
  {alltrue(full(n11())) == T{} : Bool}

def ft38():
  {==}

law mt38:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt38():
  {==}

law ft39:
  {alltrue(full(n10())) == T{} : Bool}

def ft39():
  {==}

law mt39:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt39():
  {==}

law ft40:
  {alltrue(full(n9())) == T{} : Bool}

def ft40():
  {==}

law mt40:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt40():
  {==}

law ft41:
  {alltrue(full(n8())) == T{} : Bool}

def ft41():
  {==}

law mt41:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt41():
  {==}

law ft42:
  {alltrue(full(n7())) == T{} : Bool}

def ft42():
  {==}

law mt42:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt42():
  {==}

law ft43:
  {alltrue(full(n12())) == T{} : Bool}

def ft43():
  {==}

law mt43:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt43():
  {==}

law ft44:
  {alltrue(full(n11())) == T{} : Bool}

def ft44():
  {==}

law mt44:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt44():
  {==}

law ft45:
  {alltrue(full(n10())) == T{} : Bool}

def ft45():
  {==}

law mt45:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt45():
  {==}

law ft46:
  {alltrue(full(n9())) == T{} : Bool}

def ft46():
  {==}

law mt46:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt46():
  {==}

law ft47:
  {alltrue(full(n8())) == T{} : Bool}

def ft47():
  {==}

law mt47:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt47():
  {==}

law ft48:
  {alltrue(full(n7())) == T{} : Bool}

def ft48():
  {==}

law mt48:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt48():
  {==}

law ft49:
  {alltrue(full(n12())) == T{} : Bool}

def ft49():
  {==}

law mt49:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt49():
  {==}

law ft50:
  {alltrue(full(n11())) == T{} : Bool}

def ft50():
  {==}

law mt50:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt50():
  {==}

law ft51:
  {alltrue(full(n10())) == T{} : Bool}

def ft51():
  {==}

law mt51:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt51():
  {==}

law ft52:
  {alltrue(full(n9())) == T{} : Bool}

def ft52():
  {==}

law mt52:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt52():
  {==}

law ft53:
  {alltrue(full(n8())) == T{} : Bool}

def ft53():
  {==}

law mt53:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt53():
  {==}

law ft54:
  {alltrue(full(n7())) == T{} : Bool}

def ft54():
  {==}

law mt54:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt54():
  {==}

law ft55:
  {alltrue(full(n12())) == T{} : Bool}

def ft55():
  {==}

law mt55:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt55():
  {==}

law ft56:
  {alltrue(full(n11())) == T{} : Bool}

def ft56():
  {==}

law mt56:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt56():
  {==}

law ft57:
  {alltrue(full(n10())) == T{} : Bool}

def ft57():
  {==}

law mt57:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt57():
  {==}

law ft58:
  {alltrue(full(n9())) == T{} : Bool}

def ft58():
  {==}

law mt58:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt58():
  {==}

law ft59:
  {alltrue(full(n8())) == T{} : Bool}

def ft59():
  {==}

law mt59:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt59():
  {==}

law ft60:
  {alltrue(full(n7())) == T{} : Bool}

def ft60():
  {==}

law mt60:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt60():
  {==}

law ft61:
  {alltrue(full(n12())) == T{} : Bool}

def ft61():
  {==}

law mt61:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt61():
  {==}

law ft62:
  {alltrue(full(n11())) == T{} : Bool}

def ft62():
  {==}

law mt62:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt62():
  {==}

law ft63:
  {alltrue(full(n10())) == T{} : Bool}

def ft63():
  {==}

law mt63:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt63():
  {==}

law ft64:
  {alltrue(full(n9())) == T{} : Bool}

def ft64():
  {==}

law mt64:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt64():
  {==}

law ft65:
  {alltrue(full(n8())) == T{} : Bool}

def ft65():
  {==}

law mt65:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt65():
  {==}

law ft66:
  {alltrue(full(n7())) == T{} : Bool}

def ft66():
  {==}

law mt66:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt66():
  {==}

law ft67:
  {alltrue(full(n12())) == T{} : Bool}

def ft67():
  {==}

law mt67:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt67():
  {==}

law ft68:
  {alltrue(full(n11())) == T{} : Bool}

def ft68():
  {==}

law mt68:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt68():
  {==}

law ft69:
  {alltrue(full(n10())) == T{} : Bool}

def ft69():
  {==}

law mt69:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt69():
  {==}

law ft70:
  {alltrue(full(n9())) == T{} : Bool}

def ft70():
  {==}

law mt70:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt70():
  {==}

law ft71:
  {alltrue(full(n8())) == T{} : Bool}

def ft71():
  {==}

law mt71:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt71():
  {==}

law ft72:
  {alltrue(full(n7())) == T{} : Bool}

def ft72():
  {==}

law mt72:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt72():
  {==}

law ft73:
  {alltrue(full(n12())) == T{} : Bool}

def ft73():
  {==}

law mt73:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt73():
  {==}

law ft74:
  {alltrue(full(n11())) == T{} : Bool}

def ft74():
  {==}

law mt74:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt74():
  {==}

law ft75:
  {alltrue(full(n10())) == T{} : Bool}

def ft75():
  {==}

law mt75:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt75():
  {==}

law ft76:
  {alltrue(full(n9())) == T{} : Bool}

def ft76():
  {==}

law mt76:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt76():
  {==}

law ft77:
  {alltrue(full(n8())) == T{} : Bool}

def ft77():
  {==}

law mt77:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt77():
  {==}

law ft78:
  {alltrue(full(n7())) == T{} : Bool}

def ft78():
  {==}

law mt78:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt78():
  {==}

law ft79:
  {alltrue(full(n12())) == T{} : Bool}

def ft79():
  {==}

law mt79:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt79():
  {==}

law ft80:
  {alltrue(full(n11())) == T{} : Bool}

def ft80():
  {==}

law mt80:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt80():
  {==}

law ft81:
  {alltrue(full(n10())) == T{} : Bool}

def ft81():
  {==}

law mt81:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt81():
  {==}

law ft82:
  {alltrue(full(n9())) == T{} : Bool}

def ft82():
  {==}

law mt82:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt82():
  {==}

law ft83:
  {alltrue(full(n8())) == T{} : Bool}

def ft83():
  {==}

law mt83:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt83():
  {==}

law ft84:
  {alltrue(full(n7())) == T{} : Bool}

def ft84():
  {==}

law mt84:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt84():
  {==}

law ft85:
  {alltrue(full(n12())) == T{} : Bool}

def ft85():
  {==}

law mt85:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt85():
  {==}

law ft86:
  {alltrue(full(n11())) == T{} : Bool}

def ft86():
  {==}

law mt86:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt86():
  {==}

law ft87:
  {alltrue(full(n10())) == T{} : Bool}

def ft87():
  {==}

law mt87:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt87():
  {==}

law ft88:
  {alltrue(full(n9())) == T{} : Bool}

def ft88():
  {==}

law mt88:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt88():
  {==}

law ft89:
  {alltrue(full(n8())) == T{} : Bool}

def ft89():
  {==}

law mt89:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt89():
  {==}

law ft90:
  {alltrue(full(n7())) == T{} : Bool}

def ft90():
  {==}

law mt90:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt90():
  {==}

law ft91:
  {alltrue(full(n12())) == T{} : Bool}

def ft91():
  {==}

law mt91:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt91():
  {==}

law ft92:
  {alltrue(full(n11())) == T{} : Bool}

def ft92():
  {==}

law mt92:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt92():
  {==}

law ft93:
  {alltrue(full(n10())) == T{} : Bool}

def ft93():
  {==}

law mt93:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt93():
  {==}

law ft94:
  {alltrue(full(n9())) == T{} : Bool}

def ft94():
  {==}

law mt94:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt94():
  {==}

law ft95:
  {alltrue(full(n8())) == T{} : Bool}

def ft95():
  {==}

law mt95:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt95():
  {==}

law ft96:
  {alltrue(full(n7())) == T{} : Bool}

def ft96():
  {==}

law mt96:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt96():
  {==}

law ft97:
  {alltrue(full(n12())) == T{} : Bool}

def ft97():
  {==}

law mt97:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt97():
  {==}

law ft98:
  {alltrue(full(n11())) == T{} : Bool}

def ft98():
  {==}

law mt98:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt98():
  {==}

law ft99:
  {alltrue(full(n10())) == T{} : Bool}

def ft99():
  {==}

law mt99:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt99():
  {==}

law ft100:
  {alltrue(full(n9())) == T{} : Bool}

def ft100():
  {==}

law mt100:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt100():
  {==}

law ft101:
  {alltrue(full(n8())) == T{} : Bool}

def ft101():
  {==}

law mt101:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt101():
  {==}

law ft102:
  {alltrue(full(n7())) == T{} : Bool}

def ft102():
  {==}

law mt102:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt102():
  {==}

law ft103:
  {alltrue(full(n12())) == T{} : Bool}

def ft103():
  {==}

law mt103:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt103():
  {==}

law ft104:
  {alltrue(full(n11())) == T{} : Bool}

def ft104():
  {==}

law mt104:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt104():
  {==}

law ft105:
  {alltrue(full(n10())) == T{} : Bool}

def ft105():
  {==}

law mt105:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt105():
  {==}

law ft106:
  {alltrue(full(n9())) == T{} : Bool}

def ft106():
  {==}

law mt106:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt106():
  {==}

law ft107:
  {alltrue(full(n8())) == T{} : Bool}

def ft107():
  {==}

law mt107:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt107():
  {==}

law ft108:
  {alltrue(full(n7())) == T{} : Bool}

def ft108():
  {==}

law mt108:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt108():
  {==}

law ft109:
  {alltrue(full(n12())) == T{} : Bool}

def ft109():
  {==}

law mt109:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt109():
  {==}

law ft110:
  {alltrue(full(n11())) == T{} : Bool}

def ft110():
  {==}

law mt110:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt110():
  {==}

law ft111:
  {alltrue(full(n10())) == T{} : Bool}

def ft111():
  {==}

law mt111:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt111():
  {==}

law ft112:
  {alltrue(full(n9())) == T{} : Bool}

def ft112():
  {==}

law mt112:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt112():
  {==}

law ft113:
  {alltrue(full(n8())) == T{} : Bool}

def ft113():
  {==}

law mt113:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt113():
  {==}

law ft114:
  {alltrue(full(n7())) == T{} : Bool}

def ft114():
  {==}

law mt114:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt114():
  {==}

law ft115:
  {alltrue(full(n12())) == T{} : Bool}

def ft115():
  {==}

law mt115:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt115():
  {==}

law ft116:
  {alltrue(full(n11())) == T{} : Bool}

def ft116():
  {==}

law mt116:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt116():
  {==}

law ft117:
  {alltrue(full(n10())) == T{} : Bool}

def ft117():
  {==}

law mt117:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt117():
  {==}

law ft118:
  {alltrue(full(n9())) == T{} : Bool}

def ft118():
  {==}

law mt118:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt118():
  {==}

law ft119:
  {alltrue(full(n8())) == T{} : Bool}

def ft119():
  {==}

law mt119:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt119():
  {==}

law ft120:
  {alltrue(full(n7())) == T{} : Bool}

def ft120():
  {==}

law mt120:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt120():
  {==}

law ft121:
  {alltrue(full(n12())) == T{} : Bool}

def ft121():
  {==}

law mt121:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt121():
  {==}

law ft122:
  {alltrue(full(n11())) == T{} : Bool}

def ft122():
  {==}

law mt122:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt122():
  {==}

law ft123:
  {alltrue(full(n10())) == T{} : Bool}

def ft123():
  {==}

law mt123:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt123():
  {==}

law ft124:
  {alltrue(full(n9())) == T{} : Bool}

def ft124():
  {==}

law mt124:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt124():
  {==}

law ft125:
  {alltrue(full(n8())) == T{} : Bool}

def ft125():
  {==}

law mt125:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt125():
  {==}

law ft126:
  {alltrue(full(n7())) == T{} : Bool}

def ft126():
  {==}

law mt126:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt126():
  {==}

law ft127:
  {alltrue(full(n12())) == T{} : Bool}

def ft127():
  {==}

law mt127:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt127():
  {==}

law ft128:
  {alltrue(full(n11())) == T{} : Bool}

def ft128():
  {==}

law mt128:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt128():
  {==}

law ft129:
  {alltrue(full(n10())) == T{} : Bool}

def ft129():
  {==}

law mt129:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt129():
  {==}

law ft130:
  {alltrue(full(n9())) == T{} : Bool}

def ft130():
  {==}

law mt130:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt130():
  {==}

law ft131:
  {alltrue(full(n8())) == T{} : Bool}

def ft131():
  {==}

law mt131:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt131():
  {==}

law ft132:
  {alltrue(full(n7())) == T{} : Bool}

def ft132():
  {==}

law mt132:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt132():
  {==}

law ft133:
  {alltrue(full(n12())) == T{} : Bool}

def ft133():
  {==}

law mt133:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt133():
  {==}

law ft134:
  {alltrue(full(n11())) == T{} : Bool}

def ft134():
  {==}

law mt134:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt134():
  {==}

law ft135:
  {alltrue(full(n10())) == T{} : Bool}

def ft135():
  {==}

law mt135:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt135():
  {==}

law ft136:
  {alltrue(full(n9())) == T{} : Bool}

def ft136():
  {==}

law mt136:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt136():
  {==}

law ft137:
  {alltrue(full(n8())) == T{} : Bool}

def ft137():
  {==}

law mt137:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt137():
  {==}

law ft138:
  {alltrue(full(n7())) == T{} : Bool}

def ft138():
  {==}

law mt138:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt138():
  {==}

law ft139:
  {alltrue(full(n12())) == T{} : Bool}

def ft139():
  {==}

law mt139:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt139():
  {==}

law ft140:
  {alltrue(full(n11())) == T{} : Bool}

def ft140():
  {==}

law mt140:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt140():
  {==}

law ft141:
  {alltrue(full(n10())) == T{} : Bool}

def ft141():
  {==}

law mt141:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt141():
  {==}

law ft142:
  {alltrue(full(n9())) == T{} : Bool}

def ft142():
  {==}

law mt142:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt142():
  {==}

law ft143:
  {alltrue(full(n8())) == T{} : Bool}

def ft143():
  {==}

law mt143:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt143():
  {==}

law ft144:
  {alltrue(full(n7())) == T{} : Bool}

def ft144():
  {==}

law mt144:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt144():
  {==}

law ft145:
  {alltrue(full(n12())) == T{} : Bool}

def ft145():
  {==}

law mt145:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt145():
  {==}

law ft146:
  {alltrue(full(n11())) == T{} : Bool}

def ft146():
  {==}

law mt146:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt146():
  {==}

law ft147:
  {alltrue(full(n10())) == T{} : Bool}

def ft147():
  {==}

law mt147:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt147():
  {==}

law ft148:
  {alltrue(full(n9())) == T{} : Bool}

def ft148():
  {==}

law mt148:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt148():
  {==}

law ft149:
  {alltrue(full(n8())) == T{} : Bool}

def ft149():
  {==}

law mt149:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt149():
  {==}

law ft150:
  {alltrue(full(n7())) == T{} : Bool}

def ft150():
  {==}

law mt150:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt150():
  {==}

law ft151:
  {alltrue(full(n12())) == T{} : Bool}

def ft151():
  {==}

law mt151:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt151():
  {==}

law ft152:
  {alltrue(full(n11())) == T{} : Bool}

def ft152():
  {==}

law mt152:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt152():
  {==}

law ft153:
  {alltrue(full(n10())) == T{} : Bool}

def ft153():
  {==}

law mt153:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt153():
  {==}

law ft154:
  {alltrue(full(n9())) == T{} : Bool}

def ft154():
  {==}

law mt154:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt154():
  {==}

law ft155:
  {alltrue(full(n8())) == T{} : Bool}

def ft155():
  {==}

law mt155:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt155():
  {==}

law ft156:
  {alltrue(full(n7())) == T{} : Bool}

def ft156():
  {==}

law mt156:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt156():
  {==}

law ft157:
  {alltrue(full(n12())) == T{} : Bool}

def ft157():
  {==}

law mt157:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt157():
  {==}

law ft158:
  {alltrue(full(n11())) == T{} : Bool}

def ft158():
  {==}

law mt158:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt158():
  {==}

law ft159:
  {alltrue(full(n10())) == T{} : Bool}

def ft159():
  {==}

law mt159:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt159():
  {==}

law ft160:
  {alltrue(full(n9())) == T{} : Bool}

def ft160():
  {==}

law mt160:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt160():
  {==}

law ft161:
  {alltrue(full(n8())) == T{} : Bool}

def ft161():
  {==}

law mt161:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt161():
  {==}

law ft162:
  {alltrue(full(n7())) == T{} : Bool}

def ft162():
  {==}

law mt162:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt162():
  {==}

law ft163:
  {alltrue(full(n12())) == T{} : Bool}

def ft163():
  {==}

law mt163:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt163():
  {==}

law ft164:
  {alltrue(full(n11())) == T{} : Bool}

def ft164():
  {==}

law mt164:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt164():
  {==}

law ft165:
  {alltrue(full(n10())) == T{} : Bool}

def ft165():
  {==}

law mt165:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt165():
  {==}

law ft166:
  {alltrue(full(n9())) == T{} : Bool}

def ft166():
  {==}

law mt166:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt166():
  {==}

law ft167:
  {alltrue(full(n8())) == T{} : Bool}

def ft167():
  {==}

law mt167:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt167():
  {==}

law ft168:
  {alltrue(full(n7())) == T{} : Bool}

def ft168():
  {==}

law mt168:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt168():
  {==}

law ft169:
  {alltrue(full(n12())) == T{} : Bool}

def ft169():
  {==}

law mt169:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt169():
  {==}

law ft170:
  {alltrue(full(n11())) == T{} : Bool}

def ft170():
  {==}

law mt170:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt170():
  {==}

law ft171:
  {alltrue(full(n10())) == T{} : Bool}

def ft171():
  {==}

law mt171:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt171():
  {==}

law ft172:
  {alltrue(full(n9())) == T{} : Bool}

def ft172():
  {==}

law mt172:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt172():
  {==}

law ft173:
  {alltrue(full(n8())) == T{} : Bool}

def ft173():
  {==}

law mt173:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt173():
  {==}

law ft174:
  {alltrue(full(n7())) == T{} : Bool}

def ft174():
  {==}

law mt174:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt174():
  {==}

law ft175:
  {alltrue(full(n12())) == T{} : Bool}

def ft175():
  {==}

law mt175:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt175():
  {==}

law ft176:
  {alltrue(full(n11())) == T{} : Bool}

def ft176():
  {==}

law mt176:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt176():
  {==}

law ft177:
  {alltrue(full(n10())) == T{} : Bool}

def ft177():
  {==}

law mt177:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt177():
  {==}

law ft178:
  {alltrue(full(n9())) == T{} : Bool}

def ft178():
  {==}

law mt178:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt178():
  {==}

law ft179:
  {alltrue(full(n8())) == T{} : Bool}

def ft179():
  {==}

law mt179:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt179():
  {==}

law ft180:
  {alltrue(full(n7())) == T{} : Bool}

def ft180():
  {==}

law mt180:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt180():
  {==}

law ft181:
  {alltrue(full(n12())) == T{} : Bool}

def ft181():
  {==}

law mt181:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt181():
  {==}

law ft182:
  {alltrue(full(n11())) == T{} : Bool}

def ft182():
  {==}

law mt182:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt182():
  {==}

law ft183:
  {alltrue(full(n10())) == T{} : Bool}

def ft183():
  {==}

law mt183:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt183():
  {==}

law ft184:
  {alltrue(full(n9())) == T{} : Bool}

def ft184():
  {==}

law mt184:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt184():
  {==}

law ft185:
  {alltrue(full(n8())) == T{} : Bool}

def ft185():
  {==}

law mt185:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt185():
  {==}

law ft186:
  {alltrue(full(n7())) == T{} : Bool}

def ft186():
  {==}

law mt186:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt186():
  {==}

law ft187:
  {alltrue(full(n12())) == T{} : Bool}

def ft187():
  {==}

law mt187:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt187():
  {==}

law ft188:
  {alltrue(full(n11())) == T{} : Bool}

def ft188():
  {==}

law mt188:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt188():
  {==}

law ft189:
  {alltrue(full(n10())) == T{} : Bool}

def ft189():
  {==}

law mt189:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt189():
  {==}

law ft190:
  {alltrue(full(n9())) == T{} : Bool}

def ft190():
  {==}

law mt190:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt190():
  {==}

law ft191:
  {alltrue(full(n8())) == T{} : Bool}

def ft191():
  {==}

law mt191:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt191():
  {==}

law ft192:
  {alltrue(full(n7())) == T{} : Bool}

def ft192():
  {==}

law mt192:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt192():
  {==}

law ft193:
  {alltrue(full(n12())) == T{} : Bool}

def ft193():
  {==}

law mt193:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt193():
  {==}

law ft194:
  {alltrue(full(n11())) == T{} : Bool}

def ft194():
  {==}

law mt194:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt194():
  {==}

law ft195:
  {alltrue(full(n10())) == T{} : Bool}

def ft195():
  {==}

law mt195:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt195():
  {==}

law ft196:
  {alltrue(full(n9())) == T{} : Bool}

def ft196():
  {==}

law mt196:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt196():
  {==}

law ft197:
  {alltrue(full(n8())) == T{} : Bool}

def ft197():
  {==}

law mt197:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt197():
  {==}

law ft198:
  {alltrue(full(n7())) == T{} : Bool}

def ft198():
  {==}

law mt198:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt198():
  {==}

law ft199:
  {alltrue(full(n12())) == T{} : Bool}

def ft199():
  {==}

law mt199:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt199():
  {==}

law ft200:
  {alltrue(full(n11())) == T{} : Bool}

def ft200():
  {==}

law mt200:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt200():
  {==}

law ft201:
  {alltrue(full(n10())) == T{} : Bool}

def ft201():
  {==}

law mt201:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt201():
  {==}

law ft202:
  {alltrue(full(n9())) == T{} : Bool}

def ft202():
  {==}

law mt202:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt202():
  {==}

law ft203:
  {alltrue(full(n8())) == T{} : Bool}

def ft203():
  {==}

law mt203:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt203():
  {==}

law ft204:
  {alltrue(full(n7())) == T{} : Bool}

def ft204():
  {==}

law mt204:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt204():
  {==}

law ft205:
  {alltrue(full(n12())) == T{} : Bool}

def ft205():
  {==}

law mt205:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt205():
  {==}

law ft206:
  {alltrue(full(n11())) == T{} : Bool}

def ft206():
  {==}

law mt206:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt206():
  {==}

law ft207:
  {alltrue(full(n10())) == T{} : Bool}

def ft207():
  {==}

law mt207:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt207():
  {==}

law ft208:
  {alltrue(full(n9())) == T{} : Bool}

def ft208():
  {==}

law mt208:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt208():
  {==}

law ft209:
  {alltrue(full(n8())) == T{} : Bool}

def ft209():
  {==}

law mt209:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt209():
  {==}

law ft210:
  {alltrue(full(n7())) == T{} : Bool}

def ft210():
  {==}

law mt210:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt210():
  {==}

law ft211:
  {alltrue(full(n12())) == T{} : Bool}

def ft211():
  {==}

law mt211:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt211():
  {==}

law ft212:
  {alltrue(full(n11())) == T{} : Bool}

def ft212():
  {==}

law mt212:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt212():
  {==}

law ft213:
  {alltrue(full(n10())) == T{} : Bool}

def ft213():
  {==}

law mt213:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt213():
  {==}

law ft214:
  {alltrue(full(n9())) == T{} : Bool}

def ft214():
  {==}

law mt214:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt214():
  {==}

law ft215:
  {alltrue(full(n8())) == T{} : Bool}

def ft215():
  {==}

law mt215:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt215():
  {==}

law ft216:
  {alltrue(full(n7())) == T{} : Bool}

def ft216():
  {==}

law mt216:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt216():
  {==}

law ft217:
  {alltrue(full(n12())) == T{} : Bool}

def ft217():
  {==}

law mt217:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt217():
  {==}

law ft218:
  {alltrue(full(n11())) == T{} : Bool}

def ft218():
  {==}

law mt218:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt218():
  {==}

law ft219:
  {alltrue(full(n10())) == T{} : Bool}

def ft219():
  {==}

law mt219:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt219():
  {==}

law ft220:
  {alltrue(full(n9())) == T{} : Bool}

def ft220():
  {==}

law mt220:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt220():
  {==}

law ft221:
  {alltrue(full(n8())) == T{} : Bool}

def ft221():
  {==}

law mt221:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt221():
  {==}

law ft222:
  {alltrue(full(n7())) == T{} : Bool}

def ft222():
  {==}

law mt222:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt222():
  {==}

law ft223:
  {alltrue(full(n12())) == T{} : Bool}

def ft223():
  {==}

law mt223:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt223():
  {==}

law ft224:
  {alltrue(full(n11())) == T{} : Bool}

def ft224():
  {==}

law mt224:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt224():
  {==}

law ft225:
  {alltrue(full(n10())) == T{} : Bool}

def ft225():
  {==}

law mt225:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt225():
  {==}

law ft226:
  {alltrue(full(n9())) == T{} : Bool}

def ft226():
  {==}

law mt226:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt226():
  {==}

law ft227:
  {alltrue(full(n8())) == T{} : Bool}

def ft227():
  {==}

law mt227:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt227():
  {==}

law ft228:
  {alltrue(full(n7())) == T{} : Bool}

def ft228():
  {==}

law mt228:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt228():
  {==}

law ft229:
  {alltrue(full(n12())) == T{} : Bool}

def ft229():
  {==}

law mt229:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt229():
  {==}

law ft230:
  {alltrue(full(n11())) == T{} : Bool}

def ft230():
  {==}

law mt230:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt230():
  {==}

law ft231:
  {alltrue(full(n10())) == T{} : Bool}

def ft231():
  {==}

law mt231:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt231():
  {==}

law ft232:
  {alltrue(full(n9())) == T{} : Bool}

def ft232():
  {==}

law mt232:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt232():
  {==}

law ft233:
  {alltrue(full(n8())) == T{} : Bool}

def ft233():
  {==}

law mt233:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt233():
  {==}

law ft234:
  {alltrue(full(n7())) == T{} : Bool}

def ft234():
  {==}

law mt234:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt234():
  {==}

law ft235:
  {alltrue(full(n12())) == T{} : Bool}

def ft235():
  {==}

law mt235:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt235():
  {==}

law ft236:
  {alltrue(full(n11())) == T{} : Bool}

def ft236():
  {==}

law mt236:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt236():
  {==}

law ft237:
  {alltrue(full(n10())) == T{} : Bool}

def ft237():
  {==}

law mt237:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt237():
  {==}

law ft238:
  {alltrue(full(n9())) == T{} : Bool}

def ft238():
  {==}

law mt238:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt238():
  {==}

law ft239:
  {alltrue(full(n8())) == T{} : Bool}

def ft239():
  {==}

law mt239:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt239():
  {==}

law ft240:
  {alltrue(full(n7())) == T{} : Bool}

def ft240():
  {==}

law mt240:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt240():
  {==}

law ft241:
  {alltrue(full(n12())) == T{} : Bool}

def ft241():
  {==}

law mt241:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt241():
  {==}

law ft242:
  {alltrue(full(n11())) == T{} : Bool}

def ft242():
  {==}

law mt242:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt242():
  {==}

law ft243:
  {alltrue(full(n10())) == T{} : Bool}

def ft243():
  {==}

law mt243:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt243():
  {==}

law ft244:
  {alltrue(full(n9())) == T{} : Bool}

def ft244():
  {==}

law mt244:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt244():
  {==}

law ft245:
  {alltrue(full(n8())) == T{} : Bool}

def ft245():
  {==}

law mt245:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt245():
  {==}

law ft246:
  {alltrue(full(n7())) == T{} : Bool}

def ft246():
  {==}

law mt246:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt246():
  {==}

law ft247:
  {alltrue(full(n12())) == T{} : Bool}

def ft247():
  {==}

law mt247:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt247():
  {==}

law ft248:
  {alltrue(full(n11())) == T{} : Bool}

def ft248():
  {==}

law mt248:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt248():
  {==}

law ft249:
  {alltrue(full(n10())) == T{} : Bool}

def ft249():
  {==}

law mt249:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt249():
  {==}

law ft250:
  {alltrue(full(n9())) == T{} : Bool}

def ft250():
  {==}

law mt250:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt250():
  {==}

law ft251:
  {alltrue(full(n8())) == T{} : Bool}

def ft251():
  {==}

law mt251:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt251():
  {==}

law ft252:
  {alltrue(full(n7())) == T{} : Bool}

def ft252():
  {==}

law mt252:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt252():
  {==}

law ft253:
  {alltrue(full(n12())) == T{} : Bool}

def ft253():
  {==}

law mt253:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt253():
  {==}

law ft254:
  {alltrue(full(n11())) == T{} : Bool}

def ft254():
  {==}

law mt254:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt254():
  {==}

law ft255:
  {alltrue(full(n10())) == T{} : Bool}

def ft255():
  {==}

law mt255:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt255():
  {==}

law ft256:
  {alltrue(full(n9())) == T{} : Bool}

def ft256():
  {==}

law mt256:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt256():
  {==}

law ft257:
  {alltrue(full(n8())) == T{} : Bool}

def ft257():
  {==}

law mt257:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt257():
  {==}

law ft258:
  {alltrue(full(n7())) == T{} : Bool}

def ft258():
  {==}

law mt258:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt258():
  {==}

law ft259:
  {alltrue(full(n12())) == T{} : Bool}

def ft259():
  {==}

law mt259:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt259():
  {==}

law ft260:
  {alltrue(full(n11())) == T{} : Bool}

def ft260():
  {==}

law mt260:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt260():
  {==}

law ft261:
  {alltrue(full(n10())) == T{} : Bool}

def ft261():
  {==}

law mt261:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt261():
  {==}

law ft262:
  {alltrue(full(n9())) == T{} : Bool}

def ft262():
  {==}

law mt262:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt262():
  {==}

law ft263:
  {alltrue(full(n8())) == T{} : Bool}

def ft263():
  {==}

law mt263:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt263():
  {==}

law ft264:
  {alltrue(full(n7())) == T{} : Bool}

def ft264():
  {==}

law mt264:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt264():
  {==}

law ft265:
  {alltrue(full(n12())) == T{} : Bool}

def ft265():
  {==}

law mt265:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt265():
  {==}

law ft266:
  {alltrue(full(n11())) == T{} : Bool}

def ft266():
  {==}

law mt266:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt266():
  {==}

law ft267:
  {alltrue(full(n10())) == T{} : Bool}

def ft267():
  {==}

law mt267:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt267():
  {==}

law ft268:
  {alltrue(full(n9())) == T{} : Bool}

def ft268():
  {==}

law mt268:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt268():
  {==}

law ft269:
  {alltrue(full(n8())) == T{} : Bool}

def ft269():
  {==}

law mt269:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt269():
  {==}

law ft270:
  {alltrue(full(n7())) == T{} : Bool}

def ft270():
  {==}

law mt270:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt270():
  {==}

law ft271:
  {alltrue(full(n12())) == T{} : Bool}

def ft271():
  {==}

law mt271:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt271():
  {==}

law ft272:
  {alltrue(full(n11())) == T{} : Bool}

def ft272():
  {==}

law mt272:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt272():
  {==}

law ft273:
  {alltrue(full(n10())) == T{} : Bool}

def ft273():
  {==}

law mt273:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt273():
  {==}

law ft274:
  {alltrue(full(n9())) == T{} : Bool}

def ft274():
  {==}

law mt274:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt274():
  {==}

law ft275:
  {alltrue(full(n8())) == T{} : Bool}

def ft275():
  {==}

law mt275:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt275():
  {==}

law ft276:
  {alltrue(full(n7())) == T{} : Bool}

def ft276():
  {==}

law mt276:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt276():
  {==}

law ft277:
  {alltrue(full(n12())) == T{} : Bool}

def ft277():
  {==}

law mt277:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt277():
  {==}

law ft278:
  {alltrue(full(n11())) == T{} : Bool}

def ft278():
  {==}

law mt278:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt278():
  {==}

law ft279:
  {alltrue(full(n10())) == T{} : Bool}

def ft279():
  {==}

law mt279:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt279():
  {==}

law ft280:
  {alltrue(full(n9())) == T{} : Bool}

def ft280():
  {==}

law mt280:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt280():
  {==}

law ft281:
  {alltrue(full(n8())) == T{} : Bool}

def ft281():
  {==}

law mt281:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt281():
  {==}

law ft282:
  {alltrue(full(n7())) == T{} : Bool}

def ft282():
  {==}

law mt282:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt282():
  {==}

law ft283:
  {alltrue(full(n12())) == T{} : Bool}

def ft283():
  {==}

law mt283:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt283():
  {==}

law ft284:
  {alltrue(full(n11())) == T{} : Bool}

def ft284():
  {==}

law mt284:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt284():
  {==}

law ft285:
  {alltrue(full(n10())) == T{} : Bool}

def ft285():
  {==}

law mt285:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt285():
  {==}

law ft286:
  {alltrue(full(n9())) == T{} : Bool}

def ft286():
  {==}

law mt286:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt286():
  {==}

law ft287:
  {alltrue(full(n8())) == T{} : Bool}

def ft287():
  {==}

law mt287:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt287():
  {==}

law ft288:
  {alltrue(full(n7())) == T{} : Bool}

def ft288():
  {==}

law mt288:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt288():
  {==}

law ft289:
  {alltrue(full(n12())) == T{} : Bool}

def ft289():
  {==}

law mt289:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt289():
  {==}

law ft290:
  {alltrue(full(n11())) == T{} : Bool}

def ft290():
  {==}

law mt290:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt290():
  {==}

law ft291:
  {alltrue(full(n10())) == T{} : Bool}

def ft291():
  {==}

law mt291:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt291():
  {==}

law ft292:
  {alltrue(full(n9())) == T{} : Bool}

def ft292():
  {==}

law mt292:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt292():
  {==}

law ft293:
  {alltrue(full(n8())) == T{} : Bool}

def ft293():
  {==}

law mt293:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt293():
  {==}

law ft294:
  {alltrue(full(n7())) == T{} : Bool}

def ft294():
  {==}

law mt294:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt294():
  {==}

law ft295:
  {alltrue(full(n12())) == T{} : Bool}

def ft295():
  {==}

law mt295:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt295():
  {==}

law ft296:
  {alltrue(full(n11())) == T{} : Bool}

def ft296():
  {==}

law mt296:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt296():
  {==}

law ft297:
  {alltrue(full(n10())) == T{} : Bool}

def ft297():
  {==}

law mt297:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt297():
  {==}

law ft298:
  {alltrue(full(n9())) == T{} : Bool}

def ft298():
  {==}

law mt298:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt298():
  {==}

law ft299:
  {alltrue(full(n8())) == T{} : Bool}

def ft299():
  {==}

law mt299:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt299():
  {==}

law ft300:
  {alltrue(full(n7())) == T{} : Bool}

def ft300():
  {==}

law mt300:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt300():
  {==}

law ft301:
  {alltrue(full(n12())) == T{} : Bool}

def ft301():
  {==}

law mt301:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt301():
  {==}

law ft302:
  {alltrue(full(n11())) == T{} : Bool}

def ft302():
  {==}

law mt302:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt302():
  {==}

law ft303:
  {alltrue(full(n10())) == T{} : Bool}

def ft303():
  {==}

law mt303:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt303():
  {==}

law ft304:
  {alltrue(full(n9())) == T{} : Bool}

def ft304():
  {==}

law mt304:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt304():
  {==}

law ft305:
  {alltrue(full(n8())) == T{} : Bool}

def ft305():
  {==}

law mt305:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt305():
  {==}

law ft306:
  {alltrue(full(n7())) == T{} : Bool}

def ft306():
  {==}

law mt306:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt306():
  {==}

law ft307:
  {alltrue(full(n12())) == T{} : Bool}

def ft307():
  {==}

law mt307:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt307():
  {==}

law ft308:
  {alltrue(full(n11())) == T{} : Bool}

def ft308():
  {==}

law mt308:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt308():
  {==}

law ft309:
  {alltrue(full(n10())) == T{} : Bool}

def ft309():
  {==}

law mt309:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt309():
  {==}

law ft310:
  {alltrue(full(n9())) == T{} : Bool}

def ft310():
  {==}

law mt310:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt310():
  {==}

law ft311:
  {alltrue(full(n8())) == T{} : Bool}

def ft311():
  {==}

law mt311:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt311():
  {==}

law ft312:
  {alltrue(full(n7())) == T{} : Bool}

def ft312():
  {==}

law mt312:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt312():
  {==}

law ft313:
  {alltrue(full(n12())) == T{} : Bool}

def ft313():
  {==}

law mt313:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt313():
  {==}

law ft314:
  {alltrue(full(n11())) == T{} : Bool}

def ft314():
  {==}

law mt314:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt314():
  {==}

law ft315:
  {alltrue(full(n10())) == T{} : Bool}

def ft315():
  {==}

law mt315:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt315():
  {==}

law ft316:
  {alltrue(full(n9())) == T{} : Bool}

def ft316():
  {==}

law mt316:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt316():
  {==}

law ft317:
  {alltrue(full(n8())) == T{} : Bool}

def ft317():
  {==}

law mt317:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt317():
  {==}

law ft318:
  {alltrue(full(n7())) == T{} : Bool}

def ft318():
  {==}

law mt318:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt318():
  {==}

law ft319:
  {alltrue(full(n12())) == T{} : Bool}

def ft319():
  {==}

law mt319:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt319():
  {==}

law ft320:
  {alltrue(full(n11())) == T{} : Bool}

def ft320():
  {==}

law mt320:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt320():
  {==}

law ft321:
  {alltrue(full(n10())) == T{} : Bool}

def ft321():
  {==}

law mt321:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt321():
  {==}

law ft322:
  {alltrue(full(n9())) == T{} : Bool}

def ft322():
  {==}

law mt322:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt322():
  {==}

law ft323:
  {alltrue(full(n8())) == T{} : Bool}

def ft323():
  {==}

law mt323:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt323():
  {==}

law ft324:
  {alltrue(full(n7())) == T{} : Bool}

def ft324():
  {==}

law mt324:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt324():
  {==}

law ft325:
  {alltrue(full(n12())) == T{} : Bool}

def ft325():
  {==}

law mt325:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt325():
  {==}

law ft326:
  {alltrue(full(n11())) == T{} : Bool}

def ft326():
  {==}

law mt326:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt326():
  {==}

law ft327:
  {alltrue(full(n10())) == T{} : Bool}

def ft327():
  {==}

law mt327:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt327():
  {==}

law ft328:
  {alltrue(full(n9())) == T{} : Bool}

def ft328():
  {==}

law mt328:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt328():
  {==}

law ft329:
  {alltrue(full(n8())) == T{} : Bool}

def ft329():
  {==}

law mt329:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt329():
  {==}

law ft330:
  {alltrue(full(n7())) == T{} : Bool}

def ft330():
  {==}

law mt330:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt330():
  {==}

law ft331:
  {alltrue(full(n12())) == T{} : Bool}

def ft331():
  {==}

law mt331:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt331():
  {==}

law ft332:
  {alltrue(full(n11())) == T{} : Bool}

def ft332():
  {==}

law mt332:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt332():
  {==}

law ft333:
  {alltrue(full(n10())) == T{} : Bool}

def ft333():
  {==}

law mt333:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt333():
  {==}

law ft334:
  {alltrue(full(n9())) == T{} : Bool}

def ft334():
  {==}

law mt334:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt334():
  {==}

law ft335:
  {alltrue(full(n8())) == T{} : Bool}

def ft335():
  {==}

law mt335:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt335():
  {==}

law ft336:
  {alltrue(full(n7())) == T{} : Bool}

def ft336():
  {==}

law mt336:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt336():
  {==}

law ft337:
  {alltrue(full(n12())) == T{} : Bool}

def ft337():
  {==}

law mt337:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt337():
  {==}

law ft338:
  {alltrue(full(n11())) == T{} : Bool}

def ft338():
  {==}

law mt338:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt338():
  {==}

law ft339:
  {alltrue(full(n10())) == T{} : Bool}

def ft339():
  {==}

law mt339:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt339():
  {==}

law ft340:
  {alltrue(full(n9())) == T{} : Bool}

def ft340():
  {==}

law mt340:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt340():
  {==}

law ft341:
  {alltrue(full(n8())) == T{} : Bool}

def ft341():
  {==}

law mt341:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt341():
  {==}

law ft342:
  {alltrue(full(n7())) == T{} : Bool}

def ft342():
  {==}

law mt342:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt342():
  {==}

law ft343:
  {alltrue(full(n12())) == T{} : Bool}

def ft343():
  {==}

law mt343:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt343():
  {==}

law ft344:
  {alltrue(full(n11())) == T{} : Bool}

def ft344():
  {==}

law mt344:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt344():
  {==}

law ft345:
  {alltrue(full(n10())) == T{} : Bool}

def ft345():
  {==}

law mt345:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt345():
  {==}

law ft346:
  {alltrue(full(n9())) == T{} : Bool}

def ft346():
  {==}

law mt346:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt346():
  {==}

law ft347:
  {alltrue(full(n8())) == T{} : Bool}

def ft347():
  {==}

law mt347:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt347():
  {==}

law ft348:
  {alltrue(full(n7())) == T{} : Bool}

def ft348():
  {==}

law mt348:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt348():
  {==}

law ft349:
  {alltrue(full(n12())) == T{} : Bool}

def ft349():
  {==}

law mt349:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt349():
  {==}

law ft350:
  {alltrue(full(n11())) == T{} : Bool}

def ft350():
  {==}

law mt350:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt350():
  {==}

law ft351:
  {alltrue(full(n10())) == T{} : Bool}

def ft351():
  {==}

law mt351:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt351():
  {==}

law ft352:
  {alltrue(full(n9())) == T{} : Bool}

def ft352():
  {==}

law mt352:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt352():
  {==}

law ft353:
  {alltrue(full(n8())) == T{} : Bool}

def ft353():
  {==}

law mt353:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt353():
  {==}

law ft354:
  {alltrue(full(n7())) == T{} : Bool}

def ft354():
  {==}

law mt354:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt354():
  {==}

law ft355:
  {alltrue(full(n12())) == T{} : Bool}

def ft355():
  {==}

law mt355:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt355():
  {==}

law ft356:
  {alltrue(full(n11())) == T{} : Bool}

def ft356():
  {==}

law mt356:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt356():
  {==}

law ft357:
  {alltrue(full(n10())) == T{} : Bool}

def ft357():
  {==}

law mt357:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt357():
  {==}

law ft358:
  {alltrue(full(n9())) == T{} : Bool}

def ft358():
  {==}

law mt358:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt358():
  {==}

law ft359:
  {alltrue(full(n8())) == T{} : Bool}

def ft359():
  {==}

law mt359:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt359():
  {==}

law ft360:
  {alltrue(full(n7())) == T{} : Bool}

def ft360():
  {==}

law mt360:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt360():
  {==}

law ft361:
  {alltrue(full(n12())) == T{} : Bool}

def ft361():
  {==}

law mt361:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt361():
  {==}

law ft362:
  {alltrue(full(n11())) == T{} : Bool}

def ft362():
  {==}

law mt362:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt362():
  {==}

law ft363:
  {alltrue(full(n10())) == T{} : Bool}

def ft363():
  {==}

law mt363:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt363():
  {==}

law ft364:
  {alltrue(full(n9())) == T{} : Bool}

def ft364():
  {==}

law mt364:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt364():
  {==}

law ft365:
  {alltrue(full(n8())) == T{} : Bool}

def ft365():
  {==}

law mt365:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt365():
  {==}

law ft366:
  {alltrue(full(n7())) == T{} : Bool}

def ft366():
  {==}

law mt366:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt366():
  {==}

law ft367:
  {alltrue(full(n12())) == T{} : Bool}

def ft367():
  {==}

law mt367:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt367():
  {==}

law ft368:
  {alltrue(full(n11())) == T{} : Bool}

def ft368():
  {==}

law mt368:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt368():
  {==}

law ft369:
  {alltrue(full(n10())) == T{} : Bool}

def ft369():
  {==}

law mt369:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt369():
  {==}

law ft370:
  {alltrue(full(n9())) == T{} : Bool}

def ft370():
  {==}

law mt370:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt370():
  {==}

law ft371:
  {alltrue(full(n8())) == T{} : Bool}

def ft371():
  {==}

law mt371:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt371():
  {==}

law ft372:
  {alltrue(full(n7())) == T{} : Bool}

def ft372():
  {==}

law mt372:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt372():
  {==}

law ft373:
  {alltrue(full(n12())) == T{} : Bool}

def ft373():
  {==}

law mt373:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt373():
  {==}

law ft374:
  {alltrue(full(n11())) == T{} : Bool}

def ft374():
  {==}

law mt374:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt374():
  {==}

law ft375:
  {alltrue(full(n10())) == T{} : Bool}

def ft375():
  {==}

law mt375:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt375():
  {==}

law ft376:
  {alltrue(full(n9())) == T{} : Bool}

def ft376():
  {==}

law mt376:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt376():
  {==}

law ft377:
  {alltrue(full(n8())) == T{} : Bool}

def ft377():
  {==}

law mt377:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt377():
  {==}

law ft378:
  {alltrue(full(n7())) == T{} : Bool}

def ft378():
  {==}

law mt378:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt378():
  {==}

law ft379:
  {alltrue(full(n12())) == T{} : Bool}

def ft379():
  {==}

law mt379:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt379():
  {==}

law ft380:
  {alltrue(full(n11())) == T{} : Bool}

def ft380():
  {==}

law mt380:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt380():
  {==}

law ft381:
  {alltrue(full(n10())) == T{} : Bool}

def ft381():
  {==}

law mt381:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt381():
  {==}

law ft382:
  {alltrue(full(n9())) == T{} : Bool}

def ft382():
  {==}

law mt382:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt382():
  {==}

law ft383:
  {alltrue(full(n8())) == T{} : Bool}

def ft383():
  {==}

law mt383:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt383():
  {==}

law ft384:
  {alltrue(full(n7())) == T{} : Bool}

def ft384():
  {==}

law mt384:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt384():
  {==}

law ft385:
  {alltrue(full(n12())) == T{} : Bool}

def ft385():
  {==}

law mt385:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt385():
  {==}

law ft386:
  {alltrue(full(n11())) == T{} : Bool}

def ft386():
  {==}

law mt386:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt386():
  {==}

law ft387:
  {alltrue(full(n10())) == T{} : Bool}

def ft387():
  {==}

law mt387:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt387():
  {==}

law ft388:
  {alltrue(full(n9())) == T{} : Bool}

def ft388():
  {==}

law mt388:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt388():
  {==}

law ft389:
  {alltrue(full(n8())) == T{} : Bool}

def ft389():
  {==}

law mt389:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt389():
  {==}

law ft390:
  {alltrue(full(n7())) == T{} : Bool}

def ft390():
  {==}

law mt390:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt390():
  {==}

law ft391:
  {alltrue(full(n12())) == T{} : Bool}

def ft391():
  {==}

law mt391:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt391():
  {==}

law ft392:
  {alltrue(full(n11())) == T{} : Bool}

def ft392():
  {==}

law mt392:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt392():
  {==}

law ft393:
  {alltrue(full(n10())) == T{} : Bool}

def ft393():
  {==}

law mt393:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt393():
  {==}

law ft394:
  {alltrue(full(n9())) == T{} : Bool}

def ft394():
  {==}

law mt394:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt394():
  {==}

law ft395:
  {alltrue(full(n8())) == T{} : Bool}

def ft395():
  {==}

law mt395:
  {mirror(full(n6())) == full(n6()) : Tree}

def mt395():
  {==}

law ft396:
  {alltrue(full(n7())) == T{} : Bool}

def ft396():
  {==}

law mt396:
  {mirror(full(n9())) == full(n9()) : Tree}

def mt396():
  {==}

law ft397:
  {alltrue(full(n12())) == T{} : Bool}

def ft397():
  {==}

law mt397:
  {mirror(full(n7())) == full(n7()) : Tree}

def mt397():
  {==}

law ft398:
  {alltrue(full(n11())) == T{} : Bool}

def ft398():
  {==}

law mt398:
  {mirror(full(n10())) == full(n10()) : Tree}

def mt398():
  {==}

law ft399:
  {alltrue(full(n10())) == T{} : Bool}

def ft399():
  {==}

law mt399:
  {mirror(full(n8())) == full(n8()) : Tree}

def mt399():
  {==}

