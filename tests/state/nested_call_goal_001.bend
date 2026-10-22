# record ladder, state 0: goals holding FIELD PROJECTIONS through a
# rebuild -- getx(rot(p)) stays a ref chain on the stuck record var
type MN is Data:
  Z{}
  S{p: MN}

type P3 is Data:
  Mk{x: MN, y: MN, z: MN}

law rot:
  for p: P3
  P3

def rot(p):
  match p:
    case Mk{x, y, z}:
      Mk{y, z, x}

law getx:
  for p: P3
  MN

def getx(p):
  match p:
    case Mk{x, y, z}:
      x

law gety:
  for p: P3
  MN

def gety(p):
  match p:
    case Mk{x, y, z}:
      y

law rot_x:
  for p: P3
  {getx(rot(p)) == gety(p) : MN}

def rot_x(p):
  {==}

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Context:
#|- p : P3
#|Location: rot_x
#|41 | def rot_x(p):
#|42>|   {==}
#|   |   ^^^^
#|43 |
#|- left     : getx(rot(p))  =>  getx(rot(p))
#|- right    : gety(p)  =>  gety(p)
#|exit 1
