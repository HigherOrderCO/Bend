type Expr
  = (Var name)
  | (Num val)
  | (App fun arg)
  | (Fun name body)
  | (If cond then else)
  | (Let bind val next)
  | (Dup fst snd val next)
  | (Tup fst snd)
  | (Op2 op fst snd)

type Op
  = Add
  | Sub
  | Mul
  | Div
