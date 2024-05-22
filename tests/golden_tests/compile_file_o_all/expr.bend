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

main = (Expr/Let
  (Expr/Var 0)
  (Expr/Op2 Op/Mul (Expr/Op2 Op/Sub (Expr/Num 2) (Expr/Num 1) (Expr/Num 3)))
  (Expr/Dup (Expr/Var 1) (Expr/Var 2) (Expr/Var 0) (Expr/App (Expr/Var 1) (Expr/Var 2)))
)
