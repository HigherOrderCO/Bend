type Expr
  = (Lit Int)
  | (Plus Expr Expr)

eval (Expr/Lit i) = i
eval (Expr/Plus l r) = (+ (eval l) (eval r))

main = (eval (Expr/Plus (Expr/Lit 80) (Expr/Lit 90)))
