main =
  let base = 0
  def aux [] = base
      aux (List/Cons head tail) = (+ head (aux tail))
  (aux [1, 2, 3])
