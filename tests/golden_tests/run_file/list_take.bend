Take_ n list =
  switch _ = (== n 0) {
  | 0: (Take n list)
  | _: []
  }
Take n (List/Nil) = []
Take n (List/Cons x xs) = (List/Cons x (Take_ (- n 1) xs))

main = (Take 2 [3, 2, 1, 5, 5, 5])
