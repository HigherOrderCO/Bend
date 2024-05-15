List/len list = (List/len/go list 0)
List/len/go [] count = count
List/len/go (List/Cons x xs) count = (List/len/go xs (+ count 1))

Take/go n list =
  switch _ = (== n 0) {
  | 0: (Take n list)
  | _: []
  }
Take n [] = []
Take n (List/Cons x xs) = (List/Cons x (Take/go (- n 1) xs))

Drop/go n list =
  switch _ = (== n 0) {
  | 0: (Drop n list)
  | _: list
  }
Drop n [] = []
Drop n (List/Cons x xs) = (Drop/go (- n 1) xs)

List/toTree [] = *
List/toTree [x] = x
List/toTree xs =
  let half = (/ (List/len xs) 2);
  let x = (Take half xs);
  let y = (Drop half xs);
  {(List/toTree x), (List/toTree y)}

Main = (List/toTree [1, 2, 3, 4, 5])
