data Bool = True | False
data List_ = (Cons head tail) | Nil

If Bool/True then else = then
If Bool/False then else = else

Pure x = (List_/Cons x List_/Nil)

Map List_/Nil f = List_/Nil
Map (List_/Cons h t) f = (List_/Cons (f h) (Map t f))

MergeSort cmp xs = (Unpack cmp (Map xs Pure))

Unpack cmp List_/Nil = List_/Nil
Unpack cmp (List_/Cons h List_/Nil) = h
Unpack cmp xs = (Unpack cmp (MergePair cmp xs))

MergePair cmp (List_/Cons h1 (List_/Cons h2 t)) = (List_/Cons (Merge cmp h1 h2) (MergePair cmp t))
MergePair cmp xs = xs

Merge cmp List_/Nil ys = ys
Merge cmp xs List_/Nil = xs
Merge cmp (List_/Cons xh xt) (List_/Cons yh yt) =
  (If (cmp xh yh)
    let ys = (List_/Cons yh yt)
    (List_/Cons xh (Merge cmp xt ys))
    let xs = (List_/Cons xh xt)
    (List_/Cons yh (Merge cmp xs yt))
  )