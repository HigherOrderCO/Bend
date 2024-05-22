type list = (Cons head tail) | Nil

Unpack cmp list/Nil = list/Nil
Unpack cmp (list/Cons h list/Nil) = h
Unpack cmp xs = (cmp xs)
