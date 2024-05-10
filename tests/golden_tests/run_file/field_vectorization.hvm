// Vectorizes if the adts are encoded with tagged scott encoding
data Box = (New a)
data Bool = T | F
data List_ = (Cons x xs) | Nil
data Pair = (Tup a b)

(Tup.and (Pair/Tup (Box/New Bool/T) (Box/New Bool/T))) = Bool/T
(Tup.and (Pair/Tup a b)) = Bool/F

(Not Bool/T) = Bool/F
(Not Bool/F) = Bool/T

main = (Not
  (Tup.and 
    (List_/Cons 
      (Pair/Tup (Box/New Bool/T) (Box/New Bool/F))
      (List_/Cons 
        (Pair/Tup (Box/New Bool/F) (Box/New Bool/F))
        (List_/Cons
          (Pair/Tup (Box/New Bool/T) (Box/New Bool/T))
          (List_/Cons
            (Pair/Tup (Box/New Bool/F) (Box/New Bool/T))
            List_/Nil
          )
        )
      )
    )
  )
)
