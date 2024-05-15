(Len []) = 0
(Len (List/Cons _ tail)) = (+ 1 (Len tail))

(Main) = (Len [* * * * * *])
