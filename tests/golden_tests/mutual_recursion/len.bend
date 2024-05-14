(Len []) = 0
(Len (List/cons _ tail)) = (+ 1 (Len tail))

(Main) = (Len [* * * * * *])
