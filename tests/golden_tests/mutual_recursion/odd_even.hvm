data Bool = True | False

(if_ 0 then else) = else
(if_ _ then else) = then

(isOdd  n) = (if_ (== n 0) Bool/False (isEven (- n 1)))
(isEven n) = (if_ (== n 0) Bool/True  (isOdd  (- n 1)))

(Main) = (isOdd 4)
