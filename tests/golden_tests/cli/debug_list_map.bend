(Map f []) = []
(Map f (List/Cons head tail)) = (List/Cons (f head) (Map f tail))

(Inc) = @x (+ x 1)

(Main) = (Map Inc [1 41 0xFF 90])
