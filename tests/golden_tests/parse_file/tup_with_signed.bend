# Test that we can parse tuples and apps beginning with signed numbers
main = 
  let a = (+1, +1)
  let b = (+1.1324)
  let c = (-6.234, -1)
  let d = (-1)
  let e = (+6 * λx x)
  let f = ((*) λx x)
  (+ a (+ b (+ c (+ d (- e f)))))
