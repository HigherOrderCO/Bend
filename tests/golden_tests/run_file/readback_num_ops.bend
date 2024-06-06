main = [
  @a (+ a 1),
  @a (+ 1 a),
  @a @b (+ a b),
  @a (- a 1),
  @a (- 1 a),
  @a @b @c (+ a (- b c))
  @a @b @c (+ (- a b) c)
]
