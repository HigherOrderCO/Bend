# We probably will not be able to handle all these
main = (
  @a (1, a),
  (1, 2),
  (*, 2),
  ($x, @$x *),
  ((1, @$y $z), (2, @$z $y)),
  ((@$a $b, @$b $c), (@$c $d, @$d $a))
)