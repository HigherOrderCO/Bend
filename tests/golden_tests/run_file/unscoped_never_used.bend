type Bool = T | F

main = @x match x {
  Bool/T : @$x *
  Bool/F : @x *
}
