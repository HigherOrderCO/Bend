main = @x match x {
  Bool/True : @$x *
  Bool/False : @x *
}
