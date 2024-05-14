// Test that we don't mess up with that unscoped lambda/var
data bool = T | F

main = @x match x {
  Bool/T: @$x $x
  Bool/F: @x x
}
