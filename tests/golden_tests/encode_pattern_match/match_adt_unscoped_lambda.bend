main = (match x = (Maybe/Some 1) {
  Maybe/None: @$x *
  Maybe/Some: x.value
} $x)
