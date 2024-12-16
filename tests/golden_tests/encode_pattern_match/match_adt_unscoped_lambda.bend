type Maybe_ = None | (Some val)

main = (match x = (Maybe_/Some 1) {
  Maybe_/None: @$x *
  Maybe_/Some: x.val
} $x)
