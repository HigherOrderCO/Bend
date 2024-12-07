type Maybe_ = None | (Some val)

Foo = @$x match x = (Maybe_/Some 1) {
  Maybe_/None: $x
  Maybe_/Some: x.val
}

Bar = (match x = (Maybe_/Some 1) {
  Maybe_/None: $x
  Maybe_/Some: x.val
} @$x *)

main = *
