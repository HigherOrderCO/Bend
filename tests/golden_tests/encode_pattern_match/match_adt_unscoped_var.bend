data Maybe = None | (Some val)

Foo = @$x match x = (Maybe/Some 1) {
  Maybe/None: $x
  Maybe/Some: x.val
}

Bar = (match x = (Maybe/Some 1) {
  Maybe/None: $x
  Maybe/Some: x.val
} @$x *)

main = *
