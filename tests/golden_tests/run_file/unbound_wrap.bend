type Maybe_ = (Some x) | None

Maybe_/bind val nxt = match val {
  Maybe_/Some: (nxt val.x)
  Maybe_/None: Maybe_/None
}

main = with Maybe_ {
  (wrap 1)
}