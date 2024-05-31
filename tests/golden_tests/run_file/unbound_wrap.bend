type Maybe = (Some x) | None

Maybe/bind val nxt = match val {
  Maybe/Some: (nxt val.x)
  Maybe/None: Maybe/None
}

main = with Maybe {
  (wrap 1)
}