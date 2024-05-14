data _Box = (Box val)

Box.subst (_Box/Box x) cmp to = switch _ = (cmp x) {
  0: (_Box/Box x)
  _: (_Box/Box to)
}

Main = (Box.subst (_Box/Box 4) @x (== x 4) (_Box/Box 10))
