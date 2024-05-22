id cool-var-name = cool-var-name

type Done_ = (Done done-var)

toZero n =
  switch n {
    0: (Done_/Done 1)
    _: (toZero n-1)
  }

main =
  match is-done = (toZero 4) {
    Done_/Done: is-done.done-var
  }
