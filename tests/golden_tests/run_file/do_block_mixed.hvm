// Mixed contents in a `do` block should still work.

Result/bind r nxt = match r {
  Result/ok: (nxt r.val)
  Result/err: r
}

main = do Result {
  let x = 1
  let y = (Result/ok x)
  ask y = y
  if (+ x y) {
    ask w = (Result/ok x)
    (Result/ok w)
  } else {
    (Result/err 0)
  }
}