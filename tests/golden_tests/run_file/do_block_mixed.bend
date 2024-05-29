# Mixed contents in a `do` block should still work.
type Result = (Ok val) | (Err val)

Result/bind r nxt = match r {
  Result/Ok: (nxt r.val)
  Result/Err: r
}

main = with Result {
  let x = 1
  let y = (Result/Ok x)
  ask y = y
  if (+ x y) {
    ask w = (Result/Ok x)
    (Result/Ok w)
  } else {
    (Result/Err 0)
  }
}
