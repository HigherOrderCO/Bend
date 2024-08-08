# Mixed contents in a `do` block should still work.
Result/bind r nxt = match r {
  Result/Ok: ((undefer nxt) r.val)
  Result/Err: r
}

Result/wrap x = (Result/Ok x)

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
