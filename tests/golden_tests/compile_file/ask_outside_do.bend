type Result = (Ok val) | (Err val)

Result/bind r nxt = match r {
  Result/Ok: (nxt r.val)
  Result/Err: r
}

main = ask x = (Result/Ok x); x
