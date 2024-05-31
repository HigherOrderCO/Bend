# This will only work if we make the call to `(Result/foo a b)` lazy (by converting it to a combinator).
type Result = (Ok val) | (Err val)

Result/bind = @val @nxt match val {
  Result/Ok: ((undefer nxt) val.val)
  Result/Err: (Result/Err val.val)
}
Result/foo x y = 
  with Result {
    ask a = (Result/Ok x)
    ask b = switch y { 0: (Result/Err a); _: (Result/Ok y-1) }
    (Result/foo a b)
  }

main = (Result/foo 1 2)