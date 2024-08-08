Result/bind (Result/Ok val) f = ((undefer f) val)
Result/bind err _ = err

Result/wrap x = (Result/Ok x)

Bar x = (Result/Err 0)

Foo x y = with Result {
  ask x = (Bar x);
  (Foo x y)
}

Main = (Foo "a" 0)
