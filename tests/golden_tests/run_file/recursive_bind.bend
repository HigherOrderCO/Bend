type Result = (Ok val) | (Err val)

Result/bind (Result/Ok val) f = (f val)
Result/bind err _ = err

Bar x = (Result/Err 0)

Foo x y = with Result {
  ask x = (Bar x);
  (Foo x y)
}

Main = (Foo "a" 0)
