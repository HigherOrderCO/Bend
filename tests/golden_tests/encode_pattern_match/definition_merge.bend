type Either = (Left value) | (Right value)
type Bool = Bool/True | Bool/False

Foo (Either/Left  Bool/False) (Either/Left  Bool/False) = 1
Foo (Either/Left  Bool/False) (Either/Left  Bool/True)  = 1
Foo (Either/Left  Bool/True)  (Either/Left  Bool/False) = 1
Foo (Either/Left  Bool/True)  (Either/Left  Bool/True)  = 1

Foo (Either/Left  Bool/False) (Either/Right Bool/False) = 2
Foo (Either/Left  Bool/False) (Either/Right Bool/True)  = 2
Foo (Either/Left  Bool/True)  (Either/Right Bool/False) = 2
Foo (Either/Left  Bool/True)  (Either/Right Bool/True)  = 2

Foo (Either/Right Bool/False) (Either/Right Bool/False) = 3
Foo (Either/Right Bool/False) (Either/Right Bool/True)  = 3
Foo (Either/Right Bool/True)  (Either/Right Bool/False) = 3
Foo (Either/Right Bool/True)  (Either/Right Bool/True)  = 3

Foo (Either/Right Bool/False) (Either/Left Bool/False)  = 3
Foo (Either/Right Bool/False) (Either/Left Bool/True)   = 3
Foo (Either/Right Bool/True)  (Either/Left Bool/False)  = 3
Foo (Either/Right Bool/True)  (Either/Left Bool/True)   = 3
