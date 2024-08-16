# Test issue #674
type Ctr:
  Foo { x, y }

def Ctr/wrap(x):
  return Ctr/Foo { x: x, y: 0 }
