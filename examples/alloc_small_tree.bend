type Bool: 
  true
  false

type Tree:
  Node { ~lft, ~rgt }
  Leaf { val }

def bool_and(p, q):
  match p:
    case Bool/true:
      return q
    case Bool/false:
      return Bool/false

def alloc(n):
  switch n:
    case 0:
      return Tree/Leaf { val: 0 }
    case _:
      return Tree/Node { lft: alloc(n-1), rgt: alloc(n-1) } 

def destroy(t):
  match t:
    case Tree/Leaf:
      return Bool/true
    case Tree/Node:
      return bool_and(destroy(t.lft), destroy(t.rgt))

def main():
  return destroy(alloc(2 ^4))
