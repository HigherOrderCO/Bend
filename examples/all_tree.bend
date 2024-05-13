type Bool:
  true
  false

type Tree:
  node { ~lft, ~rgt }
  leaf { val }

def and(a, b):
  match a:
    case Bool/true:
      return b;
    case Bool/false:
      return Bool/false;

def all(tree):
  fold tree:
    case Tree/node:
      return and(tree.lft tree.rgt);
    case Tree/leaf:
      return tree.val;

def gen(n):
  switch n:
    case 0:
      return Tree/leaf(Bool/true);
    case _:
      return Tree/node { lft: gen(n-1), rgt: gen(n-1) };

def main():
  return all(gen(8));
