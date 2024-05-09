enum Bool:
  Bool/true()
  Bool/false()

enum Tree:
  Tree/node(~lft, ~rgt)
  Tree/leaf(val)

def and(a, b):
  match a:
    Bool/true:
      return b;
    Bool/false:
      return Bool/false;

def all(tree):
  fold tree:
    Tree/node:
      return and(tree.lft tree.rgt);
    Tree/leaf:
      return tree.val;

def gen(n):
  switch n:
    0:
      return Tree/leaf(Bool/true);
    _:
      return Tree/node(gen(n-1), gen(n-1)); 

def main():
  return all(gen(8));

