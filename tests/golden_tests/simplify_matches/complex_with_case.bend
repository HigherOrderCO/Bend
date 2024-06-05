type Tree_ = (Node lt rt rd ld) | (Leaf val)

(map) =
  λarg1 λarg2 use tree = arg2;
  use f = arg1;
  match tree with f {
    Tree_/Node: (Tree_/Node (map f tree.lt) (map f tree.rt) (map f tree.rd) (map f tree.ld));
    Tree_/Leaf: (Tree_/Leaf (f tree.val));
  }

main = map
