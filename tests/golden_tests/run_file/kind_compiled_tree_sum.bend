#WARNING: unsolved metas.
#WARNING: unsolved metas.
_Char = 0
_List = λ_T 0
_List.cons = λ_head λ_tail λ_P λ_cons λ_nil ((_cons _head) _tail)
_List.nil = λ_P λ_cons λ_nil _nil
_Nat = 0
_Nat.succ = λ_n λ_P λ_succ λ_zero (_succ _n)
_Nat.zero = λ_P λ_succ λ_zero _zero
_String = (_List _Char)
_String.cons = λ_head λ_tail λ_P λ_cons λ_nil ((_cons _head) _tail)
_String.nil = λ_P λ_cons λ_nil _nil
_Tree = λ_A 0

_Tree.fold =
  λ_bm λ_nd λ_lf
  let _bm.P = 0;
  let _bm.node = λ_bm.val λ_bm.lft λ_bm.rgt λ_nd λ_lf (((_nd _bm.val) (((_Tree.fold _bm.lft) _nd) _lf)) (((_Tree.fold _bm.rgt) _nd) _lf));
  let _bm.leaf = λ_nd λ_lf _lf;
  ((((let _bm = _bm (_bm _bm.P) _bm.node) _bm.leaf) _nd) _lf)

_Tree.gen = λ_n λ_x switch _n = _n {
  0: _Tree.leaf
  _: let _n-1 = _n-1 (
    _Tree.node
    _x
    (_Tree.gen _n-1 (+ (* _x 2) 1))
    (_Tree.gen _n-1 (+ (* _x 2) 2))
  )
}

_Tree.leaf = λ_P λ_node λ_leaf _leaf

_Tree.node = λ_val λ_lft λ_rgt λ_P λ_node λ_leaf (((_node _val) _lft) _rgt)

_Tree.sum = λ_x
  let _x.P = 0
  let _x.node = λ_x.val λ_x.lft λ_x.rgt (+ _x.val (+ _x.lft _x.rgt))
  let _x.leaf = 0
  ((let _x = _x (_Tree.fold _x) _x.node) _x.leaf)

main = (_Tree.sum ((_Tree.gen 16) 0))

