// bend-ids 1
// CID #Empty BEND_CID_Empty
// CID #Leaf BEND_CID_Leaf
// CID #Node BEND_CID_Node
// CID #End BEND_CID_End
// CID #Cell BEND_CID_Cell
// CID :tree.make BEND_CID_tree_make
// CID :chain.make BEND_CID_chain_make
// bend-ids end
// Tree
// ====

// A balanced tree of the depth asked, its leaves 1.. left to right, every
// odd one Empty, as the C lays it.
function tree_make(depth) {
  let next = 0;
  const at = (d) => {
    if (d === 0) {
      next += 1;
      return next % 2 === 1 ? { $: BEND_CID_Empty } : { $: BEND_CID_Leaf, v: next };
    }
    const l = at(d - 1);
    return { $: BEND_CID_Node, l, r: at(d - 1) };
  };
  return at(depth);
}

// Chain
// =====

function chain_make(n) {
  let c = { $: BEND_CID_End };
  for (let i = 0; i < n; i += 1) {
    c = { $: BEND_CID_Cell, v: i, next: c };
  }
  return c;
}

io_eff(BEND_CID_tree_make, tree_make);
io_eff(BEND_CID_chain_make, chain_make);
