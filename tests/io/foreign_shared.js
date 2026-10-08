// bend-ids 1
// CID #Empty BEND_CID_Empty
// CID #Leaf BEND_CID_Leaf
// CID #Node BEND_CID_Node
// CID :shared.make BEND_CID_shared_make
// bend-ids end
// A balanced tree of the depth asked, its leaves 1.. left to right, every
// odd one Empty, as the C lays it.
function shared_make(depth) {
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

io_eff(BEND_CID_shared_make, shared_make);
