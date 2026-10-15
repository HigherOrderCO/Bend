// A balanced tree of the depth asked, its leaves 1.. left to right, every
// odd one Empty, as the C lays it.
function shared_make(depth) {
  let next = 0;
  const at = (d) => {
    if (d === 0) {
      next += 1;
      return next % 2 === 1 ? { $: CID(Empty) } : { $: CID(Leaf), v: next };
    }
    const l = at(d - 1);
    return { $: CID(Node), l, r: at(d - 1) };
  };
  return at(depth);
}

io_eff(CID(shared.make), shared_make);
