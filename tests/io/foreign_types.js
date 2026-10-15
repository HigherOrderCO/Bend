// Tree
// ====

// A balanced tree of the depth asked, its leaves 1.. left to right, every
// odd one Empty, as the C lays it.
function tree_make(depth) {
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

// Chain
// =====

function chain_make(n) {
  let c = { $: CID(End) };
  for (let i = 0; i < n; i += 1) {
    c = { $: CID(Cell), v: i, next: c };
  }
  return c;
}

io_eff(CID(tree.make), tree_make);
io_eff(CID(chain.make), chain_make);
