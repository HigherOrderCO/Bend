// bend-ids 1
// CID #Node BEND_CID_Node
// CID #Leaf BEND_CID_Leaf
// CID :exchange BEND_CID_exchange
// bend-ids end
function exchange(tree) {
  const input = 123456789;
  let node = tree;
  let depth = 0;
  while (node.$ === BEND_CID_Node) {
    if (node.n !== input || node.kids.length !== 1) {
      throw new Error("invalid Node at depth " + depth);
    }
    node = node.kids[0];
    depth += 1;
  }
  if (node.$ !== BEND_CID_Leaf || node.n !== input || depth !== 20000) {
    throw new Error("invalid Leaf at depth " + depth);
  }
  node.n = 987654321;
  return tree;
}

io_eff(BEND_CID_exchange, exchange);
