function exchange(tree) {
  const input = 123456789n;
  let node = tree;
  let depth = 0;
  while (node.$ === CID(Node)) {
    if (node.n !== input || node.kids.length !== 1) {
      throw new Error("invalid Node at depth " + depth);
    }
    node = node.kids[0];
    depth += 1;
  }
  if (node.$ !== CID(Leaf) || node.n !== input || depth !== 20000) {
    throw new Error("invalid Leaf at depth " + depth);
  }
  node.n = 987654321n;
  return tree;
}

io_eff(CID(exchange), exchange);
