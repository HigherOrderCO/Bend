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
// odd one Empty, laid as the program lays one: an Empty or a Leaf packed
// in its word, a Node a node whose fields io_node seals as the program
// would.
static Term tree_make_at(Env e, u32 depth, u32* next) {
  if (depth == 0) {
    *next += 1;
    return *next % 2 == 1 ? term_pak(BEND_CID_Empty, 0) : term_pak(BEND_CID_Leaf, *next);
  }
  Term l = tree_make_at(e, depth - 1, next);
  Term r = tree_make_at(e, depth - 1, next);
  return io_node(e, BEND_CID_Node, l, r);
}

Term tree_make_run(Env e, Term* f, IoWork* w) {
  u32 next = 0;
  return tree_make_at(e, (u32)f[0], &next);
}

// Chain
// =====

Term chain_make_run(Env e, Term* f, IoWork* w) {
  Term c = term_pak(BEND_CID_End, 0);
  for (u32 i = 0; i < (u32)f[0]; i += 1) {
    c = io_node(e, BEND_CID_Cell, i, c);
  }
  return c;
}

static void __attribute__((constructor)) foreign_types_use(void) {
  io_eff(BEND_CID_tree_make, tree_make_run);
  io_eff(BEND_CID_chain_make, chain_make_run);
}
