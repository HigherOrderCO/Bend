// bend-ids 1
// CID #None BEND_CID_None
// CID #Con BEND_CID_Con
// CID #Nil BEND_CID_Nil
// CID :peek BEND_CID_peek
// CID :list BEND_CID_list
// bend-ids end
Term cid_capture_peek(Env e, Term* f, IoWork* w) {
  return term_pak(BEND_CID_None, 0);
}

Term cid_capture_list(Env e, Term* f, IoWork* w) {
  return io_node(e, BEND_CID_Con, (Term)7, term_pak(BEND_CID_Nil, 0));
}

static void __attribute__((constructor)) cid_capture_use(void) {
  io_eff(BEND_CID_peek, cid_capture_peek);
  io_eff(BEND_CID_list, cid_capture_list);
}
