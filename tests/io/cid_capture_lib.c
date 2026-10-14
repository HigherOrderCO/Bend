Term cid_capture_peek(Env e, Term* f, IoWork* w) {
  return term_pak(CID(None), 0);
}

Term cid_capture_list(Env e, Term* f, IoWork* w) {
  return io_node(e, CID(Con), (Term)7, term_pak(CID(Nil), 0));
}

static void __attribute__((constructor)) cid_capture_use(void) {
  io_eff(CID(peek), cid_capture_peek);
  io_eff(CID(list), cid_capture_list);
}
