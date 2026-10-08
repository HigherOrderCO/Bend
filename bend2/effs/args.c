// bend-ids 1
// CID #Nil BEND_CID_Nil
// CID #Con BEND_CID_Con
// CID :IO.args BEND_CID_IO_args
// bend-ids end
// IO
// ==

Term io_args_run(Env e, Term* f, IoWork* w) {
  Term xs = term_pak(BEND_CID_Nil, 0);
  for (int i = io_argc; i > 0; i -= 1) {
    const char* a = io_argv[i - 1];
    xs = io_node(e, BEND_CID_Con, io_str(e, a, strlen(a)), xs);
  }
  return xs;
}

static void __attribute__((constructor)) io_args_use(void) {
  io_eff(BEND_CID_IO_args, io_args_run);
}
