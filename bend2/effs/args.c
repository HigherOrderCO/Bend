// IO
// ==

Term io_args_run(Env e, Term* f, IoWork* w) {
  Term xs = term_pak(CID(Nil), 0);
  for (int i = io_argc; i > 0; i -= 1) {
    const char* a = io_argv[i - 1];
    xs = io_node(e, CID(Con), io_str(e, a, strlen(a)), xs);
  }
  return xs;
}

static void __attribute__((constructor)) io_args_use(void) {
  io_eff(CID(IO.args), io_args_run);
}
