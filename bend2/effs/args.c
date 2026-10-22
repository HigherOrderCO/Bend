// IO
// ==

// str makes an argument's value from its bytes: io_str's String, or
// io_list's bytes for IO.args_bytes.
static Term io_args_with(Env e, Term (*str)(Env e, const char* p, u64 n)) {
  Term xs = term_pak(CID(Nil), 0);
  for (int i = io_argc; i > 0; i -= 1) {
    const char* a = io_argv[i - 1];
    xs = io_node(e, CID(Con), str(e, a, strlen(a)), xs);
  }
  return xs;
}

#ifdef CID(IO.args)

Term io_args_run(Env e, Term* f, IoWork* w) {
  return io_args_with(e, io_str);
}

static void __attribute__((constructor)) io_args_use(void) {
  io_eff(CID(IO.args), io_args_run);
}

#endif

#ifdef CID(IO.args_bytes)

Term io_args_bytes_run(Env e, Term* f, IoWork* w) {
  return io_args_with(e, io_list);
}

static void __attribute__((constructor)) io_args_bytes_use(void) {
  io_eff(CID(IO.args_bytes), io_args_bytes_run);
}

#endif
