// A backslash continues a C comment, even before CRLF or after spaces: \
CID(NotASymbol)
// so this quoted id stays in the comment too: \  
FID(nothing)
Term answer_run(Env e, Term* f, IoWork* w) {
  return (Term)42;
}

static void __attribute__((constructor)) answer_use(void) {
  io_eff(C\
ID(answer), answer_run);
}
