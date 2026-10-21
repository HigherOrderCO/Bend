// bend-ids 1
// CID :answer BEND_CID_answer
// bend-ids end
// A backslash continues a C comment, even before CRLF or after spaces: \
CID(NotASymbol)
// so this quoted id stays in the comment too: \  
FID(nothing)
Term answer_run(Env e, Term* f, IoWork* w) {
  return (Term)42;
}

static void __attribute__((constructor)) answer_use(void) {
  io_eff(BEND_C\
ID_answer, answer_run);
}
