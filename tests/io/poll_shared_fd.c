static int shared_fd_pair[2];
static u32 shared_fd_seen;
static IoWork* shared_fd_root;

static Term shared_fd_more(Env e, IoWork* w) {
  char byte;
  if (read((int)w->word, &byte, 1) != 1 || byte != 'a' + shared_fd_seen)
    err_fail("shared fd resumed out of order");
  shared_fd_seen++;
  if (w == shared_fd_root) {
    if (shared_fd_seen == 1)
      return io_wait_on(w, shared_fd_pair[0], POLLIN, 0, shared_fd_more);
    if (shared_fd_seen != 3) err_fail("shared fd lost its second waiter");
    close(shared_fd_pair[0]);
    close(shared_fd_pair[1]);
    return shared_fd_seen;
  }
  if (shared_fd_seen != 2) err_fail("shared fd displaced its second waiter");
  return term_pak(CID(Unit), 0);
}

Term shared_fd_run(Env e, Term* f, IoWork* w) {
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, shared_fd_pair)
    || write(shared_fd_pair[1], "abc", 3) != 3)
    err_fail("shared fd socket setup failed");
  shared_fd_root = w;
  io_wait_on(w, shared_fd_pair[0], POLLIN, 0, shared_fd_more);
  IoWork* sibling = io_mem(calloc(1, sizeof *sibling));
  sibling->cont = term_clo(FID(IO~emit), 0);
  io_live++;
  io_wait_on(sibling, shared_fd_pair[0], POLLIN, 0, shared_fd_more);
  return IO_PARK;
}

static void __attribute__((constructor)) shared_fd_use(void) {
  io_eff(CID(Shared.read), shared_fd_run);
}
