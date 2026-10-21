// bend-ids 1
// CID :Host.small_send BEND_CID_Host_small_send
// bend-ids end
// Keep the real non-blocking send partial before the reader starts.
Term small_send_run(Env e, Term* f, IoWork* w) {
  int size = 1024;
  if (setsockopt((int)io_hand_v(f[0]), SOL_SOCKET, SO_SNDBUF,
      &size, sizeof(size)) != 0) {
    err_fail("test could not set SO_SNDBUF");
  }
  return f[0];
}

static void __attribute__((constructor)) small_send_use(void) {
  io_eff(BEND_CID_Host_small_send, small_send_run);
}
