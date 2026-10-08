// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :Socket.close BEND_CID_Socket_close
// bend-ids end
// Socket
// ======

Term socket_close_run(Env e, Term* f, IoWork* w) {
  close((int)io_hand_v(f[0]));
  return term_pak(BEND_CID_Unit, 0);
}

static void __attribute__((constructor)) socket_close_use(void) {
  io_eff(BEND_CID_Socket_close, socket_close_run);
}
