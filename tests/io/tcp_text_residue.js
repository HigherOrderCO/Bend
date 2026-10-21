// bend-ids 1
// CID :Host.small_send BEND_CID_Host_small_send
// bend-ids end
// Keep the real non-blocking send partial before the reader starts.
function small_send(socket) {
  const sys = io_sys();
  const size = new Int32Array([1024]);
  if (sys.setsockopt(socket, sys.mac ? 0xffff : 1,
    sys.mac ? 0x1001 : 7, sys.ptr(size), 4) !== 0) {
    throw "test could not set SO_SNDBUF";
  }
  return socket;
}

io_eff(BEND_CID_Host_small_send, small_send);
