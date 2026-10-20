function receive(c, k) {
  io_park_on(undefined, false, k, () => {
    $0eff[CID(Chan.send)](c, 7, () => {
      throw "the send parked";
    });
  }, io_until(1));
  const answer = $0eff[CID(Chan.try_recv)](c, 10, k);
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 20);
  return answer;
}
io_eff(CID(Host.receive), receive);
