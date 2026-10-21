// bend-ids 1
// CID :Chan.send BEND_CID_Chan_send
// CID :Chan.try_recv BEND_CID_Chan_try_recv
// CID :Host.receive BEND_CID_Host_receive
// bend-ids end
function receive(c, k) {
  io_park_on(undefined, false, k, () => {
    $0eff[BEND_CID_Chan_send](c, 7, () => {
      throw "the send parked";
    });
  }, io_until(1));
  const answer = $0eff[BEND_CID_Chan_try_recv](c, 10, k);
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 20);
  return answer;
}
io_eff(BEND_CID_Host_receive, receive);
