// bend-ids 1
// CID :Chan.recv BEND_CID_Chan_recv
// CID :Chan.close BEND_CID_Chan_close
// CID :Chan.send BEND_CID_Chan_send
// CID #Unit BEND_CID_Unit
// CID :Host.channel BEND_CID_Host_channel
// bend-ids end
function host_channel(c, mode) {
  const code = io_run(() => (cont) => ({
    $: mode === 2 ? BEND_CID_Chan_recv : mode === 3 ? BEND_CID_Chan_close : BEND_CID_Chan_send,
    args: mode === 2 || mode === 3 ? [c] : [c, 7],
    kont: cont,
  }));
  if (code !== 0) throw "nested channel IO failed";
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_Host_channel, host_channel);
