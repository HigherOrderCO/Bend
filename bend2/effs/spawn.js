// bend-ids 1
// CID #Emit BEND_CID_Emit
// CID #Unit BEND_CID_Unit
// CID :IO.spawn BEND_CID_IO_spawn
// bend-ids end
// IO
// ==

function io_spawn(act) {
  io_push(act, (x) => ({ $: BEND_CID_Emit, value: x }), true);
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_IO_spawn, io_spawn);
