// IO
// ==

function io_spawn(act) {
  io_push(act, (x) => ({ $: CID(Emit), value: x }), true);
  return { $: CID(Unit) };
}

io_eff(CID(IO.spawn), io_spawn);
