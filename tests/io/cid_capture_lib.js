function cid_capture_peek() {
  return { $: CID(None) };
}

function cid_capture_list() {
  return { $: CID(Con), head: 7, tail: { $: CID(Nil) } };
}

io_eff(CID(peek), cid_capture_peek);
io_eff(CID(list), cid_capture_list);
