function box_val(b) {
  return b.val;
}
function box_new() {
  return {$: CID(MkBox), val: 99};
}

io_eff(CID(box_val), box_val);
io_eff(CID(box_new), box_new);
