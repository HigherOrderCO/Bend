// bend-ids 1
// CID #MkBox BEND_CID_MkBox
// CID :box_val BEND_CID_box_val
// CID :box_new BEND_CID_box_new
// bend-ids end
function box_val(b) {
  return b.val;
}
function box_new() {
  return {$: BEND_CID_MkBox, val: 99};
}

io_eff(BEND_CID_box_val, box_val);
io_eff(BEND_CID_box_new, box_new);
