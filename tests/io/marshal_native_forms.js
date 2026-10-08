// bend-ids 1
// CID #Some BEND_CID_Some
// CID #EQ BEND_CID_EQ
// CID :five BEND_CID_five
// CID :zero BEND_CID_zero
// CID :wrap BEND_CID_wrap
// CID :dub BEND_CID_dub
// CID :flip BEND_CID_flip
// CID :ord BEND_CID_ord
// bend-ids end
function five() {
  return 5;
}
function zero() {
  return 0;
}
function wrap() {
  return {$: BEND_CID_Some, value: 9};
}
function dub(n) {
  return n * 2;
}
function flip(b) {
  return !b;
}
function ord() {
  return {$: BEND_CID_EQ};
}

io_eff(BEND_CID_five, five);
io_eff(BEND_CID_zero, zero);
io_eff(BEND_CID_wrap, wrap);
io_eff(BEND_CID_dub, dub);
io_eff(BEND_CID_flip, flip);
io_eff(BEND_CID_ord, ord);
