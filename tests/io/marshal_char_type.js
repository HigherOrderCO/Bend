// bend-ids 1
// CID #Some BEND_CID_Some
// CID #Tuple BEND_CID_Tuple
// CID :getc BEND_CID_getc
// CID :pick BEND_CID_pick
// CID :stamp BEND_CID_stamp
// bend-ids end
function getc() {
  return "A";
}
function pick() {
  return {$: BEND_CID_Some, value: "Z"};
}
function stamp() {
  return {$: BEND_CID_Tuple, fst: "q", snd: "ok"};
}

io_eff(BEND_CID_getc, getc);
io_eff(BEND_CID_pick, pick);
io_eff(BEND_CID_stamp, stamp);
