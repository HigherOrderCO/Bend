// bend-ids 1
// CID :main BEND_CID_main
// bend-ids end
function main() {
  require("fs").writeSync(1, "EFF RAN\n");
  return 7;
}

io_eff(BEND_CID_main, main);
