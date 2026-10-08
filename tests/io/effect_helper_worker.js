// bend-ids 1
// CID :A.run BEND_CID_A_run
// bend-ids end
let asked = false;

function a_run(ms) {
  a_run_helper();
  const value = asked ? 11 : 99;
  asked = false;
  return value;
}

function a_run_helper() {
  asked = true;
}

io_eff(BEND_CID_A_run, a_run);
