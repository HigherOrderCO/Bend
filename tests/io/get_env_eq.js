// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :put_eq BEND_CID_put_eq
// bend-ids end
// Set a variable whose value holds a "=", for get_env to look past.
function put_eq() {
  process.env.BEND_ENV_EQ = "x=y";
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_put_eq, put_eq);
