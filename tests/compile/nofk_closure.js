// bend-ids 1
// CID :flags BEND_CID_flags
// CID #Unit BEND_CID_Unit
// bend-ids end
io_eff(BEND_CID_flags, (par, seq) => {
  if (typeof par !== "function" || typeof seq !== "function") {
    throw new Error("expected retained functions");
  }
  return { $: BEND_CID_Unit };
});
