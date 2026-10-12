// bend-ids 1
// CID :keep BEND_CID_keep
// bend-ids end
io_eff(BEND_CID_keep, (f) => {
  if (typeof f !== "function") {
    throw new Error("expected a retained closure");
  }
  return 1;
});
