// bend-ids 1
// CID :dispose BEND_CID_dispose
// bend-ids end
io_eff(BEND_CID_dispose, (f) => {
  if (typeof f !== "function") {
    throw new Error("expected a retained closure");
  }
  return 5;
});
