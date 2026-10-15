io_eff(CID(keep), (f) => {
  if (typeof f !== "function") {
    throw new Error("expected a retained closure");
  }
  return 1;
});
