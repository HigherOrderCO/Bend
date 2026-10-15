io_eff(CID(flags), (par, seq) => {
  if (typeof par !== "function" || typeof seq !== "function") {
    throw new Error("expected retained functions");
  }
  return { $: CID(Unit) };
});
