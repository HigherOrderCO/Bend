// A host whose WebCrypto answers by throwing, which is the shape the
// report gave. The patch puts the original getRandomValues back before it
// throws, so the failure is scoped to the one call: the gate interprets a
// whole shard in one process, and shards run largest first, so a patch
// left installed here would take random_u32.bend down with it and the
// gate would pass and fail as the shards happened to split.
function crypto_broken_run() {
  const host = globalThis.crypto;
  const original = host.getRandomValues;
  const failing = () => {
    Object.defineProperty(host, "getRandomValues",
      { value: original, configurable: true, writable: true });
    throw new Error("entropy source failed");
  };
  Object.defineProperty(host, "getRandomValues",
    { value: failing, configurable: true, writable: true });
  return { $: CID(Unit) };
}

io_eff(CID(crypto_broken), crypto_broken_run);
