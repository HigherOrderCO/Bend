function probe() {
  // A path ending in backslash: C:\
  return { $: CID(Some), value: 42 };
}
io_eff(CID(probe), probe);
