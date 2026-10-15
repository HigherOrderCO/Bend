// Tag
// ===

// The tags are this module's spelling, from wherever the program sits.
function tag_off() {
  return { $: CID(Off) };
}
function tag_on() {
  return { $: CID(On), n: 3 };
}

io_eff(CID(tag.off), tag_off);
io_eff(CID(tag.on), tag_on);
