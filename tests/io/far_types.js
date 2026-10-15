// Far
// ===

// The tags are this module's file name, far_types, from wherever the
// program importing it sits.
function far_make(depth) {
  let next = 0;
  const at = (d) => {
    if (d === 0) {
      next += 1;
      return { $: CID(Near), v: next };
    }
    const l = at(d - 1);
    return { $: CID(Deep), l, r: at(d - 1) };
  };
  return at(depth);
}

io_eff(CID(far.make), far_make);
