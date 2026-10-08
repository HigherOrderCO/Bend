// bend-ids 1
// CID #Near BEND_CID_Near
// CID #Deep BEND_CID_Deep
// CID :far.make BEND_CID_far_make
// bend-ids end
// Far
// ===

// The tags are this module's file name, far_types, from wherever the
// program importing it sits.
function far_make(depth) {
  let next = 0;
  const at = (d) => {
    if (d === 0) {
      next += 1;
      return { $: BEND_CID_Near, v: next };
    }
    const l = at(d - 1);
    return { $: BEND_CID_Deep, l, r: at(d - 1) };
  };
  return at(depth);
}

io_eff(BEND_CID_far_make, far_make);
