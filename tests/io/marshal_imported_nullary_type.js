// bend-ids 1
// CID #Off BEND_CID_Off
// CID #On BEND_CID_On
// CID :tag.off BEND_CID_tag_off
// CID :tag.on BEND_CID_tag_on
// bend-ids end
// Tag
// ===

// The tags are this module's spelling, from wherever the program sits.
function tag_off() {
  return { $: BEND_CID_Off };
}
function tag_on() {
  return { $: BEND_CID_On, n: 3 };
}

io_eff(BEND_CID_tag_off, tag_off);
io_eff(BEND_CID_tag_on, tag_on);
