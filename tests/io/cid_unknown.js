// bend-ids 1
// CID #Ful BEND_CID_Ful
// CID :mk BEND_CID_mk
// bend-ids end
function mk() {
  return { $: BEND_CID_Ful, v: 1 };
}

io_eff(BEND_CID_mk, mk);
