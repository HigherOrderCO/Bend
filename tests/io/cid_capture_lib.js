// bend-ids 1
// CID #None BEND_CID_None
// CID #Con BEND_CID_Con
// CID #Nil BEND_CID_Nil
// CID :peek BEND_CID_peek
// CID :list BEND_CID_list
// bend-ids end
function cid_capture_peek() {
  return { $: BEND_CID_None };
}

function cid_capture_list() {
  return { $: BEND_CID_Con, head: 7, tail: { $: BEND_CID_Nil } };
}

io_eff(BEND_CID_peek, cid_capture_peek);
io_eff(BEND_CID_list, cid_capture_list);
