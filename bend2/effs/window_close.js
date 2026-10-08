// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :Window.close BEND_CID_Window_close
// bend-ids end
// Window
// ======

function window_close(window) {
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_Window_close, window_close);
