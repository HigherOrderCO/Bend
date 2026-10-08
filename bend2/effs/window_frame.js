// bend-ids 1
// CID #Nil BEND_CID_Nil
// CID :Window.frame BEND_CID_Window_frame
// bend-ids end
// Window
// ======

function window_frame(window, image) {
  return io_tup(window, image, { $: BEND_CID_Nil });
}

io_eff(BEND_CID_Window_frame, window_frame);
