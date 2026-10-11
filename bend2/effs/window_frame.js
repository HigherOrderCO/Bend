// Window
// ======

function window_frame(window, image) {
  return io_tup(window, image, { $: CID(Nil) });
}

io_eff(CID(Window.frame), window_frame);
