// bend-ids 1
// CID :Window.set_title BEND_CID_Window_set_title
// bend-ids end
// Window
// ======

function window_set_title(window, title) {
  return window;
}

io_eff(BEND_CID_Window_set_title, window_set_title);
