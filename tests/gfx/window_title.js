function window_title(window) {
  throw new Error("Window.open returned a window on the JS backend");
}

io_eff(CID(title), window_title);
