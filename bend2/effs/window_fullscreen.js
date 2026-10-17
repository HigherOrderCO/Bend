// Window
// ======

function window_fullscreen(window, on) {
  const code = process.platform === "darwin" ? 45 : 95;
  const text = "Window.fullscreen: build a native binary and run it from a desktop session";
  return io_tup(window, { $: CID(Fail), error: io_tup(code, text) });
}

io_eff(CID(Window.fullscreen), window_fullscreen);
