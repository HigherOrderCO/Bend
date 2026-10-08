// bend-ids 1
// CID #Fail BEND_CID_Fail
// CID :Window.open BEND_CID_Window_open
// bend-ids end
// Window
// ======

function window_open(title, width, height) {
  const code = process.platform === "darwin" ? 45 : 95;
  const text = "Window.open: no display (build a native binary with bend <file> -o <out> and run it from a desktop session)";
  return { $: BEND_CID_Fail, error: io_tup(code, text) };
}

io_eff(BEND_CID_Window_open, window_open);
