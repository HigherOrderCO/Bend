Term title_run(Env e, Term* f, IoWork* w) {
#ifdef __OBJC__
  NSWindow* win = (__bridge NSWindow*)(void*)(uintptr_t)io_hand_v(f[0]);
  const char* text = win.title.UTF8String;
  return io_tup(e, f[0], io_str(e, text, strlen(text)));
#elif defined(__linux__)
  BendWin* win = (BendWin*)(uintptr_t)io_hand_v(f[0]);
  char* text = NULL;
  XFetchName(win->dpy, win->win, &text);
  Term got = io_str(e, text == NULL ? "" : text,
    text == NULL ? 0 : strlen(text));
  if (text != NULL) {
    XFree(text);
  }
  return io_tup(e, f[0], got);
#else
  return io_tup(e, f[0], io_str(e, "", 0));
#endif
}

static void __attribute__((constructor)) title_use(void) {
  io_eff(CID(title), title_run);
}
