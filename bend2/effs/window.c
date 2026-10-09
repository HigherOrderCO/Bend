// Window
// ======

// All window effects share this source; each entry is present only when
// its effect is reachable.

#ifdef __OBJC__

#import <AppKit/AppKit.h>
#import <QuartzCore/QuartzCore.h>

#elif defined(__linux__)

// The X11 window: its own connection and UTF-8 input context, the
// frame's image, the queued event words, whether it holds the pointer,
// and the display's refresh period in ns.
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <dlfcn.h>
#include <limits.h>

typedef struct {
  Display* dpy;
  Window   win;
  XIM      im;
  XIC      ic;
  Atom     del;
  XImage*  img;
  u32      n;
  u32      cap;
  u32*     evs;
  u32      grab;
  u64      period;
} BendWin;

#endif

#ifdef CID(Window.open)

#ifdef __OBJC__

@interface BendView : NSView <NSWindowDelegate> {
  @public
  NSMutableData* evs;
  u64            flags;
  BOOL           grab;
}
@end

@implementation BendView

- (CALayer*)makeBackingLayer {
  return [CAMetalLayer layer];
}

- (BOOL)acceptsFirstResponder {
  return YES;
}

- (BOOL)acceptsFirstMouse:(NSEvent*)ev {
  return YES;
}

- (BOOL)isFlipped {
  return YES;
}

- (void)push:(u32)cid a:(u32)a b:(u32)b c:(u32)c d:(u32)d {
  u32 ev[5] = { cid, a, b, c, d };
  [evs appendBytes:ev length:sizeof ev];
}

- (void)key:(NSEvent*)ev down:(BOOL)down {
  NSString* s = [ev.charactersIgnoringModifiers lowercaseString];
  u32 code = s.length > 0 ? [s characterAtIndex:0] : 65536 + ev.keyCode;
  [self push:CID(Key) a:code b:down c:0 d:0];
}

- (void)keyDown:(NSEvent*)ev {
  [self key:ev down:YES];
  NSString* s = ev.characters;
  NSUInteger n = [s lengthOfBytesUsingEncoding:NSUTF8StringEncoding];
  if (n > UINT32_MAX) err_fail("Window: text event too large");
  if (n > 0) {
    u32 head[2] = { CID(Text), (u32)n };
    u32 zero = 0;
    [evs appendBytes:head length:sizeof head];
    [evs appendBytes:s.UTF8String length:n];
    [evs appendBytes:&zero length:(4 - n % 4) % 4];
  }
}

- (void)keyUp:(NSEvent*)ev {
  [self key:ev down:NO];
}

- (void)flagsChanged:(NSEvent*)ev {
  u64 now = ev.modifierFlags;
  [self push:CID(Key) a:65536 + ev.keyCode b:(now & ~flags) != 0 c:0
    d:0];
  flags = now;
}

- (NSPoint)at:(NSEvent*)ev {
  CGSize  size = ((CAMetalLayer*)self.layer).drawableSize;
  NSPoint p    = [self convertPoint:ev.locationInWindow fromView:nil];
  return NSMakePoint(fmax(0, fmin(floor(p.x), size.width - 1)),
    fmax(0, fmin(floor(p.y), size.height - 1)));
}

- (void)mouse:(NSEvent*)ev down:(BOOL)down {
  NSPoint p = [self at:ev];
  [self push:CID(Mouse) a:p.x b:p.y c:(u32)ev.buttonNumber d:down];
}

- (void)move:(NSEvent*)ev {
  NSPoint p = [self at:ev];
  if (grab) {
    [self push:CID(Look) a:f32_rewrap(ev.deltaX) b:f32_rewrap(ev.deltaY)
      c:0 d:0];
  } else {
    [self push:CID(Move) a:p.x b:p.y c:0 d:0];
  }
}

- (void)scrollWheel:(NSEvent*)ev {
  NSPoint p = [self at:ev];
  [self push:CID(Scroll) a:p.x b:p.y c:f32_rewrap(ev.scrollingDeltaX)
    d:f32_rewrap(ev.scrollingDeltaY)];
}

// Window.grab sets this by key: the cursor is hidden and held at the
// window's centre, and only a focused window takes it.
- (void)setGrab:(BOOL)on {
  if (on == grab || (on && !self.window.isKeyWindow)) {
    return;
  }
  grab = on;
  if (on) {
    NSRect r = [self.window convertRectToScreen:self.frame];
    CGWarpMouseCursorPosition(CGPointMake(NSMidX(r),
      NSMaxY(NSScreen.screens[0].frame) - NSMidY(r)));
    [NSCursor hide];
  } else {
    [NSCursor unhide];
  }
  CGAssociateMouseAndMouseCursorPosition(!on);
}

- (void)windowDidResignKey:(NSNotification*)note {
  [self setGrab:NO];
}

- (void)mouseDown:(NSEvent*)ev {
  [self mouse:ev down:YES];
}

- (void)mouseUp:(NSEvent*)ev {
  [self mouse:ev down:NO];
}

- (void)rightMouseDown:(NSEvent*)ev {
  [self mouse:ev down:YES];
}

- (void)rightMouseUp:(NSEvent*)ev {
  [self mouse:ev down:NO];
}

- (void)otherMouseDown:(NSEvent*)ev {
  [self mouse:ev down:YES];
}

- (void)otherMouseUp:(NSEvent*)ev {
  [self mouse:ev down:NO];
}

- (void)mouseMoved:(NSEvent*)ev {
  [self move:ev];
}

- (void)mouseDragged:(NSEvent*)ev {
  [self move:ev];
}

- (void)rightMouseDragged:(NSEvent*)ev {
  [self move:ev];
}

- (void)otherMouseDragged:(NSEvent*)ev {
  [self move:ev];
}

- (BOOL)windowShouldClose:(NSWindow*)sender {
  [self push:CID(Close) a:0 b:0 c:0 d:0];
  return NO;
}

@end

static id<MTLDevice> window_dev;

static u32 window_make(const char* title, u32 w, u32 h, intptr_t* out,
  const char** why) {
  if (w < 1 || h < 1 || w > 16384 || h > 16384) {
    return EINVAL;
  }
  if (NSScreen.screens.count == 0) {
    *why = "Window.open: no display (build a native binary with bend <file> -o <out> and run it from a desktop session)";
    return ENOTSUP;
  }
  if (window_dev == nil) {
    window_dev = gpu_buf != nil ? gpu_dev : MTLCreateSystemDefaultDevice();
  }
  if (window_dev == nil) {
    *why = "Window.open: no Metal device";
    return ENXIO;
  }
  if (NSApp == nil) {
    [NSApplication sharedApplication];
    NSApp.activationPolicy = NSApplicationActivationPolicyRegular;
    [NSApp finishLaunching];
  }
  @autoreleasepool {
    NSWindow* win = [[NSWindow alloc]
      initWithContentRect:NSMakeRect(0, 0, 1, 1)
      styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskClosable
        | NSWindowStyleMaskMiniaturizable
      backing:NSBackingStoreBuffered defer:NO];
    win.releasedWhenClosed = NO;
    win.acceptsMouseMovedEvents = YES;
    win.title = [NSString stringWithCString:title
      encoding:NSISOLatin1StringEncoding];
    [win setContentSize:NSMakeSize(w, h)];
    BendView* view = [[BendView alloc] initWithFrame:win.contentLayoutRect];
    view->evs   = [NSMutableData new];
    view->flags = NSEvent.modifierFlags;
    view.wantsLayer = YES;
    CAMetalLayer* layer = (CAMetalLayer*)view.layer;
    layer.device = window_dev;
    layer.pixelFormat = MTLPixelFormatBGRA8Unorm;
    layer.framebufferOnly = NO;
    layer.drawableSize = CGSizeMake(w, h);
    layer.displaySyncEnabled = YES;
    layer.maximumDrawableCount = 2;
    win.contentView = view;
    win.delegate = view;
    [win makeFirstResponder:view];
    [win center];
    [win makeKeyAndOrderFront:nil];
    [NSApp activateIgnoringOtherApps:YES];
    *out = (intptr_t)CFBridgingRetain(win);
  }
  return 0;
}

#elif defined(__linux__)

// The display's refresh period in ns: the rate RandR reports, through a
// libXrandr loaded at run time (a build needs no libxrandr-dev), else
// 60 Hz, as under a display that reports none or has no RandR (asked
// first: libXrandr faults on a server without it). The library stays
// loaded: it hooks the connection's close.
static u64 window_period(Display* dpy) {
  int op, ev, er;
  void* xrr = XQueryExtension(dpy, "RANDR", &op, &ev, &er)
    ? dlopen("libXrandr.so.2", RTLD_LAZY | RTLD_LOCAL) : NULL;
  if (xrr == NULL) {
    return 16666667;
  }
  void* (*info)(Display*, Window) =
    (void* (*)(Display*, Window))dlsym(xrr, "XRRGetScreenInfo");
  short (*rate)(void*) = (short (*)(void*))dlsym(xrr, "XRRConfigCurrentRate");
  void (*done)(void*) = (void (*)(void*))dlsym(xrr, "XRRFreeScreenConfigInfo");
  void* cfg = info && rate && done ? info(dpy, DefaultRootWindow(dpy)) : NULL;
  short hz  = cfg != NULL ? rate(cfg) : 0;
  if (cfg != NULL) {
    done(cfg);
  }
  return hz >= 24 && hz <= 1000 ? 1000000000 / (u64)hz : 16666667;
}

static u32 window_make(const char* title, u32 w, u32 h, intptr_t* out,
  const char** why) {
  if (w < 1 || h < 1 || w > 16384 || h > 16384) {
    return EINVAL;
  }
  Display* dpy = XOpenDisplay(NULL);
  if (dpy == NULL) {
    *why = "Window.open: no display (build a native binary with bend <file> -o <out> and run it from a desktop session)";
    return ENOTSUP;
  }
  int scr = DefaultScreen(dpy);
  if (DefaultDepth(dpy, scr) < 24) {
    XCloseDisplay(dpy);
    *why = "Window.open: the display has no 24-bit visual";
    return ENOTSUP;
  }
  BendWin* win = io_mem(calloc(1, sizeof *win));
  win->dpy = dpy;
  win->period = window_period(dpy);
  win->win = XCreateSimpleWindow(dpy, RootWindow(dpy, scr), 0, 0, w, h, 0, 0,
    BlackPixel(dpy, scr));
  win->im = XOpenIM(dpy, NULL, NULL, NULL);
  win->ic = win->im == NULL ? NULL : XCreateIC(win->im,
    XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
    XNClientWindow, win->win, XNFocusWindow, win->win, NULL);
  if (win->ic == NULL) {
    *why = "Window.open: X11 UTF-8 input context unavailable";
    if (win->im != NULL) {
      XCloseIM(win->im);
    }
    XDestroyWindow(dpy, win->win);
    XCloseDisplay(dpy);
    free(win);
    return ENOTSUP;
  }
  win->del = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
  win->img = XCreateImage(dpy, DefaultVisual(dpy, scr), DefaultDepth(dpy, scr),
    ZPixmap, 0, io_mem(calloc(w * h, 4)), w, h, 32, w * 4);
  win->img->byte_order = LSBFirst;
  XSizeHints hints = { .flags = PMinSize | PMaxSize, .min_width = w,
    .min_height = h, .max_width = w, .max_height = h };
  XSetWMNormalHints(dpy, win->win, &hints);
  XSetWMProtocols(dpy, win->win, &win->del, 1);
  XStoreName(dpy, win->win, title);
  XSelectInput(dpy, win->win, KeyPressMask | KeyReleaseMask | ButtonPressMask
    | ButtonReleaseMask | PointerMotionMask | FocusChangeMask);
  XMapRaised(dpy, win->win);
  XFlush(dpy);
  *out = (intptr_t)win;
  return 0;
}

#else

static u32 window_make(const char* title, u32 w, u32 h, intptr_t* out,
  const char** why) {
  *why = "Window.open: no display (build a native binary with bend <file> -o <out> and run it from a desktop session)";
  return ENOTSUP;
}

#endif

Term window_open_run(Env e, Term* f, IoWork* w) {
  uint64_t n = 0;
  char* title = io_cstr(e, f[0], &n);
  intptr_t out;
  const char* why = NULL;
  u32 q = io_nul(title, n) ? EILSEQ
    : window_make(title, (u32)f[1], (u32)f[2], &out, &why);
  free(title);
  if (q != 0) {
    return io_fail(e, q, why);
  }
  return io_done(e, io_hand(out));
}

static void __attribute__((constructor)) window_open_use(void) {
  io_eff(CID(Window.open), window_open_run);
}

#endif

#ifdef CID(Window.frame)

// Ordinary events take five words (cid and four fields). Text takes
// (cid, byte length, padded UTF-8 bytes), copied before the OS releases it.
#if defined(__OBJC__) || defined(__linux__)

static Term window_node(Env e, const u32* ev) {
  if (ev[0] == CID(Text)) {
    return io_box(e, CID(Text), io_str(e, (const char*)(ev + 2), ev[1]));
  }
  u32 n = cid_arity(ev[0]);
  if (n == 0) {
    return term_pak(ev[0], 0);
  }
  u64 l = heap_alloc(e, cls_fit(n));
  for (u32 j = 0; j < n; j += 1) {
    e.mem[l + j] = ev[1 + j];
  }
  return term_ctr(ev[0], l);
}

static u32 window_depth(u32 n) {
  return n > 1 ? 32 - CLZ(n - 1) : 0;
}

static Term window_list(Env e, const u32* p, u64 n) {
  Term list = term_pak(CID(Nil), 0);
  Term* hole = &list;
  for (u64 i = 0; i < n;) {
    const u32* ev = p + i;
    Term node = io_node(e, CID(Con), window_node(e, ev),
      term_pak(CID(Nil), 0));
    *hole = hole == &list ? node : io_seal(e, node, CID(Con));
    hole = &e.mem[term_peek(e.mem, node) + 1];
    i += ev[0] == CID(Text) ? 2 + ((u64)ev[1] + 3) / 4 : 5;
  }
  return list;
}

#endif

#ifdef __OBJC__

#define WIN_STR_(x) #x
#define WIN_STR(x)  WIN_STR_(x)
#define WIN_DEF(m)  "#define " #m " " WIN_STR(m) "\n"

typedef struct {
  u64 root;
  u32 w;
  u32 h;
  u32 k;
} WinArgs;

static id<MTLCommandQueue>         window_que;
static id<MTLBuffer>               window_buf;
static id<MTLComputePipelineState> window_pso;
static u64                         window_len;

static const char* window_msl =
  "#include <metal_stdlib>\n"
  "using namespace metal;\n"
  WIN_DEF(TAG_CTR)
  WIN_DEF(RFC_BIT)
  WIN_DEF(LOC_MASK)
  "struct Args { ulong root; uint w; uint h; uint k; };\n"
  "ulong node(device const ulong* mem, ulong t) {\n"
  "  return t & RFC_BIT ? mem[t & LOC_MASK] >> 24 : t & LOC_MASK;\n"
  "}\n"
  "kernel void window_dev(device const ulong* mem [[buffer(0)]],\n"
  "  constant Args& a [[buffer(1)]],\n"
  "  texture2d<float, access::write> out [[texture(0)]],\n"
  "  uint2 p [[thread_position_in_grid]]) {\n"
  "  ulong t = a.root;\n"
  "  for (uint i = a.k; ((t >> 56) & 0x7f) == TAG_CTR;) {\n"
  "    uint j = 0;\n"
  "    if (i > 0) {\n"
  "      i -= 1;\n"
  "      j = ((p.y >> i) & 1) * 2 + ((p.x >> i) & 1);\n"
  "    }\n"
  "    t = mem[node(mem, t) + j];\n"
  "  }\n"
  "  float4 c = unpack_unorm4x8_to_float(uint(t & LOC_MASK));\n"
  "  out.write(float4(c.zyx, 1.0), p);\n"
  "}\n";

static void window_pipe(id<MTLDevice> dev) {
  if (window_pso != nil) {
    return;
  }
  window_que = gpu_buf != nil ? gpu_que : [dev newCommandQueue];
  NSError* err = nil;
  id<MTLLibrary> lib = [dev
    newLibraryWithSource:[NSString stringWithUTF8String:window_msl]
    options:nil error:&err];
  if (lib == nil) {
    err_fail(err.localizedDescription.UTF8String);
  }
  window_pso = [dev newComputePipelineStateWithFunction:
    [lib newFunctionWithName:@"window_dev"] error:&err];
  if (window_pso == nil) {
    err_fail(err.localizedDescription.UTF8String);
  }
}

// Waits for done inside the run loop, dispatching the events as they
// come: the window server's work on this thread (a title-bar drag) runs
// while the frame waits, not a frame later.
static void window_wait(bool* done) {
  for (;;) {
    @autoreleasepool {
      NSEvent* ev = [NSApp nextEventMatchingMask:NSEventMaskAny
        untilDate:NSDate.distantPast inMode:NSDefaultRunLoopMode dequeue:YES];
      if (ev != nil) {
        [NSApp sendEvent:ev];
      } else if (*done) {
        return;
      } else {
        [NSRunLoop.currentRunLoop runMode:NSDefaultRunLoopMode
          beforeDate:NSDate.distantFuture];
      }
    }
  }
}

static id<MTLBuffer> window_corpus(Env e, id<MTLDevice> dev) {
  if (gpu_buf != nil) {
    return gpu_buf;
  }
  u64 bump = a32_load(a32_at(e.mem, H_BUMP));
  u64 need = ((HEAP_OFF + (bump << PAGE_BITS)) * 8 + 16383) & ~16383ull;
  if (need > window_len) {
    u64 most = [dev maxBufferLength] & ~16383ull;
    if (need > most) {
      err_fail("the frame's memory is past the Metal buffer limit");
    }
    u64 len = window_len * 2 > need ? window_len * 2 : need;
    len = len < most ? len : most;
    window_buf = [dev newBufferWithBytesNoCopy:e.mem length:len
      options:MTLResourceStorageModeShared
        | MTLResourceHazardTrackingModeUntracked deallocator:nil];
    if (window_buf == nil) {
      err_fail("the corpus prefix does not map as a Metal buffer");
    }
    window_len = len;
  }
  return window_buf;
}

static void window_show(Env e, CAMetalLayer* layer, Term image) {
  id<MTLDevice> dev = layer.device;
  window_pipe(dev);
  id<MTLBuffer> buf = window_corpus(e, dev);
  WinArgs args = { image, layer.drawableSize.width, layer.drawableSize.height,
    0 };
  args.k = window_depth(args.w > args.h ? args.w : args.h);
  __block bool done = false;
  id<MTLCommandBuffer> cb = [window_que commandBuffer];
  // nextDrawable blocks until the display frees one: on a helper thread
  dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INTERACTIVE, 0), ^{
    @autoreleasepool {
      id<CAMetalDrawable> d = [layer nextDrawable];
      if (d != nil) {
        id<MTLComputeCommandEncoder> enc = [cb computeCommandEncoder];
        NSUInteger tw = window_pso.threadExecutionWidth;
        [enc setComputePipelineState:window_pso];
        [enc setBuffer:buf offset:0 atIndex:0];
        [enc setBytes:&args length:sizeof(args) atIndex:1];
        [enc setTexture:d.texture atIndex:0];
        [enc dispatchThreads:MTLSizeMake(args.w, args.h, 1)
          threadsPerThreadgroup:MTLSizeMake(tw,
            window_pso.maxTotalThreadsPerThreadgroup / tw, 1)];
        [enc endEncoding];
        [cb presentDrawable:d];
        [cb commit];
        [cb waitUntilCompleted];
      }
      dispatch_async(dispatch_get_main_queue(), ^{
        done = true;
        CFRunLoopStop(CFRunLoopGetMain());
      });
    }
  });
  window_wait(&done);
  if (cb.error != nil) {
    err_fail(cb.error.localizedDescription.UTF8String);
  }
}

static Term window_frame(Env e, intptr_t at, Term image) {
  NSView*        view = ((__bridge NSWindow*)(void*)at).contentView;
  NSMutableData* evs  = [view valueForKey:@"evs"];
  io_sync();
  window_show(e, (CAMetalLayer*)view.layer, image);
  Term list = window_list(e, evs.bytes, evs.length / 4);
  evs.length = 0;
  return list;
}

#elif defined(__linux__)

// The Mac's key codes: a key's character in lower case (Escape, Return
// and Tab are theirs), the function keys' private-use characters (the
// arrows at 63232), a modifier's 65536 + its key code.
static const u32 window_keys[][2] = {
  { XK_BackSpace, 127 },   { XK_Up,        63232 }, { XK_Down,      63233 },
  { XK_Left,      63234 }, { XK_Right,     63235 }, { XK_Insert,    63271 },
  { XK_Delete,    63272 }, { XK_Home,      63273 }, { XK_End,       63275 },
  { XK_Page_Up,   63276 }, { XK_Page_Down, 63277 }, { XK_Super_R,   65590 },
  { XK_Super_L,   65591 }, { XK_Shift_L,   65592 }, { XK_Caps_Lock, 65593 },
  { XK_Alt_L,     65594 }, { XK_Control_L, 65595 }, { XK_Shift_R,   65596 },
  { XK_Alt_R,     65597 }, { XK_Control_R, 65598 }, { XK_ISO_Left_Tab, 25 },
};

static u32 window_key(XKeyEvent* ev) {
  char   c[8];
  KeySym ks = 0;
  ev->state &= ShiftMask | LockMask;
  int n = XLookupString(ev, c, sizeof c, &ks, NULL);
  for (u32 i = 0; i < sizeof window_keys / sizeof *window_keys; i += 1) {
    if (window_keys[i][0] == ks) {
      return window_keys[i][1];
    }
  }
  if (ks >= XK_F1 && ks <= XK_F12) {
    return 63236 + (u32)(ks - XK_F1);
  }
  if (n == 1) {
    return (u8)c[0] >= 'A' && (u8)c[0] <= 'Z' ? (u8)c[0] + 32 : (u8)c[0];
  }
  return 65536 + ev->keycode;
}

static void window_room(BendWin* win, u32 words) {
  u64 need = (u64)win->n + words;
  if (need > UINT32_MAX) {
    err_fail("Window: event queue too large");
  }
  if (need > win->cap) {
    u64 cap = win->cap == 0 ? 320 : win->cap;
    while (cap < need) {
      cap = cap > UINT32_MAX / 2 ? need : cap * 2;
    }
    win->cap = (u32)cap;
    win->evs = io_mem(realloc(win->evs, cap * sizeof *win->evs));
  }
}

static void window_push(BendWin* win, u32 cid, u32 a, u32 b, u32 c, u32 d) {
  window_room(win, 5);
  u32 ev[5] = { cid, a, b, c, d };
  memcpy(win->evs + win->n, ev, sizeof ev);
  win->n += 5;
}

static void window_text(BendWin* win, XKeyEvent* key) {
  window_room(win, 18);
  KeySym symbol;
  Status status;
  int n;
  for (;;) {
    u64 bytes = (u64)(win->cap - win->n - 2) * 4;
    n = Xutf8LookupString(win->ic, key, (char*)(win->evs + win->n + 2),
      bytes > INT_MAX ? INT_MAX : (int)bytes, &symbol, &status);
    if (status != XBufferOverflow) {
      break;
    }
    window_room(win, 2 + ((u32)n + 3) / 4);
  }
  if (n > 0 && (status == XLookupChars || status == XLookupBoth)) {
    win->evs[win->n] = CID(Text);
    win->evs[win->n + 1] = (u32)n;
    win->n += 2 + ((u32)n + 3) / 4;
  }
}

static u32 window_clip(int v, u32 most) {
  return v < 0 ? 0 : (u32)v < most ? (u32)v : most - 1;
}

static void window_pump(BendWin* win) {
  u32 w  = win->img->width;
  u32 h  = win->img->height;
  int cx = w / 2;
  int cy = h / 2;
  int x  = cx;
  int y  = cy;
  while (XPending(win->dpy) > 0) {
    XEvent ev;
    XNextEvent(win->dpy, &ev);
    bool filtered = XFilterEvent(&ev, win->win);
    if (ev.type == KeyPress || ev.type == KeyRelease) {
      XKeyEvent key = ev.xkey; // window_key folds only its private copy.
      window_push(win, CID(Key), window_key(&key), ev.type == KeyPress, 0, 0);
      if (ev.type == KeyPress && !filtered) {
        window_text(win, &ev.xkey);
      }
    } else if (ev.type == ButtonPress || ev.type == ButtonRelease) {
      // the wheel: 4 up, 5 down, 6 left, 7 right
      u32 b  = ev.xbutton.button;
      u32 bx = window_clip(ev.xbutton.x, w);
      u32 by = window_clip(ev.xbutton.y, h);
      f32 s  = b % 2 ? -1 : 1;
      if (b <= 3) {
        window_push(win, CID(Mouse), bx, by, b == 1 ? 0 : 4 - b,
          ev.type == ButtonPress);
      } else if (b <= 7 && ev.type == ButtonPress) {
        window_push(win, CID(Scroll), bx, by, f32_rewrap(b > 5 ? s : 0),
          f32_rewrap(b > 5 ? 0 : s));
      }
    } else if (ev.type == MotionNotify) {
      x = ev.xmotion.x;
      y = ev.xmotion.y;
      if (!win->grab) {
        window_push(win, CID(Move), window_clip(x, w), window_clip(y, h), 0,
          0);
      }
    } else if (ev.type == FocusIn) {
      XSetICFocus(win->ic);
    } else if (ev.type == FocusOut) {
      XUnsetICFocus(win->ic);
      XUngrabPointer(win->dpy, CurrentTime);
      win->grab = 0;
    } else if (ev.type == ClientMessage
      && (Atom)ev.xclient.data.l[0] == win->del) {
      window_push(win, CID(Close), 0, 0, 0, 0);
    }
  }
  // grabbed, the frame's motion is one look from the centre
  if (win->grab && (x != cx || y != cy)) {
    window_push(win, CID(Look), f32_rewrap(x - cx), f32_rewrap(y - cy), 0,
      0);
    XWarpPointer(win->dpy, None, win->win, 0, 0, 0, 0, cx, cy);
  }
}

#if BEND_CUDA
static CUfunction  window_pso;
static CUdeviceptr window_buf;
static u64         window_len;
#endif

// The s x s square of t at (x, y) of the frame: a Qua by quarters,
// else the color window_pix reads at its corner.
static void window_sq(u64* H, u32* pix, u32 w, u32 h, Term t, u32 s, u32 x,
  u32 y) {
  if (x >= w || y >= h) {
    return;
  }
  if (s > 1 && term_tag(t) == TAG_CTR) {
    u64 l = term_peek(H, t);
    s /= 2;
    for (u32 j = 0; j < 4; j += 1) {
      window_sq(H, pix, w, h, H[l + j], s, x + j % 2 * s, y + j / 2 * s);
    }
    return;
  }
  u32 c = window_pix(H, t, 0, 0, 0);
  for (u32 i = y; i < h && i < y + s; i += 1) {
    for (u32 j = x; j < w && j < x + s; j += 1) {
      pix[i * w + j] = c;
    }
  }
}

// The frame's pixels: window_dev on the device while the corpus is
// there (the tree's pages never leave it), else window_sq from the root.
static void window_fill(Env e, u32* pix, u32 w, u32 h, Term image, u32 k) {
#if BEND_CUDA
  if (io_gpu) {
    u64*   H    = e.mem;
    u64    len  = (u64)w * h * 4;
    void*  args[] = { &H, &image, &w, &h, &k, &window_buf };
    if (window_pso == NULL && cuModuleGetFunction(&window_pso, gpu_lib,
      "window_dev") != CUDA_SUCCESS) {
      err_fail("cannot load the window kernel");
    }
    if (len > window_len) {
      if (window_buf != 0) {
        cuMemFree(window_buf);
      }
      if (cuMemAlloc(&window_buf, len) != CUDA_SUCCESS) {
        err_fail("the frame's device buffer failed");
      }
      window_len = len;
    }
    if (cuLaunchKernel(window_pso, (w + 31) / 32, (h + 7) / 8, 1, 32, 8, 1, 0,
      NULL, args, NULL) != CUDA_SUCCESS
      || cuMemcpyDtoH(pix, window_buf, len) != CUDA_SUCCESS) {
      err_fail("the frame's device fill failed");
    }
    return;
  }
#endif
  window_sq(e.mem, pix, w, h, image, 1u << k, 0, 0);
}

// A frame waits for the display's next tick, as the Mac's display sync.
static void window_pace(u64 period) {
  static u64 due;
  u64 now = io_tick();
  if (due > now) {
    struct timespec ts = { 0, (long)(due - now) };
    nanosleep(&ts, NULL);
  }
  due = (due > now ? due : now) + period;
}

static void window_show(Env e, BendWin* win, Term image) {
  u32 w = win->img->width;
  u32 h = win->img->height;
  u32 k = window_depth(w > h ? w : h);
  window_fill(e, (u32*)win->img->data, w, h, image, k);
  window_pace(win->period);
  XPutImage(win->dpy, win->win, DefaultGC(win->dpy, DefaultScreen(win->dpy)),
    win->img, 0, 0, 0, 0, w, h);
  XFlush(win->dpy);
}

static Term window_frame(Env e, intptr_t at, Term image) {
  BendWin* win = (BendWin*)at;
  io_sync();
  window_pump(win);
  window_show(e, win, image);
  Term list = window_list(e, win->evs, win->n);
  win->n = 0;
  return list;
}

#else

static Term window_frame(Env e, intptr_t at, Term image) {
  return term_pak(CID(Nil), 0);
}

#endif

Term window_frame_run(Env e, Term* f, IoWork* w) {
  Term events = window_frame(e, (intptr_t)io_hand_v(f[0]), f[1]);
  return io_tup(e, f[0], io_tup(e, f[1], events));
}

static void __attribute__((constructor)) window_frame_use(void) {
  io_eff(CID(Window.frame), window_frame_run);
}

#endif

#ifdef CID(Window.set_title)

#ifdef __OBJC__

static void window_set_title(intptr_t at, const char* text, u64 n) {
  NSWindow* win = (__bridge NSWindow*)(void*)at;
  win.title = [[NSString alloc] initWithBytes:text length:n
    encoding:NSUTF8StringEncoding];
}

#elif defined(__linux__)

static void window_set_title(intptr_t at, const char* text, u64 n) {
  BendWin* win = (BendWin*)at;
  XStoreName(win->dpy, win->win, text);
  XFlush(win->dpy);
}

#else

static void window_set_title(intptr_t at, const char* text, u64 n) {
}

#endif

Term window_set_title_run(Env e, Term* f, IoWork* w) {
  u64   n    = 0;
  char* text = io_cstr(e, f[1], &n);
  window_set_title((intptr_t)io_hand_v(f[0]), text, n);
  free(text);
  return f[0];
}

static void __attribute__((constructor)) window_set_title_use(void) {
  io_eff(CID(Window.set_title), window_set_title_run);
}

#endif

#ifdef CID(Window.grab)

#ifdef __OBJC__

static void window_grab(intptr_t at, bool on) {
  NSWindow* win = (__bridge NSWindow*)(void*)at;
  [win.contentView setValue:@(on) forKey:@"grab"];
}

#elif defined(__linux__)

// The pointer is confined to the window under a blank cursor and warped
// to its centre, where each frame puts it back; only a focused window
// takes it.
static void window_grab(intptr_t at, bool on) {
  BendWin* win = (BendWin*)at;
  Window   focus;
  int      revert;
  XGetInputFocus(win->dpy, &focus, &revert);
  if (on && !win->grab && focus == win->win) {
    char   zero = 0;
    XColor none = { 0 };
    Pixmap pix  = XCreateBitmapFromData(win->dpy, win->win, &zero, 1, 1);
    Cursor cur  = XCreatePixmapCursor(win->dpy, pix, pix, &none, &none, 0, 0);
    win->grab = XGrabPointer(win->dpy, win->win, True, PointerMotionMask
      | ButtonPressMask | ButtonReleaseMask, GrabModeAsync, GrabModeAsync,
      win->win, cur, CurrentTime) == GrabSuccess;
    XFreeCursor(win->dpy, cur);
    XFreePixmap(win->dpy, pix);
    if (win->grab) {
      XWarpPointer(win->dpy, None, win->win, 0, 0, 0, 0,
        win->img->width / 2, win->img->height / 2);
    }
  } else if (!on && win->grab) {
    XUngrabPointer(win->dpy, CurrentTime);
    win->grab = 0;
  }
  XFlush(win->dpy);
}

#else

static void window_grab(intptr_t at, bool on) {
}

#endif

Term window_grab_run(Env e, Term* f, IoWork* w) {
  window_grab((intptr_t)io_hand_v(f[0]), term_aux(f[1]) == CID(True));
  return f[0];
}

static void __attribute__((constructor)) window_grab_use(void) {
  io_eff(CID(Window.grab), window_grab_run);
}

#endif

#ifdef CID(Window.close)

#ifdef __OBJC__

static void window_close(intptr_t at) {
  NSWindow* win = CFBridgingRelease((void*)at);
  [win.contentView setValue:@NO forKey:@"grab"];
  [win close];
}

#elif defined(__linux__)

static void window_close(intptr_t at) {
  BendWin* win = (BendWin*)at;
  XDestroyIC(win->ic);
  XCloseIM(win->im);
  XDestroyImage(win->img);
  XCloseDisplay(win->dpy);
  free(win->evs);
  free(win);
}

#else

static void window_close(intptr_t at) {
}

#endif

Term window_close_run(Env e, Term* f, IoWork* w) {
  window_close((intptr_t)io_hand_v(f[0]));
  return term_pak(CID(Unit), 0);
}

static void __attribute__((constructor)) window_close_use(void) {
  io_eff(CID(Window.close), window_close_run);
}

#endif
