# Read the title after Window.open. Check UTF-8, ASCII and empty titles.
# Skip the title check when Window.open reports no display.
import Base

def title(window: Window) -> IO(Window & String):
  import "./window_title.c"
  import "./window_title.js"

def report(want: String, pair: Window & String) -> IO(Unit):
  (window, got) = pair
  do IO<Unit>:
    Window.close(window)
    IO.print(Bool.show(String.eq(want, got)))

def shown(want: String, window: Window) -> IO(Unit):
  do IO<Unit>:
    frame : Window & Image & List<Event> <- Window.frame(window, Pix{0})
    pair : Window & String <- title(Pair.fst(Window, Image & List<Event>, frame))
    report(want, pair)

def opened(want: String, result: Result<&1, &1, U32 & String, Window>) -> IO(Unit):
  match result:
    case Done{window}: shown(want, window)
    case Fail{(+code, message)}:
      IO.print(Bool.show(U32.is_eq(code, 45) || U32.is_eq(code, 95)))

def check(+text: String) -> IO(Unit):
  do IO<Unit>:
    result : Result<&1, &1, U32 & String, Window> <- Window.open(text, 64, 48)
    opened(text, result)

def main() -> IO(Unit):
  do IO<Unit>:
    check("Bend café 日本語 😀")
    check("Bend")
    check("")

#|True
#|True
#|True
