data Opt = (Some x) | None

Opt/map = @opt @f
  match opt {
    Opt/Some: (Opt/Some (f opt.x));
    Opt/None: Opt/None
  }

inc = @x (+ x 1)

main =
  let opt = (Opt/Some 1);
  (Opt/map opt inc)
