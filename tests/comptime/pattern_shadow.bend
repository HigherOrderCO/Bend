# A pattern binder shadows the template parameter with the same name.
type Box is Type:
  Mk{x: Quant -> Quant}

def shadow(~p: Box, box: Box) -> Quant:
  match box:
    case p:
      match p:
        case Mk{f}: f(&1)

def main() -> Quant:
  shadow(~Mk{x => &2}, Mk{x => x})

#|&1
