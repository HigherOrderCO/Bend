# a bang over a chain whose index grows with each link, where only the
# link at index 12 holds a closure: the grown index makes the walk open
# the chain with it unknown, where that field is stuck, so it counts as
# a closure: the lambda minted on the host is on the device
import Base

def Down(n: Nat, k: Nat) -> Type:
  match n:
    case 0n:
      match k:
        case 0n:
          U32 -> U32
        case 1n+j:
          U32
    case 1n+m:
      match k:
        case 0n:
          U32
        case 1n+j:
          Down(m, j)

type Ch<-n: Nat> is Type:
  End{}
  Link{f: Down(n, 12n), next: Ch<(1n + n : Nat)>}

def use(c: Ch<0n>) -> U32:
  match c:
    case End{}:
      0
    case Link{f0, n0}:
      match n0:
        case End{}:
          0
        case Link{f1, n1}:
          match n1:
            case End{}:
              0
            case Link{f2, n2}:
              match n2:
                case End{}:
                  0
                case Link{f3, n3}:
                  match n3:
                    case End{}:
                      0
                    case Link{f4, n4}:
                      match n4:
                        case End{}:
                          0
                        case Link{f5, n5}:
                          match n5:
                            case End{}:
                              0
                            case Link{f6, n6}:
                              match n6:
                                case End{}:
                                  0
                                case Link{f7, n7}:
                                  match n7:
                                    case End{}:
                                      0
                                    case Link{f8, n8}:
                                      match n8:
                                        case End{}:
                                          0
                                        case Link{f9, n9}:
                                          match n9:
                                            case End{}:
                                              0
                                            case Link{f10, n10}:
                                              match n10:
                                                case End{}:
                                                  0
                                                case Link{f11, n11}:
                                                  match n11:
                                                    case End{}:
                                                      0
                                                    case Link{f12, n12}:
                                                      f12(f0)

def main() -> IO(Unit):
  IO.print(U32.show(use!(Link{41, Link{1, Link{2, Link{3, Link{4, Link{5, Link{6, Link{7, Link{8, Link{9, Link{10, Link{11, Link{x => (x + 1 : U32), End{}}}}}}}}}}}}}})))

#|42
