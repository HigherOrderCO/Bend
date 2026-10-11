# a match carries a variable its arms use more than once (q=1, not Data)
# into them, and each arm binds it again; a later variable whose type
# names it stayed out, at its type outside the arms, so the kernel saw
# {x1 == x0} where the arm's Box needs {x4 == x0} (#1292). It now rides
# too, at its own quantity: erased (pick), live (pick_live), + (pick_plus),
# into a match in one arm (pick_inner), three matches deep, where an arm
# built again sees an inner match only by the variables it names, so a
# dead use there counts too (pick_deep), into a default arm (pick_rest)
# and past a self-call (pick_rec). A default's binder is a tag and its
# fields, both riding when one does: when it is the one riding
# (pick_tag) and when it is the later variable, used in one arm only
# (pick_pair). The law checks main's value in the emitted book too
import Base

type Owner is Type:
  Owner{n: Nat}

type Box<-original: Owner> is Type:
  Box{owner: Owner, -same: {owner == original : Owner}}

type Three is Data:
  One{}
  Two{}
  Many{}

type Tag<-o: Owner> is Data:
  Tag{n: Nat}

type Pack is Type:
  Pack{owner: Owner, a: Tag<owner>, b: Tag<owner>}

type TBox<-orig: Three> is Type:
  TBox{x: Three, -e: {x == orig : Three}}

type Idx<-o: Owner> is Data:
  I1{}
  I2{}
  I3{}

type Pair2 is Type:
  Pair2{owner: Owner, i: Idx<owner>}

type Got is Type:
  Nobody{}
  Held{p: Pair2}

def pick(-original: Owner, owner: Owner, -same: {owner == original : Owner}, flag: Bool) -> Box<original>:
  match flag:
    case False{}:
      Box{owner, same}
    case True{}:
      Box{owner, same}

def pick_live(-original: Owner, owner: Owner, same: {owner == original : Owner}, flag: Bool) -> Box<original>:
  match flag:
    case False{}:
      Box{owner, same}
    case True{}:
      Box{owner, same}

def pick_inner(-original: Owner, owner: Owner, -same: {owner == original : Owner}, a: Bool, b: Bool) -> Box<original>:
  match a:
    case False{}:
      match b:
        case False{}:
          Box{owner, same}
        case True{}:
          Box{owner, same}
    case True{}:
      Box{owner, same}

def pick_deep(-original: Owner, owner: Owner, -same: {owner == original : Owner}, a: Bool, b: Bool, c: Bool) -> Box<original>:
  match a:
    case False{}:
      match b:
        case False{}:
          match c:
            case False{}:
              Box{owner, same}
            case True{}:
              Box{owner, same}
        case True{}:
          match c:
            case False{}:
              Box{owner, same}
            case True{}:
              Box{owner, same}
    case True{}:
      Box{owner, same}

def pick_rest(-original: Owner, owner: Owner, -same: {owner == original : Owner}, t: Three) -> Box<original>:
  match t:
    case One{}:
      Box{owner, same}
    case _:
      Box{owner, same}

def pick_rec(-original: Owner, owner: Owner, -same: {owner == original : Owner}, k: Nat) -> Box<original>:
  match k:
    case 0n:
      Box{owner, same}
    case 1n+p:
      pick_rec(original, owner, same, p)

def pick_plus(owner: Owner, +t: Tag<owner>, flag: Bool) -> Pack:
  match flag:
    case False{}:
      Pack{owner, t, t}
    case True{}:
      Pack{owner, t, t}

def pick_tag(-orig: Three, t: Three, -p: {t == orig : Three}, flag: Bool) -> TBox<orig>:
  match t:
    case One{}:
      TBox{One{}, p}
    case c:
      match flag:
        case False{}:
          TBox{c, p}
        case True{}:
          TBox{c, p}

def pick_pair(owner: Owner, i: Idx<owner>, flag: Bool) -> Got:
  match i:
    case I1{}:
      Nobody{}
    case c:
      match flag:
        case False{}:
          Held{Pair2{owner, c}}
        case True{}:
          Held{Pair2{owner, I2{}}}

def num(-original: Owner, box: Box<original>) -> Nat:
  match box:
    case Box{owner, same}:
      match owner:
        case Owner{n}:
          n

def num_plus(p: Pack) -> Nat:
  match p:
    case Pack{owner, a, b}:
      match a:
        case Tag{n}:
          n

def num_tag(-orig: Three, b: TBox<orig>) -> Nat:
  match b:
    case TBox{x, e}:
      match x:
        case One{}:
          0n
        case Two{}:
          0n
        case Many{}:
          8n

def num_pair(g: Got) -> Nat:
  match g:
    case Nobody{}:
      0n
    case Held{p}:
      match p:
        case Pair2{owner, i}:
          match i:
            case I1{}:
              0n
            case I2{}:
              0n
            case I3{}:
              9n

def main() -> Nat:
  Nat.add(num(Owner{1n}, pick(Owner{1n}, Owner{1n}, {==}, True{})),
  Nat.add(num(Owner{2n}, pick_live(Owner{2n}, Owner{2n}, {==}, False{})),
  Nat.add(num(Owner{3n}, pick_inner(Owner{3n}, Owner{3n}, {==}, False{}, True{})),
  Nat.add(num(Owner{4n}, pick_deep(Owner{4n}, Owner{4n}, {==}, False{}, False{}, True{})),
  Nat.add(num(Owner{5n}, pick_rest(Owner{5n}, Owner{5n}, {==}, Many{})),
  Nat.add(num(Owner{6n}, pick_rec(Owner{6n}, Owner{6n}, {==}, 2n)),
  Nat.add(num_plus(pick_plus(Owner{0n}, Tag{7n}, True{})),
  Nat.add(num_tag(Many{}, pick_tag(Many{}, Many{}, {==}, False{})),
    num_pair(pick_pair(Owner{0n}, I3{}, False{}))))))))))

law convoy_dependent_result:
  {main() == 45n : Nat}

def convoy_dependent_result():
  {==}

#|45n
