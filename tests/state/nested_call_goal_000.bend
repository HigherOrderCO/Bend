# 4-ctor enum ladder, state 0: a NESTED stuck dispatch (turn(turn(d)))
# renders as the ref chain, no arm literal leaking
type Dir is Data:
  Up{}
  Rt{}
  Dn{}
  Lf{}

law turn:
  for d: Dir
  Dir

def turn(d):
  match d:
    case Up{}:
      Rt{}
    case Rt{}:
      Dn{}
    case Dn{}:
      Lf{}
    case Lf{}:
      Up{}

law back:
  for d: Dir
  Dir

def back(d):
  match d:
    case Up{}:
      Dn{}
    case Rt{}:
      Lf{}
    case Dn{}:
      Up{}
    case Lf{}:
      Rt{}

law turn_turn:
  for d: Dir
  {turn(turn(d)) == back(d) : Dir}

def turn_turn(d):
  {==}

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Context:
#|- d : Dir
#|Location: turn_turn
#|43 | def turn_turn(d):
#|44>|   {==}
#|   |   ^^^^
#|45 |
#|- left     : turn(turn(d))  =>  turn(turn(d))
#|- right    : back(d)  =>  back(d)
#|exit 1
