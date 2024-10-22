# Should give a type error in Erase when unifying the field of type Unit with the match type of Box 
type Box:
  Box {x: Unit}

type Unit:
  Unit

Erase: Box -> None
(Erase (Box/Box x)) = (Erase x)

main: None
main = (Erase (Box/Box Unit/Unit))
