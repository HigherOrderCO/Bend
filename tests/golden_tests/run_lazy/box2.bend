data _Box = (Box val)

Box/subst (_Box/Box n) from to = (Box/subst/go n (== from n) to)
Box/subst/go a 0 to = (_Box/Box a)
Box/subst/go a * to = to

Main = (Box/subst (_Box/Box 4) 8000 (_Box/Box 10))
