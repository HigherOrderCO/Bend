#{




#}

def main:
  #{
  ok
  #}#
  #
  return 0

#{foo type#}
type Foo:
  #{foo constructor#}
  Foo { foo }

#{bar type#}
object Bar { bar }
#{
# ignore
#}#

#{X x x x#}
(X x x x) = #{ x #} x #{ x #}

#{V#}
type V
  = #{V constructor#} V

(String/is_empty s) =
  #{test if string is nil#}
  match s #{

  #}{
    String/Nil: 1
    String/Cons: 0
  }

def String/not_empty(s):
  #{test if string if not nil#}
  match s:
    #{
    its
    not
    #}
    case String/Nil:
      return 0
    case String/Cons:
      #{
        it
        is#}
      return 1
