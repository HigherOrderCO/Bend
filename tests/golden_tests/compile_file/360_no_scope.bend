main =
  let $d = ((@$a $c) 360)
  let $b = ((@$c $a) $b)
  let #x{#y($e, $f) #y($g, $h)} = #y(#x{$d $f}, #x{$e, $g})
  $h
