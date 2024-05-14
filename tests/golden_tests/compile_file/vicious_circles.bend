dup_self = let {$x $y} = $x; $y
disconnected_self_lam = let $x = @* $x; *

main = (dup_self, disconnected_self_lam)
