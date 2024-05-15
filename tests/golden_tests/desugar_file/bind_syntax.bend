Result/bind (Result/Ok val) f = (f val)
Result/bind err _ = err

safe_div a b = switch b {
  0: (Result/Err "Div by 0")
  _: (Result/Ok (/ a b))
}

safe_rem a b = switch b {
  0: (Result/Err "Mod by 0")
  _: (Result/Ok (% a b))
}

Main = do Result {
  ask y = (safe_div 3 2)
  ask x = (safe_rem y 0);
  x
}
