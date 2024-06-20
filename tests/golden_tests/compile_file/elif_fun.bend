main =
  let cond1 = (== 1 2)
  let cond2 = (< 2 1)
  let cond3 = (> 3 2)
  let cond4 = (== 2 2)
  if cond1 {
    1
  } elif cond2 {
    2
  } elif cond3 {
    3
  } elif cond4 {
    4
  } else {
    0
  }
