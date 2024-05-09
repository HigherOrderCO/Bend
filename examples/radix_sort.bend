#flavor core
data Map = Free | Used | (Both a b)
data Arr = Null | (Leaf x) | (Node a b)

(Swap s a b) = switch s {
  0: (Both a b)
  _: (Both b a)
}

// Sort : Arr -> Arr
(Sort t) = (ToArr 0 (ToMap t))

// ToMap : Arr -> Map
(ToMap Null)       = Free
(ToMap (Leaf a))   = (Radix a)
(ToMap (Node a b)) = (Merge (ToMap a) (ToMap b))

// ToArr : U60 -> Map -> Arr
(ToArr x Free) = Null
(ToArr x Used) = (Leaf x)
(ToArr x (Both a b)) =
  let a = (ToArr (+ (* x 2) 0) a)
  let b = (ToArr (+ (* x 2) 1) b)
  (Node a b)

// Merge : Map -> Map -> Map
(Merge Free       Free)       = Free
(Merge Free       Used)       = Used
(Merge Used       Free)       = Used
(Merge Used       Used)       = Used
(Merge Free       (Both c d)) = (Both c d)
(Merge (Both a b) Free)       = (Both a b)
(Merge (Both a b) (Both c d)) = (Both (Merge a c) (Merge b d))
(Merge (Both a b) Used) = *
(Merge Used (Both a b)) = *

// Radix : U60 -> Map
(Radix n) =
  let r = Used
  let r = (Swap (& n 0x1) r Free)
  let r = (Swap (& n 0x2) r Free)
  let r = (Swap (& n 0x4) r Free)
  let r = (Swap (& n 0x8) r Free)
  let r = (Swap (& n 0x10) r Free)
  (Radix2 n r)

(Radix2 n r) =
  let r = (Swap (& n 0x20) r Free)
  let r = (Swap (& n 0x40) r Free)
  let r = (Swap (& n 0x80) r Free)
  let r = (Swap (& n 0x100) r Free)
  let r = (Swap (& n 0x200) r Free)
  (Radix3 n r)

(Radix3 n r) =
  let r = (Swap (& n 0x400) r Free)
  let r = (Swap (& n 0x800) r Free)
  let r = (Swap (& n 0x1000) r Free)
  let r = (Swap (& n 0x2000) r Free)
  let r = (Swap (& n 0x4000) r Free)
  (Radix4 n r)

(Radix4 n r) =
  let r = (Swap (& n 0x8000) r Free)
  let r = (Swap (& n 0x10000) r Free)
  let r = (Swap (& n 0x20000) r Free)
  let r = (Swap (& n 0x40000) r Free)
  let r = (Swap (& n 0x80000) r Free)
  (Radix5 n r)

(Radix5 n r) =
  let r = (Swap (& n 0x100000) r Free)
  let r = (Swap (& n 0x200000) r Free)
  let r = (Swap (& n 0x400000) r Free)
  let r = (Swap (& n 0x800000) r Free)
  r

// Reverse : Arr -> Arr
(Reverse Null)       = Null
(Reverse (Leaf a))   = (Leaf a)
(Reverse (Node a b)) = (Node (Reverse b) (Reverse a))

// Sum : Arr -> U60
(Sum Null)       = 0
(Sum (Leaf x))   = x
(Sum (Node a b)) = (+ (Sum a) (Sum b))

// Gen : U60 -> Arr
(Gen n) = (Gen.go n 0)
  (Gen.go n x) = switch n {
    0: (Leaf x)
    _:
      let a = (* x 2)
      let b = (| (* x 2) 1)
      (Node (Gen.go n-1 a) (Gen.go n-1 b))
  }

Main = (Sum (Sort (Reverse (Gen 4))))
