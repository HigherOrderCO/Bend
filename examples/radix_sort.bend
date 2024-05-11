data Map_ = Free | Used | (Both a b)
data Arr = Null | (Leaf x) | (Node a b)

(Swap s a b) = switch s {
  0: (Map_/Both a b)
  _: (Map_/Both b a)
}

// Sort : Arr -> Arr
(Sort t) = (ToArr 0 (ToMap t))

// ToMap : Arr -> Map
(ToMap Arr/Null)       = Map_/Free
(ToMap (Arr/Leaf a))   = (Radix a)
(ToMap (Arr/Node a b)) = (Merge (ToMap a) (ToMap b))

// ToArr : U60 -> Map -> Arr
(ToArr x Map_/Free) = Arr/Null
(ToArr x Map_/Used) = (Arr/Leaf x)
(ToArr x (Map_/Both a b)) =
  let a = (ToArr (+ (* x 2) 0) a)
  let b = (ToArr (+ (* x 2) 1) b)
  (Arr/Node a b)

// Merge : Map -> Map -> Map
(Merge Map_/Free       Map_/Free)       = Map_/Free
(Merge Map_/Free       Map_/Used)       = Map_/Used
(Merge Map_/Used       Map_/Free)       = Map_/Used
(Merge Map_/Used       Map_/Used)       = Map_/Used
(Merge Map_/Free       (Map_/Both c d)) = (Map_/Both c d)
(Merge (Map_/Both a b) Map_/Free)       = (Map_/Both a b)
(Merge (Map_/Both a b) (Map_/Both c d)) = (Map_/Both (Merge a c) (Merge b d))
(Merge (Map_/Both a b) Map_/Used) = *
(Merge Map_/Used (Map_/Both a b)) = *

// Radix : U60 -> Map
(Radix n) =
  let r = Map_/Used
  let r = (Swap (& n 1) r Map_/Free)
  let r = (Swap (& n 2) r Map_/Free)
  let r = (Swap (& n 4) r Map_/Free)
  let r = (Swap (& n 8) r Map_/Free)
  let r = (Swap (& n 16) r Map_/Free)
  (Radix2 n r)

(Radix2 n r) =
  let r = (Swap (& n 32) r Map_/Free)
  let r = (Swap (& n 64) r Map_/Free)
  let r = (Swap (& n 128) r Map_/Free)
  let r = (Swap (& n 256) r Map_/Free)
  let r = (Swap (& n 512) r Map_/Free)
  (Radix3 n r)

(Radix3 n r) =
  let r = (Swap (& n 1024) r Map_/Free)
  let r = (Swap (& n 2048) r Map_/Free)
  let r = (Swap (& n 4096) r Map_/Free)
  let r = (Swap (& n 8192) r Map_/Free)
  let r = (Swap (& n 16384) r Map_/Free)
  (Radix4 n r)

(Radix4 n r) =
  let r = (Swap (& n 32768) r Map_/Free)
  let r = (Swap (& n 65536) r Map_/Free)
  let r = (Swap (& n 131072) r Map_/Free)
  let r = (Swap (& n 262144) r Map_/Free)
  let r = (Swap (& n 524288) r Map_/Free)
  (Radix5 n r)

(Radix5 n r) =
  let r = (Swap (& n 1048576) r Map_/Free)
  let r = (Swap (& n 2097152) r Map_/Free)
  let r = (Swap (& n 4194304) r Map_/Free)
  let r = (Swap (& n 8388608) r Map_/Free)
  r


// Reverse : Arr -> Arr
(Reverse Arr/Null)       = Arr/Null
(Reverse (Arr/Leaf a))   = (Arr/Leaf a)
(Reverse (Arr/Node a b)) = (Arr/Node (Reverse b) (Reverse a))

// Sum : Arr -> U60
(Sum Arr/Null)       = 0
(Sum (Arr/Leaf x))   = x
(Sum (Arr/Node a b)) = (+ (Sum a) (Sum b))

// Gen : U60 -> Arr
(Gen n) = (Gen.go n 0)
  (Gen.go n x) = switch n {
    0: (Arr/Leaf x)
    _:
      let a = (* x 2)
      let b = (| (* x 2) 1)
      (Arr/Node (Gen.go n-1 a) (Gen.go n-1 b))
  }

Main = (Sum (Sort (Reverse (Gen 4))))
