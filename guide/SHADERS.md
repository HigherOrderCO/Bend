# Shaders in Bend

AIs wrote this tutorial, for AIs, from the logs of building
`demos/app_slash_boss_3d`. A human will revise it later. Report anything wrong
on GitHub.

A fast shader in Bend is one fork tree that ends in flat loops. The reference is
`demos/app_slash_boss_3d/bend3d.bend`, a rasterizer that draws 1920x1200 at 120
FPS (8.0 ms a frame) on a 10-core M4 mini. Every ms below is that machine and
that frame unless stated.

## The machine

A `!` call (a bang) ships one call to the GPU, which runs a cube of 16384 lanes
(128 x 128). A parallel let `a b = f(x) g(y)` creates one task per call plus a
join task. A kernel iteration grows the fork tree breadth-first until the
frontier fills the cube, then every lane runs its task to the end: ~0.4-0.5 ms
plus the slowest lane. A continuation that forks again lands in the next
iteration. A def is flat when it has no parallel let, no bang, no closure apply,
no non-flat call and no non-tail self call: it compiles to one native tail loop
(`spin_N` in the C). A non-tail self call becomes a segmented continuation: a
frame per call. Memory is one heap shared by the CPU and the GPU: unified on
Apple; managed on CUDA, where each page faults across PCIe on the first touch by
the other side. A machine with no GPU, or `--gpu off`, runs bangs on the CPU
pool. Divergent per-lane work (a search, a tree walk per ray) is faster there
than on the GPU. Aim for 4^7 leaves per bang, one per lane, and sweep one fork
level either side on the device: the CPU pool is flat across that sweep, so it
cannot tune it. Fewer leaves leave lanes idle (256 of 16384 busy: 234 ms for a
frame that takes 28 ms at 4096); more cost an iteration each.

## A frame

1. Host: build the scene (mesh, project, light, cull) into a quadtree of 64-px
   cells, one cons list of flat screen triangles per cell. 1.6 ms.
2. One bang: fork four ways per node, five levels to the cells, two more over
   each cell's list, to 16-px tiles: 4^7 = 16384 = the lane cube. A tile filters
   the cell's list into its own and walks it in flat loops. Draw ~6 ms.
3. The last frame rides down the same tree as `Four` quadrants; each tile leaves
   its quadrant unused, so the runtime sinks it there. Drop 0 ms.
4. Host: free the last scene as one branch of the next build. 0.35 ms.

```
2048 root   Frame.node, k=5   fork 4   levels 1-5  -> 64-px cells (32 x 32)
64-px cell  Cell.fork, k=2    fork 4   levels 6-7  -> 16-px tiles (16384)
16-px tile  Tile.b16 = 4 Tile.b8 = 4 Tile.b4       straight-line, no fork
4-px square Blk.go            one flat loop over the list, four Blk
2-px block  Blk.four          one Pix, or four
```

The root is the power of two over the frame (2048 over 1920x1200); a region off
the frame, or empty, answers one `Pix` before any fork (`Frame.sky`). A fork per
pixel drowns in scheduling; a fork per cell leaves lanes idle. A fold per level
plus a fold per pixel: 600 ms and 31 iterations a frame, against 8 ms and 1.

## The code

The list is the one shape a flat loop walks with no stack. One list per cell,
not per pixel: a list shared by pixels is contended atomics.

```python
type Tris is Data:
  TNil{}
  TCons{+tri: Tri, next: Tris}

type Cells is Data:
  CNil{}
  CLeaf{+n: U32, +ts: Tris}
  CQua{tl: Cells, tr: Cells, bl: Cells, br: Cells}
```

The fork. `Frame.show` bangs once and returns the tree beside the image, so the
host still owns it. `Image.open(old) -> Four` splits the last image at the call
(a `Pix` opens into four); each tile receives `old: Four` and ignores it.

```python
def Frame.node(+k: Nat, c: Cells, old: Four) -> Image:
  match k c:
    case _ CLeaf{_, ts}:
      Cell.fork(2n, ts, 0, old)
    case 1n+e CQua{tl, tr, bl, br}:
      Four{oa, ob, oc, od} = old
      a b c d = Frame.node(e, tl, Image.open(oa)) Frame.node(e, tr, Image.open(ob))
        Frame.node(e, bl, Image.open(oc)) Frame.node(e, br, Image.open(od))
      Qua{a, b, c, d}
    case _ _:
      Pix{0}

def Frame.show(+cells: Cells, old: Image) -> Image & Cells:
  (Frame.node!(5n, cells, Image.open(old)), cells)
```

`Cell.fork(+k, +cs, ..)` has the same shape over the cell's list and calls
`Tile.go` at `0n`. The tile: one flat loop reads each entry once for a block of
`Hit{w, c, i}`, then frees its list after the last read.

```python
def Blk.go(ts: Tris, +i: U32, +h0: Hit, +h1: Hit, +h2: Hit, +h3: Hit) -> Image:
  match ts:
    case TNil{}:
      Blk.four(h0, h1, h2, h3)
    case TCons{t, n}:
      Blk.go(n, (i + 1 : U32), Hit.step(t, i, h0), Hit.step(t, i, h1),
        Hit.step(t, i, h2), Hit.step(t, i, h3))

def Tile.go(cs: Tris, +x: U32) -> Image:
  +h = {Hit{0.0, 0, 0} : Hit}
  +ts = Tris.filter(cs, x, TNil{})
  Tile.fin(Blk.go(ts, 1, h, h, h, h), ts)
```

`Blk.four` answers `Pix{k0}` when the four hits are one triangle, else a `Qua`
of four `Pix`; `Tile.fin` walks the list to `TNil` and returns the image.
Transparency: the filter fills a second list (`Filt{+n, ts, bs}`) and `Blend.go`
walks it in order over the blocks after the opaque loop. Text and HUD: strokes
as opaque quads nearer than the world (`Text.put`, `Quad.put` at inverse depth
3000), layered by depth, order-free. The host: one parallel let per def, the
last scene freed as a branch by `Cells.drop(4n)`: four forked levels in
`Frame.node`'s shape, then `Tris.free` loops.

```python
def Scene.build(+t: F32, used: Cells) -> Cells:
  a b c = Hero.build(t) Stage.build(t) Cells.drop(4n, used)
  Cells.m4(a, b, c, CNil{})
```

A turn keeps the last image and scene as one pair: the build takes the old
scene, the bang the old image. An `IO.now()` step sits between them: a bang that
follows the host's forks in one evaluation runs on the CPU pool.
`Window.frame(window, image)` shows the image.

```python
def Turn.go(+t: F32, last: Image & Cells) -> IO(Image & Cells):
  (old, used) = last
  do IO<Image & Cells>:
    cells : Cells = Scene.build(t, used)
    now : Nat <- IO.now()
    IO.pure(Image & Cells, Frame.show(cells, old))
```

## Any scene

The pixel never sees the scene. The fork tree opens each node once on the way
down and hands each tile one short flat list; the pixel loop marches that list.
For triangles, voxels, distance fields and sprites alike, the host walks the
world once per frame into per-tile candidates (the primitives, columns or spans
whose screen bounds meet the tile); the device reads only those. A field with no
scene data (a procedural distance field) is the best case: one flat loop per
pixel on a U32 step counter. A per-ray tree walk fails three ways: a fold over a
tree is a non-tail recursion (a frame per node), the lanes of a SIMD group
diverge, and a `+` tree pays an atomic per node per pixel. A reported case: a
voxel world as a `+Data` quadtree of 32x32 columns, read per ray: 176 ms a
frame, against 24 ms for the same picture computed per pixel with no scene data
(the reporter's machine). The fix: keep the editable map on the host, and hand
each tile a flat list of the column words its rays cross (a 32-high column is
one U32 of bits) each frame. An `Array` has one owner, so it cannot go down a
fork tree: build lists.

## Data

- Records of scalars ride in registers through every call and fork; a boxed
  record shared by every vertex is an atomic count per use. `Tri` is 24 flat
  words (barycentric, depth and colour planes, mode, alpha, box): one F32 per
  64-bit word, ~200 B loaded per entry per walk.
- A wide record round-trips the stack: a 52-word record per list entry made the
  draw 6.3 -> 12.1 ms; a 22-word camera on every mesh call, +2-4% build. Build
  wide data where used (`Hook.cam(F)`).
- A def or continuation takes at most 247 words ("an arity over 247"): four
  15-field records in one parallel let fail to compile. Wrap a big header in a
  constructor of the tree it heads.
- Use the typed `pick(c, a: F32, b: F32)` and `word(c, a: U32, b: U32)`; the
  generic `Bool.pick` boxes its words.
- Tables are `word` chains and Bool-column matches. A `match` on U32 literals is
  a bit-by-bit tree with the default arm copied per branch: +21% host C, +6.4%
  `.gpu`, 1-4% slower.
- Pass a constant as a `~` template argument (a def like `Screen.w()`), not as a
  parameter: a parameter rides in every task.
- Tessellate in screen space: ~20-px patches; a circle gets `max(4, ceil(pi
  sqrt(r/2)))` segments. Slivers and sub-pixel triangles pass every box test and
  pile 100-400 entries on a tile (42 ms frames); exact triangle-vs-cell overlap
  holds the worst tile at 80-110. Length past that is not the cost: a cap of
  16/32/64 entries per tile changed nothing.

## Ownership

The compiler decides borrows; `+` does not. The checker needs `+` for a second
use. The compiler borrows a boxed parameter (not an `Array`) that the def only
matches or passes to a borrower: every read is `term_peek`, no count. It owns
one that the def returns, stores in a constructor, or passes to an owner. An
owned use of a value that is used later shares it: a count on the value and,
since sealing is per type, on every node of that type, silently. So `+ts` above
borrows: `Blk.go` only matches, `Tile.fin` consumes last. So the scene is
host-owned, borrowed by the bang, and returned (`Frame.show`). Read through
counts, the frame costs 8.0 -> 16.5 ms (draw 6.0 -> 14.1).

When a fork is the last use of a locally owned box, its readers take counted
ownership together, so recursive readers free nodes as they walk rather than
leaving one whole-value sink at the join. All sibling readers must own a
reference before that handoff; giving ownership to only one could invalidate
another's borrow. A value needed after the fork retains its original owner.

- Two consumers of one list share it: 45 ms frames and a 448 s Metal compile. A
  `+` where one use would do heats its type (symreg 2.83 -> 3.30 s).
- A def that returns its argument shares it: `Bool.pick(Cloth, c, a, b)` on a
  tree gave 14 keeps and a hot type. Match the selector and recurse into one
  field (`Cells.put` has eight arms for this). Refolding a matched `+` record at
  a call is an owned argument (8-16 keeps): pass the fields.
- The last reader of a borrowed list needs a later use, else it becomes the
  owner: `Tile.fin` walks the lists after the pixels read them (as dead
  parameters the runtime's sink is 0.55 ms slower).
- A bang inside a `do` continuation flips its callee to owner (the tree is a
  captured local): 2x reads. Bang in a pure def that returns the pair.
- Drop the last image inside the fork tree (a dead `Four` per tile) and the last
  scene on the host by a walk (35% faster than the runtime sink); never inside a
  bang: 12.9 ms. `_ = x` drops a value on the spot.
- Never let the device own the scene: draw 6.2 -> 12.0 ms.

Verify in the emitted C (`bend x.bend -o x.c`): in a def's body, `term_peek` is
a borrow; `term_keep`, `ctr_take` and `rfc_seal` are counts. A new keep in a
device def is a regression.

## Cost model

- Mesh, project, light and cull on the host: 0.6-1.6 ms in parallel lets; on the
  GPU 14-33 ms (a P-core is ~75x a lane on serial work).
- Leaves are flat loops over cons lists; joins are constructors. Unroll the
  squares: `Tile.b16` = 4 x `Tile.b8` = 4 x `Tile.b4`; folded into one recursive
  def they made the draw 5.5 -> 12.6 ms.
- Walk the list once per 4x4 square, not per pixel or per 2x2 block: 6.6 -> 5.6
  ms. Anything that depends on x alone, once per column pair: sky per pixel 3.8
  ms, per block 1.6, per column pair ~1.0 less.
- The draw is throughput-bound, not tail-bound: splitting heavy tiles +0.03 ms,
  32-px cells +0.85. A per-lane early-out saves nothing while a SIMD sibling
  runs the body.
- Drops: `Image.drop` forked at every `Qua` (~100 ops of task machinery per
  node), 2.6 ms; base's `Image.free(7n)`, seven levels then a sink, 1.6 ms;
  inside the draw's tree, 0; a whole image sunk on one lane, 19.8.
- Host: one parallel let per def (a second costs an iteration: build 2.32 ->
  1.93 ms); every extra branch or level ~0.1 ms; a 0.3 ms job loses to the pool
  wake-up; a heavy branch serializes a 16-ring chunk (1.2-1.6 ms): split it.
- CUDA: a scene built by the host and read by the device faults each page over
  PCIe, both ways: 17-19 FPS on an RTX 4090 (25 ms `gpu_pass` + ~21 ms host
  turn) for the frame the M4 draws in 8. Keep a whole frame on one side of the
  bus.

## Measuring

Probe as the game does (`SLASH_PROBE=1200`): scripted frames headless, each
stage's microseconds as medians after warm-ups. Single-run medians vary 0.3-0.5
ms across days, and the second binary of a GPU pair runs ~7% faster: compare by
3-4 interleaved 1200-frame pairs in both orders, plus an A-vs-A pair. Check
frames pixel-identical (`SLASH_DUMP=T` prints one as a tree) before trusting a
gain. Count the keeps in the C and the `.gpu` bytes (+6% was a real 1-4%
slowdown). Probe without audio: it wakes `audiomxd` at 70-100% of a core.

## Do not

1. Read the scene through counts: 2x per read, 8.0 -> 16.5 ms.
2. Fold per level, or bang twice a frame: 600 ms against 8.
3. Recurse non-tail in the bang, or fold a tree per pixel: draw 5.5 -> 12.6.
4. Mesh on the GPU: 14-33 ms against 0.6-1.6.
5. Fork per pixel or per `Qua`: drop 2.6 vs 1.6 ms.
6. Leave slivers and sub-pixel triangles: 42 ms frames.
7. Wide records through frames and joins: 2x per entry.
8. `match` on U32 literals: +21% C, +6.4% `.gpu`.
9. Generic `Bool.pick` on words or trees: boxed words, hot types.
10. Free the scene in a bang (12.9 ms) or the image on the host (milliseconds).
11. Trust one run, or the CPU pool (the 600 ms frame took 11 ms there), as a
    device measurement.

## Reference

`demos/app_slash_boss_3d/bend3d.bend` is the library; its comments are these
rules at their sites. `main.bend` beside it is the app, with `Play.loop` and the
probes.
