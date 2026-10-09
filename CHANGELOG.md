# Changelog

Each release names what changed for a user. `bend update` installs the
latest one; the GitHub release carries the same notes.

## Unreleased

- Native windows emit `Text{text: String}` after key-down when the OS
  supplies characters, preserving Shift, Caps Lock and the keyboard
  layout without changing `Key` codes (#1019). Linux uses an X11 UTF-8
  input context; macOS uses `NSEvent.characters`.
  A missing Linux UTF-8 input context is reported as `ENOTSUP`.

## 2.0.36 (2026-10-07)

- **HOC's proving agent is now named BendAI**: `bend login` logs in to
  BendAI at bend-lang.com/bendai and writes its key to
  `~/.bend/bendai.json`; a key an older bend wrote moves there at the next
  `--publish <name>@…`, so it keeps working. The publish notice links
  https://bend-lang.com/bendai/terms#s18, and the installer's card ends
  "You're now a Bend developer." The old addresses redirect.
- **Every channel and socket wait has a `try_` twin** with a limit in ms
  that answers `Poll` (PR #1259 by nicolas-abril), and UDP gets the byte
  twins TCP and File already have: `send_bytes_to`, `recv_bytes_from` and
  their `try_` twins (PR #1359 by kbrianps).
- **BendTT's kinds are quantity terms**, with meets, Σ at any kind and
  subsumption under ∀, and `--verdict` emits a generic def once (PR #1203
  by nicolas-abril); a λ+ converts as a λ (#1189, PR #1224 by
  nicolas-abril).
- **Faster checks**: a name lookup is one map read, not a scan of the book
  (PR #1372); types never read, or already reduced, are not reduced again
  (PR #1362); roots share their `~` constants, so a template goes to the
  kernel once (PR #1358 by chelokot); n instances of a def take O(n log n)
  characters of names, not O(n²) (PR #1350 by chelokot); a graph closure
  is linear in the graph (PR #1298 by moorbrook).
- **Fixes**:
  - A value past 2^24-1 live copies fail-stops with its own message, and
    WONTFIX names the cap (PR #1366 by nicolas-abril).
  - A type under a rewrite stuck on an open proof is read as its body, and
    a type the C facts can't read is a compile error (#1273, PR #1276 by
    nicolas-abril).
  - A constructor's fields compile with their declared types, so a Word
    folded out of a U32 literal emits (#1100, PR #1361 by nicolas-abril);
    datatypes of boxed constructors are registered (PR #1351 by
    vicmcorrea); spare nodes stay apart across sum arms (PR #1279 by
    jasisz); C emits right when a native effect calls back into Bend (PR
    #1286 by mizchi).
  - The work pass after a grow runs each lane's own ring (#925, PR #1132),
    and on MSL 3.2+ `heap_free` reads the error flag with a coherent
    volatile load (#1143, PR #1299 by nicolas-abril).
  - `--verdict` checks what bend checks for a match carrying a later
    variable (#1292, PR #1293 by aaaxn), respects Unit provenance and word
    constructor families (PR #1338 by oxura), verifies the Lean version and
    drops unversioned kernel caches (PR #1339 by oxura), and cleans up its
    temporary input when the kernel build fails (PR #1327 by Osraka).
  - Unsafe dependencies are tracked in datatype kinds (#1205, PR #1220 by
    oxura).
  - A nested match on a split value is refused with its name and where to
    match it (#1125, PR #1290 by nicolas-abril); only a statement
    `a[i] <- v` is the write let, so `Array.set(..);` in an inline match
    keeps the next row (#1126, PR #1289 by nicolas-abril).
  - `F32.read` skips leading whitespace (PR #1335 by vicmcorrea); the
    formatter keeps unsafe declaration suffixes (PR #1296 by vicmcorrea).
  - A CUDA probe failure names its stage and the Driver API error (PR
    #1341 by oxura).

## 2.0.35 (2026-10-03)

- **M1 and M2 run the GPU again** (#1154, PR #1274 by nicolas-abril): since
  2.0.29 Apple's compiler died building the GPU program of any program with
  a `!` on M1 and M2 chips; M3 and M4 were never affected.
- **A library's names stay its own** (#1045, #1124, #1184, PR #1156 by
  nicolas-abril): a name the entry file declares as `lib.None` no longer
  takes the key of `lib.bend`'s own `None`, so its foreign `CID(None)`
  means the constructor its Bend code means, and C no longer takes the
  wrong arm or frees into the wrong size class.
- **Less memory on the CPU** (PRs #1171, #1269 by nicolas-abril): the host
  grows fewer tasks up front and splits a drain into finer units, so
  parallel tree-matmul peaks at 14.5 MB instead of 68.5 MB and kmeans at
  10.8 MB instead of 22.9 MB, with every bench as fast or faster. A spin
  called in a jump's argument frees the variable it was lent, so a loop
  over a list no longer keeps the old list's cells (81.6 MB to 1.6 MB, PR
  #1243 by mizchi). U32 `/` and `%` divide directly on the host and CUDA;
  only Metal keeps its workaround (PR #1277 by nicolas-abril).
- **Arrays of equality proofs** build in JS and C (#1130, PR #1219 by oxura).
- **A deadline or a timer fires while computations keep the loop busy**
  (#1122, PR #1245 by nicolas-abril): `IO.within` and `IO.sleep` no longer
  wait for a channel pipeline to end.
- **Fixes**:
  - A chain of more than about 86 U32 or 130 F32 operators builds in C
    (#1123, PR #1239 by nicolas-abril).
  - `bend f.bend -o f.c` builds at `-O0` with clang 19 and later, for a
    debug build (PR #1074 by aldeni).
  - A type with deeply nested sums compiles without hundreds of MB (#1270,
    PRs #1196 and #1244 by vicmcorrea).
  - A template with many instances names them in linear time, not
    quadratic (#1271, PR #1275 by nicolas-abril).
  - `IO.random_u32` answers `Fail` on the JS lane, as on C, instead of
    ending the run (#1146, PR #1198 by Yi-111-a).
  - A match on a binder a constructor column closed says so and lists the
    causes (#1134, PR #1226 by nicolas-abril).
  - `bend ... | head` keeps the command's exit status when the reader
    closes early (#1211, PR #1214 by oxura).
  - `--verdict --publish` is refused instead of publishing unchecked
    (#1191, PR #1199 by vicmcorrea).
  - `--publish` drops a leading byte order mark, so a package whose LICENSE
    was saved with one imports (PR #1188).
  - `--verdict`: model search is bounded (#1181, PR #1218 by oxura);
    recursive groups follow live references only (#1212, PR #1215 by
    oxura); a dead fallback past the last constructor checks (#1204); an
    empty datatype goes out with `D.efq : D -> <>` (#1183); a specialized
    argument is elaborated at each use (#1168, #1178); a match inside a type
    goes at the goals bend2 checked it at (#1157).

## 2.0.34 (2026-09-28)

- **A shared graph is compared once** (#1071, PR #1151 by Giulio2002):
  when conversion proves two share cells equal, the second points at the
  first, so a value used twice on each side is compared once, not walked
  as a tree. Two Merkle roots of depth 32 over a symbolic leaf, proven
  equal by `{==}`, check in 0.07 s (they took 2^32 steps). `--verdict`'s
  kernel does not share yet and runs out of fuel on such a proof.

## 2.0.33 (2026-09-28)

- **Two copies of one term are equal before either unfolds** (#1071, PR
  #1151 by Giulio2002): a conversion first compares both sides with no def
  unfolded, then as before. A law proven by induction and used at a fixed
  size, like `agree(32n, x)` against its written type, checks at once
  instead of walking a shared chain as a tree of 2^32 steps.
- **Base opens a value before it copies it**: `U32.min`/`max`, the `F32`
  helpers, `Char.to_upper`/`to_lower`, `U32.div`/`mod` and `List.sort` match
  their argument first, so a stuck argument stays one call (#1075).
- **Base is smaller** (PR #1153 by nicolas-abril, from #1059 by
  jnadeau207-collab), and `U32.log2` takes five native shifts instead of
  thirty-two.
- **Fixes**:
  - `U32.to_nat` widens to u64 in C, so Nat arithmetic on a u32 local no
    longer wraps at 2^32 (PR #1142 by Giulio2002).
  - A shared Array's redirect is read without a race and without a device
    atomic (PR #1155 by nicolas-abril).
  - `TCP.recv`, `TCP.recv_bytes` and `TCP.poll` with a max of 0 fail with
    EINVAL, not a closed peer's `""` (#1121, by aldeni).
  - Emission does less work per word type and nullary constructor (#1056,
    by jnadeau207-collab).
  - `--verdict`: the kernel puts a λ argument into a type annotated with
    its domain, so a `+` let of `Equal.cong` over a function checks (#1158).
  - A pure main shows an Array element and a flat value of one type each
    by its own layout (#1166).
  - A `CID(k)` in an effect source's comment or string is left alone
    (#1161, by aldeni).

## 2.0.32 (2026-09-27)

- **One verdict: `ALL PROOFS CHECK` or `SOME PROOFS FAIL`**: `bend f.bend`
  on a file with no main (or `--check-only`) prints one of the two. A proof
  holds when bend checks it and it uses no `@unsafe` def and no user foreign
  code, imports included. `--verdict` (was `--safe`) also rechecks every
  def with the proven BendTT kernel; it no longer writes `f.bendtt`, and
  `-o f.bendtt` does. Function-typed terms go to the kernel η-long.
- **Breaking: `TCP.listen` and `UDP.bind` take the address to bind** (#1088,
  PR #1098 by oxura): a server no longer listens on every interface.
- **Breaking: `IO.args()` starts with the program as invoked** (#935), as C's
  argv does; the arguments start at index 1.
- **`IO.within` races an action against a deadline** (#1034).
- **`TCP.send_bytes` and `TCP.recv_bytes`** carry bytes as they are (#846).
- **`-o f.mjs` writes an ES module** of a Bend file (#1029).
- **Windows**: `Window.grab` holds the cursor for a first-person camera, and
  the mouse's motion comes as `Look{dx, dy}` (#921, PR #1073 by
  nicolas-abril); `Scroll{x, y, dx, dy}` events come from the wheel and the
  trackpad (#1020, PR #1114 by oxura); macOS input no longer lags (#842);
  Shift+Tab on X11 gives the Mac's back tab; the Linux window fills a frame
  by squares, about 6x faster (PR #1115 by costamatheus97).
- **Fixes**:
  - A native intrinsic on a nullary def keeps its result layout (#1093,
    PR #1094 by chiliec).
  - One file is one module however an import spells its path (#1087,
    PR #1103 by MattCozendey), and an alias that matches the file name
    works (#1082).
  - F32 text rounds once to the nearest f32 on every lane and in literals
    (#1055); `F32.pow(±1, y)` is 1 on JS as on C (#1060).
  - A pure main that prints a datatype through a family field builds (#1067).
  - Two defs with the same body are equal (#1028).
  - A fallback arm past a datatype's last constructor is dead code (#1091).
  - An error names a hub def the way you write it (#965), and a `+` binder's
    error names the right binder (#980).
  - A compound type argument without parens is a clean parse error (#1110).
  - A shared Array's redirect reads cannot race its count (#975).
  - Timers wake in deadline order; clang's version probe no longer fails
    under load.
  - `--gpu on` names the reason a CUDA GPU is unusable (#1064).
  - A JS host tag the type lacks is a clean error (#1105).

## 2.0.31 (2026-09-27)

- **`bend` help: one aligned line per command**: a table builds the list,
  so every description starts in one column; `--publish [<name>@<version>]`
  is one line, and `bend guide` is the last command.

## 2.0.30 (2026-09-27)

- **`bend f.bend --safe` rechecks a file with a proven kernel**: after
  bend's own checker, it translates the file to BendTT (`f.bendtt`) and
  checks that with `bend2/bendtt.lean`, a small kernel with a Lean proof
  that no def it accepts has type `Empty` and that live code halts. The
  first run builds the kernel with Lean v4.34.0 (elan's toolchain, or
  `$BENDTT` names a built one). `@unsafe` defs stay out of scope, and
  `--safe` lists them. `-o f.bendtt` only writes the translation.
- **The kernel has full J**: a rewrite's motive can name the evidence.
- **base.bend**: the `Array.get`, `Array.swap` and `Map` helpers recurse on
  their own pieces, so the kernel checks them; a few `.if`/`.bit`/`.deep`
  helpers and five laws are gone.
- **The BendTT paper** (`paper/BendTT.pdf`) is rewritten for the new kernel;
  `bend2/bend.lean` is gone, and `bend2/bendtt.lean` is the only Lean file.

## 2.0.29 (2026-09-26)

- **The JS lane runs about 2.3x faster** (PR #1061 by nicolas-abril, and a
  Nat that is a JS number): a Nat is a double, exact below the 2^48 - 1 cap
  the C lane shares, and BigInt only where a value crosses to the host; a
  tail cycle is a loop, and a def calls another directly unless the callee
  can bounce. The JS lane recurses at least as deep as before. A Nat that a
  host passes in (negative, past 2^53 or not an integer) now fails with the
  C lane's Nat message instead of printing garbage.
- **Errors underline their span** (PR #1063 by nicolas-abril): a location
  marks the exact text, and a non-inferrable term is no longer echoed.
- **A checked recursion on a Nat literal is linear** (#983): the descent
  check no longer takes 2^n steps on a literal like `30n`.
- **A constructor of any width builds on C** (#991, PR #1068 by
  nicolas-abril).
- **Compiled binaries pass `--help` to `IO.args`** (#934, PR #988 by
  YidaWeng); the runtime's own help is `--bend-help`.
- **Fixes**: a boxed Bool from a generic pick reaches `Bool.or` as a flat tag
  on C (#1026, PR #1038 by vicmcorrea); `Process.run` stops at the child's
  exit even when a descendant holds its pipes (#1051, PR #1054 by
  Yi-111-a); `TCP.listen`'s backlog is 512, so a burst of 10k connections is
  answered in full (PR #977 by aldeni).
- **Simpler compiler and checker, same output**: the compiler's types go
  from 31 to 16 and the C runtime's type names from 26 to 14, with one
  atomic family for the host, Metal and CUDA; the parser reads operators
  from one table (parsing 7-22% faster, checking 2-6% faster). A file that
  ends in `<` now says "expected a term". Tested on macOS (Metal), Linux
  x86-64, and CUDA from Pascal to Blackwell.

## 2.0.28 (2026-09-25)

- **The macOS `bend` has a valid signature** (#1025): `bun build --compile`
  left the hash of the binary's last page stale, so every macOS release since
  2.0.8 failed `codesign --verify`, and a Mac that checks that page killed
  `bend` at launch with SIGKILL. The release now signs the macOS binaries
  again, ad hoc, and verifies them before publishing.
- **A package name is 1 to 64 characters** (#1053): the CLI asks the hub
  about a short name instead of refusing it, so `--publish json@…` prints the
  hub's answer (a name under 12 characters is won at auction on
  hub.bend-lang.com) and `import std@1.0.0.0/…` resolves once the name has a
  version.
- **Names and namespaces cannot collide** (#1042; closes #994, #989, #1002,
  #1005): a declared name is words joined by dots; a file's namespace is its
  real path, so a local file cannot take a hub package's `0x<hash>`
  namespace nor register names in another file's; only a `0x<hash>/` or
  `name@version/` import goes to the hub. A name declared twice, a
  redeclared Base name, a clashing alias and an effect registered twice are
  refused. **A JS effect registers with `io_eff(CID(Name), run, need)`, as a
  C effect does**: effect files written for 2.0.27 need that change (see
  `bend guide effects`).
- **An unsafe fill in an imported file is listed** (#1001, PR #1033 by
  costamatheus97): the verdict walks from every law, wherever it is filled,
  so a law filled by `@unsafe` code in a helper file no longer passes as a
  clean `All terms check.`. Exit codes are unchanged.
- **`Process.run`** (PR #1030 by oxura) runs a program directly, without a
  shell: literal arguments, UTF-8 input, bounded output and a timeout, and it
  answers the status, stdout and stderr, on every lane.
- **`IO.thread_count()`** (#971, PR #1048 by aldeni) answers the native worker
  pool's size (`--threads`, or the CPUs the process may use, up to 128), and 1
  on the JS lanes.
- **A checked `Nat.read` finishes** (#1008, PR #1009 by jkbennitt): a law like
  `{Nat.read("7") == Some{7n}}` no longer compares each digit against a unary
  2^48 - 1.
- **Fixes**: the `.bend` loader registers in Node worker threads (PR #992 by
  vicmcorrea); an empty `UDP.send_to` sends on Bun (PR #998 by vicmcorrea); a
  signal that interrupts the event loop's `select` wakes nothing, on both
  lanes (PR #1036 by aldeni); rebuilding a list in C no longer leaks 16 bytes
  (#970, PR #987 by YidaWeng); an effect `.c` that says `undefined` in a
  comment builds (PR #1049 by aldeni); Metal names the device's limit when
  `--gpu` asks for more (PR #1047 by MattCozendey); the effects guide gives
  the JS park its deadline (PR #1050 by aldeni).
- **Simpler checker and effects, same output**: every literal is one node
  that carries its Base type (PR #1004 by MattCozendey), three one-use C
  helpers go (PR #981 by tachytelicdetonation), `Nat.read` checks its bound
  with three helpers instead of eight, and the JS show escapes a surrogate as
  C does (PR #943 by This-Is-NPC).

## 2.0.27 (2026-09-23)

- **`bend` reads no `bunfig.toml` or `.env` from the directory it runs in**
  (#1018): the executable was a Bun program built with Bun's defaults, so a
  project it checked could preload its own code before bend's (and print a
  forged `All terms check.`), or set `BEND_HUB`, `BEND_ORIGIN` or `BEND_LIB`
  for you: your BendAI key went to its server on `--publish <name>@…` and
  `bend link`, `bend update` ran its script, and its own copies of hub
  packages, named ones included, were checked in place of the real ones. The
  release is now built with that loading off (`package.json` and
  `tsconfig.json` too), and the ping gate runs the installed `bend` in such a
  project (#1023). `bun bend2/main.ts` from a checkout still reads both, as
  any Bun program does: check a project you did not write with `bend`.
- **A package can carry a license** (#1013): `--publish` takes every file
  named exactly `LICENSE` beside a published file, at the same path, and the
  hash covers it. A package without one is MIT-0 under BendHub's terms, and
  the publish warns so. Every publish first prints, on stderr, that the hub is
  public and permanent under https://bend-lang.com/bendai/terms#s18, and the
  license the hub will show: the `SPDX-License-Identifier` of the shallowest
  `LICENSE`, else `see <path>`. A directory named `license` in any case is
  refused before mining, since it clashes with a `LICENSE` on a disk that
  ignores case. A `LICENSE` added to a published package changes its hash:
  publish it as a new version (bend-tensors@0.0.0.2 is 0.0.0.1 plus MIT).
- **Requests to the hub and bend-lang.com carry `User-Agent: bend/<version>`**
  (#1013): publish, publish-check, link, name lookups, package reads, the
  login and the daily check, so the hub's log shows which bend sent each.
- **Datatypes name each other in any order**: every datatype is declared up
  front, so two datatypes (a `Tree` and a `Forest`), or a def above the
  datatypes it returns, need no forward law. A forward `D<..>` spells every
  parameter. Base's `Word`, `Pair` and `IO` lose their laws.
- **An `@unsafe` def may call a def written below it**, and `def f?(..)` is
  sugar for `@unsafe def f(..)`, so two mutually recursive unsafe defs need no
  law. Safe code is as strict as before: a live call to a def below, or a
  mutual pair, is refused as an unfilled law, which is how a forward
  reference is now reported (it was an undefined name).

## 2.0.26 (2026-09-23)

- **A package has a name on the hub** (#996): `import <name>@<version>/file.bend
  as P` asks hub.bend-lang.com once what the name and version name, keeps the
  answer under `~/.bend/lib/names`, and loads the package by that hash as
  before, so a version never moves and a cached name works offline. `bend
  <file> --publish <name>@<version>` publishes and names in one run, after
  the hub confirms the name is yours or free and the version goes up; `bend
  link <name>@<version> 0x<hash>` names a package already published; `bend
  login` logs in to BendAI for both. A name is a-z, 0-9 and -, 12 to 64
  characters; a version is four numbers like 1.0.0.0. The first:
  `import bend-tensors@0.0.0.1/bend_tensors.bend as T`.
- **The effects guide calls `io_node` and `io_wait_on` as the runtime
  declares them** (#947): `io_node` takes four arguments and `io_wait_on`
  five, the fourth an absolute `io_tick()` deadline, 0 for none, so an
  effect written from the guide compiles.
- **A second book compiled in one process starts from a fresh probe list**
  (#976): the bun loader and `io_run` no longer retain the binder variables
  of every previous compilation.
- **Simpler compiler, same output**: the channel runtime lives in
  `effs/chan.c` and `effs/chan.js` beside `Chan.new`, `send`, `recv` and
  `close`, so a program carries it only when it uses a channel, and both
  match emitters share their table and arms; emitted C and JS are
  byte-identical except the channel programs' requests.
- The README links the standalone `bend2-lsp` (#953, by don2e4).

## 2.0.25 (2026-09-21)

- **A literal is a `Nat` or `String` by name only where the datatype is
  Base's**: a file that declares its own `Nat` checks a literal
  structurally again, so `type Nat: Succ{e: Empty}` no longer admits `1n`
  and a closed `Empty` (#941). An array count past the nat cap is refused
  instead of making a fractional literal that defeats termination (#954).
- **A wide record compiles**: any field list past 255 words has its
  multi-word fields boxed, for constructor layouts, nodes and def
  signatures alike, so a 512-word record no longer dies "an arity over
  255" (#944). A recursive datatype hidden behind a type family is boxed
  instead of overflowing the compiler (#959). A datatype named
  `__proto__`, `constructor` or `toString` compiles (#948). Still open: a
  join holding several wide results, or a wide value held across a
  non-tail call, is refused with the same message.
- **An annotated lambda or match applied where it stands compiles** on both
  lanes, as its let-bound form did (#956).
- **A read parked on a FIFO sees its end on macOS** (#928, PR #932 by
  PedroVIOliv): both IO loops wait with `select`, since Darwin's `poll`
  never reports a named pipe's close.
- **A foreign effect's scheduling helper cannot be overwritten** by an
  effect named `X_need`, in either discovery order (#946, PR #951 by
  tachytelicdetonation).
- **A generated C local carries a `_` prefix** (PR #926 by nood-co1), so a
  host macro such as macOS's `ts_32` cannot capture it; emitted C grows by
  1 to 5 %.
- **The Node and Bun loaders report unsafe and foreign dependencies** on
  stderr, as the CLI does (PR #933 by vicmcorrea), and two books compiled in
  one process no longer share layout memos (PR #961 by vicmcorrea).
- **Simpler compiler and effects, same output**: the layout packer assigns
  offsets once (PR #945 by tachytelicdetonation), the array intrinsics share
  one cell path (PR #955 by PedroVIOliv), the facts fixpoint compares set
  sizes (PR #960 by byronbenharris), the JS emitter keeps one descriptor per
  native constructor (PR #952 by ramonzx6), one `BEND_RTC` macro serves the
  device compilers (PR #963 by costamatheus97), and the file and audio
  effects share one source each (PRs #950 and #939 by tachytelicdetonation
  and tontontimiro).

## 2.0.24 (2026-09-21)

- **A string or nat literal is one `Lit` node in the checker** (PRs #907 and
  #924 by MattCozendey): a literal unfolds one constructor at a time when it
  is compared, matched or checked, so 50 defs of 1000-char strings check in
  0.14 s and 74 MB instead of 4 s and 2.6 GB, a 200k-char literal checks
  instead of overflowing the stack, 2000 defs of `200n` check in 0.18 s
  instead of 1.06 s, a self-call on a nat literal past 256 passes the
  termination check, and `1n+0n` is `1n`. The compiled output is unchanged.
- **The device hands no leaf off**: a fork-free leaf reached from a forking
  def inside a bang runs on heap continuations on the GPU again, as in
  2.0.21, so a `do Result` loop of hundreds of turns under a parallel tree no
  longer dies with "memory fault". The host keeps PR #876's handoff and its
  gains; symreg on Metal stays at 0.32 s (#930).

## 2.0.23 (2026-09-20)

- **`Array.map` walks the block**: Base's map reads each cell and writes the
  result into a fresh array instead of splitting and rebuilding the tree, so
  16M U32 map in 15 ms instead of 176 ms at a fifth of the memory. Its
  elements are `Data` now; a map over affine elements is written from the
  tree by hand (#911, #913).
- A template refuses a second `~` binder of one name, in a def or a law's
  `for ~T` clauses; the two became one opaque constant in the generic check
  and let a closed `Empty` through (#905).
- The C lane heats a stuck family's type argument at every instantiation, so
  a record carried through `F(n, RT)` is opened as the record it is (#916).
- Inside an imported module, a local named like one of the module's own defs
  binds, in a let, a `+` let, a pattern, a `+` pattern and a lambda (#915).
- A right spine of forks under `!` runs on the GPU at any depth the cores
  take: the device grow pass no longer stops after 128 turns (#918).
- The JS lane names a def from a hyphenated or absolute import path legally,
  `--checkup` opens an absolute import as the run does, and `-o out.cjs`
  emits the CommonJS program (#904, #906, #908, #910).

## 2.0.22 (2026-09-20)

- **An `@unsafe` def forks an array**: `Array.fork` gives two handles to one
  block, `Array.join` merges them back, and `Array.atomic.*` (add, sub, and,
  or, xor, min, max, cas, fadd) act on the shared block from the cores and the
  GPU. A match on a shared handle copies its part, as a clone does (#885).
- Two `@unsafe` defs recurse into each other through their laws: an unsafe
  body may call a law that is not yet filled, as it may call itself without
  descent.
- A typed let, `x : T = v`, binds `x` to `{v : T}`; a let with a pattern
  takes no type (destructure in the body).
- A `do` block of one statement is typed by its header, and the header's
  leading quantities are filled once for `bind`, `pure` and the annotation:
  `do Result<String, U32>:` with a bind now checks (#900).
- A word match compares the whole word: a string, char, U32 or F32 literal
  pattern is one equality and its default one else, so three string arms
  compile to 291 KB of C, not 16.7 MB (#892).
- An Array cell is its element datatype's open layout, so a generic body over
  `Array<Boxed<A>>` and its callers agree on the block class; the C lane no
  longer takes the ANode arm for a leaf (#893).
- A node shared through a family with two or more indices is opened with
  `ctr_take` on the C lane, instead of read and freed as owned (#901).
- A C table's F32 row is the constant's own bits: a signalling NaN keeps its
  payload (#897).
- A constructor refuses a repeated field name; the JS lane keyed both fields
  on one property (#899).
- A module imported through `../` or a dot directory works inside an annotated
  operator: an operator is the name whose only dot leads it (#903).
- A GPU out-of-heap reports at once instead of after seconds of aliased
  allocation, a `--gpu` span under the fixed region fails with its message
  instead of a segfault, and a lane's stack ends at the static image: its
  2049th word no longer overwrites a constant (#889).
- Fork-free leaves run sequentially and CPU ring work is dealt across workers
  (PR #876 by nicolas-abril): binarytrees 0.31 → 0.20 s and symreg on the GPU
  0.56 → 0.32 s on an M4 Max. On the GPU a fork-free leaf reached from a
  forking def now runs on its lane's 2048-word stack, as a fork kid does.

## 2.0.21 (2026-09-20)

- A template instance that calls back into an instance whose body is
  still being checked is refused as a self-call that does not decrease:
  `loop(~k, u) = bounce(~loop(~k), u)` with `bounce(~f, u) = f(u)` once
  checked, and inhabited `Empty` (#902).

## 2.0.20 (2026-09-20)

- A `U32` match whose arm is a hand-written bit pattern answers that arm:
  since 2.0.19 the lookup table filled its gaps with the last default, so
  `case U32{WCon{True{}, r}}` (every odd word) between literal cases read
  the `_` case on every lane (#867).
- An erased let binds erased names only: `-y +z = a b` is a parse error at
  the `+`, not a let that erases the `+z` it was told to keep.
- The arena's page cap is published with a release store and read with an
  acquire load, so a core that sees the new cap also sees the banks at
  their new place (#881).

## 2.0.19 (2026-09-19)

- A Bend binary starts in 2 ms, not 12: the runtime reserves 8 GiB and
  grows it in place when a program needs more, instead of mapping the whole
  8 TiB address space at every run (#881).
- A record of records compiles: a datatype past 256 machine words is a heap
  node, the way a recursive type already was, and a constant the compiler
  folds emits once. Seven levels of an eight-field record took 50 s, 19 GB
  and 97 MB of C; it now takes 0.06 s, 122 MB and 82 KB (#843).
- A `match` on dense `U32` literals compiles to a lookup table, as one on
  `Nat` already did: 256 cases took 1.46 MB of C and 3.5 GB, and now take
  80 KB and 1.6 GB (#867).
- The Metal lane computes `sin`, `cos` and `tan` with the GPU's fast trig
  (#887).
- A `-` local is erased again: its name is dead in the body, and its value
  is checked dead, so it may spend a variable twice. Since 2.0.16 the mark
  was lost on both counts.

## 2.0.18 (2026-09-19)

- A dot inside a field name is a character on the JS lane too: `Outer{a:
  Inner, a.b: U32}` read its `Inner`'s field, not its own (#868).
- A value sent through a channel is never read as a parked receiver on the
  JS lane: sending an erased proof reported a deadlock (#871).
- A name the compiler encodes itself is refused, not miscompiled: no file
  may define `Clo.apply`, and a file without `import Base` that declares
  its own `Nat`, `Bool`, `Array` or another of base.bend's types checks and
  runs, but does not compile (#870, #875).
- The verdict names the defs that rely on a foreign def, as it names the
  ones that rely on `@unsafe`: the checker reads a foreign def's type,
  never its code (#874).
- A datatype whose arguments are written in `{}` says to write them in
  `<>` (#864).

## 2.0.17 (2026-09-19)

- **Breaking: an operator takes its type from the `( .. : T)` around its own
  expression, and from nothing else.** The annotation no longer reaches an
  operator inside a call argument, a lambda body, a constructor field, a list
  element, a match arm or a `~` argument, and a bare operator is no longer
  read as `Nat`: write `(a + b : Nat)`. The error names the repair.
- **A `~` template is a definition, and an instance of it is that definition
  at its `~` arguments.** Nothing re-reads the template's text, so the checker
  and the compiled binary cannot mean different things by one call. The body
  is checked once, at its definition, against opaque parameters, so a template
  is a theorem: a `law` may take `~` parameters, a proof may use a hypothesis
  as often as it needs, and an instance no longer counts as unsafe (#848).
- A template that instantiates itself without end stops at the 64th level and
  says so, instead of running the checker out of stack.
- An annotated lambda applied, `{(x => x) : Nat -> Nat}(1)`, is checked at its
  annotation.
- The verdict names the defs that rely on `@unsafe`, following the calls and
  the types, instead of counting the marks. Importing a module that holds an
  `@unsafe` def no longer marks a file that never calls it (#848).
- A nat literal in a pattern is checked against its constructor's arity: it
  was a closed proof of `Empty` (#852).
- A def with no return type that fills no law says which law is missing (#850).
- The C lane seals the fields a hot constructor holds at its own
  instantiation (#853), with five emitter fixes found by a fuzzer (#855).
- `TCP.poll(sock, max, ms)`, a receive with a deadline: a server can drop an
  idle connection (#858).
- `Nat.min` and `Nat.max` are structural, with order laws (#860).
- macOS: the click that brings a window forward reaches the program (#857).
- `bend <file.bend> --check-only` checks a file and its imports, and runs
  nothing (#856).
- `bend version` replaces `bend --version`.

## 2.0.16 (2026-09-19)

- The template memo and the compiler's show table key on the syntax tree
  (`term_key`), not on a printed term: two `~` arguments share an instance
  only when they are the same term (#838).
- A template instance is picked after the enclosing `( .. : T)` closes, so
  its operators carry that namespace (#841).
- A def that is a template instance counts as unsafe: a file with a
  template prints "All terms check, with N unsafe annotations." until the
  checker verifies template expansion itself.
- A template instance is the template's body at its `~` arguments, minted
  and checked by the call, never re-parsed from its text: the checker and
  the compiled program mean the same thing, so a lemma about `M.F(~1n, 2n)`
  is a lemma about what the binary runs. A `~` binder after a plain one, a
  `~` argument past the template's, and a foreign template are refused
  where they are written; a call may omit `~`. `(x = v; x + 1n : Nat)`
  annotates its body's operator again (#848).

## 2.0.15 (2026-09-19)

- `U32` literals as `~` arguments instantiate again (2.0.14 broke them).

## 2.0.14 (2026-09-19)

- Template keys carry no sugar, so NaN payloads that print alike no longer
  share an instance (#838).
- `bend guide shaders` states at the top that AIs wrote it.

## 2.0.13 (2026-09-18)

- `IO.random_u32`, a CSPRNG (#837).
- `File.read_at`, `File.size`, `File.write_bytes` (#823).
- A UTF-8 byte order mark survives on the JS lane (#833).
- A forky continuation that becomes ready during a grow turn runs in place
  (#831).
- A Nix flake: `nix profile install github:bendlang/bend` (#830).
- `bend guide effects` prints a note on the C and JS side of custom
  effects (#825).

## 2.0.12 (2026-09-18)

- Metal: `U32` division and remainder near 2^32 are exact (#824).
- Closure applies allocate no tasks in fork-free code (#832).
- The publisher refuses a file with no name before mining (#835).

## 2.0.11 (2026-09-18)

- Two `Array` shapes with the same leaves no longer share a template
  instance or a show descriptor (#834).

## 2.0.10 (2026-09-18)

- `bend guide shaders` prints "Shaders in Bend"; the guide's Extra section
  points to it.

## 2.0.9 (2026-09-18)

- `--help` is as before 2.0.8.

## 2.0.8 (2026-09-18)

- The installer downloads one executable per platform from a GitHub
  release, verified against a sha256 in the script, and installs nothing
  else. Bend never updates itself: `bend update` reruns the installer.
  Once a day `bend` asks bend-lang.com for the latest version, sending its
  version, OS and CPU type; `BEND_NO_TELEMETRY=1` turns that off.
- `+` on a pattern field is a quantity mark (#810).
- The hub client checks a file against the manifest's hash prefix again
  (2.0.6 refused every package).
- WONTFIX.txt gains a RUNTIME section.

## 2.0.7 (2026-09-18)

- `IO.args` answers the command line (#821).
- A `Nat` literal past `256n` is `U32.to_nat(n)` underneath (#779).
- The C runtime decodes UTF-8 as WHATWG does (#809).
- `String.length` is an intrinsic on JS (#798).
- An unbalanced `Array` traps on JS (#808).
- The verdict reads "All terms check, with N unsafe annotations."
- WONTFIX.txt lists what we will not change, and why.

## 2.0.6 (2026-09-18)

- Hub installs are atomic and pinned (#794, #787).
- The JS lane refuses names it cannot mangle apart (#790, #799) and scopes
  FFI to its file (#800); emit is linear in program size (#785).
- Nullary imported constructors (#815); `Char.to_upper`/`to_lower` on
  control chars (#803); `F32.read` follows one grammar on C and JS (#801).

## 2.0.5 (2026-09-17)

- `CUDA_HOME` names the CUDA install; `lib` and `lib64` both link (#771).
- `$CC` is tried first, then `clang` and `clang-NN` (#773).
- The runtime's reservation halves until it fits, down to 8 GiB (#774).
- comp.ts passes strict TypeScript (#778).

## 2.0.4 (2026-09-17)

- A `!`-free program on macOS builds as plain C (#769).
- The descent error states its left-to-right rule; the guide says it too
  (#770).

## 2.0.3 (2026-09-17)

- A plain parallel call forks on the CPU pool (#767).
- Hub paths are validated before any directory is made (#768).

## 2.0.2 (2026-09-17)

- The Metal bag stays at 128 groups; only CUDA sizes it to the L2.

## 2.0.1 (2026-09-17)

- The guide as revised on launch day.

## 2.0.0 (2026-09-17)

- Bend 2: a new type checker (an affine dependent type theory), a new
  compiler to C, Metal, CUDA and JavaScript, and a new runtime.
