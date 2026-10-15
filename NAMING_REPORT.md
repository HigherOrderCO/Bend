# Simplifying naming/uniform-identities

Branch `naming/uniform-identities-simplify` lives in the worktree `/tmp/w-naming`. Paulo's `naming/uniform-identities` was never touched.

- **First pass:** from `746528dc5` to `79305b95`, plus `657b0990`, which adds this report and is pushed by Unii.
- **Second pass (this one):** 6 local commits on top: 5 of code, ending at `42b8f24e`, then one that updates this report. They are not pushed, and nothing was written to GitHub.

## Sizes (lines / ttok)

| file | main 32d23b434 | start 746528dc5 | 1st pass 79305b95 | 2nd pass 42b8f24e | vs start | vs 1st pass | vs main |
|---|---|---|---|---|---|---|---|
| bend.ts | 3882 / 43,759 | 3998 / 46,834 | 3902 / 45,775 | **3838 / 44,743** | −160 / **−2,091** | −64 / −1,032 | −44 / **+984 (+2.2%)** |
| comp.ts | 6396 / 63,675 | 6324 / 64,345 (over cap) | 6301 / 63,988 | 6298 / 63,995 | −26 / −350 | −3 / +7 | −98 / +320 |
| main.ts | 1015 / 11,230 | 1085 / 12,255 | 1088 / 12,251 | 1086 / 12,214 | +1 / −41 | −2 / −37 | +71 / +984 |
| safe.ts | 1577 / 22,494 | 1555 / 22,184 | 1547 / 22,071 | 1568 / 22,373 | +13 / +189 | +21 / +302 | −9 / −121 |

- **bend.ts:** the branch added 3,075 tokens over main; 68% of that is gone. bend.ts now has fewer lines than main.
- **Diff without the report file:**
  - main → final: 34 files, +1670/−1599 lines (the starting branch had 171 files, +3151/−2216).
  - start → final: 155 files, +908/−1772.
  - 1st pass → final: 6 files, +225/−274.
- **safe.ts grew by 302 tokens this pass.** It takes back main's own lowered-form `subst`, about 120 tokens of it, in place of the shared copier; see point 1 below. The other ~180 are the deterministic kernel names (point 3) and ordinary churn.
- **What the remaining +984 in bend.ts is:**
  - the key helpers: format, file ids, reading keys back, display, C/JS symbol;
  - one resolver with a per-file reading cache;
  - sugar and literals naming their vocabulary file;
  - checked-type transport, now about 60 tokens;
  - the `Op` node, the constructor field-name check, and the instance key and rollback.
  - The branch's `term_map`/`term_show` refactors pay part of this back.

## Second pass: what changed in bend.ts

1. **Checked-type transport without a copier.**
   - *What was removed:* `scope_mapper` (445 tokens), plus `checked_copy`, `term_at`, the `Slot` type and the `slot` field on `Var`, about 720 tokens in all.
   - *How it works now:* the checker's output keeps its types in share cells, and those name the def's binders by level. `checked_higher` = `term_higher(e, null, true)`. In that mode, each cell is lowered at `DEEP` (2^30, past every binder) and raised again in the current env. Its free levels then become this copy's binder values, and its own binders get levels at `DEEP` and above, so they collide with nothing.
   - *Effect:* the copy is fresh, so evaluating it fills no cell of the original.
   - *Instance arguments:* `Def.args` stores the checked arguments as the checker returned them (LTerms). safe.ts raises them where it uses them, so normal checking pays nothing.
   - *Other users:*
     - comp's `scoped_key` lowers at `DEEP` and numbers free probes by binding.
     - safe.ts uses main's lowered-form `subst` again, with a loop instead of `map` callbacks. This keeps it within the stack: a 6000n literal in a no-Base file now verifies, which it did on main but not on the starting branch.
2. **A file reads Base when Base is in the book by the time the file parses.**
   - `book_load` is main's recursive loader again. The discover-then-parse pass is gone (−175 tokens).
   - A file's names and its sugar now follow the same rule: sugar means Base's vocabulary, or the file's own vocabulary when Base isn't loaded yet.
   - This matches how ordinary names already resolved on main and on the starting branch.
3. **Name resolution keeps only identities in terms.**
   - `Ref.raw`, `ADT.raw`, `Ref.issue`, `term_raw` and `parse_find` are gone.
   - An alias/Base ambiguity fails where the name is read, which is main's position and wording; `tests/import/alias_shadow.bend` is identical to main again.
   - `parse_head` re-reads a head (a match label, an operator's namespace) by spelling its key as the file spells it, using `name_display(book, k, file)`. The datatype note in `term_check` uses the same spelling.
   - `+D` and `D<..>` read the key directly.
4. **Generated names no longer use a process counter.**
   - `name_gen` requires a member: a position, or a string that is hashed.
   - safe.ts names each kernel item by its item key, each root constant by its column and type, and each group by its members. Kernel names are now deterministic, and the same entity always gets the same name.
5. **Smaller cuts.**
   - `display_text` is dropped; main never escaped labels.
   - The finite-syntax guard is gone from `term_key`: lowered terms carry no closures or cells.
   - `SOURCE_NAME` is inlined again.
   - The hub-names cache is keyed by `encodeURIComponent(BEND_HUB)`.
   - `name_display` is about half the size.
   - `sugar_file(p)` replaces `p.base ?? p.ns`.
   - `term_map` keeps a context argument, so `term_higher` and `term_lower` allocate no closure per node; removing it had made the checker about 10% slower.
6. **Restored from main or the branch.**
   - The TODO count is back in `book_valid`; the branch had moved it into main.ts.
   - A failed instance check rolls back its `tmps` entry and its placeholder definition again, which fixes the first pass's concern 3.

## Why the rules hold

- **Local binders.**
  - *By position, not spelling:* binders are identified by level or by HOAS argument. Labels are only displayed.
  - *Inside checked types:* a variable in a checked type names its binder by level. `checked_higher` maps that level through the env of the copy being built, so it lands on the same binder object the body uses. Two references to one binder stay connected, and a level can't alias a compiler probe.
  - *Unbound variables:* an unbound level throws instead of turning into a `Ref` by its label, as main did.
- **Source declarations.**
  - *The key:* `<file>:<text>` for defs and types, `<file>:#<text>` for constructors. Two aliases of one file give the same file id, and categories never mix.
  - *Resolution is stable within a file:* a file reads a spelling one way throughout. `parse_fresh` refuses to declare a name the reading would change for (a name Base declares), and every imported file is fully parsed before the importer.
  - *Re-reading a head:* this goes through `name_display(book, k, file)`. For keys this file can name (its own, Base's, or an alias's), that is exactly the spelling that resolves back to the key's file.
- **Generated entities.**
  - *The key:* `<owner>~<role>:<member>` names the owner and the purpose. The member is a position, or the hash of the exact string that picks the entity: an instance's argument key, a kernel item's key, a group's members.
  - *Re-referencing:* referring to the same entity again builds the same key.
- **Sugar.** Sugar resolves to the file Base would, or to the writing file's own vocabulary when Base isn't loaded, which is the same rule ordinary names follow. Literals carry that file, so `lit_step` builds constructors of the same vocabulary.

## Gates (final commit 42b8f24e)

| gate | start 746528dc5 | 1st pass 79305b95 | final 42b8f24e |
|---|---|---|---|
| repo | FAIL 53/54 (comp.ts over cap) | 54/54 | **53/54, only because of `NAMING_REPORT.md`**; 54/54 with that file removed |
| test (BEND_HOC=lorenzo) | 1622/1622 | 1623/1623 | 1623/1623 |
| safe (tests corpus, bendtt b9f3e0253abffb11) | agree 1050, u 88, rej 490, − 4, ! 1 | agree 1050, u 89, rej 490, − 4, ! 1 | agree 1050, u 89, rej 490, − 4, ! 1, t 0, e 0 |
| perf | 124/124 ×2, 123/124 ×1 | 124/124 ×1, 123/124 ×3 | 124/124 and 123/124 (merkle compiler at 1.16x) |
| ping (SITE_REPO=~/work/bend-lang.com) | 58/58 | 58/58 | 58/58 |
| bend-lint `bun test` / tsc | 124 / clean | 124 / clean | 124 / clean |
| tsc (bend2/pack) | clean | clean | clean |

- **repo gate:** `NAMING_REPORT.md` sits at the repo root, which the allow list doesn't permit. It needs to move out of the tree, or be deleted, before anything merges.
- **safe:** every row matches the first pass once generated kernel names are stripped. The `!` is the known `agree_32` case (#1193).
- **perf:**
  - *Checker, against main's pin:*

    | bench | final | 1st pass |
    |---|---|---|
    | compute_1600 | 0.92x | 0.91x |
    | defs_12800 | 1.06–1.07x | 1.07x |
    | generics_3200 | **1.13–1.14x** | 1.16x |
    | proofs_3200 | 1.02–1.04x | 1.01x |
    | trees_400 | 1.04–1.05x | 1.05x |

  - *Compiler and runtime benches:* unchanged. merkle's compiler time sits at 1.13–1.16x in every version, including the first pass.
  - *In-process check of generics_3200* (load and check, median of 30): final 327–335 ms, 1st pass 316–340 ms.

## Behavior changes in this pass

- **A file that comes before `import Base` in the importer and doesn't import Base itself** now reads sugar (`3n`, `"s"`, `[..]`) as its own vocabulary. The starting branch read it as Base's, but that file's Base names already failed to resolve, as on main.
- **An alias that hides a name Base also declares** fails where the name is written, for the def reading. On main it also failed there, for either category.
- **Kernel names in `--verdict` output** are hashes of what each item stands for, not counters.

## Concerns still open

1. **Canonical keys contain absolute file paths.** They reach JS tags and exports, C symbols and BendTT names. That means builds can't be reproduced across checkouts, and separately built JS modules only agree on tags when built from the same path. This is the branch's design.
2. **`CID` inside a JS template literal's `${}`** is not rewritten. This is pre-existing (main's scanner).
3. **Template parameters display as internal names**, for example `cast~param2`.
4. **main.ts is +984 over main**, mostly `pkg_files` publication checks (symlinked or case-folded paths, unchanged imports, the PROOF/LAWS sibling). I left them alone; they are under the cap.

## Decisions for you

- **Effects keep `CID(..)`/`FID(..)`** (first pass), resolved in the importing file's scope.
- **`NAMING_REPORT.md` in the repo root breaks the repo gate.**

Scratch is in `/tmp/nm`. The `/tmp/w-prev`, `/tmp/w-start` and `/tmp/w-main` worktrees are removed after this report.

## Appendix: first pass (746528dc5 → 79305b95)

### What it simplified

1. **Key encoding.**
   - Only file ids are escaped now, and only `%`, `:` and `~`. Before, every key part was percent-encoded and decoded back.
   - Every source text is a `SOURCE_NAME` spelling or a fixed literal, so it never needs escaping.
   - A string member of `name_gen` (an instance key, comp's native key) is now hashed to `@<sha256>` inside `name_gen`.
   - Removed: `name_part`, `name_unpart`, `file_id`, the BigInt counter, the empty and negative checks, and the hash-collision checks (in bend.ts and in comp's `name_global`, with `FL.symbols`).
   - `name_text` and `name_display` are about half their size.
2. **Resolution.**
   - One `name_resolve` keeps the same rule: alias, else the file's own name unless only Base declares it, and the alias/Base ambiguity still becomes a deferred issue.
   - The `Resolution` type, `parse_lookup` and the two-way `string | Resolution` heads are gone.
   - `parse_head(p, t, category, op)` re-reads a written head in any category or with a method suffix. For a sugar head it keeps the key's file; the branch reused a def key as a constructor label there, which was a category bug.
   - The per-file cache is kept as `p.rs`. It is required for speed: without it the checker benches were about 5% slower.
3. **Template instances.**
   - Dropped the try/catch rollback, the generic-check assertion and the collision check.
   - The checked arguments are copied in the empty environment (`checked_copy(null)`), not mapped through the caller's context.
4. **Cache keys.** One `term_key(tm, size?)` replaces `term_json`, `term_key` and `term_size`. The finite-syntax guard is kept. The −0/NaN branch is gone: literals store integer bits.
5. **Removed the `recheck` machinery.** That was the `book.recheck` flag, the invalidation of every instance on each `parse_book`, the forced recheck on law fills, and the freezing and copying of the alias table.
6. **Removed `term_children`** (176 tokens). `safe.ts` (`mentions`, `back`, `model_key`) and `main.ts` (`term_refs`) walk lowered terms field by field again, as main did.
7. **Display.** `display_text` was applied to 23 labels; labels are always parser spellings or compiler names, so those calls did nothing. It stays where file paths are shown.
8. **Smaller pieces.**
   - `scope_mapper` lost its cycle detector and its `Sub` throw; `term_lower` lost its slot and `Sub` cases.
   - The constructor note in `term_check` takes 2 lines instead of 5.
   - The `~` lookup in a call is main's line again.
9. **Effects keep the released `CID(..)`/`FID(..)` macros instead of `// bend-ids 1` headers.** This is the one interface decision you need to make; see below.
   - Each name resolves the same way a header name did: in the scope of the Bend file that imports the source, with `#K` or `:f` to pick constructor or def.
   - A bare name that matches both a constructor and a def is an error.
   - A whole key (`Base:#Unit`, `!Clo.apply`) is accepted.
   - A source imported from several files must resolve the same in each.
   - JS gets the canonical key.
   - Result: about 130 effect and test files are byte-identical to main again (window.c differs only by the branch's GPU kernel rename). `foreign_src` (602 tokens) became a ~300-token `c_ids`. JS runtime tags are written as literal `"Base:#X"` (the `js_tag` helper is gone). comp.ts is under its cap again.

### Why its rules hold

- **Each key reads back to one thing.** File ids contain no `:`, `~` or `%`-ambiguity, so the first `:` ends the file. `#` marks a constructor. Source spellings and roles never contain `: # ~`, so a key is never misread as generated. Generated members are numbers or `@`+hex.
- **Different entities get different keys.** Category, file and spelling all stay in the key. Hashed members are sha256 of the exact key text.
- **One reference stays one reference.**
  - The cache in point 2 can't change any reading. Within a file, a spelling's reading could only change when that file declares its own name over a Base one. `parse_fresh` refuses exactly that, and imported files are fully parsed first.
  - Dropping recheck is safe for the same kind of reason. Base imports nothing and can't be filled by users (law fills need `b !== true`). So a seeded Base book's checked bodies and instances can't depend on the user file, and an unseeded book is validated from 0 anyway.
- **Template arguments stay correct.** A `~` argument checks in the empty context, so a caller's variable survives only in dead code, which the checker drops. `dead_arm_shared` (#1204) is the evidence. If one ever survived, copying now throws. The branch instead mapped it to the caller's level index, which the elaborator reads as a different variable. My first try kept unchecked arguments, and the safe gate caught that (`dead_arm_shared` hit an unbound variable), so I went back to checked arguments.
- **The walkers see the same things.** `term_lower` forces cells and applies closures, so a lowered term holds no functions, and a field walk sees the same children as `term_children` did.
- **Display only changes for impossible inputs.** Labels pass `SOURCE_NAME` or are compiler-made.

