# Simplifying naming/uniform-identities

Branch `naming/uniform-identities-simplify` is local only, in the worktree `/tmp/w-naming` (repo `~/work/bend2-core`). It starts at `746528dc5` and has 5 commits, ending at `79305b95`. Nothing was pushed and nothing was written to GitHub. Paulo's branch is untouched.

## Sizes (lines / ttok)

| file | main 32d23b434 | start 746528dc5 | final 79305b95 | final vs start | final vs main |
|---|---|---|---|---|---|
| bend.ts | 3882 / 43,759 | 3998 / 46,834 | 3902 / 45,775 | −96 / **−1,059** | +20 / **+2,016 (+4.6%)** |
| comp.ts | 6396 / 63,675 | 6324 / **64,345 (over the 64k cap)** | 6301 / 63,988 | −23 / −357 | −95 / +313 |
| main.ts | 1015 / 11,230 | 1085 / 12,255 | 1088 / 12,251 | +3 / −4 | +73 / +1,021 |
| safe.ts | 1577 / 22,494 | 1555 / 22,184 | 1547 / 22,071 | −8 / −113 | −30 / −423 |

- **Whole branch against main:** 171 files and +3151/−2216 lines at the start; 33 files and +1786/−1689 now.
- **Against the start:** 154 files, +782/−1597 lines.
- **bend.ts:** 34% of the branch's growth is gone. The remaining +2,016 is roughly:
  - ~570: key helpers (format, file ids, reading keys back, display, C/JS emit).
  - ~430: resolution (aliases, Base fallback, categories, the deferred alias issue, re-reading a head by its spelling, the per-file cache).
  - ~430: sugar and literals bound to Base or to the file's own names, plus reading imports first.
  - ~700: scope-correct copying of checked terms (`scope_mapper`, `checked_copy`, `term_at`, `checked_higher`).
  - ~350: the rest (`book_load`, instances, cache keys, the `Op` term, the field-name check, the IO check by key).
  - −780: the branch's own `term_map`/`term_show` refactors pay part of this back.

## What I simplified

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

## Why the rules still hold

- **Each key reads back to one thing.** File ids contain no `:`, `~` or `%`-ambiguity, so the first `:` ends the file. `#` marks a constructor. Source spellings and roles never contain `: # ~`, so a key is never misread as generated. Generated members are numbers or `@`+hex.
- **Different entities get different keys.** Category, file and spelling all stay in the key. Hashed members are sha256 of the exact key text.
- **One reference stays one reference.**
  - The cache in point 2 can't change any reading. Within a file, a spelling's reading could only change when that file declares its own name over a Base one. `parse_fresh` refuses exactly that, and imported files are fully parsed first.
  - Dropping recheck is safe for the same kind of reason. Base imports nothing and can't be filled by users (law fills need `b !== true`). So a seeded Base book's checked bodies and instances can't depend on the user file, and an unseeded book is validated from 0 anyway.
- **Template arguments stay correct.** A `~` argument checks in the empty context, so a caller's variable survives only in dead code, which the checker drops. `dead_arm_shared` (#1204) is the evidence. If one ever survived, copying now throws. The branch instead mapped it to the caller's level index, which the elaborator reads as a different variable. My first try kept unchecked arguments, and the safe gate caught that (`dead_arm_shared` hit an unbound variable), so I went back to checked arguments.
- **The walkers see the same things.** `term_lower` forces cells and applies closures, so a lowered term holds no functions, and a field walk sees the same children as `term_children` did.
- **Display only changes for impossible inputs.** Labels pass `SOURCE_NAME` or are compiler-made.

## Gates (final commit compared with the start)

| gate | start 746528dc5 | final 79305b95 |
|---|---|---|
| repo | **FAIL 53/54** (comp.ts 64,345) | PASS 54/54 |
| test (BEND_HOC=lorenzo) | 1622/1622 | 1623/1623 (+ new `tests/io/cid_both`) |
| safe (tests corpus, bendtt b9f3e0253abffb11) | agree 1050, u 88, rej 490, − 4, ! 1, t 0, e 0 | agree 1050, u 89 (new test), rej 490, − 4, ! 1, t 0, e 0 |
| perf | 124/124 twice, 123/124 once (`generics_3200` at 1.12–1.15x) | 124/124 once, 123/124 three times (`generics_3200` at 1.15–1.16x) |
| ping (SITE_REPO=~/work/bend-lang.com) | 58/58 | 58/58 |
| bend-lint `bun test` / tsc | 124 pass / clean | 124 pass / clean |
| tsc (bend2/pack) | clean | clean |

- **Safe:** the only row difference is the shorter alias message. The `!` on both is `conversion_shared_cells_once` `agree_32`, a known issue (#1193).
- **Perf:** `generics_3200` sits on the 1.15x line for both versions, against main's pin. That slowdown comes from the branch, not from this work.
  - Local profiles of start and final are equal: 401.7 ms against 401.9 ms on generics, and medians equal within noise on all five checker benches.
  - On the cluster, generics read 1.15/1.16/1.15/1.16x for final against 1.12/1.15/1.15x for start.
  - Every runtime bench is unchanged.
  - Profiling found the one real slowdown, the dropped resolution cache, and restoring it brought the checker benches back to parity.

## Correctness concerns found (not fixed)

1. **Canonical keys contain absolute file paths.**
   - They leak into JS constructor tags and exports (`{$: "/Users/…/lib.bend:#Foo"}`), C symbols and BendTT names.
   - Builds can't be reproduced across checkouts, and "separately compiled modules agree on tags" only holds on the same path.
   - This is the branch's design; changing it is your call.
2. **`name_gen` without a member uses a counter shared by the whole process.** safe.ts's `tt`, `root` and `group` names depend on what ran before. That is fine for one CLI run, not for a long-lived host.
3. **No rollback when an instance fails to check.** A host that keeps the book after a failed check would get "a decreasing self-call" on a retry. The CLI and lint stop at the first failure, so nothing sees this today.
4. **The `c_ids` scan (main's regex) does not rewrite a `CID` inside a JS template literal's `${}`.** This is pre-existing.
5. **Template parameters now print as internal names** (`cast~param2`, `grow~inst`, from the branch). You may want the binder label instead.

## Decisions for you

- **Effects interface.** I kept main's released `CID(..)`/`FID(..)` in the source body, with the branch's resolution rules, rather than `// bend-ids 1` headers.
  - It is smaller, isn't a breaking change for effect authors, and it is what brings comp.ts back under its cap.
  - The headers do let the compiler copy host text unchanged. Restoring them means taking back `foreign_src` and the header versions of the effect files, and cutting about 350 tokens from comp.ts some other way.
  - The CHANGELOG, EFFECTS.md and GUIDE.md now describe the macros.
- **Error message wording.** The alias shadow message is main's wording again: "an unambiguous name (the alias … shadows …)", not "an unambiguous definition name". `tests/import/alias_shadow.bend` is updated to match.

Scratch is in `/tmp/nm` (diffs, profiles, logs). I removed the temporary worktrees for main and the start.
