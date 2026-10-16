# AGENTS

Bend is a dependently typed, affine language that checks in one linear
bidirectional pass and runs massively parallel on CPU threads and GPUs.
bend2/bend.ts is the language (parser, theory, checker) and is human-written:
do not edit it. bend2/comp.ts is the one compiler: the C runtime (host and
device from one source), the C emitter and the JS emitter; base.bend is the
prelude; main.ts is the CLI. Every test is a Bend file that ends in the `#|`
lines its run must print, and the gates run on the mini cluster.

    bend2/bend.ts       the language: parser, theory, checker
    bend2/comp.ts       the compiler and the runtimes (C, Metal, CUDA, JS)
    bend2/main.ts       the CLI; imported, the .bend loader for bun and node
    bend2/safe.ts       --verdict and -o <out>.bendtt: the elaborator from a
                        checked book to BendTT text
    bend2/bendtt.lean   BendTT: the kernel, its claims (no checked def has type
                        Empty, live code halts) and their proofs; --verdict
                        builds its CLI once, with Lean v4.34.0
    bend2/base.bend     the base library
    bend2/effs/         IO effect sources per backend; related effects may share
    bend2/pack/         package.json, tsconfig.json, bun.lock
    bend2/docs/         the papers' Typst sources, the film, gen_pins.ts (the
                        record pins on this Mac), gen_charts.ts (the landing
                        page's and the film's numbers), gen_gifs.ts (the images)
    bench/runtime/      one dir per bench: main.bend and its twins (C, TS, Lean)
    bench/checker/      one dir per bench: main.bend and its rivals
    bench/*/_pin_/      the pins, one file per hardware: apple_m4 the gate's,
                        apple_m4_max the record
    tests/<ns>/         the tests by namespace, with their foreign .c/.js
    gates/test.ts       every test, one shard per live mini, PASS: n / n
    gates/perf.ts       the benches on 48 minis against the pins (--pin writes
                        the medians of three runs)
    gates/repo.ts       the allow list of files and the permanent ttok caps
                        (bend.ts 48k, comp.ts 66k, main.ts 16k, bendtt.lean
                        64k, README 4k, GUIDE 8k); only Taelin changes them
    gates/ping.ts       the installer, the compiled bend, its daily version
                        check and a release, on a localhost hub
    gates/safe.ts       bend2's verdict against BendTT's, per file, on the
                        minis: `tests` or `hub` (a BendHub store, $SAFE_HUB)
    gates/_run.ts       the four gates with --gate
    demos/              one dir per demo
    guide/              GUIDE.md
    paper/              BendTT.pdf, BendRT.pdf
    media/              the film and the charts
    .github/            ISSUE_TEMPLATE/bug.yml, the bug report form, and
                        config.yml, which points questions at Discord
    ../bend-lang.com    the site repo (bendlang/bend-lang.com), a sibling
                        checkout: the sites, install.sh, the hub, release.ts
                        (the executables per platform) and the droplet ops;
                        gates/ping.ts and gen_charts.ts read it there (or at
                        $SITE_REPO)
