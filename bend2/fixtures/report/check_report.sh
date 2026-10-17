#!/bin/sh
set -eu
[ -n "$BUN" ]
[ -n "$BASE" ]
root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd -P)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
run() {
  BEND_NO_TELEMETRY=1 "$BUN" --smol "$1/main.ts" --check-only "$2" >"$3" 2>"$3.err"
}
set +e
run "$root" "$root/fixtures/report/rp_report.bend" "$out/fixed.out"; fixed=$?
run "$BASE" "$root/fixtures/report/rp_report.bend" "$out/base.out"; base=$?
set -e
printf 'rp_report exit fixed=%s base=%s\n' "$fixed" "$base"
cmp "$out/fixed.out" "$root/fixtures/report/rp_expected.txt"
cmp "$out/base.out" "$root/fixtures/report/rp_expected.txt"
cmp "$out/fixed.out" "$out/base.out"
run "$root" "$root/fixtures/report/rp_dependent.bend" "$out/fixed-dependent.out"
run "$BASE" "$root/fixtures/report/rp_dependent.bend" "$out/base-dependent.out"
cmp "$out/fixed-dependent.out" "$root/fixtures/report/rp_dependent_expected.txt"
cmp "$out/base-dependent.out" "$root/fixtures/report/rp_dependent_expected.txt"
cmp "$out/fixed-dependent.out" "$out/base-dependent.out"
run "$root" "$root/fixtures/report/rp_clean.bend" "$out/fixed-clean.out"
run "$BASE" "$root/fixtures/report/rp_clean.bend" "$out/base-clean.out"
printf 'All terms check.\n' >"$out/expected-clean.out"
cmp "$out/fixed-clean.out" "$out/expected-clean.out"
cmp "$out/fixed-clean.out" "$out/base-clean.out"
python3 "$root/fixtures/report/gen_stress.py" "$out/stress.bend" 400 96
for label in fixed base; do
  compiler="$root"
  [ "$label" = fixed ] || compiler="$BASE"
  /usr/bin/time -f 'elapsed=%e max_rss_kb=%M' -o "$out/$label.stress.time" \
    env BEND_NO_TELEMETRY=1 "$BUN" --smol "$compiler/main.ts" --check-only "$out/stress.bend" \
    >"$out/$label.stress.out" 2>"$out/$label.stress.err"
done
cmp "$out/fixed.stress.out" "$out/base.stress.out"
printf 'stress fixture defs=400 depth=96 output=IDENTICAL\n'
cat "$out/fixed.stress.time" "$out/base.stress.time"
printf 'cli_report outputs byte-identical; report, clean, stress fixtures passed\n'
