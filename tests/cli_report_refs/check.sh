#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd -P)
bun=${BUN:-bun}
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
set +e
BEND_NO_TELEMETRY=1 "$bun" "$root/bend2/main.ts" "$root/tests/cli_report_refs/dependent_type.bend" --check-only >"$out/stdout" 2>"$out/stderr"
status=$?
set -e
[ "$status" -eq 1 ]
[ ! -s "$out/stdout" ]
cat >"$out/expected" <<'EOF'
SOME PROOFS FAIL
Error: 2 defs rely on unsafe or foreign code:
defs that depend on @unsafe: Rp.BadType, Rp.dependent
EOF
cmp "$out/expected" "$out/stderr"
printf 'dependent type reference report passed\n'
