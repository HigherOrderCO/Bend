#!/usr/bin/env python3
"""Reproduce the destination loop's stack bound, or its Metal stress test."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]


def run(args, **kwargs):
    result = subprocess.run(args, text=True, capture_output=True,
                            timeout=180, **kwargs)
    if result.returncode:
        raise RuntimeError(f"{args}\n{result.stdout}\n{result.stderr}")
    return result


def instrument(source):
    # Host-only, one thread. Track the high-water mark of the explicit
    # work-loop stack at its room checks, relative to each task's entry.
    at = source.index("typedef struct {")
    globals = (
        "static __thread u64* dp_base;\n"
        "static __thread u64 dp_peak;\n"
        "__attribute__((destructor)) static void dp_report(void) { "
        'fprintf(stderr, "DPS_STACK=%llu\\n", (unsigned long long)dp_peak); }\n'
    )
    source = source[:at] + globals + source[at:]
    room = "#define WL_ROOM(N) \\\n"
    source = source.replace(room, room +
        "  if ((u64)(sp - dp_base) + (N) > dp_peak) "
        "dp_peak = (u64)(sp - dp_base) + (N); \\\n")
    entry = "static Term work_loop(Env e, DEV Term* sp, Term t, u32 seq) {"
    return source.replace(entry, entry + "\n  dp_base = sp;")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--backend", choices=["cpu", "gpu"], default="cpu")
    parser.add_argument("--depth", type=int, default=100000)
    parser.add_argument("--fused", action="store_true",
                        help="Use destination writes fused into sibling continuations")
    args = parser.parse_args()
    if args.depth < 1:
        parser.error("--depth must be positive")
    meta = json.loads((HERE/"metadata.json").read_text())
    if hashlib.sha256((ROOT/"bend2/comp.ts").read_bytes()).hexdigest() != meta["baseline_comp_sha256"]:
        raise SystemExit("Use the recorded baseline compiler.")
    work = Path(tempfile.mkdtemp(prefix="bend-dps-stack-"))
    print(f"Artifacts: {work}", flush=True)
    for variant in ["base", "destination"]:
        shutil.copytree(ROOT/"bend2", work/variant/"bend2",
                        ignore=shutil.ignore_patterns("docs", "pack", "node_modules"))
    patch = "fused.patch" if args.fused else "destination.patch"
    run(["patch", "-p1", "-i", str(HERE.parent/patch)],
        cwd=work/"destination")
    fixture = (work/"destination/tests/compile/destination_tail.bend").read_text()
    source = work/"deep.bend"
    source.write_text(fixture.replace("256n", str(args.depth)+"n")
                     .replace("#|256", "#|"+str(args.depth)))
    records = []
    for variant in ["base", "destination"]:
        binary = work/variant/"deep"
        c = binary.with_suffix(".c")
        run(["bun", str(work/variant/"bend2/main.ts"), str(source), "-o", str(c)],
            env=dict(os.environ, BEND_DPS=str(int(variant == "destination")),
                     BEND_DPS_ASYNC=str(int(variant == "destination"))))
        flags = ["-std=c11", "-O3"]
        libs = ["-lpthread", "-lm"]
        if args.backend == "cpu":
            c.write_text(instrument(c.read_text()))
        else:
            flags += ["-DBEND_METAL=1", "-x", "objective-c", "-fobjc-arc"]
            libs += ["-framework", "Metal", "-framework", "Foundation"]
        run(["clang", *flags, str(c), *libs, "-o", str(binary)])
        if args.backend == "gpu":
            run([str(binary), "--gpu-build"])
        result = subprocess.run([str(binary), "--threads", "1", "--gpu",
            "off" if args.backend == "cpu" else "1GB"],
            capture_output=True, text=True, timeout=60)
        row = dict(variant=variant, depth=args.depth, backend=args.backend,
                   code=result.returncode, stdout=result.stdout.strip(),
                   stderr=result.stderr.strip())
        records.append(row)
        print(json.dumps(row), flush=True)
        (work/"results.json").write_text(json.dumps(records, indent=2)+"\n")
        if variant == "destination" or args.backend == "cpu":
            if result.returncode or result.stdout.strip() != str(args.depth):
                raise SystemExit("Stress test failed")


if __name__ == "__main__":
    main()
