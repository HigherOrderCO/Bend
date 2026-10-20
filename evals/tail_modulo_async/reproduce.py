#!/usr/bin/env python3
"""Build and measure isolated baseline/prototype copies; never edit the checkout."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]


def command(args, **kwargs):
    result = subprocess.run(args, capture_output=True, text=True,
                            timeout=180, **kwargs)
    if result.returncode:
        raise RuntimeError(f"{args}\n{result.stdout}\n{result.stderr}")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--backend", choices=["cpu", "gpu"], default="cpu")
    parser.add_argument("--threads", type=int, default=8)
    parser.add_argument("--repetitions", type=int, default=5)
    parser.add_argument("--bench", action="append",
                        help="Runtime benchmark or pure demo name; repeatable")
    parser.add_argument("--unbounded", action="store_true",
                        help="Reproduce the initial unbounded prototype")
    parser.add_argument("--destination", action="store_true",
                        help="Reproduce the replacement destination-passing worker loop")
    parser.add_argument("--fused", action="store_true",
                        help="Reproduce destination writes fused into sibling continuations")
    args = parser.parse_args()
    if sum([args.unbounded, args.destination, args.fused]) > 1:
        parser.error("--unbounded, --destination and --fused are separate prototypes")
    metadata = json.loads((HERE / "metadata.json").read_text())
    current = hashlib.sha256((ROOT / "bend2/comp.ts").read_bytes()).hexdigest()
    if current != metadata["baseline_comp_sha256"]:
        raise SystemExit("Compiler differs from the measured baseline; use the recorded revision.")
    work = Path(tempfile.mkdtemp(prefix="bend-tma-reproduce-"))
    print(f"Artifacts and results: {work}", flush=True)
    for variant in ["base", "prototype"]:
        shutil.copytree(ROOT / "bend2", work / variant / "bend2",
                        ignore=shutil.ignore_patterns("docs", "pack", "node_modules"))
    patch = ("fused.patch" if args.fused else
             "destination.patch" if args.destination else
             "unbounded.patch" if args.unbounded else "prototype.patch")
    command(["patch", "-p1", "-i", str(HERE / patch)],
            cwd=work / "prototype")
    command(["clang", "-O2", str(HERE / "tm.c"), "-o", str(work / "tm")])
    names = args.bench or ["kmeans", "tree-bitonic", "tree-matmul", "tree-radix"]
    sources = {}
    for name in names:
        candidates = [ROOT / "bench/runtime" / name / "main.bend",
                      ROOT / "demos" / name / "main.bend"]
        sources[name] = next((p for p in candidates if p.is_file()), None)
        if sources[name] is None:
            raise SystemExit(f"No benchmark/demo: {name}")
    for name, source in sources.items():
        for variant in ["base", "prototype"]:
            binary = work / f"{name}-{variant}"
            env = dict(os.environ, BEND_TMA=str(int(variant == "prototype")),
                       BEND_TMA_UNBOUNDED=str(int(args.unbounded)),
                       BEND_DPS=str(int((args.destination or args.fused) and variant == "prototype")),
                       BEND_DPS_ASYNC=str(int((args.destination or args.fused) and variant == "prototype")))
            command(["bun", str(work / variant / "bend2/main.ts"), str(source),
                     "-o", str(binary) + ".c"], env=env)
            flags = ["-std=c11", "-O3"]
            libraries = ["-lpthread", "-lm"]
            if args.backend == "gpu":
                flags += ["-DBEND_METAL=1", "-x", "objective-c", "-fobjc-arc"]
                libraries += ["-framework", "Metal", "-framework", "Foundation"]
            command(["clang", *flags, str(binary) + ".c", *libraries,
                     "-o", str(binary)])
            if args.backend == "gpu":
                command([str(binary), "--gpu-build"])
    records = []
    for repetition in range(-1, args.repetitions):
        for name in names:
            outputs = []
            variants = ["base", "prototype"]
            if repetition % 2:
                variants.reverse()
            for variant in variants:
                span = "4GB" if name in ["tree-radix", "tree-matmul"] else "1GB"
                binary = work / f"{name}-{variant}"
                result = command([str(work / "tm"), str(work / "time.txt"),
                                  str(binary), "--threads", str(args.threads),
                                  "--gpu", span if args.backend == "gpu" else "off"])
                code, seconds, rss = (work / "time.txt").read_text().split()
                record = dict(name=name, variant=variant, repetition=repetition,
                              seconds=float(seconds), rss=int(rss), code=int(code),
                              stdout=result.stdout.strip(), stderr=result.stderr.strip())
                records.append(record)
                (work / "results.json").write_text(json.dumps(records, indent=2))
                print(json.dumps(record), flush=True)
                if int(code):
                    raise SystemExit("Benchmark failed; see results.json")
                outputs.append(record["stdout"])
            if outputs[0] != outputs[1]:
                raise SystemExit(f"Output mismatch: {name}")


if __name__ == "__main__":
    main()
