#!/usr/bin/env python3
"""Bounded TS-flow fixture for compiler/cache.ts (no Main artifacts).

Runs the pinned Bun against THIS worktree's bend2/main.ts on a tiny
foreign-effect fixture (pure temp dirs): miss+compile+store, rebuild hit with
byte-identity and skipped generation, Bend-mutation miss, diagnostics-env
miss, foreign-only refresh, and refresh-vs-fresh byte-identity. Records
wall/RSS per bun invocation via /usr/bin/time -v plus the tool's own
log.jsonl event/ms evidence (the event log is what proves compile() was or
was not invoked). Writes results/fixture-ts-result.json.
"""

import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile

WORKTREE = os.path.normpath(os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "..", ".."))
MAIN_TS = os.path.join(WORKTREE, "bend2", "main.ts")
BUN = ("/run/baton2-native-homelab/retained/baton2-compiler-741-20261015"
       "/postrelease-751-761-20261015-11/toolchain-home/bin/bun")
HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS = os.path.normpath(os.path.join(
    HERE, "..", "results", "fixture-ts-result.json"))

FX_BEND = """import Base

def Fx.tag() -> IO(U32):
  import "./host.c"
  import "./host.js"

def main() -> IO(Unit):
  do IO<Unit>:
    t : U32 <- Fx.tag()
    IO.print("done")
"""
HOST_C = """// IO
// ==

Term io_fx_tag_run(Env e, Term* f, IoWork* w) {
  return (Term)%d;
}

static void __attribute__((constructor)) io_fx_tag_use(void) {
  io_eff(CID_FX_TAG, io_fx_tag_run, 0);
}
"""
HOST_JS = """// IO
// ==

function io_fx_tag() {
  return 7n;
}
"""

ENV = {"PATH": "/usr/bin:/bin", "BEND_NO_TELEMETRY": "1",
       "HOME": os.environ.get("HOME", "/tmp")}


def sha(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def build(fx_dir, cache_dir, out_name, extra_env=None):
    """One bounded fixture build. Returns (returncode, out_c, wall_s, rss_kib,
    log_events)."""
    import time
    env = dict(ENV)
    env["CGEN_CACHE_DIR"] = cache_dir
    if extra_env:
        env.update(extra_env)
    cmd = ["/usr/bin/time", "-v", BUN, MAIN_TS,
           os.path.join(fx_dir, "fx.bend"), "-o",
           os.path.join(fx_dir, out_name)]
    start = time.perf_counter()
    proc = subprocess.run(cmd, cwd=fx_dir, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, env=env)
    wall = time.perf_counter() - start
    err = proc.stderr.decode()
    rss = None
    match = re.search(r"Maximum resident set size \(kbytes\): (\d+)", err)
    if match:
        rss = int(match.group(1))
    events = []
    log_path = os.path.join(cache_dir, "log.jsonl")
    if os.path.isfile(log_path):
        with open(log_path) as handle:
            events = [json.loads(line) for line in handle if line.strip()]
    return proc.returncode, os.path.join(fx_dir, out_name), wall, rss, events


def main():
    checks = []

    def check(name, condition, detail=""):
        checks.append({"name": name, "pass": bool(condition),
                       "detail": detail})

    runs = []
    with tempfile.TemporaryDirectory(prefix="cgen-ts-") as tmp:
        fx = os.path.join(tmp, "fx")
        os.makedirs(fx)
        with open(os.path.join(fx, "fx.bend"), "w") as handle:
            handle.write(FX_BEND)
        with open(os.path.join(fx, "host.c"), "w") as handle:
            handle.write(HOST_C % 7)
        with open(os.path.join(fx, "host.js"), "w") as handle:
            handle.write(HOST_JS)
        cache = os.path.join(tmp, "cache")

        code, c1, wall, rss, events = build(fx, cache, "a.c")
        runs.append({"step": "A-first-build", "exit": code, "wall_s": wall,
                     "rss_kib": rss,
                     "event": events[-1]["event"] if events else None,
                     "event_ms": events[-1]["ms"] if events else None})
        check("A-miss-compile-exit-0", code == 0
              and events and events[-1]["event"] == "compile",
              events[-1] if events else "no log")
        check("A-artifact-nonempty",
              code == 0 and os.path.getsize(c1) > 1000)

        code, c2, wall, rss, events = build(fx, cache, "b.c")
        runs.append({"step": "B-rebuild", "exit": code, "wall_s": wall,
                     "rss_kib": rss,
                     "event": events[-1]["event"] if events else None,
                     "event_ms": events[-1]["ms"] if events else None})
        check("B-hit-exit-0", code == 0
              and events[-1]["event"] == "hit", events[-1])
        check("B-byte-identical", sha(c1) == sha(c2),
              "%s vs %s" % (sha(c1)[:12], sha(c2)[:12]))
        compile_ms = runs[0]["event_ms"]
        hit_ms = runs[1]["event_ms"]
        check("B-hit-skips-generation",
              hit_ms is not None and compile_ms is not None
              and hit_ms < compile_ms,
              "hit %.1fms < compile %.1fms (tool log)" % (hit_ms or -1,
                                                          compile_ms or -1))

        with open(os.path.join(fx, "fx.bend"), "a") as handle:
            handle.write("# fixture comment change\n")
        code, c3, wall, rss, events = build(fx, cache, "c.c")
        runs.append({"step": "C-bend-mutation", "exit": code, "wall_s": wall,
                     "rss_kib": rss,
                     "event": events[-1]["event"] if events else None,
                     "event_ms": events[-1]["ms"] if events else None})
        check("C-bend-mutation-miss", code == 0
              and events[-1]["event"] == "compile", events[-1])

        with open(os.path.join(fx, "fx.bend"), "w") as handle:
            handle.write(FX_BEND)
        diag_env = {"BEND_COMPILER_DIAGNOSTICS": "1"}
        code, cdiag1, _, _, events = build(
            fx, cache, "d.c", extra_env=diag_env)
        check("F-diagnostics-env-miss", code == 0
              and events[-1]["event"] == "rebuild-uncertain", events[-1])
        code, cdiag2, _, _, events = build(
            fx, cache, "d2.c", extra_env=diag_env)
        check("G-diagnostics-bypass-same-key", code == 0
              and events[-1]["event"] == "rebuild-uncertain", events[-1])
        check("G-diagnostics-byte-identical", code == 0
              and sha(cdiag1) == sha(cdiag2),
              "%s vs %s" % (sha(cdiag1)[:12], sha(cdiag2)[:12]))

        with open(os.path.join(fx, "host.c"), "w") as handle:
            handle.write(HOST_C % 8)
        code, c4, wall, rss, events = build(fx, cache, "e.c")
        runs.append({"step": "D-foreign-refresh", "exit": code,
                     "wall_s": wall, "rss_kib": rss,
                     "event": events[-1]["event"] if events else None,
                     "event_ms": events[-1]["ms"] if events else None})
        check("D-foreign-refresh", code == 0
              and events[-1]["event"] == "refresh", events[-1])

        fresh_cache = os.path.join(tmp, "fresh-cache")
        code, c5, wall, rss, events = build(fx, fresh_cache, "f.c")
        runs.append({"step": "E-fresh-of-changed-tree", "exit": code,
                     "wall_s": wall, "rss_kib": rss,
                     "event": events[-1]["event"] if events else None,
                     "event_ms": events[-1]["ms"] if events else None})
        check("E-fresh-compile", code == 0
              and events[-1]["event"] == "compile", events[-1])
        check("E-refresh-byte-identical-to-fresh", sha(c4) == sha(c5),
              "%s vs %s" % (sha(c4)[:12], sha(c5)[:12]))

    failed = [c for c in checks if not c["pass"]]
    result = {"tool": "compiler/cache.ts via compiler/main.ts default path",
              "bun": "pinned toolchain-home/bin/bun",
              "checks": checks, "runs": runs,
              "passed": len(checks) - len(failed), "failed": len(failed),
              "compiler_invoked": True, "clang_invoked": False,
              "network_used": False, "main_artifacts_used": False,
              "note": "compiler_invoked=true ONLY for bounded synthetic "
                      "fixtures through the instrumented default path; no "
                      "Main build, no release impact. Event log proves "
                      "compile() skipped on hit. No generation-time saving "
                      "claimed beyond observed per-event ms."}
    os.makedirs(os.path.dirname(RESULTS), exist_ok=True)
    with open(RESULTS, "w") as handle:
        json.dump(result, handle, indent=2, sort_keys=True)
    print("%d/%d checks passed" % (result["passed"], len(checks)))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
