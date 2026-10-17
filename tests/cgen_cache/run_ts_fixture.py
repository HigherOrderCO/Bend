#!/usr/bin/env python3
"""Bounded TS-flow fixture for compiler/cache.ts (no Main artifacts).

Runs the pinned Bun against THIS worktree's bend2/main.ts on a tiny
foreign-effect fixture (pure temp dirs): miss+compile+store, rebuild hit with
byte-identity and skipped generation, Bend-mutation miss, diagnostics-env
miss/bypass parity, foreign-only refresh, and refresh-vs-fresh byte-identity,
CID-introducing foreign edit declining raw refresh with fresh parity,
untrusted cache directories (open mode, symlink) rebuilding with zero cache I/O.
Records wall/RSS per bun invocation via /usr/bin/time -v plus the tool's own
log.jsonl event/ms evidence (the event log is what proves compile() was or
was not invoked). Writes tests/results/fixture-ts-result.json.

EXECUTION GATE: this fixture launches the Bend compiler on synthetic
fixtures. Per tasks/COMMON.md it must not run without Principal Conductor
approval. The gate refuses unless BATON_COMPILER_RUN_APPROVED=1; without
approval the committed status is NOT RUN (no result file is written and no
pass is claimed).
"""

import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile

WORKTREE = os.path.normpath(os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "..", ".."))
MAIN_TS = os.path.join(WORKTREE, "bend2", "main.ts")
BUN = ("/run/baton2-native-homelab/retained/baton2-compiler-741-20261015"
       "/postrelease-751-761-20261015-11/toolchain-home/bin/bun")
HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS = os.path.normpath(os.path.join(HERE, "results",
                                        "fixture-ts-result.json"))

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
// Unicode byte-count fixture: λ

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
HOST_CID = """// IO
// ==

Term io_fx_tag_run(Env e, Term* f, IoWork* w) {
  return (Term)%d;
}

static void __attribute__((constructor)) io_fx_tag_use(void) {
  io_eff(CID(Fx.tag), io_fx_tag_run, 0);
  io_eff(CID_FX_TAG, io_fx_tag_run, 0);
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
    if os.environ.get("BATON_COMPILER_RUN_APPROVED") != "1":
        print("REFUSED: set BATON_COMPILER_RUN_APPROVED=1 only with "
              "Principal Conductor approval (tasks/COMMON.md). "
              "Status: NOT RUN, no pass claimed.")
        return 2
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
        first_key = events[-1]["key"] if events else None
        receipt_path = (os.path.join(cache, first_key, "receipt.json")
                        if first_key else "")
        receipt = None
        if receipt_path and os.path.isfile(receipt_path):
            with open(receipt_path) as handle:
                receipt = json.load(handle)
        check("A-receipt-counts-utf8-bytes",
              receipt is not None and receipt.get("artifact_bytes")
              == os.path.getsize(c1), receipt)

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

        # A damaged cache entry must never become the source of a foreign
        # refresh. Exercise artifact and receipt integrity independently.
        for label in ("artifact", "receipt"):
            with open(os.path.join(fx, "fx.bend"), "w") as handle:
                handle.write(FX_BEND)
            with open(os.path.join(fx, "host.c"), "w") as handle:
                handle.write(HOST_C % 7)
            corrupt_cache = os.path.join(tmp, "corrupt-" + label)
            code, origin, _, _, events = build(
                fx, corrupt_cache, label + "-origin.c")
            key = events[-1]["key"] if events else None
            check(label + "-origin-compiled", code == 0 and key is not None
                  and events[-1]["event"] == "compile", events[-1])
            if key is None:
                continue
            entry = os.path.join(corrupt_cache, key)
            if label == "artifact":
                with open(os.path.join(entry, "artifact.c"), "ab") as handle:
                    handle.write(b"damaged")
            else:
                with open(os.path.join(entry, "receipt.sha256"), "w") as handle:
                    handle.write("0" * 64 + "\n")
            with open(os.path.join(fx, "host.c"), "w") as handle:
                handle.write(HOST_C % 8)
            code, repaired, _, _, events = build(
                fx, corrupt_cache, label + "-repaired.c")
            check(label + "-origin-rebuilds", code == 0
                  and events[-1]["event"] == "compile", events[-1])
            fresh_cache = os.path.join(tmp, "fresh-" + label)
            code, fresh, _, _, _ = build(
                fx, fresh_cache, label + "-fresh.c")
            check(label + "-repair-equals-fresh",
                  code == 0 and sha(repaired) == sha(fresh),
                  "%s vs %s" % (sha(repaired)[:12], sha(fresh)[:12]))

        # Well-formed JSON can still contain an unusable origin receipt.
        # Refresh scans must skip each malformed shape and run ordinary codegen.
        malformed_cases = {
            "null-manifest": lambda receipt: receipt.update(manifest=None),
            "missing-manifest-key": lambda receipt: receipt["manifest"].pop(
                "k3_entry"),
            "null-foreign-rows": lambda receipt: receipt["manifest"].update(
                k5_effs=None),
            "malformed-foreign-row": lambda receipt: receipt["manifest"].update(
                k5_effs=[None]),
        }
        for label, corrupt in malformed_cases.items():
            with open(os.path.join(fx, "fx.bend"), "w") as handle:
                handle.write(FX_BEND)
            with open(os.path.join(fx, "host.c"), "w") as handle:
                handle.write(HOST_C % 7)
            corrupt_cache = os.path.join(tmp, "malformed-" + label)
            code, _, _, _, events = build(
                fx, corrupt_cache, label + "-origin.c")
            key = events[-1]["key"] if events else None
            check(label + "-origin-compiled", code == 0 and key is not None
                  and events[-1]["event"] == "compile", events[-1])
            if key is None:
                continue
            entry = os.path.join(corrupt_cache, key)
            receipt_path = os.path.join(entry, "receipt.json")
            with open(receipt_path) as handle:
                receipt = json.load(handle)
            corrupt(receipt)
            raw = json.dumps(receipt, indent=2).encode() + b"\n"
            with open(receipt_path, "wb") as handle:
                handle.write(raw)
            with open(os.path.join(entry, "receipt.sha256"), "w") as handle:
                handle.write(hashlib.sha256(raw).hexdigest() + "\n")
            with open(os.path.join(fx, "host.c"), "w") as handle:
                handle.write(HOST_C % 8)
            code, repaired, _, _, events = build(
                fx, corrupt_cache, label + "-repaired.c")
            check(label + "-malformed-origin-rebuilds", code == 0
                  and events[-1]["event"] == "compile", events[-1])
            fresh_cache = os.path.join(tmp, "fresh-malformed-" + label)
            code, fresh, _, _, _ = build(
                fx, fresh_cache, label + "-fresh.c")
            check(label + "-repair-equals-fresh",
                  code == 0 and sha(repaired) == sha(fresh),
                  "%s vs %s" % (sha(repaired)[:12], sha(fresh)[:12]))

        i_cache = os.path.join(tmp, "i-cache")
        code, i1, _, _, events = build(fx, i_cache, "i1.c")
        check("I-setup-compile", code == 0
              and events[-1]["event"] == "compile", events[-1])
        with open(os.path.join(fx, "host.c"), "w") as handle:
            handle.write(HOST_CID % 8)
        code, i2, _, _, events = build(fx, i_cache, "i2.c")
        check("I-cid-decline-compiles", code == 0
              and events[-1]["event"] == "compile", events[-1])
        i_fresh = os.path.join(tmp, "i-fresh")
        code, i3, _, _, events = build(fx, i_fresh, "i3.c")
        check("I-fresh-compile", code == 0
              and events[-1]["event"] == "compile", events[-1])
        check("I-decline-parity", code == 0
              and sha(i2) == sha(i3),
              "%s vs %s" % (sha(i2)[:12], sha(i3)[:12]))

        # Refused directories get zero artifact I/O: event logs stay empty
        # and the directory tree is byte-unchanged. Outputs are anchored to
        # a separately built trusted-cache binary, not to each other.
        # Writable ancestors are trusted by design (ownership is enforced
        # at the leaf only); a hostile rename there remains a documented
        # residual, still gated per served byte by digests.
        def snapshot(d):
            return {os.path.relpath(os.path.join(r, f), d): sha(
                os.path.join(r, f)) for r, _, fs in os.walk(
                d, followlinks=False) for f in fs}

        j_fresh = os.path.join(tmp, "j-fresh")
        code, jf, _, _, events = build(fx, j_fresh, "jf.c")
        check("J-fresh-compile", code == 0
              and events[-1]["event"] == "compile", events[-1])
        for jlabel in ("J-open-mode", "J-symlink-dir",
                       "J-ancestor-symlink"):
            j_cache = os.path.join(tmp, "j-" + jlabel)
            if jlabel == "J-open-mode":
                os.makedirs(j_cache)
                os.chmod(j_cache, 0o777)
            elif jlabel == "J-symlink-dir":
                real = os.path.join(tmp, "j-" + jlabel + "-real")
                os.makedirs(real)
                os.symlink(real, j_cache)
            else:
                areal = os.path.join(tmp, "j-" + jlabel + "-areal")
                os.makedirs(os.path.join(areal, "sub"))
                os.symlink(areal, os.path.join(tmp, "j-" + jlabel))
                j_cache = os.path.join(tmp, "j-" + jlabel, "sub")
            before = snapshot(j_cache)
            code, j1, _, _, events1 = build(fx, j_cache, "j1.c")
            code2, j2, _, _, events2 = build(fx, j_cache, "j2.c")
            check(jlabel + "-empty-events", code == 0 and code2 == 0
                  and events1 == [] and events2 == [],
                  [e.get("event") for e in events1 + events2])
            check(jlabel + "-dir-unchanged",
                  snapshot(j_cache) == before,
                  "%d entries" % len(before))
            check(jlabel + "-fresh-parity", code == 0 and code2 == 0
                  and sha(j1) == sha(jf) and sha(j2) == sha(jf),
                  "%s vs %s" % (sha(j1)[:12], sha(jf)[:12]))



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
