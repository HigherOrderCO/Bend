// safe.ts's node side: each verdict child has its own CAP-second deadline.
// ms measures only that child. Diagnostic failures and duration are separate;
// neither replaces its verdict. Infrastructure errors are not proof rejections.
import * as child from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

export type Got = {
  f: string; code: number | null; ms: number; out: string; timeout: boolean;
  signal?: string; error?: string;
  diagnostic?: string; diagnostic_error?: string; diagnostic_ms?: number;
};

// Classification consumes explicit process outcomes, never elapsed diagnostics.
export function judge(g: Got): [string, string] {
  const out = g.out.trim();
  if (g.timeout) return ["t", "timeout"];
  if (g.error || g.code === null || g.code < 0) {
    const why = (g.error ?? out).replace(/\s+/g, " ").slice(0, 300);
    return ["e", "infrastructure: " + why];
  }
  if (g.code === 0 && out === "ALL PROOFS CHECK") return [" ", "agree"];
  if (/^Error: \d+ defs? rel(y|ies) on unsafe or foreign code/m.test(out)) {
    return ["u", "unsafe: " + [...out.matchAll(/^- (\S+)$/gm)].map((m) => m[1]).slice(0, 3).join(" ")];
  }
  if (!out.includes("Sorry - ")) {
    return [" ", "bend2 rejects: " + out.split("\n").slice(1, 3).join(" ").slice(0, 80)];
  }
  const tt = g.diagnostic ?? out;
  if (tt.startsWith("out of scope")) {
    return ["-", "out of scope: " + [...tt.matchAll(/^- \S+: (.*)$/gm)].map((m) => m[1]).slice(0, 2).join(" | ").slice(0, 160)];
  }
  // the kernel's first line is SOME PROOFS FAIL; why starts at In <def>:
  return ["!", tt.split("\n").slice(1, 3).join(" | ").slice(0, 300)];
}

async function run(bin: string, args: string[], cap: number): Promise<Omit<Got, "f">> {
  const g: Omit<Got, "f"> = { code: null, ms: 0, out: "", timeout: false };
  const t0 = Date.now();
  await new Promise<void>((done) => {
    const kid = child.spawn(bin, args, { env: process.env });
    kid.stdout.on("data", (d) => { g.out += d; });
    kid.stderr.on("data", (d) => { g.out += d; });
    kid.on("error", (e) => { g.error = e.message; });
    const bomb = setTimeout(() => { g.timeout = kid.kill("SIGKILL"); }, cap);
    kid.on("close", (code, signal) => {
      clearTimeout(bomb);
      g.code = code;
      g.ms = Date.now() - t0;
      if (signal) {
        g.signal = signal;
        if (!g.timeout) g.error = "child terminated by " + signal;
      }
      done();
    });
  });
  return g;
}

async function one(f: string, cap: number): Promise<Got> {
  const g: Got = { f, ...await run(process.execPath, ["bend2/main.ts", f, "--verdict"], cap) };
  if (g.out.includes("Error: BendTT infrastructure:")) g.error = g.out.trim();
  if (!g.timeout && !g.error && g.out.includes("Sorry - ")) {
    const t1 = Date.now();
    let dir: string | undefined;
    try {
      dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-safe-"));
      const tt = path.join(dir, "diagnostic.bendtt");
      const emitted = await run(process.execPath, ["bend2/main.ts", f, "-o", tt], 20000);
      if (emitted.error || emitted.timeout || emitted.code !== 0) {
        throw new Error("elaboration diagnostic: " + (emitted.error ?? (emitted.out.trim()
          || "exit " + emitted.code + ", signal " + emitted.signal)));
      }
      if (emitted.out.trim() !== "") {
        g.diagnostic = "out of scope\n" + emitted.out.trim();
      } else {
        const kernel = await run(process.env.BENDTT ?? "", [tt], 20000);
        if (kernel.error || kernel.timeout || kernel.code === null
          || (kernel.code !== 0 && !kernel.out.startsWith("SOME PROOFS FAIL"))) {
          throw new Error("kernel diagnostic: " + (kernel.error ?? (kernel.out.trim()
            || "exit " + kernel.code + ", signal " + kernel.signal)));
        }
        g.diagnostic = kernel.out.trim();
        const m = /^In (\S+):\naffine live code/m.exec(kernel.out);
        if (m !== null) {
          const detail = await run(process.execPath, ["gates/safe_diag.ts", tt, m[1]], 20000);
          if (detail.error || detail.timeout || detail.code !== 0) {
            throw new Error("descent diagnostic: " + (detail.error ?? (detail.out.trim()
              || "exit " + detail.code + ", signal " + detail.signal)));
          }
          g.diagnostic += "\n" + detail.out.trim().slice(0, 300);
        }
      }
    } catch (e) {
      g.diagnostic_error = String(e);
    } finally {
      if (dir !== undefined) fs.rmSync(dir, { recursive: true, force: true });
      g.diagnostic_ms = Date.now() - t1;
    }
  }
  return g;
}

if (import.meta.main) {
  const files = fs.readFileSync(0, "utf8").split("\n").filter((l) => l !== "");
  const cap = Number(process.env.CAP ?? 30) * 1000;
  const res: Got[] = [];
  let next = 0;
  await Promise.all(Array.from({ length: Number(process.env.PAR ?? 8) }, async () => {
    while (next < files.length) res.push(await one(files[next++], cap));
  }));
  process.stdout.write(JSON.stringify(res));
}
