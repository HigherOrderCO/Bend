// safe.ts's node side: runs `bend <f> --verdict` on each file named on
// stdin, PAR at a time, each capped at CAP s, and prints one JSON array
// of { f, code, ms, out }. On a mismatch (bend2 checks, the kernel does
// not), out gains why: the defs `-o` leaves out of scope, or else the
// kernel's error on the translation, with the failing call if live
import * as child from "node:child_process";
import * as fs from "node:fs";

const files = fs.readFileSync(0, "utf8").split("\n").filter((l) => l !== "");
const CAP = Number(process.env.CAP ?? 30) * 1000;
type Got = { f: string; code: number | null; ms: number; out: string };
const res: Got[] = [];

function one(f: string): Promise<void> {
  return new Promise((done) => {
    const kid = child.spawn(process.execPath, ["bend2/main.ts", f, "--verdict"], { env: process.env });
    let txt = "";
    kid.stdout.on("data", (d) => { txt += d; });
    kid.stderr.on("data", (d) => { txt += d; });
    const t0 = Date.now();
    const bomb = setTimeout(() => kid.kill("SIGKILL"), CAP);
    kid.on("close", (code) => {
      clearTimeout(bomb);
      if (txt.includes("Sorry - ")) {
        const tt = f.replace(/\.bend$/, ".bendtt");
        const run = (bin: string, args: string[]) => child.spawnSync(bin, args, { encoding: "utf8", timeout: 20000 });
        const oos = run(process.execPath, ["bend2/main.ts", f, "-o", tt]).stderr.trim();
        let why = oos !== "" ? oos : "BendTT: " + run(process.env.BENDTT ?? "", [tt]).stdout.trim();
        const m = /^In (\S+):\naffine live code/m.exec(why);
        if (m !== null) {
          const d = run(process.execPath, ["gates/safe_diag.ts", tt, m[1]]);
          why = why.replace("affine live code, calls that descend", "affine live code, calls that descend: " + (d.stdout + d.stderr).trim().slice(0, 300));
        }
        txt += why + "\n";
      }
      res.push({ f, code, ms: Date.now() - t0, out: txt.slice(0, 1500) });
      done();
    });
  });
}

let next = 0;
await Promise.all(Array.from({ length: Number(process.env.PAR ?? 8) }, async () => {
  while (next < files.length) {
    await one(files[next++]);
  }
}));
const ok = res.find((g) => g.code === 0);
if (ok !== undefined) {
  const extra = child.spawnSync(process.execPath,
    ["bend2/main.ts", ok.f, "--verdict", "argument"],
    { env: process.env, encoding: "utf8", timeout: CAP });
  if (extra.status !== 1) {
    throw new Error("--verdict accepted program arguments: " + extra.stdout + extra.stderr);
  }
}
process.stdout.write(JSON.stringify(res));
