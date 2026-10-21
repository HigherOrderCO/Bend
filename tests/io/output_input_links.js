function output_input_links() {
  const fs = require("node:fs");
  const os = require("node:os");
  const path = require("node:path");
  const child = require("node:child_process");
  // Outside the repo, so a run the alarm kills leaves no files in the tree.
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-output-input-"));
  const inputs = new Map([
    ["main.bend", 'import Base\nimport ./lib.bend as Lib\n'
      + 'def Probe.value() -> IO(U32):\n'
      + '  import "./effect.js"\n  import "./effect.c"\n'
      + 'def main() -> U32:\n  Lib.value()\n'],
    ["lib.bend", "import Base\ndef value() -> U32:\n  42\n"],
    ["effect.js", "io_eff(CID(Probe.value), () => 42);\n"],
    ["effect.c", "// Foreign input.\n"],
  ].map(([name, text]) => [path.join(dir, name), text]));
  const main = path.join(dir, "main.bend");
  const options = { encoding: "utf8", timeout: 4000,
    env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9",
      BEND_NO_TELEMETRY: "1" },
  };
  const build = (...outs) => child.spawnSync(process.execPath,
    [path.resolve("bend2/main.ts"), main, ...outs.flatMap((out) => ["-o", out])],
    options);
  const intact = () => [...inputs].every(([file, text]) =>
    fs.readFileSync(file, "utf8") === text);
  const rejects = (got, msg) => got.status === 1 && got.stderr.includes(msg)
    && intact();
  try {
    for (const [file, text] of inputs) fs.writeFileSync(file, text);
    const link = path.join(dir, "out.js");
    const ok = path.join(dir, "ok.js");
    let code = 1;
    if (!rejects(build(main), "is a file the program reads")) return code;
    code++;
    fs.symlinkSync(main, link);
    if (!rejects(build(link), "is a file the program reads")) return code;
    fs.unlinkSync(link);
    code++;
    for (const file of inputs.keys()) {
      fs.linkSync(file, link);
      if (!rejects(build(link), "is a file the program reads")) return code;
      fs.unlinkSync(link);
      code++;
    }
    if (!rejects(build(dir), "or a directory")) return code;
    code++;
    // A rejected output fails the build before any output is written.
    fs.linkSync(main, link);
    if (!rejects(build(ok, link), "is a file the program reads")
      || fs.existsSync(ok)) return code;
    fs.unlinkSync(link);
    code++;
    if (build(ok).status !== 0 || !intact()) return code;
    const run = child.spawnSync(process.execPath, [ok], options);
    if (run.status !== 0 || run.stdout.trim() !== "42") return code;
    return 0;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Output.input_links), output_input_links);
