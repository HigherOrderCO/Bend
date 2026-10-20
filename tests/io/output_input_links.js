function output_input_links() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-output-input-"));
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
  const options = { encoding: "utf8", timeout: 6000,
    env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9",
      BEND_NO_TELEMETRY: "1" },
  };
  const build = (out) => child.spawnSync(process.execPath,
    [path.resolve("bend2/main.ts"), main, "-o", out], options);
  const intact = () => [...inputs].every(([file, text]) =>
    fs.readFileSync(file, "utf8") === text);
  try {
    for (const [file, text] of inputs) fs.writeFileSync(file, text);
    let code = 1;
    for (const file of inputs.keys()) {
      for (const link of [null, fs.symlinkSync, fs.linkSync]) {
        const out = link === null ? file : path.join(dir, "out.js");
        if (link !== null) link(file, out);
        const got = build(out);
        if (got.status !== 1 || !got.stderr.includes("is a file the program reads")
          || !intact()) return code;
        if (link !== null) fs.unlinkSync(out);
        code++;
      }
    }
    for (const ext of [".c", ".mjs", ".bendtt", ""]) {
      const out = path.join(dir, "out" + ext);
      fs.linkSync(main, out);
      if (build(out).status !== 1 || !intact()) return code;
      fs.unlinkSync(out);
      code++;
    }
    const directory = build(dir);
    if (directory.status !== 1
      || !directory.stderr.includes("or a directory") || !intact()) return code;
    code++;
    const out = path.join(dir, "ok.js");
    for (const existing of [false, true]) {
      if (existing) fs.writeFileSync(out, inputs.get(main));
      if (build(out).status !== 0 || !intact()) return code;
      const run = child.spawnSync(process.execPath, [out], options);
      if (run.status !== 0 || run.stdout.trim() !== "42") return code;
      code++;
    }
    return 0;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Output.input_links), output_input_links);
