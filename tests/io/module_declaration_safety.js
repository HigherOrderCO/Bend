function declaration_safety() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-declaration-"));
  try {
    const input = path.join(dir, "source.bend");
    const source = "import Base\n\ndef value(x: U32) -> U32:\n  x\n";
    fs.writeFileSync(input, source);
    const output = path.join(dir, "module.mjs");
    const declaration = path.join(dir, "module.d.mts");
    const run = () => child.spawnSync(process.execPath,
      [path.resolve("bend2/main.ts"), input, "-o", output], { encoding: "utf8" });
    fs.symlinkSync(input, declaration);
    const alias = run();
    if (alias.status === 0 || fs.readFileSync(input, "utf8") !== source
      || fs.existsSync(output)) return 1;
    fs.unlinkSync(declaration);
    fs.mkdirSync(declaration);
    const directory = run();
    if (directory.status === 0 || fs.readFileSync(input, "utf8") !== source
      || fs.existsSync(output)) return 2;
    return 0;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Module.declaration_safety), declaration_safety);
