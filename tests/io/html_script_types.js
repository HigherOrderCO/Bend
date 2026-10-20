function html_script_types() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-html-types-"));
  const options = { encoding: "utf8", timeout: 6000,
    env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9",
      BEND_NO_TELEMETRY: "1" },
  };
  try {
    fs.writeFileSync(path.join(dir, "lib.bend"), "import Base\ndef value() -> U32:\n  42\n");
    const cases = [
      ["module", "42\n"],
      ["mod&#117;le", "42\n"],
      ["&#x6d;odule", "42\n"],
      ["&Tab;MODULE&NewLine;", "42\n"],
      ["\u00a0module", ""],
      ["&#160;module", ""],
      ["&tab;module", ""],
    ];
    for (const [i, [type, expected]] of cases.entries()) {
      const page = path.join(dir, "index.html");
      const out = path.join(dir, "out" + i);
      fs.writeFileSync(page, `<script type="${type}">import lib from './lib.bend'; console.log(lib.value());</script>`);
      const built = child.spawnSync(process.execPath,
        [path.resolve("bend2/main.ts"), page, "-o", out], options);
      if (built.status !== 0) return 1;
      const entries = [];
      new HTMLRewriter().on("script", { element(el) {
        const src = el.getAttribute("src");
        if (src !== null) entries.push(path.resolve(out, src));
      }}).transform(fs.readFileSync(path.join(out, "index.html"), "utf8"));
      let output = "";
      for (const entry of entries) {
        const run = child.spawnSync(process.execPath, [entry], options);
        if (run.status !== 0) return 2;
        output += run.stdout;
      }
      if (output !== expected) return 3 + i;
    }
    return 0;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(HTML.script_types), html_script_types);
