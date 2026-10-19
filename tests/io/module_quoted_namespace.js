function module_quoted_namespace() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-quoted-name-"));
  try {
    const input = path.join(dir, "lib.bend");
    const output = path.join(dir, "lib.mjs");
    const namespace = 'quoted"back\\slash';
    fs.writeFileSync(input, "import Base\ndef value(x: U32) -> U32:\n  U32.add(x, 1)\n");
    const source = `
      import * as Bend from ${JSON.stringify(path.resolve("bend2/bend.ts"))};
      import * as Comp from ${JSON.stringify(path.resolve("bend2/comp.ts"))};
      const book = Bend.book_nil();
      await Bend.book_load(book, ${JSON.stringify(input)},
        ${JSON.stringify(namespace)}, new Map());
      Bend.book_valid(book);
      await Bun.write(${JSON.stringify(output)}, Comp.js_lib(book, true));
    `;
    const built = child.spawnSync(process.execPath, ["-e", source], {
      encoding: "utf8",
      env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
    });
    if (built.status !== 0) return 1;
    const lib = require(output).default;
    return lib[namespace + ":value"](41) === 42 ? 0 : 2;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Module.quoted_namespace), module_quoted_namespace);
