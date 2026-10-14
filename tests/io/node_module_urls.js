function node_module_urls() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-node-urls-"));
  try {
    const input = path.join(dir, "lib with space.bend");
    const plain = path.join(dir, "plain.mjs");
    fs.writeFileSync(plain, "export default 88;\n");
    const program = `
import assert from "node:assert/strict";
import fs from "node:fs";
import {pathToFileURL} from "node:url";
const [input, plain] = process.argv.slice(1);
const source = (n) => "import Base\\n\\ndef add(a: U32, b: U32) -> U32:\\n  U32.add(U32.add(a, b), " + n + ")\\n";
const file = pathToFileURL(input).href;
fs.writeFileSync(input, source(0));
const first = (await import(file)).default;
assert.equal(first.add(19, 23), 42);
fs.writeFileSync(input, source(1));
const cached = (await import(file)).default;
assert.equal(cached.add(19, 23), 42);
const query = (await import(file + "?revision=1")).default;
assert.equal(query.add(19, 23), 43);
fs.writeFileSync(input, source(2));
const fragment = (await import(file + "#revision2")).default;
assert.equal(fragment.add(19, 23), 44);
assert.equal((await import(pathToFileURL(plain).href + "?asset=types.bend")).default, 88);
assert.equal((await import("data:text/javascript,export%20default%207%3B%2F%2F.bend")).default, 7);
`;
    const ran = child.spawnSync("node", ["--experimental-transform-types",
      "--import", path.resolve("bend2/main.ts"), "--input-type=module",
      "--eval", program, input, plain], { encoding: "utf8" });
    if (ran.status !== 0) {
      process.stderr.write(ran.stderr ?? String(ran.error));
    }
    return ran.status ?? 1;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Module.node_urls), node_module_urls);
