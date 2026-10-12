// bend-ids 1
// CID :Module.export_keys BEND_CID_Module_export_keys
// bend-ids end
function module_export_keys() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-export-keys-"));
  try {
    const input = path.join(dir, "lib.bend");
    const output = path.join(dir, "lib.mjs");
    fs.writeFileSync(path.join(dir, "dep.bend"), `import Base
def value(x: U32) -> U32:
  U32.add(x, 10)
`);
    fs.writeFileSync(input, `import Base
import ./dep.bend as D

def __proto__(x: U32) -> U32:
  U32.add(x, 1)

def constructor(x: U32) -> U32:
  U32.add(x, 2)

def toString(x: U32) -> U32:
  U32.add(x, 3)

def ordinary(x: U32) -> U32:
  U32.add(x, 4)

def dep.value(x: U32) -> U32:
  U32.add(x, 20)
`);
    const built = child.spawnSync(process.execPath,
      [path.resolve("bend2/main.ts"), input, "-o", output], {
        encoding: "utf8",
        env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
      });
    if (built.status !== 0) {
      return 1;
    }
    const lib = require(output).default;
    const Bend = require(path.resolve("bend2/bend.ts"));
    const file = Bend.file_name(fs.realpathSync(input));
    const dep = Bend.file_name(fs.realpathSync(path.join(dir, "dep.bend")));
    const values = { ["__proto__"]: 6, constructor: 7, toString: 8, ordinary: 9, "dep.value": 25 };
    const keys = Object.keys(values), imported = Bend.name_source(dep, "value");
    if (keys.some((k) => !Object.hasOwn(lib, k)) || !Object.hasOwn(lib, imported)) {
      return 2;
    }
    const names = Object.keys(lib);
    if (names.length !== keys.length * 2 + 1 || keys.some((k) =>
      !Object.hasOwn(lib, Bend.name_source(file, k))
      || lib[Bend.name_source(file, k)] !== lib[k])) {
      return 3;
    }
    if (Object.getPrototypeOf(lib) !== null) {
      return 4;
    }
    const copy = { ...lib };
    return Object.getPrototypeOf(copy) === Object.prototype
      && names.every((k) => Object.hasOwn(copy, k) && copy[k] === lib[k])
      && lib[imported](5) === 15 && copy[imported](5) === 15
      && keys.every((k) => lib[k](5) === values[k] && copy[k](5) === values[k]) ? 0 : 5;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(BEND_CID_Module_export_keys, module_export_keys);
