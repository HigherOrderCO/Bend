function module_text_boundaries() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-text-boundaries-"));
  try {
    const input = path.join(dir, "lib.bend");
    const output = path.join(dir, "lib.mjs");
    fs.writeFileSync(input, `import Base

def pre(a: String) -> String -> String:
  String.append(a)

def join(a: String, b: String) -> String:
  String.append(a, b)

def plain(s: String) -> String:
  s

def char(c: Char) -> Char:
  c

def raw(n: U32) -> List<String>:
  [SCon{Char.from_u32(n), SNil{}}]

def heads(xs: List<String>) -> U32 & U32:
  match xs:
    case []:
      (0, 0)
    case s <> _:
      match s:
        case SNil{}:
          (0, 0)
        case SCon{h, t}:
          (Char.to_u32(h), U32.from_nat(String.length(t)))

def packet() -> List<String>:
  [SCon{Chr{55296}, SCon{Chr{97}, SNil{}}}]

def size(xs: List<String>) -> U32:
  match xs:
    case []:
      0
    case s <> _:
      U32.from_nat(String.length(s))
`);
    const built = child.spawnSync(process.execPath,
      [path.resolve("bend2/main.ts"), input, "-o", output], {
        encoding: "utf8",
        env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
      });
    if (built.status !== 0) throw new Error(built.stdout + built.stderr);
    const lib = require(output).default;
    const rows = ["closure: " + lib.pre("ab")("cd"),
      "curried: " + lib.join("ab")("cd")];
    for (const [key, text] of [["DC00", "\uDC00xyz"], ["DC05", "\uDC05xyz"]]) {
      const result = lib.heads({ $: "Con", head: text, tail: { $: "Nil" } });
      rows.push("low " + key + ": " + result.fst + "," + result.snd);
    }
    const packet = lib.packet();
    rows.push("payload controls: " + /[\u0000-\u001F]/u.test(packet.head));
    rows.push("length: " + lib.size(packet));
    rows.push("surrogate: " + lib.char("\uD800"));
    const codes = [0xD7FF, 0xD800, 0xDC00, 0xDC03, 0xDC05, 0xDFFF,
      0xE000, 0x10FFFF, 0x110000, 0x40000000, 0x80000000, 0xC0000000,
      0xFFFFFFFF];
    rows.push("raw U32 codes: " +
      codes.every(code => lib.heads(lib.raw(code)).fst === code));
    let errors = 0;
    for (const invoke of [() => lib.plain(undefined), () => lib.char(7)]) {
      try {
        invoke();
      } catch (error) {
        if (String(error).startsWith("bend:")) ++errors;
      }
    }
    rows.push("bad host types: " + errors);
    return rows.join("\n");
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Module.text_boundaries), module_text_boundaries);
