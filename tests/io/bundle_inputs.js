function bundle_inputs() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(require("node:os").tmpdir(), "bend-bundle-test-"));
  const cli = path.resolve("bend2/main.ts");
  const html = '<!-- source -->\n<script type="module">console.log(42);</script>\n';
  const put = (name, text) => {
    const file = path.join(dir, name);
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, text);
    return file;
  };
  const build = (page, out, preload) => child.spawnSync(process.execPath,
    [...(preload ? ["--preload", preload] : []), cli, page, "-o", out], {
      encoding: "utf8", env: { ...process.env, BEND_NO_TELEMETRY: "1" },
    });
  const check = (ok, what) => {
    if (!ok) throw new Error("bundle_inputs: " + what);
  };
  const unchanged = (files) => [...files].every(([p, text]) =>
    fs.readFileSync(p, "utf8") === text);
  const snapshot = (out) => new Map(fs.readdirSync(out).map((name) => {
    const p = path.join(out, name);
    return [p, fs.readFileSync(p, "utf8")];
  }));
  try {
    const page = put("source/page.html", html);
    const inplace = build(page, path.dirname(page));
    check(inplace.status === 1 && fs.readFileSync(page, "utf8") === html,
      "direct input collision");
    const link = path.join(path.dirname(page), "alias.html");
    fs.symlinkSync(page, link);
    const linked = path.join(dir, "linked");
    fs.mkdirSync(linked);
    fs.symlinkSync(page, path.join(linked, "page.html"));
    check(build(link, linked).status === 1 && fs.readFileSync(page, "utf8") === html
      && fs.readdirSync(linked).length === 1, "symlink input collision");
    const hard = path.join(dir, "hard");
    fs.mkdirSync(hard);
    fs.linkSync(page, path.join(hard, "page.html"));
    check(build(page, hard).status === 1 && fs.readFileSync(page, "utf8") === html
      && fs.readdirSync(hard).length === 1, "hardlink input collision");
    const safe = path.join(dir, "safe");
    fs.mkdirSync(safe);
    const safe_link = path.join(safe, "alias.html");
    fs.symlinkSync(page, safe_link);
    check(build(safe_link, safe).status === 0
      && fs.readFileSync(page, "utf8") === html, "safe input symlink");
    // Find the output rather than requiring Bun's old _.._ directory layout.
    const built_html = fs.readdirSync(safe, { recursive: true }).find((n) => n.endsWith(".html")
      && !fs.lstatSync(path.join(safe, n)).isSymbolicLink());
    check(built_html !== undefined, "safe symlink produced no HTML");
    const built_page = path.join(safe, built_html);
    const js = fs.readFileSync(built_page, "utf8")
      .match(/src="([^"]+\.js)"/)[1];
    const ran = child.spawnSync(process.execPath, [path.resolve(path.dirname(built_page), js)],
      { encoding: "utf8" });
    check(ran.status === 0 && ran.stdout.trim() === "42", "safe bundle runs");

    const inner = put("graph/inner.bend", "import Base\n\ndef value() -> U32:\n  42\n");
    put("graph/outer.bend", "import Base\nimport ./inner.bend as Inner\n\n"
      + "def value() -> U32:\n  Inner.value()\n");
    const binary = put("graph/image.bin", Buffer.from([0, 1, 2, 255]));
    const config = put("graph/tsconfig.json",
      '{ // JSONC\n "extends": "../settings/base.json",\n}\n');
    const base = put("settings/base.json",
      '{"compilerOptions":{"baseUrl":"../graph","paths":{"@value":["./value.ts"]}}}\n');
    put("graph/value.ts", "export default 42;\n");
    const pkg = put("graph/node_modules/pkg/package.json",
      '{"name":"pkg","exports":"./value.ts"}\n');
    put("graph/node_modules/pkg/value.ts", "export default 42;\n");
    const ts = put("graph/main.ts", 'import M from "./outer.bend"; '
      + 'import image from "./image.bin"; import value from "@value"; '
      + 'import pkg from "pkg"; console.log(M.value(), image, value, pkg);\n');
    const css = put("graph/style.css", "/* source */\nbody { color: red; }\n");
    const graph = put("graph/page.html", '<link rel="stylesheet" href="./style.css">\n'
      + '<script type="module" src="./main.ts"></script>\n');
    const out = path.join(dir, "out");
    check(build(graph, out).status === 0, "graph preview build");
    const asset = fs.readdirSync(out).find((n) => n.endsWith(".bin"));
    check(asset !== undefined && fs.readFileSync(path.join(out, asset))
      .equals(fs.readFileSync(binary)), "binary asset bytes");
    const chunk = path.join(out, fs.readdirSync(out).find((n) => n.endsWith(".js")));
    const chunk_text = fs.readFileSync(chunk, "utf8");
    for (const source of [graph, ts, css, inner, binary, config, base, pkg]) {
      const original = fs.readFileSync(source);
      fs.unlinkSync(chunk);
      fs.symlinkSync(source, chunk);
      const previous = snapshot(out);
      check(build(graph, out).status === 1
        && fs.readFileSync(source).equals(original) && unchanged(previous),
        "output aliases " + source);
      fs.unlinkSync(chunk);
      fs.writeFileSync(chunk, chunk_text);
    }
    const file_out = put("not-a-directory", "keep\n");
    const invalid = build(graph, file_out);
    check(invalid.status === 1
      && invalid.stderr.includes("-o " + file_out + " is not a directory"),
      "clear output-directory error");
    const page_out = path.join(out, "page.html");
    for (const file of fs.readdirSync(out)) put("out/" + file, "old " + file);
    const old = snapshot(out);
    if (process.getuid?.() !== 0) {
      fs.chmodSync(page_out, 0o444);
      try {
        check(build(graph, out).status === 1 && unchanged(old), "read-only output");
      } finally {
        fs.chmodSync(page_out, 0o644);
      }
    }
    function fail_publish() {
      const fs = require("node:fs");
      const rename = fs.renameSync;
      let n = 0;
      fs.renameSync = (from, to) => {
        if (String(from).endsWith("/next") && ++n === 2) {
          throw new Error("injected bundle publish failure");
        }
        return rename(from, to);
      };
      require("node:module").syncBuiltinESMExports();
    }
    const fault = put("fault.js", "(" + fail_publish.toString() + ")();\n");
    const failed = build(graph, out, fault);
    check(failed.status === 1 && failed.stderr.includes("injected bundle publish failure")
      && unchanged(old) && fs.readdirSync(out).length === old.size,
      "failed publication restores old outputs");
    const fresh = path.join(dir, "fresh");
    const first = build(graph, fresh, fault);
    check(first.status === 1 && first.stderr.includes("injected bundle publish failure")
      && !fs.existsSync(fresh), "failed publication removes new outputs");
    return 0;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Bundle.inputs), bundle_inputs);
