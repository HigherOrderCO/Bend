# Bend 2 formatter language server

`bend2-fmt-lsp` is a formatting-only language server for Bend 2. It supports
full-document formatting over stdio and intentionally exposes no diagnostics,
completion, hover, range-formatting, or on-type-formatting features.

## Install and run

Node.js 22 or newer is required. From the repository root:

```sh
cd tools/bend-fmt-lsp
npm ci
npm run build
```

To run the server directly from this checkout, use `node dist/server.js --stdio`.
To use the editor configurations below, install the built package as a command:

```sh
npm install -g .
```

Run this from `tools/bend-fmt-lsp` after building. Without the global install,
configure your editor to run `node` with the absolute path to `dist/server.js`
and `--stdio` instead.

The server handles documents whose language ID is `bend` or `bend2`. It
preserves line breaks, blank lines, comments, literal spelling, line endings,
and the final-newline state. Formatting normalizes indentation and safe token
spacing without wrapping code. A line that Bend pins to a column, as the second
statement after `do IO<Unit>: IO.print("a")`, moves as far as the token it
lines up with. When a document cannot be tokenized safely, the
server returns no edits.

## Editor setup

Neovim with `nvim-lspconfig`:

```lua
vim.api.nvim_create_autocmd("FileType", {
  pattern = "bend",
  callback = function()
    vim.lsp.start({
      name = "bend2-fmt-lsp",
      cmd = { "bend2-fmt-lsp", "--stdio" },
      root_dir = vim.fs.root(0, { ".git" }),
    })
  end,
})
```

Helix (`languages.toml`):

```toml
[language-server.bend2-fmt-lsp]
command = "bend2-fmt-lsp"
args = ["--stdio"]

[[language]]
name = "bend"
scope = "source.bend"
file-types = ["bend"]
language-servers = ["bend2-fmt-lsp"]
auto-format = true
```

Emacs with Eglot:

```elisp
(add-to-list 'eglot-server-programs
             '(bend-mode . ("bend2-fmt-lsp" "--stdio")))
```
