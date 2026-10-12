# bend-lint

A linter and formatter for Bend 2.

It runs your file through Bend's type checker first. If that passes, it runs
whatever rules you loaded. Rules are TypeScript or Bend files, and they can see
the source text, the types, variable scope and how often each variable is used.
A rule can also suggest a fix.

bend-lint is also maintained at
[github.com/MattCozendey/bend-lint](https://github.com/MattCozendey/bend-lint).

## Setup

You need Bun 1.2 or newer. Run everything from the repo root. Type checking
needs `bun install` in `tools/bend-lint` first.

## Running it

```sh
bun tools/bend-lint/src/lint.ts file.bend --rules tools/bend-lint/rules/format.ts
```

Nothing runs by default. You have to pass `--rules`, and you can pass it more
than once:

```sh
bun tools/bend-lint/src/lint.ts file.bend --rules tools/bend-lint/rules/format.ts --rules tools/bend-lint/rules/file_length.bend
```

Add `--fix` to write the fixes back to the file.

```
bun tools/bend-lint/src/lint.ts <file.bend> [--rules <file>]... [--config <file>]
                [--fix | --fix-suggested | --fix-dangerously]
                [--json] [--bend <dir>]
```

Every fix is marked `safe`, `suggested` or `dangerous`:

- `--fix` applies the safe ones. They shouldn't change what the program does.
- `--fix-suggested` adds the suggested ones, which might.
- `--fix-dangerously` applies everything. Expect breakage sometimes.

Fixes go into the file you passed in, even if the run had errors. A fix that
also edits another file is skipped. When two fixes overlap, the later one is
skipped. Run it again to get it.

Exit code is 0 if there were no errors (warnings don't count), 1 for a checker
or rule error, and 2 for bad arguments or a crash.

`--json` prints the findings as JSON. The format is in
[JSON output](#json-output).

## Where Bend comes from

bend-lint loads Bend's TypeScript source and patches it in memory. To find that
source it tries, in order:

1. `--bend <dir>` or `$BEND_DIR`. A Bend checkout or its `bend2` folder both work.
2. The Bend checkout around it. This is the case in `tools/bend-lint`.
3. A release downloaded from GitHub. It matches `bend version` if Bend is on
   your PATH, otherwise it takes the latest.

Downloads are cached in `~/.cache/bend-lint/`. `XDG_CACHE_HOME` moves that, and
on Windows it's under `LOCALAPPDATA`. With no network it falls back to the
newest cached release.

## Rules included

`format/layout` (`tools/bend-lint/rules/format.ts`) is the formatter. It fixes indentation,
spacing, comments, blank lines between declarations, wrapping and the final
newline. Options: `tabWidth` (default 2) and `wrapAtWidth` (default 100). Both
must be positive integers, or `"never"` for `wrapAtWidth`. The width is a target;
long literals and comments can run past it. `endOfLine` is `"lf"` (default),
`"crlf"` or `"preserve"`; preserve uses the first line ending, or LF if there
is none. Line endings inside literals stay as written. Its fix is `safe`.

Declaration order and literals stay as written. The only comment change is a
space added after `#` when it's missing. Before offering a fix it parses
its own output and compares that to the original program. If they differ you
get a warning and no fix.

`style/file-length` (`tools/bend-lint/rules/file_length.bend`) warns when the file has more than
`maxLines` lines (default 500). Imports don't count. No fix.

## Config

Put a `bend-lint.json`, `bend-lint.js` or `bend-lint.ts` next to the file you're
linting or in any parent folder, or point at one with `--config`. The closest
folder wins. Inside a folder, JSON beats JS beats TS. Only one file is read.

```json
{
  "rules": {
    "format/layout": { "tabWidth": 2, "wrapAtWidth": 100 },
    "style/file-length": { "maxLines": 400, "severity": "warning" }
  }
}
```

JS and TS configs export the same thing as a named `config`.

```ts
export const config = {
  rules: {
    "style/file-length": { maxLines: 400 },
  },
};
```

Rules still come from `--rules`; config sets their options and severity.
`"off"` turns a rule off. Severities are `error`, `warning`, `information` and `hint`, and an
`error` stops the rules after it.

If a rule declares defaults for its options, unknown keys and wrong types are
rejected. Otherwise the rule checks its own options.

## What you get back

If Bend's checker rejects the file you get a `bend/check` error and no rules
run.

## Writing a rule

A rule is a TypeScript or Bend file. Save one under `rules/`, or anywhere, and load it with `--rules`.

### TypeScript rules

A module exports `rules: LintRule[]`. A rule has an `id` (`namespace/name`, it
becomes the finding's `code`) and `run(cx, signal)`, which returns diagnostics
and can be async. It can also have `facts` and `options`. If `run` throws or the
signal aborts, the error goes to whoever called the library.

Build findings with `cx.diag()`. Severity defaults to `warning`. Rules run in
order and see earlier findings in `cx.prior`. The first `error` ends the run:
the rest of that rule's findings and every later rule are dropped.

What's on `cx`:

- `root`, `sources`: the linted file and its imports, text as on disk
- `options`: defaults merged with config
- `facts`: just the facts this rule asked for
- `prior`: findings from earlier rules
- `view(fact)`: what the fact's term is (see Facts)
- `type(fact)`: the type the term was checked as
- `binder(fact)`: the declared type of the variable a `Var` uses
- `show`, `same`, `normal`: print, compare and normalize types in a fact's scope
- `uses(fact)`: used variables with their quantities
- `sameDeclarations(text)`: whether `text` declares what the linted file
  declares, compared as parsed, not checked
- `body(name)`, `node(fact)`, `shape(node)`, `fact(node)`: the term view (see
  Nodes)
- `diag(init)`: make a diagnostic with this rule's ID
- `unstable`: Bend's own objects (`Bend`, the checked `book`, `raw(fact)`).
  Code that uses them breaks when Bend changes; nothing else on `cx` does.

The exported types in [src/lint.ts](src/lint.ts) are the full contract. They
are bend-lint's own: facts, types and nodes are handles, and only `cx` reads
them.

#### Fixes

A fix is a title, an applicability (`safe`, `suggested`, `dangerous`) and edits.
An edit swaps a span for text. A span is a source from `cx.sources` and two
offsets. A zero-length span inserts. Offsets are integer UTF-16 units inside the
source, and edits in one fix can't overlap.

```ts
const span = { file: cx.root, beg: 0, end: 1 };
return [cx.diag({
  message: "Replace the first character with a space.",
  span,
  fixes: [{
    title: "Replace character",
    applicability: "suggested",
    edits: [{ span, text: " " }],
  }],
})];
```

(Needs a nonempty file.)

#### Facts

A fact is one checked term. `cx.view(fact)` gives its kind (annotations
stripped: `Var`, `Ref`, `App`, ...), the name a `Var` or `Ref` points to, its
enclosing definition (`owner`), how many times it is demanded (`quantity`:
`erased`, `once` or `many`), and its source span (`span`, and `inner` without
the annotations), when there is one. Its type and its scope are reached through
the other `cx` operations.

Ask for them with `facts: true` (everything in the linted file) or a filter:

```ts
facts: {
  scope: "file",   // default. "program" adds imports, not Base
  kinds: ["Var"],  // term kind without annotations: Var, Ref, App, ...
  defs: ["main"],  // enclosing definition, instances count as their template
  names: ["foo"],  // name used by a Var or Ref
}
```

All the lists you give must match. Leave one out or empty and it matches
everything. Keep filters narrow, since memory goes up with the number of facts.

Template bodies get checked as written and again per instance (`generic~0`),
sometimes at the same span. `view(fact).inst` marks instances. Skip them to
avoid double reports:

```ts
import type { LintRule } from "../src/lint.ts";

export const rules: LintRule[] = [{
  id: "demo/var-types",
  facts: { kinds: ["Var"] },
  run: (cx) => cx.facts!
    .filter((fact) => !cx.view(fact).inst)
    .map((fact) => cx.diag({
      message: "type: " + cx.show(fact, cx.type(fact)),
      severity: "hint",
      span: cx.view(fact).span,
      fact,
    })),
}];
```

(Saved under `rules/`.)

#### Nodes

A node is one term of a def's checked body. `cx.body(name)` gives the root,
`cx.shape(node)` its kind (annotations kept: `Ann`, `Var`, `App`, ...), the name
a `Var` or `Ref` points to, its span and its children, in Bend's order.
`cx.node(fact)` is the node a fact is about, and `cx.fact(node)` is the node's
fact, if this rule asked for it. Kinds and child order are Bend's, so a rule
that reads them may need changes when Bend's terms change; facts alone do not.

```ts
const nodes = (node: Node): Node[] => [node, ...cx.shape(node).children.flatMap(nodes)];
const matches = nodes(cx.body("main")!).filter((n) => cx.shape(n).kind === "Mat");
```

#### Options

`options: { tabWidth: 2, breakLines: false }` sets defaults. They also set which
keys are allowed and their types (`number`, `boolean`, `string`). Unknown keys
and wrong types are errors. A rule with no defaults gets the raw options and has
to check them itself.

### Bend

A `.bend` rule imports [src/bend/lint.bend](src/bend/lint.bend). Here's one that reports
nothing, saved under `rules/`:

```python
import Base
import ../src/bend/lint.bend as Lint

def id() -> String:
  "demo/nothing"

def facts() -> Lint.Want:
  Lint.NoFacts{}

def run(input: Lint.Input) -> IO(List<&2, Lint.Diag>):
  IO.pure(List<&2, Lint.Diag>, [])

def main() -> IO(Unit):
  Lint.serve(run)
```

`input` has the sources and options. To get facts, return a filter from
`facts()`, e.g. `Lint.Want{Lint.File{}, ["Var"], [], []}`. Empty lists match
everything, and `Lint.Program{}` adds imports (not Base).

Read facts one by one with `next_fact`, or fold:

```python
def step(found: List<&2, Lint.Diag>, +f: Lint.Fact) -> IO(List<&2, Lint.Diag>):
  ...

def run(input: Lint.Input) -> IO(List<&2, Lint.Diag>):
  Lint.fold_facts(~List<&2, Lint.Diag>, ~step, [])
```

`step` goes in as a template (`~step`) and takes the fact as `+f`. Effects:
`view`, `type_of`, `binder`, `same`, `show`, `normal`, `uses`, `text`, and the
term view `body`, `node`, `shape` and `fact`, plus Base's. Options come from `Lint.option_number`, `option_flag` and `option_text`,
each with a default. Numbers are whole, 0 to 4294967295 (U32).

Spans count Unicode code points here. TypeScript counts UTF-16 units.

The rule is compiled once and runs on Bend's JavaScript runtime. Use tail
recursion over long text or lists to stay inside the stack. Streaming facts
saves building a list of all of them.

For examples see [file_length.bend](rules/file_length.bend) and
[shared.bend](rules/shared.bend). `COMMA_BEND`, `TYPES_BEND` and `COUNT_BEND` in
[src/lint.test.ts](src/lint.test.ts) cover fixes and facts.

### As a library

From a file in `tools/bend-lint`:

```ts
import { applyFixes, bendRule, lint, render } from "./src/lint.ts";
import { rules } from "./rules/format.ts";

const result = await lint("example.bend", [
  ...rules,
  await bendRule("rules/file_length.bend"),
]);
console.log(result.ok, result.diags.map(render));

if (result.ok) {
  const root = result.sources.find((source) => source.root)!;
  const { text, skipped } = applyFixes(root, result.diags);
  // text is the edited source; saving it is up to you
  console.log(text, skipped);
}
```

Set `BEND_DIR` before importing bend-lint to pick a Bend checkout. Import
bend-lint before you load Bend yourself, so the patching happens first. `lint`
takes `{ config, signal, unsaved }` as a third argument. Without `config` it looks
next to the file. `unsaved` maps file paths to editor text that isn't saved yet.
Bend and the rules read that text instead of the file, for that run only. Runs
at the same time wait for each other's check, one at a time; rules still run
side by side. `position(span)` gives an LSP range.

`findConfig(file)` checks the file's directory, then each parent, for
`bend-lint.json`, `.js` or `.ts`. Closest directory wins, then JSON, JS, TS.
`readConfig(file)` loads a path you give it. Both are synchronous. JS and TS
configs export a named `config` object and go through Bun's loader, cache
included. They can import relative files. Rule settings and option validation
are the same for all three formats.

### JSON output

`--json` prints one JSON object on stdout. Status and failure messages can still
show up on stderr. A finding with a fix:

```json
{
  "ok": true,
  "findings": [{
    "code": "style/example",
    "severity": "warning",
    "message": "Replace this character.",
    "def": "main",
    "path": "/abs/example.bend",
    "range": {
      "start": { "line": 2, "character": 0 },
      "end": { "line": 2, "character": 1 }
    },
    "fixes": [{
      "title": "Replace character",
      "applicability": "suggested",
      "edits": [{
        "path": "/abs/example.bend",
        "range": {
          "start": { "line": 2, "character": 0 },
          "end": { "line": 2, "character": 1 }
        },
        "text": " "
      }]
    }]
  }]
}
```

`def`, `path` and `range` are left out when there's nothing to put. Lines start
at 0, characters are UTF-16. With a fix flag, findings describe the source as it
was before the fixes.

## How it works

### Patching

Bun patches Bend's modules while they load. `term_infer` and `term_check` are
renamed and wrapped to record what they return to `hook.see` in seam.ts. The
wrappers pass every argument on. The hook is global, so checks run one at a
time. The `fs` and `path` adapters, in `bend.ts` and `main.ts`, turn real paths
into `/` paths so imports resolve on Windows. `comp.ts` also exports
`RUNTIME_MAIN` and `js_sat`, which compiling Bend rules needs. `main.ts` exports
`book_read`, `book_err` and `Check_Fail`: a file is read and checked by the same
code as `bend <file>`, never a copy of it. Imported, `main.ts` also registers
Bend's loader for `import "x.bend"`.

If a file has a line that's exactly `import Base` (what `bend --checkup` reads),
it reuses one Base, checked once per process. Base facts are left out of rule
requests.

A failed check is one `bend/check` finding. Its message is Bend's, without the
location: expected and observed, with the names in scope, and Bend's note.

### Staying compatible

We don't pin a Bend version. Instead, on load, for checkouts and downloads
alike:

- every source edit has to match exactly once, the compiler exports we need
  have to be declared, and the runtime names a Bend rule's run calls
  (`cli_args`, `io_run`) have to be there once
- wrapper signatures are checked against Bend's at type-check time, and argument
  counts at runtime
- `book_read`, `book_err` and `Check_Fail` from `main.ts` must take the
  arguments bend-lint gives them
- a self-check runs `src/bend/sample.bend` through the check and the rule
  operations: `x` in `def id(x: N) -> N: x` must have the right type, binder,
  quantity, uses and span, and each wrapper must report the terms only it sees

Any mismatch throws a drift error (`name` is `DriftError`, and `e[DRIFT]` is
true, with `DRIFT` from `src/seam.ts`) and loading stops. The tests add more, including
drift against the chosen checkout's own tests.

## Tests

```sh
bun test tools/bend-lint
bunx tsc -p tools/bend-lint/tsconfig.json --noEmit
```

Bun doesn't type check, so run `tsc` as well (`bun install` in `tools/bend-lint` provides `@types/bun`).
`tsc` reports two errors in `bend2/bend.ts` (lines 2040 and 3771), which are in
Bend itself.
