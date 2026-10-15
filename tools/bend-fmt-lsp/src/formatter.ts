export type FormatOptions = {
  tabSize?: number;
  insertSpaces?: boolean;
};

type Token = {
  text: string;
  kind: "word" | "number" | "literal" | "symbol";
  gap: boolean;
  col: number;
};

type Line = {
  indent: string;
  code: string;
  comment: string;
  tokens: Token[];
};

const MULTI = ["<&>", ".|.", ".^.", ".&.", "==", "!=", "->", "<-", "=>", "&&", "||", "++", "<>", "<=", ">=", "<<", ">>"];
const WORD = /[A-Za-z0-9_.]/;
const HEAD = /[A-Za-z_]/;
const DIGIT = /[0-9]/;
const BINARY = new Set(["=", "==", "!=", "->", "<-", "=>", "+", "-", "*", "/", "%", "&&", "||", "++", "<>", "<&>", "<=", ">=", "<<", ">>", ".|.", ".^.", ".&.", "&", "|"]);
const PREFIX_CONTEXT = new Set(["(", "{", "[", "<", ",", ":", "=", "for", "case", "~"]);
const ANGLES = new Set(["<", ">", "<<", ">>"]);
const KEYWORDS = new Set(["return", "match", "case", "do", "for", "exs", "where", "is", "import", "def", "type", "law"]);

function splitLine(text: string): Line {
  const indent = text.match(/^[\t ]*/)?.[0] ?? "";
  const code = text.slice(indent.length);
  const tokens: Token[] = [];
  let i = 0;
  let hadGap = false;
  while (i < code.length) {
    if (/\s/.test(code[i])) {
      hadGap = tokens.length > 0;
      i++;
      continue;
    }
    if (code[i] === "#") break;
    const start = i;
    const char = code[i];
    let kind: Token["kind"] = "symbol";
    if (char === "\"" || char === "'") {
      kind = "literal";
      const quote = char;
      i++;
      let escaped = false;
      let closed = false;
      while (i < code.length) {
        const next = code[i++];
        if (escaped) escaped = false;
        else if (next === "\\") escaped = true;
        else if (next === quote) {
          closed = true;
          break;
        }
      }
      if (!closed) throw new Error("unterminated literal");
    } else if (tokens.length === 1 && tokens[0].text === "import" && hadGap) {
      kind = "word";
      while (i < code.length && !/\s/.test(code[i]) && code[i] !== "#") i++;
    } else if (HEAD.test(char)) {
      kind = "word";
      i++;
      while (i < code.length && WORD.test(code[i])) i++;
    } else if (DIGIT.test(char)) {
      kind = "number";
      i++;
      while (i < code.length && DIGIT.test(code[i])) i++;
      if (code[i] === "." && DIGIT.test(code[i + 1] ?? "")) {
        i++;
        while (i < code.length && DIGIT.test(code[i])) i++;
      }
      if (/[eE]/.test(code[i] ?? "") && DIGIT.test(code[i + 1] ?? "") || /[eE]/.test(code[i] ?? "") && /[+-]/.test(code[i + 1] ?? "") && DIGIT.test(code[i + 2] ?? "")) {
        i++;
        if (/[+-]/.test(code[i] ?? "")) i++;
        while (i < code.length && DIGIT.test(code[i])) i++;
      }
      if (code[i] === "n") i++;
    } else {
      const multi = MULTI.find((value) => code.startsWith(value, i));
      i += multi?.length ?? 1;
    }
    tokens.push({ text: code.slice(start, i), kind, gap: hadGap, col: indent.length + start });
    hadGap = false;
  }
  return { indent, code: code.slice(0, i).trim(), comment: code.slice(i), tokens };
}

function unary(tokens: Token[], index: number): boolean {
  const token = tokens[index].text;
  if (!["+", "-", "~", "?", "@", "&", "%"].includes(token)) return false;
  const previous = tokens[index - 1]?.text;
  const next = tokens[index + 1];
  if (!next) return false;
  const atPrefix = index === 0 || PREFIX_CONTEXT.has(previous) || BINARY.has(previous);
  if (token === "~" || token === "%") return atPrefix;
  if (token === "+" || token === "-") return next.kind === "word" && (!next.gap || (index > 0 && atPrefix));
  return next.kind === "word" || next.kind === "number" ? atPrefix : false;
}

function keepAngleGap(left: Token, right: Token): boolean | null {
  if (!ANGLES.has(left.text) && !ANGLES.has(right.text)) return null;
  return right.gap;
}

function needsSpace(tokens: Token[], index: number): boolean {
  const left = tokens[index - 1];
  const right = tokens[index];
  if (!left) return false;
  if ([")", "]", "}", ",", ";"].includes(right.text)) return false;
  if (right.text === ":") return right.gap;
  if (["(", "[", "{"].includes(left.text)) return false;
  if (left.text === ",") return true;
  if (right.text === "!" && (left.kind === "word" || [")", "]", "}"].includes(left.text))) return false;
  if (left.text === "!" && right.text === "(") return false;
  if (right.text === "?" && left.kind === "word" && (!tokens[index + 1] || tokens[index + 1].text === "(")) return right.gap;
  if (left.text === "?" && right.text === "(" && tokens[index - 2]?.kind === "word") return false;
  if (right.text === "(" || right.text === "[") {
    const suffix = (left.kind === "word" && !KEYWORDS.has(left.text)) || left.kind === "number" || left.kind === "literal" || [")", "]", "}", ">", ">>"].includes(left.text);
    return !suffix;
  }
  // A separated brace can start the next annotated argument, rather than a constructor.
  if (right.text === "{" && ((left.kind === "word" && !["return", "case"].includes(left.text)) || [">", ">>", "}"].includes(left.text))) return left.kind === "word" && right.gap;
  if (left.text === "\\" && right.text === "{") return false;
  if (left.text === "." || right.text === ".") return false;
  if (left.kind === "number" && left.text.endsWith("n") && (right.text === "+" || right.text === "++")) return right.gap;
  if ((left.text === "+" || left.text === "++") && tokens[index - 2]?.kind === "number" && tokens[index - 2].text.endsWith("n")) return right.gap;
  if (unary(tokens, index - 1)) return false;
  if (unary(tokens, index)) return !["(", "[", "{", "<"].includes(left.text);
  const angle = keepAngleGap(left, right);
  if (angle !== null) return angle;
  if (BINARY.has(left.text) || BINARY.has(right.text)) return true;
  if (left.text === ":") return true;
  return true;
}

// The formatted code and where each token starts in it.
function formatTokens(tokens: Token[]): [string, number[]] {
  let output = "";
  const starts: number[] = [];
  for (let i = 0; i < tokens.length; i++) {
    if (needsSpace(tokens, i)) output += " ";
    starts.push(output.length);
    output += tokens[i].text;
  }
  return [output, starts];
}

// Lines inside brackets and comment-only lines: their columns mean nothing to
// the parser.
function looseLines(lines: Line[]): boolean[] {
  let open = 0;
  return lines.map((line) => {
    const loose = open > 0 || line.tokens.length === 0;
    for (const token of line.tokens) {
      if (["(", "[", "{"].includes(token.text)) open++;
      else if ([")", "]", "}"].includes(token.text)) open = Math.max(0, open - 1);
    }
    return loose;
  });
}

// The tokens that start a body on the line of its `do`, `match`, `case` or
// `def` header or `=>` lambda opener. As in `do IO<Unit>: a`, Bend pins the
// lines after it to the column of `a`.
function inlineBodies(lines: Line[]): number[][] {
  let open = 0;
  let header: { open: number; angle: number; typed: boolean } | null = null;
  return lines.map((line) => line.tokens.flatMap((token, index) => {
    if (token.kind === "word" && ["do", "match", "case", "def"].includes(token.text)) {
      header = { open, angle: 0, typed: token.text === "do" || token.text === "def" };
    } else if (token.text === "=>" && open === 0) {
      return index + 1 < line.tokens.length ? [index + 1] : [];
    } else if (["(", "[", "{"].includes(token.text)) {
      open++;
    } else if ([")", "]", "}"].includes(token.text)) {
      open = Math.max(0, open - 1);
    } else if (header?.typed && ANGLES.has(token.text)) {
      header.angle += token.text.length * (token.text[0] === "<" ? 1 : -1);
    } else if (header && token.text === ":" && open === header.open && header.angle === 0) {
      header = null;
      return index + 1 < line.tokens.length ? [index + 1] : [];
    }
    return [];
  }));
}

// A tab is one column, as in the parser. Loose lines measure against a copy of
// the levels, so they can't shift the statements after them.
function indentDepths(lines: Line[], loose = looseLines(lines)): number[] {
  const depths: number[] = [];
  const stack = [0];
  let levels = stack;
  for (const [index, line] of lines.entries()) {
    if (line.code === "" && line.comment === "") {
      depths.push(-1);
      continue;
    }
    if (!loose[index]) levels = stack;
    else if (levels === stack) levels = [...stack];
    const width = line.indent.length;
    while (levels.length > 1 && width < levels[levels.length - 1]) levels.pop();
    if (width > levels[levels.length - 1]) levels.push(width);
    else if (width !== levels[levels.length - 1]) levels[levels.length - 1] = width;
    depths.push(levels.length - 1);
  }
  return depths;
}

function fingerprint(lines: Line[]): string {
  const depths = indentDepths(lines);
  return lines.map((line, index) => line.tokens.length === 0 ? "" : depths[index] + ":" + line.tokens.map((token) => token.text).join("\u0000")).join("\n");
}

export function formatBend(source: string, options: FormatOptions = {}): string {
  const eol = source.includes("\r\n") ? "\r\n" : "\n";
  const finalEol = source.endsWith("\n");
  const rawLines = source.split(/\r?\n/);
  if (finalEol) rawLines.pop();
  let lines: Line[];
  try {
    lines = rawLines.map(splitLine);
  } catch {
    return source;
  }
  const loose = looseLines(lines);
  const depths = indentDepths(lines, loose);
  const bodies = inlineBodies(lines);
  const size = Math.max(1, options.tabSize ?? 2);
  const spaces = options.insertSpaces !== false;
  // A line at or right of an inline body's column moves as far as that body.
  const pins: { from: number; to: number; prefix: string }[] = [];
  const formatted = lines.map((line, index) => {
    if (line.code === "" && line.comment === "") return "";
    const width = line.indent.length;
    if (!loose[index]) while (pins.length > 0 && pins[pins.length - 1].from > width) pins.pop();
    const pin = pins.filter((pin) => pin.from <= width).pop();
    const prefix = pin ? pin.prefix + " ".repeat(width - pin.from + pin.to - pin.prefix.length)
      : spaces ? " ".repeat(depths[index] * size) : "\t".repeat(depths[index]);
    const [code, starts] = formatTokens(line.tokens);
    for (const at of bodies[index]) pins.push({ from: line.tokens[at].col, to: prefix.length + starts[at], prefix });
    if (code === "") return prefix + line.comment;
    return prefix + code + (line.comment === "" ? "" : "  " + line.comment);
  }).join(eol) + (finalEol ? eol : "");
  try {
    if (fingerprint(lines) !== fingerprint(formatted.replace(/\r\n/g, "\n").split("\n").slice(0, finalEol ? -1 : undefined).map(splitLine))) return source;
  } catch {
    return source;
  }
  return formatted;
}
