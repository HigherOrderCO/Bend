import assert from "node:assert/strict";
import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import { fileURLToPath } from "node:url";
import test from "node:test";

type Message = { id?: number; result?: unknown; error?: unknown };

function send(child: ChildProcessWithoutNullStreams, message: object): void {
  const body = JSON.stringify(message);
  child.stdin.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
}

function responses(child: ChildProcessWithoutNullStreams): (id: number) => Promise<Message> {
  let buffer = Buffer.alloc(0);
  const pending = new Map<number, (message: Message) => void>();
  child.stdout.on("data", (chunk: Buffer) => {
    buffer = Buffer.concat([buffer, chunk]);
    while (true) {
      const marker = buffer.indexOf("\r\n\r\n");
      if (marker < 0) return;
      const header = buffer.subarray(0, marker).toString("ascii");
      const length = Number(/Content-Length: (\d+)/i.exec(header)?.[1]);
      if (!Number.isFinite(length) || buffer.length < marker + 4 + length) return;
      const message = JSON.parse(buffer.subarray(marker + 4, marker + 4 + length).toString("utf8")) as Message;
      buffer = buffer.subarray(marker + 4 + length);
      if (message.id !== undefined) pending.get(message.id)?.(message);
    }
  });
  return (id) => new Promise((resolve, reject) => {
    const timer = setTimeout(() => reject(new Error(`timed out waiting for response ${id}`)), 5000);
    pending.set(id, (message) => {
      clearTimeout(timer);
      pending.delete(id);
      resolve(message);
    });
  });
}

test("serves formatting over an LSP stdio session", async (context) => {
  const server = fileURLToPath(new URL("../server.js", import.meta.url));
  const child = spawn(process.execPath, [server, "--stdio"], { stdio: ["pipe", "pipe", "pipe"] });
  context.after(() => child.kill());
  const waitFor = responses(child);

  const initialized = waitFor(1);
  send(child, { jsonrpc: "2.0", id: 1, method: "initialize", params: { capabilities: {} } });
  const initialize = await initialized;
  assert.equal(initialize.error, undefined);
  assert.deepEqual((initialize.result as { capabilities: object }).capabilities, {
    textDocumentSync: 1,
    documentFormattingProvider: true,
  });

  send(child, { jsonrpc: "2.0", method: "initialized", params: {} });
  send(child, {
    jsonrpc: "2.0",
    method: "textDocument/didOpen",
    params: { textDocument: { uri: "file:///main.bend", languageId: "bend", version: 1, text: "def main()->U32:\n    0" } },
  });
  const formatted = waitFor(2);
  send(child, {
    jsonrpc: "2.0",
    id: 2,
    method: "textDocument/formatting",
    params: { textDocument: { uri: "file:///main.bend" }, options: { tabSize: 2, insertSpaces: true } },
  });
  assert.deepEqual((await formatted).result, [{
    range: { start: { line: 0, character: 0 }, end: { line: 1, character: 5 } },
    newText: "def main() -> U32:\n  0",
  }]);

  send(child, {
    jsonrpc: "2.0",
    method: "textDocument/didOpen",
    params: { textDocument: {
      uri: "file:///imports.bend",
      languageId: "bend",
      version: 1,
      text: "import   lib/dep.bend   as   Dep # dependency\n\ndef main()->U32:\n    Dep.answer()",
    } },
  });
  const imports = waitFor(3);
  send(child, {
    jsonrpc: "2.0",
    id: 3,
    method: "textDocument/formatting",
    params: { textDocument: { uri: "file:///imports.bend" }, options: { tabSize: 2, insertSpaces: true } },
  });
  assert.deepEqual((await imports).result, [{
    range: { start: { line: 0, character: 0 }, end: { line: 3, character: 16 } },
    newText: "import lib/dep.bend as Dep  # dependency\n\ndef main() -> U32:\n  Dep.answer()",
  }]);

  const shutdown = waitFor(4);
  send(child, { jsonrpc: "2.0", id: 4, method: "shutdown" });
  assert.equal((await shutdown).result, null);
  send(child, { jsonrpc: "2.0", method: "exit" });
});
