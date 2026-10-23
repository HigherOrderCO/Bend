// Process
// =======

const PROCESS_WORKER = `
const { errno, signals } = require("node:os").constants;
const { parentPort } = require("node:worker_threads");
const { spawn } = require("node:child_process");
let port;
let flag;

parentPort.on("message", async (data) => {
  if (data.port !== undefined) {
    ({ port, flag } = data);
    return;
  }
  let got;
  try {
    got = await run(data);
  } catch (e) {
    got = { fail: errno[e] ?? (typeof e?.errno === "number"
      ? Math.abs(e.errno) : errno.EIO) };
  }
  port.postMessage(got);
  Atomics.store(flag, 0, 1);
  Atomics.notify(flag, 0);
});

function run({ argv, input, env, max, ms }) {
  return new Promise((yes, no) => {
    const proc = spawn(argv[0], argv.slice(1), { env,
      stdio: ["pipe", "pipe", "pipe"] });
    // Node destroys proc.stdin at exit; a descendant may still read it.
    const stdin = proc.stdin;
    proc.stdin = null;
    const chunks = [[], []];
    let size = 0;
    let error;
    const fail = (e) => {
      if (error !== undefined) return;
      error = e;
      proc.kill("SIGKILL");
      stdin.destroy();
      proc.stdout.destroy();
      proc.stderr.destroy();
    };
    proc.once("error", fail);
    stdin.on("error", (e) => {
      if (e.code !== "EPIPE") fail(e);
    });
    [proc.stdout, proc.stderr].forEach((stream, i) => {
      stream.on("error", fail);
      stream.on("data", (chunk) => {
        if (error !== undefined) return;
        size += chunk.length;
        if (size > max) fail("EFBIG");
        else chunks[i].push(chunk);
      });
    });
    // Feed stdin until the child exits and its stdout and stderr close.
    let live = 3;
    const done = () => --live || stdin.destroy();
    proc.once("exit", done);
    proc.stdout.once("close", done);
    proc.stderr.once("close", done);
    const timer = setTimeout(() => fail("ETIMEDOUT"),
      Math.min(ms, 2 ** 31 - 1));
    proc.once("close", (code, signal) => {
      clearTimeout(timer);
      if (error !== undefined) no(error);
      else yes({ code: code ?? 128 + (signals[signal] ?? 0),
        out: Buffer.concat(chunks[0]).toString("utf8"),
        err: Buffer.concat(chunks[1]).toString("utf8") });
    });
    stdin.end(input);
  });
}
`;

let process_worker = null;

function process_run(program, args, input, maxOutput, timeoutMs) {
  const argv = [str_out(program)];
  for (let xs = args; xs.$ === CID(Con); xs = xs.tail) {
    argv.push(str_out(xs.head));
  }
  if (maxOutput === 0 || timeoutMs === 0
    || argv.some((arg) => arg.includes("\0"))) {
    return io_fail(22);
  }
  if (process_worker === null) {
    const { Worker, MessageChannel, receiveMessageOnPort } = require("node:worker_threads");
    const { port1, port2 } = new MessageChannel();
    const flag = new Int32Array(new SharedArrayBuffer(4));
    const worker = new Worker(PROCESS_WORKER, { eval: true });
    worker.unref();
    worker.postMessage({ port: port2, flag }, [port2]);
    process_worker = { worker, port: port1, flag, receive: receiveMessageOnPort };
  }
  const { worker, port, flag, receive } = process_worker;
  Atomics.store(flag, 0, 0);
  worker.postMessage({ argv, input: io_bytes(input), env: { ...process.env },
    max: maxOutput, ms: timeoutMs });
  if (Atomics.wait(flag, 0, 0, timeoutMs + 1000) === "timed-out") {
    worker.terminate();
    process_worker = null;
    return io_fail(5);
  }
  const got = receive(port).message;
  return got.fail !== undefined ? io_fail(got.fail)
    : io_done(io_tup(got.code, got.out, got.err));
}

io_eff(CID(Process.run), process_run);
