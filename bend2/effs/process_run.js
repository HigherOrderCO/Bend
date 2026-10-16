// Process
// =======

const PROCESS_WORKER = `
const { errno, signals } = require("node:os").constants;
let port;
let flag;

self.onmessage = async ({ data }) => {
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
};

async function run({ argv, input, env, max, ms }) {
  const proc = Bun.spawn({ cmd: argv, stdin: input, env, stdout: "pipe",
    stderr: "pipe" });
  const readers = [proc.stdout.getReader(), proc.stderr.getReader()];
  let size = 0;
  const read = async (reader) => {
    const chunks = [];
    for (let r; !(r = await reader.read()).done;) {
      chunks.push(r.value);
      size += r.value.length;
      if (size > max) {
        throw "EFBIG";
      }
    }
    return Buffer.concat(chunks).toString("utf8");
  };
  let timer;
  try {
    const [out, err] = await Promise.race([
      Promise.all([...readers.map(read), proc.exited]),
      new Promise((_, no) => timer = setTimeout(no, Math.min(ms, 2 ** 31 - 1),
        "ETIMEDOUT"))]);
    const sig = proc.signalCode === null ? 0 : signals[proc.signalCode] ?? 0;
    return { code: proc.exitCode ?? 128 + sig, out, err };
  } catch (e) {
    proc.kill("SIGKILL");
    readers.forEach((r) => r.cancel().catch(() => {}));
    await proc.exited;
    throw e;
  } finally {
    clearTimeout(timer);
  }
}
`;

let process_worker = null;

// A host without Bun (node) has no Worker and no Bun.spawn, so the request
// runs on node:child_process.spawnSync, which is the semantics the C lane
// models: it feeds stdin, reads until stdout and stderr close, answers the
// exit code (128 + the signal), kills on the deadline, and reports the
// spawn error. maxBuffer caps each stream at max + 1 bytes, and the sum is
// checked after, so the answer is EFBIG exactly when the C lane's combined
// cap would say so (the interim buffer is at most twice it).
function process_run_host(argv, input, maxOutput, timeoutMs) {
  const { spawnSync } = require("node:child_process");
  const { errno, signals } = require("node:os").constants;
  const got = spawnSync(argv[0], argv.slice(1), { input,
    timeout: Number(timeoutMs), killSignal: "SIGKILL",
    maxBuffer: maxOutput + 1, env: { ...process.env } });
  if (got.error !== undefined) {
    const code = got.error.code;
    const flood = (got.stdout?.length ?? 0) + (got.stderr?.length ?? 0)
      > maxOutput;
    // Node caps each stream at maxBuffer, so a stream past it alone puts the
    // sum past the combined cap; it reports the cap as ENOBUFS (or the older
    // ERR_CHILD_PROCESS_STDIO_MAXBUFFER code), and a flood a descendant holds
    // open may instead surface as the deadline. The cap fired first either way.
    if (flood || code === "ERR_CHILD_PROCESS_STDIO_MAXBUFFER") {
      return io_fail(errno.EFBIG);
    }
    if (code === "ETIMEDOUT") {
      return io_fail(errno.ETIMEDOUT);
    }
    return io_fail(typeof got.error.errno === "number"
      ? Math.abs(got.error.errno) : errno.ENOENT);
  }
  const out = got.stdout ?? Buffer.alloc(0);
  const err = got.stderr ?? Buffer.alloc(0);
  if (out.length + err.length > maxOutput) {
    return io_fail(errno.EFBIG);
  }
  const status = got.status !== null ? got.status
    : got.signal !== null ? 128 + (signals[got.signal] ?? 0) : 1;
  return io_done(io_tup(status, io_text(out, out.length),
    io_text(err, err.length)));
}

function process_run(program, args, input, maxOutput, timeoutMs) {
  const argv = [str_out(program)];
  for (let xs = args; xs.$ === CID(Con); xs = xs.tail) {
    argv.push(str_out(xs.head));
  }
  if (maxOutput === 0 || timeoutMs === 0
    || argv.some((arg) => arg.includes("\0"))) {
    return io_fail(22);
  }
  if (typeof Bun === "undefined") {
    return process_run_host(argv, input, maxOutput, timeoutMs);
  }
  if (process_worker === null) {
    const { MessageChannel, receiveMessageOnPort } = require("node:worker_threads");
    const { port1, port2 } = new MessageChannel();
    const flag = new Int32Array(new SharedArrayBuffer(4));
    const worker = new Worker(URL.createObjectURL(new Blob([PROCESS_WORKER])));
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
