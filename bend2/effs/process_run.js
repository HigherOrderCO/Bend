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

function process_run(program, args, input, maxOutput, timeoutMs) {
  const argv = [program];
  for (let xs = args; xs.$ === CID(Con); xs = xs.tail) {
    argv.push(xs.head);
  }
  if (maxOutput === 0 || timeoutMs === 0
    || argv.some((arg) => arg.includes("\0"))) {
    return io_fail(22);
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
  worker.postMessage({ argv, input: Buffer.from(input), env: { ...process.env },
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
