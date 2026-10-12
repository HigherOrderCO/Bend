function host_resolved(k) {
  return io_park_promise(Promise.resolve(7), k, (value) => value,
    (error) => { throw error; });
}

function host_timer(k) {
  const { promise, resolve } = Promise.withResolvers();
  setTimeout(() => resolve(11), 5);
  return io_park_promise(promise, k, (value) => value,
    (error) => { throw error; });
}

function host_rejected(k) {
  return io_park_promise(Promise.reject(new Error("expected")), k,
    () => { throw "a rejected host operation succeeded"; },
    (error) => error.message === "expected" ? 13 : 0);
}

function host_digest(k) {
  const bytes = new TextEncoder().encode("abc");
  const promise = require("node:crypto").webcrypto.subtle.digest("SHA-256", bytes);
  return io_park_promise(promise, k,
    (result) => Array.from(new Uint8Array(result),
      (byte) => byte.toString(16).padStart(2, "0")).join(""),
    (error) => { throw error; });
}

io_eff(CID(Host.resolved), host_resolved);
io_eff(CID(Host.timer), host_timer);
io_eff(CID(Host.rejected), host_rejected);
io_eff(CID(Host.digest), host_digest);
