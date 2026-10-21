// IO
// ==

function io_get_env(name) {
  name = str_out(name);
  // Node cuts a name at a NUL; reject it like get_env.c (#1449).
  // Node's getenv may also read "A=B" as A's entry "A=B=C"; reject a "=" too.
  if (name.includes("\0") || name.includes("=")) return io_fail(2);
  const value = Object.hasOwn(process.env, name) ? process.env[name] : undefined;
  return value === undefined ? io_fail(2) : io_done(value);
}

io_eff(CID(IO.get_env), io_get_env);
