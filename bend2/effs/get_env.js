// IO
// ==

function io_get_env(name) {
  // Node cuts a name at a NUL; reject it like get_env.c (#1449).
  if (name.includes("\0")) return io_fail(2);
  const value = Object.hasOwn(process.env, name) ? process.env[name] : undefined;
  return value === undefined ? io_fail(2) : io_done(value);
}

io_eff(CID(IO.get_env), io_get_env);
