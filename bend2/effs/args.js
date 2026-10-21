// IO
// ==

function io_args() {
  let xs = { $: CID(Nil) };
  for (let i = cli_args.length; i > 0; i -= 1) {
    xs = { $: CID(Con), head: cli_args[i - 1], tail: xs };
  }
  return xs;
}

// Node and Bun decode the arguments before a program sees them, so here a
// byte that isn't UTF-8 arrives as U+FFFD's UTF-8, EF BF BD.
function io_args_bytes() {
  let xs = { $: CID(Nil) };
  for (let i = cli_args.length; i > 0; i -= 1) {
    const b = io_bytes(cli_args[i - 1]);
    xs = { $: CID(Con), head: io_list(b, b.length), tail: xs };
  }
  return xs;
}

io_eff(CID(IO.args), io_args);
io_eff(CID(IO.args_bytes), io_args_bytes);
