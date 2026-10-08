// bend-ids 1
// CID #Nil BEND_CID_Nil
// CID #Con BEND_CID_Con
// CID :IO.args BEND_CID_IO_args
// CID :IO.args_bytes BEND_CID_IO_args_bytes
// bend-ids end
// IO
// ==

function io_args() {
  let xs = { $: BEND_CID_Nil };
  for (let i = cli_args.length; i > 0; i -= 1) {
    xs = { $: BEND_CID_Con, head: cli_args[i - 1], tail: xs };
  }
  return xs;
}

// Node and Bun decode the arguments before a program sees them, so here a
// byte that isn't UTF-8 arrives as U+FFFD's UTF-8, EF BF BD.
function io_args_bytes() {
  let xs = { $: BEND_CID_Nil };
  for (let i = cli_args.length; i > 0; i -= 1) {
    const b = io_bytes(cli_args[i - 1]);
    xs = { $: BEND_CID_Con, head: io_list(b, b.length), tail: xs };
  }
  return xs;
}

io_eff(BEND_CID_IO_args, io_args);
io_eff(BEND_CID_IO_args_bytes, io_args_bytes);
