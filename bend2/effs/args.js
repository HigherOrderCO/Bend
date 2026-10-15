// IO
// ==

function io_args() {
  let xs = { $: CID(Nil) };
  for (let i = cli_args.length; i > 0; i -= 1) {
    xs = { $: CID(Con), head: cli_args[i - 1], tail: xs };
  }
  return xs;
}

io_eff(CID(IO.args), io_args);
