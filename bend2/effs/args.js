// bend-ids 1
// CID #Nil BEND_CID_Nil
// CID #Con BEND_CID_Con
// CID :IO.args BEND_CID_IO_args
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

io_eff(BEND_CID_IO_args, io_args);
