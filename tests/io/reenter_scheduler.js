// bend-ids 1
// CID :IO.print BEND_CID_IO_print
// CID #Unit BEND_CID_Unit
// CID :Host.nested BEND_CID_Host_nested
// bend-ids end
function host_nested(mode, k) {
  if (mode === 0) {
    const code = io_run(() => (cont) => ({
      $: BEND_CID_IO_print, args: ["inner"], kont: cont,
    }));
    if (code !== 0) throw "nested IO failed";
  } else {
    try {
      io_run(() => () => { throw "expected nested error"; });
      throw "nested IO did not fail";
    } catch (e) {
      if (e !== "expected nested error") throw e;
    }
  }
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_Host_nested, host_nested);
