// bend-ids 1
// CID :native.apply BEND_CID_native_apply
// bend-ids end
function native_apply(step, value) {
  return step(value);
}

io_eff(BEND_CID_native_apply, native_apply);
