function native_apply(step, value) {
  return step(value);
}

io_eff(CID(native.apply), native_apply);
