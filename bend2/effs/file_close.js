// File
// ====

function file_close(file) {
  const fs = require("fs");
  try {
    fs.closeSync(file);
  } catch (e) {
  }
  return { $: CID(Unit) };
}

io_eff(CID(File.close), file_close);
