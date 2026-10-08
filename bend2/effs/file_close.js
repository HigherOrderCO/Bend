// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :File.close BEND_CID_File_close
// bend-ids end
// File
// ====

function file_close(file) {
  const fs = require("fs");
  try {
    fs.closeSync(file);
  } catch (e) {
  }
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_File_close, file_close);
