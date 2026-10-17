#!/usr/bin/env python3
import sys
from pathlib import Path

out = Path(sys.argv[1])
count = int(sys.argv[2]) if len(sys.argv) > 2 else 400
depth = int(sys.argv[3]) if len(sys.argv) > 3 else 96
body = "x"
for _ in range(depth):
    body = f"Nat.add({body}, 1n)"
lines = [
    "import Base",
    "",
    "@unsafe",
    "def Stress.unsafe_leaf(x: Nat) -> Nat:",
    "  x",
]
for i in range(count):
    lines.extend(["", f"def Stress.f{i:04d}(x: Nat) -> Nat:", f"  Stress.unsafe_leaf({body})"])
out.write_text("\n".join(lines) + "\n")
