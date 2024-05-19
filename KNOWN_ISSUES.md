# Known Issues

### Can I run this on Windows?
- We're still working on the windows support, for the moment, please use [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install).

### I'm getting a `CUDA not available!` error even though I have it installed?
- CUDA support is enable only during installation. If you first installed HVM and then installed CUDA and `nvcc`, please try to install HVM again.

### I'm getting a `Failed to launch kernels (error code invalid argument)!` error.
- The current iteration of the `hvm.cu` was written with the RTX 4090 in mind, and won't work on older GPUs, since they contain about half of the newer GPUs shared memory, for better understanding please refer to [HVM#283](https://github.com/HigherOrderCO/HVM/issues/283). We are working on support for older GPUs and will release it soon.

### Can I run this on AMD/Intel/Apple GPUs?
- We plan on adding support to many other GPUs as soon as the CUDA version is sufficiently stable.

### I get "Command `bend` not found" after installing, what do I do?
- If you are on Unix system (or WSL) then most likely bend did not add itself to the PATH variable in your rc file. To remedy this:
  - Determine if you are using bash or zsh (check for presence of `~/.bashrc` or `~/.zshrc`)
  - Make change to the relevant file by running `echo -n 'export PATH=$PATH:$HOME/.cargo/bin' >> ~/.bashrc`
    - use `~/.zshrc` in place of `~/.bashrc` in the case you are on zsh system
  - You will then need to run `source ~/.bashrc` or `source ~/.zshrc` for the changes to take effect immediately
  - Try running `bend` once more - it should work now! 

### I got an error when installing HVM on Linux
- If the error contains anything regarding `ccbin`, please refer to [HVM#291](https://github.com/HigherOrderCO/HVM/issues/291)

- If the error contains anything regarding `libc` missing, please refer to [HVM#355](https://github.com/HigherOrderCO/Bend/issues/355)

### How do I use IO?
- IO is still being developed and is expected to come soon.

### How to do FFI?
- Not as soon as basic IO, but planned or at least something similar to FFI.

### Are there any Libraries, Packages etc?
- A package manager will be added soon.

### Why are my numbers giving me wrong results?
Some possibilities:
- Your program is causing an overflow on 24-bit number values.
- Your program is doing operations on numbers of different types. (e.g. `2.0 + 1` is not allowed, you must use `2.0 + 1.0`)
- Floating point numbers are currently bugged and are interpreted incorrectly in some cases.
- There's a bug with signed integers numbers that flips that sometimes flips the order of the operations.

### I'm getting an error of failed assertion
- HVM currently has a bug in its conversion of f32 to f24 and it's unable to read the number 0.0. We already have a fix that we're working on.
