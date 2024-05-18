# Known Issues

## Why are my numbers giving me wrong results?
Either cause overflowing u24 or mixing different number types.


## Can i run this on AMD/METAL GPUs?
We plan on adding support to many other GPUs as soon as the CUDA version is sufficiently stable.


## Can i run this on windows?
We're still working on the windows support, for the moment, please use [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install). 

## How do i use IO?
IO is still being developed and is expected to come soon.

## how to do FFI?
not as soon as IO, but planned or at least something similar to FFI.

## Are there any Libraries, Packages etc?
A package manager will be added soon.

## I got an error when installing HVM on Linux
If the error contains anything regarding `ccbin`, please refer to [HVM#291](https://github.com/HigherOrderCO/HVM/issues/291)

If the error contains anything regarding `libc` missing, please refer to [HVM#355](https://github.com/HigherOrderCO/Bend/issues/355)

## I'm getting an error of `failed assertion`
HVM currently has a bug in it's conversion of f32 to f24 and it's unable to read the number 0.0. We already have a fix that we're working on.

## Why am i getting `CUDA not supported` even tho i have it installed?
The current iteration of the `hvm.cu` was written with the 4090 in mind, and won't work on older GPUs, since they contain about half of the newer GPUs shared memory, for better understanding please refer to [HVM#283](https://github.com/HigherOrderCO/HVM/issues/283), we are working on the support for other GPUs and will release it soon.
