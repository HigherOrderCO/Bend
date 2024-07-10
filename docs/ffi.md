# Dynamically linked libraries and foreign functions

We can add new IO functions to Bend during runtime by loading dynamic libraries.

## Using IO dynamic libraries in Bend

Here is an example of how we could load a Bend library that contains functions for working with directories.

```py
def main():
  with IO:
    # Open the dynamic library file
    # The second argument is '0' if we want to load all functions immediately.
    # Otherwise it should be '1' when we want to load functions as we use them.
    # 'dl' is the unique id of the dynamic library.
    dl <- IO/DyLib/open("./libbend_dirs.so", 0)

    # We can now call functions from the dynamic library.
    # We need to know what functions are available in the dynamic library.
    # If you're writing a library for Bend that uses a dynamically linked library
    # you should wrap the IO calls so that users don't need to know what's in the dynamic library.

    # The first argument is the dynamic library id.
    # The second argument is the name of the function we want to call as a String.
    # The third argument are the arguments to the function.
    # You need to know the types of the arguments and the return type of the function.

    # In our example, 'ls' receives a path as a String and
    # returns a String with the result of the 'ls' command.
    files_bytes <- IO/DyLib/call(dl, "ls", "/home")
    files_str = String/decode_utf8(files_bytes)
    files = String/split(files_str, '\n')

    # We want to create a directory for a new user "my_user" if it doesn't exist.
    my_dir = List/filter(files, String/equals("my_user"))
    match my_dir:
      case Cons:
        # The directory already exists, do nothing.
        * <- IO/print("Directory already exists.")
        status = -1
      case Nil:
        # The directory doesn't exist, create it.
        * <- IO/DyLib/call(dl, "mkdir", "/home/my_user")
        * <- IO/print("Directory created.")
        status = +0

    # Here the program ends so we didn't need to close the dynamic library,
    # but it's good practice to do so once we know we won't need it anymore.
    * <- IO/DyLib/close(dl)
    return wrap(status)
```

## Writing IO dynamic libraries for Bend

Bend IO libraries need to be implemented in C or Cuda (depending on the backend you're targeting) using the HVM API.

### Writing libraries for the C runtime

The functions you call from Bend using `IO/DyLib/call` must have the following signature:

```c
Port function_name(Net* net, Book* book, Port arg);
```

Where:

- `net` is a pointer to the current network state.
- `book` is a pointer to the book of function definitions.
- `arg` is a pointer to the arguments of the function.

The return value must be a `Port` that points to the return value of the function.

HVM provides some util functions to do the conversions from HVM to C and vice versa,
so that you don't need to understand the details of the HVM runtime.

We can implement the example library from earlier for the C runtime with the following C code:

```c
// This is a header file that contains the HVM API.
#include <hvm.h>

// The headers we need to open and read directories.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

// IO functions must have this exact signature.
// The first argument is a pointer to the graph with the current state of the program.
// The second argument is a pointer to the book of function definitions.
// The third argument points to the arguments of the function.
// The return value must be a port that points to the return value of the function.
Port ls(Net* net, Book* book, Port arg) {
  // The arguments first need to be converted from HVM to C.
  // For the 'ls' function, this is just a single string.
  Str ls_path = readback_str(net, book, arg);

  // Now we can do the actual IO operations.
  // In this case, we list the contents of the directory
  // by calling the 'ls' program as a subprocess.
  char* cmd = malloc(strlen(ls_path.buf) + strlen("ls ") + 1);
  sprintf(cmd, "ls %s", ls_path.buf);
  free(ls_path.buf);

  FILE* pipe = popen(cmd, "r");
  free(cmd);
  if (pipe == NULL) {
    fprintf(stderr, "failed to run command '%s': %s\n", cmd, strerror(errno));

    // It'd be best practice to return a Result type instead of a null value (ERA).
    // If this command fails and the calling Bend program tries to use the result,
    // it will get corrupted and spit out garbage.
    return new_port(ERA, 0);
  }
  char buffer[512];
  char *output;
  size_t output_len = 0;
  while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
    size_t len = strlen(buffer);
    char* new_result = realloc(output, output_len + len + 1);
    if (new_result == NULL) {
      free(output);
      pclose(pipe);
      fprintf(stderr, "failed to allocate space for output of '%s': %s\n", cmd, strerror(errno));
      return new_port(ERA, 0);
    }
    output = new_result;
    strcpy(output + output_len, buffer);
    output_len += len;
  }

  // After we're done with the operation, we convert it to HVM format.
  // In this case, the output is the output of the 'ls' command as a list of bytes.
  // We need to process it in Bend later to convert it to a list of file names.
  Bytes output_bytes = { .buf = output, .len = output_len };
  Port output_port = inject_bytes(net, &output_bytes);

  // Remember to free all the allocated memory.
  free(output);
  pclose(pipe);
  return output_port;
}

Port mkdir(Net* net, Book* book, Port arg) {
  // We do the same thing here as in the 'ls' function,
  // except we call 'mkdir' which doesn't output anything.
  Str ls_path = readback_str(net, book, arg);

  char* cmd = malloc(strlen(ls_path.buf) + strlen("mkdir ") + 1);
  sprintf(cmd, "mkdir %s", ls_path.buf);
  int res = system(cmd);

  free(ls_path.buf);
  free(cmd);
  return new_port(ERA, 0);
}
```

To compile this code into a library, we can use the `gcc` compiler and include the HVM header files.

Assuming that it's saved in a file called `libbend_dirs.c`, we can compile it with the following command:

```sh
Compile the shared library with unresolved symbols:
For macOS:
gcc -shared -o libbend_dirs.so -I /path/to/hvm/ libbend_dirs.c -undefined dynamic_lookup

For Linux:
gcc -shared -o libbend_dirs.so -I /path/to/hvm/ libbend_dirs.c -Wl,--unresolved-symbols=ignore-all
```

Now we can use the dynamic library in our Bend program, we just need to pass the path to the library to `IO/DyLib/open`.

### Writing libraries for the Cuda runtime

Writing libraries for the Cuda runtime is very similar to writing libraries for the C runtime.

The main difference is the function signature:

```c++
Port function_name(GNet* gnet, Port argm)
```

Where:

- `gnet` is a pointer to the current network state.
- `argm` is the argument to the function.

The return value must be a `Port` that points to the return value of the function.

To compile libraries of the Cuda runtime, we can use the `nvcc` compiler and include the HVM header files.

Assuming that it's saved in a file called `libbend_dirs.cu`, we can compile it with the following command:

```sh
nvcc -shared -o libbend_dirs.so -I /path/to/hvm/ libbend_dirs.cu
```

### Compiling Bend programs that use dynamic libraries

To compile the C or Cuda program generated from a Bend program that uses dynamic libraries, we need to use the `-rdynamic` flag to allow the dynamic library to use symbols from the main program.

For example, if we have a Bend program called `main.bend` that uses the dynamic library `libbend_dirs.so`, we need compile to it with the following commands:

```sh
# Compiling for C
bend gen-c my_app.bend > my_app.c
gcc -rdynamic -lm my_app.c -o my_app

# Compiling for Cuda
bend gen-cu my_app.bend > my_app.cu
nvcc --compiler-options=-rdynamic my_app.cu -o my_app
```
