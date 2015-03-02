CLOP is a DSL to write OpenCL kernels in a higher level of abstraction and implement
optimizations for them.

To compile an example

-   install [LDC](https://github.com/ldc-developers/ldc) and
    [DUB](https://github.com/D-Programming-Language/dub).
-   invoke dub for a specific sample application, e.g

    `dub --compiler=ldc2 :nw -- 32 10`

    The command will build the CLOP runtime library, and the NW application.
    If the build is successfull, the command will also run the NW application
    with the command line arguments `32 10`, which means that the length of
    sequences to align is 32 characters and the penalty value is 10.