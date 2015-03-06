# CLOP - a DSL for optimized OpenCL kernels

[![Build Status](https://travis-ci.org/dmakarov/clop.png)](https://travis-ci.org/dmakarov/clop)

CLOP is a DSL to write OpenCL kernels in a higher level of abstraction and implement
optimizations for them.

To compile an example

-   install [LDC][] and [DUB][].
-   invoke dub for a specific sample application, e.g.

    `dub --compiler=ldc2 :nw -- 32 10`

    The command will build the CLOP runtime library, and the NW
    application.  If the build is successful, the command runs the NW
    application with the arguments `32 10`, which means that the
    length of sequences to align is 32 characters and the penalty
    value is 10.

[LDC]: https://github.com/ldc-developers/ldc         "LDC GitHub repository"
[DUB]: https://github.com/D-Programming-Language/dub "DUB GitHub repository"
