-*- org -*-

PathFinder uses dynamic programming to find a path on a 2-D grid from
the bottom row to the top row with the smallest accumulated weights,
where each step of the path moves straight ahead or diagonally ahead.
It iterates row by row, each node picks a neighboring node in the
previous row that has the smallest accumulated weight, and adds its
own weight to the sum.

This kernel uses the technique of ghost zone optimization.

* Kernels
** =pathfinder=
*** Inputs
    - int iteration
    - __global int *wall
    - __global int *src
    - __global int *dst
    - int cols
    - int rows
    - int step
    - int border
    - int HALO
    - __local int *prev
    - __local int *result
*** Outputs
*** Work items
*** Work groups
