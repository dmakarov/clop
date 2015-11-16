/***********************************************************************
 * PathFinder uses dynamic programming to find a path on a 2-D grid from
 * the bottom row to the top row with the smallest accumulated weights,
 * where each step of the path moves straight ahead or diagonally ahead.
 * It iterates row by row, each node picks a neighboring node in the
 * previous row that has the smallest accumulated weight, and adds its
 * own weight to the sum.
 *
 * This kernel uses the technique of ghost zone optimization
 ***********************************************************************/

#define MIN3(a, b, c) (((a) < (b)) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))

__kernel void
pathfinder(__global int* data  , //
           __global int* src   , //
           __global int* dst   , //
                    int  rows  , //
                    int  cols  , //
                    int  height, //
                    int  start , //
                    int  steps , //
           __local  int* prev  , //
           __local  int* sums  ) //
{
  const int BLOCK_SIZE = get_local_size(0) - 2 * (height - 1);
  const int tx = get_local_id(0);
  const int bx = get_group_id(0);
  int block  = bx * BLOCK_SIZE;
  /* Thread ids run from 0 to get_local_size(0).
     The first height - 1 ids are outside the left side of
     the block and the last height - 1 ids are outside the
     right side of the block.  Thus k == block when tx == height - 1. */
  int k = block + tx - (height - 1);
  int n = get_local_size(0) + 1; // the last index in prev and sums
  int m = block + BLOCK_SIZE + height - 1; // the last index in src
  int x = tx + 1; // elements in prev and sums shifted to the
  // right by one relative to thread ids,
  // because we account for the outermost elements

  // we need to initialize prev from src
  if (tx == 0)
  { // first load two outter most elements, that no thread is mapped to
    if (0 < k   ) prev[0] = src[k - 1];
    if (m < cols) prev[n] = src[m];
  }
  // now each thread loads the element below it in src
  if (-1 < k && k < cols) prev[x] = src[k];
  barrier(CLK_LOCAL_MEM_FENCE);

  for (int r = start; r < start + steps; ++r)
  {
    if (-1 < k && k < cols)
    {
      int s = prev[x];
      int w = (k > 0       ) ? prev[x - 1] : INT_MAX;
      int e = (k < cols - 1) ? prev[x + 1] : INT_MAX;
      sums[x] = data[r * cols + k] + MIN3(w, s, e);
    }
    barrier(CLK_GLOBAL_MEM_FENCE);
    prev[x] = sums[x];
    barrier(CLK_LOCAL_MEM_FENCE);
  }
  // now save sums to dst
  if (height - 1 < x && x < BLOCK_SIZE + height)
    dst[k] = sums[x];
}
