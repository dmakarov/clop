#define IN_RANGE(x, min, max) ( (min) <= (x) && (x) <= (max) )

__kernel void
pathfinder( int iteration, __global int *wall, __global int *src, __global int *dst, int cols, int rows, int step, int border, int HALO, __local int *prev, __local int *result )
{
  const int BLOCK_SIZE = get_local_size( 0 );
  const int tx = get_local_id( 0 );
  /**
   * Each block finally computes result for a small block after N iterations.
   * It is the non-overlapping small blocks that cover all the input data
   **/
  // calculate the small block size.
  int small_block_cols = BLOCK_SIZE - ( iteration * HALO * 2 );
  // calculate the boundary for the block according to the boundary of its small block
  int blkX = ( small_block_cols * get_group_id( 0 ) ) - border;
  int blkXmax = blkX + BLOCK_SIZE - 1;
  // calculate the global thread coordination
  int xidx = blkX + tx;
  // effective range within this block that falls within
  // the valid range of the input data
  // used to rule out computation outside the boundary.
  int validXmin = ( blkX < 0 ) ? -blkX : 0;
  int validXmax = ( blkXmax > cols - 1 ) ? BLOCK_SIZE - 1 - ( blkXmax - cols + 1 ) : BLOCK_SIZE - 1;
  int W = ( tx - 1 < validXmin ) ? validXmin : tx - 1;
  int E = ( tx + 1 > validXmax ) ? validXmax : tx + 1;
  bool isValid = IN_RANGE( tx, validXmin, validXmax );
  if ( IN_RANGE( xidx, 0, cols - 1 ) ) prev[tx] = src[xidx];

  barrier( CLK_LOCAL_MEM_FENCE );

  bool computed;
  for ( int i = 0; i < iteration; ++i )
  {
    computed = false;
    if ( IN_RANGE( tx, i + 1, BLOCK_SIZE - i - 2 ) && isValid )
    {
      result[tx] = min( min( prev[W], prev[tx] ), prev[E] ) + wall[cols * ( step + i ) + xidx];
      computed   = true;
    }
    barrier( CLK_LOCAL_MEM_FENCE );
    // we are on the last iteration, and thus don't need to compute for the next step.
    if ( i == iteration - 1 ) break;
    if ( computed ) prev[tx] = result[tx]; // assign the computation range
    barrier( CLK_LOCAL_MEM_FENCE );
  }
  // update the global memory
  // after the last iteration, only threads coordinated within the
  // small block perform the calculation and switch on "computed"
  if ( computed ) dst[xidx] = result[tx];
}
