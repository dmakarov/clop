module clop.examples.nw;

import std.conv;
import std.datetime;
import std.getopt;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;

/**
 * This application implements the Floyd-Warshall algorithm for
 * finding the shortest path between any two nodes in a graph.
 *
 * Floyd-Warshall-All-Pairs-Shortest(G,w)
 *   Initialize d[i,j] ← w(i,j), ∞ if no such link
 *   Initialize path[i,j] ← ∞
 *   for k ← 1 to |V|
 *     for i ← 1 to |V|
 *       for j ← 1 to |V|
 *         if d[i,k] + d[k,j] < d[i,j] then ; update min
 *            d[i,j] ← d[i,k] + d[k,j]
 *            path[i,j] ← k                 ; store to get path
 *
 * Here we use one matrix and overwrite it for each iteration of k.
 **/
class Application {
  static immutable int HALO       =  2;
  static immutable int SEED       =  1;
  static immutable int BLOCK_SIZE = 128;

  int[] data;
  int[] move;
  int[] sums;
  int[] gold;
  int rows;
  int cols;
  int height;
  string outfile;

  cl_kernel kernel_noblocks;
  cl_kernel kernel_ghost_zone;

  this( string[] args )
  {
    getopt( args, "output|o", &outfile );
    if ( args.length != 4 )
    {
      throw new Exception( "ERROR: invalid args # " ~ to!(string)(args.length - 1) );
    }
    rows   = to!(int)( args[1] );
    cols   = to!(int)( args[2] );
    height = to!(int)( args[3] );
    data   = new int[rows * cols]; assert( null != data, "Can't allocate memory for data" );
    move   = new int[rows * cols]; assert( null != data, "Can't allocate memory for move" );
    sums   = new int[cols];        assert( null != sums, "Can't allocate memory for sums" );
    gold   = new int[cols];        assert( null != gold, "Can't allocate memory for gold" );
    Mt19937 gen;
    gen.seed( SEED );
    foreach ( ref item; data )
      item = uniform( 0, 10, gen );

    /**
     *
     * 
     */
    char[] code = q{
      #define MOV3( w, s, e ) ( ((w) < (s)) ? ((w) < (e) ?  1  : -1 ) : ((s) < (e) ?  0  :  -1) )
      #define MIN3( a, b, c ) ( ((a) < (b)) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)) )

      __kernel void
      pf_noblocks( __global int* data, __global int* src, __global int* dst, __global int* move, int cols, int step )
      {
        const int tx = get_global_id( 0 );
        int s = src[tx];
        int w = ( tx > 0        ) ? src[tx - 1] : INT_MAX;
        int e = ( tx < cols - 1 ) ? src[tx + 1] : INT_MAX;
        dst[tx] = data[step * cols + tx] + MIN3( w, s, e );
        move[step * cols + tx] = MOV3( w, s, e );
      }

      /**
       * pf_ghost_zone process several rows in blocks computing the
       * cumulative weight of the shortest path going through every
       * element of the matrix.
       * @param start -- the starting row of this run
       * @param steps -- the number of rows processed in this run
       */
      __kernel void
      pf_ghost_zone( __global int* data, __global int* src, __global int* dst, __global int* move, int rows, int cols, int height, int start, int steps, __local int* prev, __local int* sums )
      {
        const int BLOCK_SIZE = get_local_size( 0 ) - 2 * ( height - 1 );
        const int tx = get_local_id( 0 );
        const int bx = get_group_id( 0 );
        int block  = bx * BLOCK_SIZE;
        /* Thread ids run from 0 to get_local_size( 0 ).
           The first height - 1 ids are outside the left side of
           the block and the last height - 1 ids are outside the
           right side of the block.  Thus k == block when tx == height - 1. */
        int k = block + tx - ( height - 1 );
        int n = get_local_size( 0 ) + 1; // the last index in prev and sums
        int m = block + BLOCK_SIZE + height - 1; // the last index in src
        int x = tx + 1; // elements in prev and sums shifted to the
                        // right by one relative to thread ids,
                        // because we account for the outtermost elements

        // we need to initialize prev from src
        if ( tx == 0 )
        { // first load two outter most elements, that no thread is mapped to
          if ( 0 < k    ) prev[0] = src[k - 1];
          if ( m < cols ) prev[n] = src[m];
        }
        // now each thread loads the element below it in src
        if ( -1 < k && k < cols ) prev[x] = src[k];
        barrier( CLK_LOCAL_MEM_FENCE );

        for ( int r = start; r < start + steps; ++r )
        {
          if ( -1 < k && k < cols )
          {
            int s = prev[x];
            int w = ( k > 0        ) ? prev[x - 1] : INT_MAX;
            int e = ( k < cols - 1 ) ? prev[x + 1] : INT_MAX;
            sums[x] = data[r * cols + k] + MIN3( w, s, e );
            // we must update move only within the block
            move[r * cols + k] = MOV3( w, s, e );
          }
          barrier( CLK_GLOBAL_MEM_FENCE );
          prev[x] = sums[x];
          barrier( CLK_LOCAL_MEM_FENCE );
        }
        // now save sums to dst
        if ( height - 1 < x && x < BLOCK_SIZE + height )
          dst[k] = sums[x];
      }
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( runtime.context, 1, strs.ptr, &size, &status ); assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clBuildProgram( program, 1, &runtime.device, "", null, null );                   assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_noblocks = clCreateKernel( program, "pf_noblocks", &status );                      assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_ghost_zone = clCreateKernel( program, "pf_ghost_zone", &status );                  assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clReleaseProgram( program );                                                     assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
  }

  /**
   */
  void validate()
  {
    int diff = 0;
    foreach ( ii; 0 .. cols )
      if ( sums[ii] != gold[ii] )
        ++diff;
    if ( diff > 0 )
      writeln( "DIFFs ", diff );
  }

  /**
   */
  void reset()
  {
    sums[] = 0;
  }

  void dump_data()
  {
    writeln( "-----------------------------------" );
    foreach ( r; 0 .. rows )
    {
      writef( "%2d:", r );
      foreach ( c; 0 .. cols )
        writef( " %1d", data[r * cols + c] );
      writeln();
    }
    writeln( "-----------------------------------" );
  }

  void dump_move()
  {
    writeln( "---------------------------------------------------" );
    foreach ( r; 0 .. rows - 1 )
    {
      writef( "%2d:", r + 1 );
      foreach ( c; 0 .. cols )
        writef( " %2d", move[r * cols + c] );
      writeln();
    }
    writeln( "---------------------------------------------------" );
  }

  bool IN_RANGE( int x, int min, int max )
  {
    return ( min <= x && x <= max );
  }

  int max( int a, int b )
  {
    return a < b ? b : a;
  }

  int min( int a, int b )
  {
    return a < b ? a : b;
  }

  int min3( int a, int b, int c )
  {
    auto k = a < b ? a : b;
    return k < c ? k : c;
  }

  int mov3( int w, int s, int e )
  {
    return (w < s) ? (w < e ? 1 : -1) : (s < e ? 0 : -1);
  }

  void baseline()
  {
    int[][2] results;
    results[0] = new int[cols];
    results[1] = new int[cols];
    foreach ( c; 0 .. cols )
      results[0][c] = data[c];
    int src = 0;
    foreach ( r; 1 .. rows )
    {
      auto dst = 1 - src;
      foreach ( c; 0 .. cols )
      {
        auto s = results[src][c];
        auto w = ( c > 0        ) ? results[src][c - 1] : int.max;
        auto e = ( c < cols - 1 ) ? results[src][c + 1] : int.max;
        results[dst][c] = data[r * cols + c] + min3( w, s, e );
        move[r * cols + c] = mov3( w, s, e );
      }
      src = dst;
    }
    gold[] = results[src][];
  }

  void ghost_zone_blocks()
  {
    int[][2] results;
    results[0] = new int[BLOCK_SIZE + 2 * height];
    results[1] = new int[BLOCK_SIZE + 2 * height];
    int[] temp = new int[cols];
    foreach ( c; 0 .. cols )
      sums[c] = data[c];
    for ( int r = 1; r < rows; r += height )
    {
      for ( int block = 0; block < cols; block += BLOCK_SIZE )
      {
        foreach( k; -height .. BLOCK_SIZE + height )
          if ( -1 < block + k && block + k < cols )
            results[0][height + k] = sums[block + k];
        int src = 0;
        int border = height - 1;
        foreach ( step; r .. min( r + height, rows ) )
        {
          auto dst = 1 - src;
          foreach ( k; -border .. BLOCK_SIZE + border )
            if ( -1 < block + k && block + k < cols )
            {
              auto c = block + k;
              auto s = results[src][height + k];
              auto w = ( c > 0        ) ? results[src][height + k - 1] : int.max;
              auto e = ( c < cols - 1 ) ? results[src][height + k + 1] : int.max;
              results[dst][height + k] = data[step * cols + c] + min3( w, s, e );
              move[step * cols + c] = mov3( w, s, e );
            }
          border -= 1;
          src = dst;
        }
        foreach ( k; 0 .. BLOCK_SIZE )
          temp[block + k] = results[src][height + k];
      }
      sums[] = temp[];
    }
  }

  void opencl_noblocks()
  {
    foreach ( c; 0 .. cols )
      sums[c] = data[c];
    cl_int status;
    cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
    cl_mem ddata = clCreateBuffer( runtime.context, flags, cl_int.sizeof * data.length, data.ptr, &status );                assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    flags = CL_MEM_WRITE_ONLY;
    cl_mem dmove = clCreateBuffer( runtime.context, flags, cl_int.sizeof * move.length, null, &status );                    assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    cl_mem dsums[2];
    flags = CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR;
    dsums[0] = clCreateBuffer( runtime.context, flags, cl_int.sizeof * cols, sums.ptr, &status );                           assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    flags = CL_MEM_READ_WRITE;
    dsums[1] = clCreateBuffer( runtime.context, flags, cl_int.sizeof * cols, null, &status );                               assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

    int src = 0;
    size_t gwsize = cols;
    foreach ( step; 1 .. rows )
    {
      int dst = 1 - src;

      status = clSetKernelArg( kernel_noblocks,  0, cl_mem.sizeof, &ddata );                                                assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  1, cl_mem.sizeof, &dsums[src] );                                           assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  2, cl_mem.sizeof, &dsums[dst] );                                           assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  3, cl_mem.sizeof, &dmove );                                                assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  4, cl_int.sizeof, &cols );                                                 assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  5, cl_int.sizeof, &step );                                                 assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

      status = clEnqueueNDRangeKernel( runtime.queue, kernel_noblocks, 1, null, &gwsize, null, 0, null, null );             assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

      src = dst;
    }

    status = clEnqueueReadBuffer( runtime.queue, dsums[src], CL_TRUE, 0, cl_int.sizeof * cols, sums.ptr, 0, null, null );   assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clEnqueueReadBuffer( runtime.queue, dmove, CL_TRUE, 0, cl_int.sizeof * rows * cols, move.ptr, 0, null, null ); assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

    status = clReleaseMemObject( dsums[1] );                                                                                assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clReleaseMemObject( dsums[0] );                                                                                assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clReleaseMemObject( dmove );                                                                                   assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clReleaseMemObject( ddata );                                                                                   assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clReleaseKernel( kernel_noblocks );                                                                            assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
  }

  void opencl_ghost_zone()
  {
    foreach ( c; 0 .. cols )
      sums[c] = data[c];
    cl_int status;
    cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
    cl_mem ddata = clCreateBuffer( runtime.context, flags, cl_int.sizeof * data.length, data.ptr, &status );                assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
    flags = CL_MEM_WRITE_ONLY;
    cl_mem dmove = clCreateBuffer( runtime.context, flags, cl_int.sizeof * move.length, null, &status );                    assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
    cl_mem dsums[2];
    flags = CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR;
    dsums[0]  = clCreateBuffer( runtime.context, flags, cl_int.sizeof * cols, sums.ptr, &status );                          assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
    flags = CL_MEM_READ_WRITE;
    dsums[1]  = clCreateBuffer( runtime.context, flags, cl_int.sizeof * cols, null, &status );                              assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );

    size_t blocks = cols / BLOCK_SIZE;
    size_t lwsize = BLOCK_SIZE + 2 * ( height - 1 );
    size_t gwsize = blocks * lwsize;
    int src = 0;

    for ( int r = 1; r < rows; r += height )
    {
      int steps = min( height, rows - r );
      int dst = 1 - src;

      status = clSetKernelArg( kernel_ghost_zone,  0, cl_mem.sizeof, &ddata );                                              assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone,  1, cl_mem.sizeof, &dsums[src] );                                         assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone,  2, cl_mem.sizeof, &dsums[dst] );                                         assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone,  3, cl_mem.sizeof, &dmove );                                              assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone,  4, cl_int.sizeof, &rows );                                               assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone,  5, cl_int.sizeof, &cols );                                               assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone,  6, cl_int.sizeof, &height );                                             assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone,  7, cl_int.sizeof, &r );                                                  assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone,  8, cl_int.sizeof, &steps );                                              assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone,  9, cl_int.sizeof * ( lwsize + 2 ), null );                               assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_ghost_zone, 10, cl_int.sizeof * ( lwsize + 2 ), null );                               assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );

      status = clEnqueueNDRangeKernel( runtime.queue, kernel_ghost_zone, 1, null, &gwsize, &lwsize, 0, null, null );        assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );

      src = dst;
    }

    status = clEnqueueReadBuffer( runtime.queue, dsums[src], CL_TRUE, 0, cl_int.sizeof * cols, sums.ptr, 0, null, null );   assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
    status = clEnqueueReadBuffer( runtime.queue, dmove, CL_TRUE, 0, cl_int.sizeof * rows * cols, move.ptr, 0, null, null ); assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );

    status = clReleaseMemObject( dsums[1] );                                                                                assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
    status = clReleaseMemObject( dsums[0] );                                                                                assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
    status = clReleaseMemObject( dmove );                                                                                   assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
    status = clReleaseMemObject( ddata );                                                                                   assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
    status = clReleaseKernel( kernel_ghost_zone );                                                                          assert( status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror( status ) );
  }

  /**
   */
/*
  void clop_dsl()
  {
    try
    {
      mixin( compile(
      q{
        int min3( int a, int b, int c )
        {
          int k = a < b ? a : b;
          return k < c ? k : c;
        }

        int mov3( int w, int s, int e )
        {
          return (w < s) ? (w < e ? 1 : -1) : (s < e ? 0 : -1);
        }

        int src = 0;
        int dst = 1 - src;
        NDRange( r ; 1 .. rows, c ; 0 .. cols );
        weigths[0][c] = data[c];
        {
          int s = weight[src][c];
          int w = ( c > 0        ) ? weights[src][c - 1] : INT_MAX;
          int e = ( c < cols - 1 ) ? weights[src][c + 1] : INT_MAX;
          weights[dst][c] = data[r * cols + c] + min3( w, s, e );
          move[r * cols + c] = mov3( w, s, e );
        }
        src = dst;
      } ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
  }
*/
  void run()
  {
    size_t size;
    StopWatch timer;
    TickDuration ticks;

    timer.start();
    baseline();
    timer.stop();
    ticks = timer.peek();
    writefln( "SEQUENTIAL %8.6f [s]", ticks.usecs / 1E6 );

    timer.reset();
    timer.start();
    ghost_zone_blocks();
    timer.stop();
    ticks = timer.peek();
    writefln( "GHOST ZONE %8.6f [s]", ticks.usecs / 1E6 );
    validate();

    reset();
    timer.reset();
    timer.start();
    opencl_noblocks();
    timer.stop();
    ticks = timer.peek();
    writefln( "CL NOBLOCK %8.6f [s]", ticks.usecs / 1E6 );
    validate();

    clGetKernelWorkGroupInfo( kernel_ghost_zone,
                              runtime.device,
                              CL_KERNEL_WORK_GROUP_SIZE,
                              size.sizeof,
                              &size,
                              null );
    if ( size >= BLOCK_SIZE + 2 * ( height - 1 ) )
    {
      reset();
      timer.reset();
      timer.start();
      opencl_ghost_zone();
      timer.stop();
      ticks = timer.peek();
      writefln( "CL GHOST Z %8.6f [s]", ticks.usecs / 1E6 );
      validate();
    }
  }
}

int
main( string[] args )
{
  uint[] platforms = runtime.get_platforms();
  for ( uint p = 0; p < platforms.length; ++p )
    foreach ( d; 0 .. platforms[p] )
    {
      try
      {
        runtime.init( p, d );
        auto app = new Application( args );
        app.run();
        runtime.shutdown();
      }
      catch ( Exception msg )
      {
        writeln( "PF: ", msg );
        return -1;
      }
    }
  return 0;
}
