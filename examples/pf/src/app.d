module clop.examples.nw;

import std.conv;
import std.datetime;
import std.getopt;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;


class Application {
  static immutable int SEED  =  1;

  int[] data;
  int rows;
  int cols;
  int height;
  string outfile;

  cl_kernel kernel_noblocks;

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
    Mt19937 gen;
    gen.seed( SEED );
    foreach ( ref item; data )
      item = uniform( 0, 10, gen );

    /**
     *
     **/
    char[] code = q{
      #define IN_RANGE(x, min, max) ( (min) <= (x) && (x) <= (max) )

      __kernel void
      pf_noblocks( int iteration, __global int *wall, __global int *src, __global int *dst, int cols, int rows, int step, int border, int HALO, __local int *prev, __local int *result )
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
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( runtime.context, 1, strs.ptr, &size, &status );      assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clBuildProgram( program, 1, &runtime.device, "", null, null );                        assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_noblocks             = clCreateKernel( program, "pf_noblocks"             , &status );  assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clReleaseProgram( program );                                                          assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
  }

  void opencl_noblocks()
  {
    cl_int status;
    size_t gwsize = rows * cols;
    size_t lwsize = cl->getWorkGroupSize( 0 );
    assert( lwsize != 0, "work group size is zero" );
    if ( lwsize > gwsize )
      lwsize = gwsize;
    else
    {
      size_t nthreads = lwsize;
      for ( nthreads = lwsize; gwsize % nthreads != 0; --nthreads );    
      lwsize = nthreads;
    }
    cl_mem result_d[2];
    cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
    cl_mem dwall = createBuffer( runtime.context, flags, cl_int.sizeof * data.length, data );             assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    flags = CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR;
    result_d[0]  = createBuffer( runtime.context, flags, cl_int.sizeof * cols       , NULL );             assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    flags = CL_MEM_READ_WRITE;
    result_d[1]  = createBuffer( runtime.context, flags, cl_int.sizeof * cols       , NULL );             assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

    int src = 0;
    for ( int h = 0 ; height < rows - 1; h += height )
    {
      int iteration = min( height, rows - h - 1 );
      int border = height * HALO;
      int halo = HALO;
      int dst = 1 - src;

      status = setKernelArg( 0,  0, cl_int.sizeof, &iteration );                                          assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0,  1, cl_mem.sizeof, &wall_d );                                             assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0,  2, cl_mem.sizeof, &result_d[src] );                                      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0,  3, cl_mem.sizeof, &result_d[dst] );                                      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0,  4, cl_int.sizeof, &cols );                                               assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0,  5, cl_int.sizeof, &rows );                                               assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0,  6, cl_int.sizeof, &h );                                                  assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0,  7, cl_int.sizeof, &border );                                             assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0,  8, cl_int.sizeof, &halo );                                               assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0,  9, cl_int.sizeof * lwsize, NULL );                                       assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = setKernelArg( 0, 10, cl_int.sizeof * lwsize, NULL );                                       assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

      status = enqueueNDRangeKernel( 0, 1, NULL, &gwsize, &lwsize, 0, NULL, NULL );                       assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

      src = dst;
    }

    status = enqueueReadBuffer( result_d[src], CL_TRUE, 0, cl_int.sizeof * cols, result, 0, NULL, NULL ); assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

    status = releaseMemObject( wall_d );                                                                  assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = releaseMemObject( result_d[0] );                                                             assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = releaseMemObject( result_d[1] );                                                             assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
  }

  void run()
  {
    opencl_noblocks();
  }

}

int
main( string[] args )
{
  try
  {
    runtime.init( 0, 2 );
    auto app = new Application( args );
    app.run();
    runtime.shutdown();
  }
  catch ( Exception msg )
  {
    writeln( "PF: ", msg );
    return -1;
  }
  return 0;
}
