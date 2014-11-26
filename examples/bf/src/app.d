module clop.examples.bf;

import std.conv;
import std.datetime;
import std.getopt;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;

/**
 * This application implements the Bellman-Ford algorithm for
 * finding the shortest paths from a single source vertex to
 * all other vertices in a weighted directed graph.
 *
 * function BellmanFord(list vertices, list edges, vertex source)
 *  ::weight[],predecessor[]
 *
 *  // This implementation takes in a graph, represented as
 *  // lists of vertices and edges, and fills two arrays
 *  // (weight and predecessor) with shortest-path
 *  // (less cost/weight/metric) information
 *
 *  // Step 1: initialize graph
 *  for each vertex v in vertices:
 *      if v is source then weight[v] := 0
 *      else weight[v] := infinity
 *      predecessor[v] := null
 *
 *  // Step 2: relax edges repeatedly
 *  for i from 1 to size(vertices)-1:
 *      for each edge (u, v) with weight w in edges:
 *          if weight[u] + w < weight[v]:
 *              weight[v] := weight[u] + w
 *              predecessor[v] := u
 *
 *  // Step 3: check for negative-weight cycles
 *  for each edge (u, v) with weight w in edges:
 *      if weight[u] + w < weight[v]:
 *          error "Graph contains a negative-weight cycle"
 *  return weight[], predecessor[]
 *
 * Here we use one matrix and overwrite it for each iteration of k.
 **/
class Application {
  static immutable int SEED       =   1;
  static immutable int BLOCK_SIZE = 128;

  int[] A; // size x size, A[i,j] != infinity <=> exists edge from i to j.
  int[] P;
  int[] W;
  int size;
  int source;

  cl_kernel kernel_noblocks;

  this( string[] args )
  {
    if ( args.length != 2 )
    {
      throw new Exception( "ERROR: invalid args # " ~ to!(string)(args.length - 1) );
    }
    size   = to!(int)( args[1] );
    A      = new int[size * size]; assert( null != A, "Can't allocate memory for graph" );
    P      = new int[size];        assert( null != P, "Can't allocate memory for predecessors" );
    W      = new int[size];        assert( null != W, "Can't allocate memory for weights" );
    Mt19937 gen;
    gen.seed( SEED );
    foreach ( ref item; A )
      item = uniform( 0, 10, gen );

    /**
     * An inset with the source code of OpenCL kernels.
     * 
     */
    char[] code = q{
      /* OpenCL kernels source code. */
      /**
       * The basic kernel for the implementation that doesn't use tiling.
       */
      __kernel void bf_noblocks( __global int* A )
      {
        const int tx = get_global_id( 0 );
      }
      /* End of OpenCL source code. */
    }.dup;
    cl_int status;
    size_t len = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( runtime.context, 1, strs.ptr, &len, &status ); assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clBuildProgram( program, 1, &runtime.device, "", null, null );                  assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_noblocks = clCreateKernel( program, "bf_noblocks", &status );                     assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clReleaseProgram( program );                                                    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
  }

  void baseline()
  {
    // Step 1: initialize graph
    foreach ( v; 0 .. size )
    {
      W[v] = is_source( v ) ? 0 : int.MAX;
      P[v] = -1;
    }

    // Step 2: relax edges repeatedly
    foreach ( i; 0 .. size - 1 )
      foreach ( u; 0 .. size )
        foreach ( v; 0 .. size )
        {
          auto w = A[u * size + v];
          if ( w != int.MAX && W[u] + w < W[v] )
          {
            W[v] = W[u] + w;
            P[v] = u;
          }
        }

    // Step 3: check for negative-weight cycles
    foreach ( u; 0 .. size )
      foreach ( v; 0 .. size )
      {
        auto w = A[u * size + v];
        if ( w != int.MAX && W[u] + w < W[v] )
          writeln( "Graph contains a negative-weight cycle" );
      }
  }

  void opencl_noblocks()
  {
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
      status = clSetKernelArg( kernel_noblocks,  0, cl_mem.sizeof, &ddata );                                                assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  1, cl_mem.sizeof, &dsums[src] );                                           assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  2, cl_mem.sizeof, &dsums[dst] );                                           assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  3, cl_mem.sizeof, &dmove );                                                assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  4, cl_int.sizeof, &cols );                                                 assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks,  5, cl_int.sizeof, &step );                                                 assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

      status = clEnqueueNDRangeKernel( runtime.queue, kernel_noblocks, 1, null, &gwsize, null, 0, null, null );             assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    }

    status = clEnqueueReadBuffer( runtime.queue, dsums[src], CL_TRUE, 0, cl_int.sizeof * cols, sums.ptr, 0, null, null );   assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clEnqueueReadBuffer( runtime.queue, dmove, CL_TRUE, 0, cl_int.sizeof * rows * cols, move.ptr, 0, null, null ); assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

    status = clReleaseMemObject( dsums[1] );                                                                                assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clReleaseMemObject( dsums[0] );                                                                                assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clReleaseMemObject( dmove );                                                                                   assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clReleaseMemObject( ddata );                                                                                   assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    status = clReleaseKernel( kernel_noblocks );                                                                            assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
  }

  void run()
  {
    StopWatch timer;
    TickDuration ticks;

    timer.start();
    baseline();
    timer.stop();
    ticks = timer.peek();
    writeln( "SEQUENTIAL ", ticks.usecs / 1E6, " [s]" );

    reset();
    timer.reset();
    timer.start();
    opencl_noblocks();
    timer.stop();
    ticks = timer.peek();
    writeln( "CL NOBLOCK ", ticks.usecs / 1E6, " [s]" );
    validate();
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
