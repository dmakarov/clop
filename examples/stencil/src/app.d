module clop.examples.stencil;

import std.conv;
import std.datetime;
import std.math;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;

/**
 */
class Application {
  alias float FLOAT_PRECISION;
  static immutable int SEED = 1;
  static immutable uint BLOCK_SIZE = 8;
  static immutable uint NUM_DOMAINS = 13;

  struct stencil_CTX {
    int bs_y;
  };

  FLOAT_PRECISION[] coef;
  FLOAT_PRECISION[] v;
  FLOAT_PRECISION[] u;
  FLOAT_PRECISION[] U;
  stencil_CTX stencil_ctx;

  uint xdim, ydim, zdim;

  cl_kernel kernel_stencil_7pt_noblocks;
  cl_kernel kernel_stencil_7pt_shared;

  /**
   */
  this( string[] args )
  {
    if ( args.length != 4 )
    {
      throw new Exception( "ERROR: invalid args # " ~ to!(string)(args.length - 1) );
    }
    xdim = to!(uint)( args[1] );
    ydim = to!(uint)( args[2] );
    zdim = to!(uint)( args[3] );
    coef = new FLOAT_PRECISION[xdim * ydim * zdim * NUM_DOMAINS]; assert( null != coef, "Can't allocate array coef" );
    v    = new FLOAT_PRECISION[xdim * ydim * zdim]; assert( null != v, "Can't allocate array v" );
    u    = new FLOAT_PRECISION[xdim * ydim * zdim]; assert( null != u, "Can't allocate array u" );
    U    = new FLOAT_PRECISION[xdim * ydim * zdim]; assert( null != U, "Can't allocate array U" );

    debug ( DEBUG )
    {
      writefln( "Stencil dimensions x:%d, y:%d, z:%d, total memory size allocated %d bytes",
                xdim, ydim, zdim, (NUM_DOMAINS + 3) * xdim * ydim * zdim * FLOAT_PRECISION.sizeof );
    }

    Mt19937 gen;
    gen.seed( SEED );
    foreach ( i; 0 .. xdim * ydim * zdim * NUM_DOMAINS )
    {
      coef[i] = uniform( 0.0, 1.0, gen );
    }
    foreach ( i; 0 .. xdim * ydim * zdim )
    {
      v[i]    = uniform( 0.0, 1.0, gen );
    }

    /**
     *  7pt stencil example implemented as kernel computing a single
     *  element of the entire space.
     **/
    char[] code = q{
      #define FLOAT_PRECISION float
      #define C( k, j, i, d ) ((d) * zdim * ydim * xdim + (k) * ydim * xdim + (j) * xdim + (i))
      #define V( k, j, i    ) (                           (k) * ydim * xdim + (j) * xdim + (i))

      __kernel void stencil_7pt_noblocks( __global       FLOAT_PRECISION * u,
                                          __global const FLOAT_PRECISION * v,
                                          __global const FLOAT_PRECISION * coef )
      {
        size_t tz = get_global_id( 0 ) + 1;
        size_t ty = get_global_id( 1 ) + 1;
        size_t tx = get_global_id( 2 ) + 1;
        size_t zdim = get_global_size( 0 ) + 2;
        size_t ydim = get_global_size( 1 ) + 2;
        size_t xdim = get_global_size( 2 ) + 2;

        u[V(tz,ty,tx)] = coef[C(tz,ty,tx,0)] * v[V(tz    ,ty    ,tx    )]
                       + coef[C(tz,ty,tx,1)] * v[V(tz    ,ty    ,tx - 1)]
                       + coef[C(tz,ty,tx,2)] * v[V(tz    ,ty    ,tx + 1)]
                       + coef[C(tz,ty,tx,3)] * v[V(tz    ,ty - 1,tx    )]
                       + coef[C(tz,ty,tx,4)] * v[V(tz    ,ty + 1,tx    )]
                       + coef[C(tz,ty,tx,5)] * v[V(tz - 1,ty    ,tx    )]
                       + coef[C(tz,ty,tx,6)] * v[V(tz + 1,ty    ,tx    )];
        /*
        u[V(tz,ty,tx)] = C(tz,ty,tx,6);
        */
      } /* stencil_7pt_noblocks */

      #define LC( k, j, i, d ) ((d) * gz * gy * gx + (k) * gy * gx + (j) * gx + (i))
      #define LV( k, j, i    ) (                     (k) * gy * gx + (j) * gx + (i))

      __kernel void stencil_7pt_shared( __global       FLOAT_PRECISION * u,
                                        __global const FLOAT_PRECISION * v,
                                        __global const FLOAT_PRECISION * coef,
                                        __local        FLOAT_PRECISION * lv,
                                        __local        FLOAT_PRECISION * lc )
      {
        size_t iz = get_local_id( 0 ) + 1;
        size_t iy = get_local_id( 1 ) + 1;
        size_t ix = get_local_id( 2 ) + 1;
        size_t gz = get_local_size( 0 ) + 2;
        size_t gy = get_local_size( 1 ) + 2;
        size_t gx = get_local_size( 2 ) + 2;
        size_t tz = get_global_id( 0 );
        size_t ty = get_global_id( 1 );
        size_t tx = get_global_id( 2 );
        size_t zdim = get_global_size( 0 );
        size_t ydim = get_global_size( 1 );
        size_t xdim = get_global_size( 2 );

        lv[LV(iz,iy,ix)] = v[V(tz,ty,tx)];
        for ( int ii = 0; ii < 7; ++ii )
          lc[LC(iz,iy,ix,ii)] = coef[C(tz,ty,tx,ii)];
        if ( 0 < tx && 0 == ix )
        {
          lv[LV(iz,iy,0)] = v[V(tz,ty,tx - 1)];
          for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz,iy,0,ii)] = coef[C(tz,ty,tx - 1,ii)];
        }
        if ( 0 < ty && 0 == iy )
        {
          lv[LV(iz,0,ix)] = v[V(tz,ty - 1,tx)];
          for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz,0,ix,ii)] = coef[C(tz,ty - 1,tx,ii)];
        }
        if ( 0 < tz && 0 == iz )
        {
          lv[LV(0,iy,ix)] = v[V(tz - 1,ty,tx)];
          for ( int ii = 0; ii < 7; ++ii )
            lc[LC(0,iy,ix,ii)] = coef[C(tz - 1,ty,tx,ii)];
        }
        if ( tx < xdim - 1 && ix + 1 == gx )
        {
          lv[LV(iz,iy,ix)] = v[V(tz,ty,tx + 1)];
          for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz,iy,ix,ii)] = coef[C(tz,ty,tx + 1,ii)];
        }
        if ( ty < ydim - 1 && iy + 1 == gy )
        {
          lv[LV(iz,iy,ix)] = v[V(tz,ty + 1,tx)];
          for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz,iy,ix,ii)] = coef[C(tz,ty + 1,tx,ii)];
        }
        if ( tz < zdim - 1 && iz + 1 == gz )
        {
          lv[LV(iz,iy,ix)] = v[V(tz + 1,ty,tx)];
          for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz,iy,ix,ii)] = coef[C(tz + 1,ty,tx,ii)];
        }
        barrier( CLK_LOCAL_MEM_FENCE );

        if ( 0 < tz && tz < zdim - 1 && 0 < ty && ty < ydim - 1 && 0 < tx && tx < xdim - 1 )
        u[V(tz,ty,tx)] = lc[LC(iz,iy,ix,0)] * lv[LV(iz    ,iy    ,ix    )]
                       + lc[LC(iz,iy,ix,1)] * lv[LV(iz    ,iy    ,ix - 1)]
                       + lc[LC(iz,iy,ix,2)] * lv[LV(iz    ,iy    ,ix + 1)]
                       + lc[LC(iz,iy,ix,3)] * lv[LV(iz    ,iy - 1,ix    )]
                       + lc[LC(iz,iy,ix,4)] * lv[LV(iz    ,iy + 1,ix    )]
                       + lc[LC(iz,iy,ix,5)] * lv[LV(iz - 1,iy    ,ix    )]
                       + lc[LC(iz,iy,ix,6)] * lv[LV(iz + 1,iy    ,ix    )];
      } /* stencil_7pt_shared_3d */
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( runtime.context, 1, strs.ptr, &size, &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clBuildProgram( program, 1, &runtime.device, "", null, null );
    if ( CL_SUCCESS != status )
    {
      size_t param_value_size;
      status = clGetProgramBuildInfo ( program, runtime.device, CL_PROGRAM_BUILD_LOG, 0, null, &param_value_size );
      assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
      char[] buffer = new char[param_value_size];
      assert( null != buffer, "Can't allocate buffer" );
      status = clGetProgramBuildInfo ( program, runtime.device, CL_PROGRAM_BUILD_LOG, param_value_size, buffer.ptr, null );
      assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
      writefln( "Kernel build log:\n%s", buffer );
    }
    kernel_stencil_7pt_noblocks  = clCreateKernel( program, "stencil_7pt_noblocks" , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_stencil_7pt_shared    = clCreateKernel( program, "stencil_7pt_shared"   , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clReleaseProgram( program );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );

    static if ( false )
    { // an example of getting the correct maximum work group
      // size. device info on OS X reports an incorrect value.
      size_t max_group_size;
      clGetKernelWorkGroupInfo( kernel_stencil_7pt_noblocks, runtime.device, CL_KERNEL_WORK_GROUP_SIZE, max_group_size.sizeof, &max_group_size, null );
    }
  } // this()

  /**
   */
  void validate()
  {
    int diff = 0;
    foreach ( ii; 0 .. U.length )
      if ( U[ii] != float.nan && u[ii] != float.nan && fabs( U[ii] - u[ii] ) > 1E-6 )
      {
        ++diff;
        writefln( "%5d : %f != %f", ii, U[ii], u[ii] );
      }
    if ( diff > 0 )
      writeln( "DIFFs ", diff );
  }

  FLOAT_PRECISION C( uint k, uint j, uint i, uint d )
  {
    return coef[d * zdim * ydim * xdim + k * ydim * xdim + j * xdim + i];
  }

  FLOAT_PRECISION V( uint k, uint j, uint i )
  {
    return v[k * ydim * xdim + j * xdim + i];
  }

  void sequential_stencil_7pt()
  {
    for ( uint jb = 1; jb < ydim - 1; jb += BLOCK_SIZE ) // blocking in Y
    {
      uint je = jb + BLOCK_SIZE < ydim - 1 ? jb + BLOCK_SIZE : ydim - 1;
      {
        for ( uint k = 1; k < zdim - 1; ++k )
          for ( uint j = jb; j < je; ++j )
            for ( uint i = 1; i < xdim - 1; ++i )
            {
              debug ( DEBUG )
                writefln( "U[%5d] <- C[%d], V[%d]",
                          ( k * ydim + j ) * xdim + i,
                          6 * zdim * ydim * xdim + k * ydim * xdim + j * xdim + i,
                          (k + 1)  * ydim * xdim + j * xdim + i);
              U[( k * ydim + j ) * xdim + i] = C(k,j,i,0) * V(k    ,j    ,i    )
                                             + C(k,j,i,1) * V(k    ,j    ,i - 1)
                                             + C(k,j,i,2) * V(k    ,j    ,i + 1)
                                             + C(k,j,i,3) * V(k    ,j - 1,i    )
                                             + C(k,j,i,4) * V(k    ,j + 1,i    )
                                             + C(k,j,i,5) * V(k - 1,j    ,i    )
                                             + C(k,j,i,6) * V(k + 1,j    ,i    );
            }
      }
    }
  }

  void sequential_stencil_25pt()
  {
    for ( uint jb = 1; jb < ydim - 1; jb += BLOCK_SIZE ) // blocking in Y
    {
      uint je = jb + BLOCK_SIZE < ydim - 1 ? jb + BLOCK_SIZE : ydim - 1;
      {
        for ( uint k = 1; k < zdim - 1; ++k )
          for ( uint j = jb; j < je; ++j )
            for ( uint i = 1; i < xdim - 1; ++i )
              U[( k * ydim + j ) * xdim + i] = C(k,j,i, 0) * ( V(k    ,j    ,i    )                        )
                                             + C(k,j,i, 1) * ( V(k    ,j    ,i + 1) + V(k    ,j    ,i - 1) )
                                             + C(k,j,i, 2) * ( V(k    ,j + 1,i    ) + V(k    ,j - 1,i    ) )
                                             + C(k,j,i, 3) * ( V(k + 1,j    ,i    ) + V(k - 1,j    ,i    ) )
                                             + C(k,j,i, 4) * ( V(k    ,j    ,i + 2) + V(k    ,j    ,i - 2) )
                                             + C(k,j,i, 5) * ( V(k    ,j + 2,i    ) + V(k    ,j - 2,i    ) )
                                             + C(k,j,i, 6) * ( V(k + 2,j    ,i    ) + V(k - 2,j    ,i    ) )
                                             + C(k,j,i, 7) * ( V(k    ,j    ,i + 3) + V(k    ,j    ,i - 3) )
                                             + C(k,j,i, 8) * ( V(k    ,j + 3,i    ) + V(k    ,j - 3,i    ) )
                                             + C(k,j,i, 9) * ( V(k + 3,j    ,i    ) + V(k - 3,j    ,i    ) )
                                             + C(k,j,i,10) * ( V(k    ,j    ,i + 4) + V(k    ,j    ,i - 4) )
                                             + C(k,j,i,11) * ( V(k    ,j + 4,i    ) + V(k    ,j - 4,i    ) )
                                             + C(k,j,i,12) * ( V(k + 4,j    ,i    ) + V(k - 4,j    ,i    ) );
      }
    }
  }

  void opencl_stencil_7pt_noblocks()
  {
    try
    {
      cl_int status;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, null, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 0, cl_mem.sizeof, &du   );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 1, cl_mem.sizeof, &dv   );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 2, cl_mem.sizeof, &dc   );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );

      size_t[3] global = [zdim - 2, ydim - 2, xdim - 2];
      status = clEnqueueNDRangeKernel( runtime.queue              , //
                                       kernel_stencil_7pt_noblocks, //
                                       3                          , //
                                       null                       , //
                                       global.ptr                 , //
                                       null                       , //
                                       0                          , //
                                       null                       , //
                                       null                      ); //
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );

      status = clEnqueueReadBuffer( runtime.queue                    , //
                                    du                               , //
                                    CL_TRUE                          , //
                                    0                                , //
                                    FLOAT_PRECISION.sizeof * u.length, //
                                    u.ptr                            , //
                                    0                                , //
                                    null                             , //
                                    null                            ); //
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( du );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_7pt_noblocks );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
  }

  void opencl_stencil_7pt_shared()
  {
    try
    {
      cl_int status;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, null, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 0, cl_mem.sizeof, &du );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 1, cl_mem.sizeof, &dv );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 2, cl_mem.sizeof, &dc );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 3, (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * FLOAT_PRECISION.sizeof, null );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 4, (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * NUM_DOMAINS * FLOAT_PRECISION.sizeof, null );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );

      size_t[3] global = [zdim, ydim, xdim];
      size_t[3] local  = [BLOCK_SIZE, BLOCK_SIZE, BLOCK_SIZE];
      status = clEnqueueNDRangeKernel( runtime.queue            , // cl_command_queue command_queue
                                       kernel_stencil_7pt_shared, // cl_kernel        kernel
                                       3                        , // cl_uint          work_dim
                                       null                     , // const size_t*    global_work_offset
                                       global.ptr               , // const size_t*    global_work_size
                                       local.ptr                , // const size_t*    local_work_size
                                       0                        , // cl_uint          num_events_in_wait_list
                                       null                     , // const cl_event*  event_wait_list
                                       null                    ); // cl_event*        event
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );

      status = clEnqueueReadBuffer( runtime.queue                    , //
                                    du                               , //
                                    CL_TRUE                          , //
                                    0                                , //
                                    FLOAT_PRECISION.sizeof * u.length, //
                                    u.ptr                            , //
                                    0                                , //
                                    null                             , //
                                    null                            ); //
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( du );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_7pt_shared );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
  }

  /**
   */
  void run()
  {
    StopWatch timer;
    TickDuration ticks;

    timer.start();
    sequential_stencil_7pt();
    timer.stop();
    ticks = timer.peek();
    writeln( "SEQUENTIAL ", ticks.usecs / 1E6, " [s]" );

    timer.reset();
    timer.start();
    opencl_stencil_7pt_noblocks();
    timer.stop();
    ticks = timer.peek();
    writeln( "NO BLOCKS  ", ticks.usecs / 1E6, " [s]" );
    debug( DEBUG )
      foreach (ii; 0 .. u.length)
        writefln( "u[%5d] : %f : %f", ii, u[ii], U[ii] );
    validate();

    timer.reset();
    timer.start();
    opencl_stencil_7pt_shared();
    timer.stop();
    ticks = timer.peek();
    writeln( "SHARED     ", ticks.usecs / 1E6, " [s]" );
    validate();
  }
}

int
main( string[] args )
{
  try
  {
    foreach ( d; 1 .. 2 )
    {
      runtime.init( 0, d );
      auto app = new Application( args );
      app.run();
      runtime.shutdown();
    }
  }
  catch ( Exception msg )
  {
    writeln( "STENCIL: ", msg );
    return -1;
  }
  return 0;
}
