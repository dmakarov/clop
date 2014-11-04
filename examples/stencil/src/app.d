module clop.examples.stencil;

import std.conv;
import std.datetime;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;

/**
 */
class Application {
  alias float FLOAT_PRECISION;
  static immutable int SEED = 1;
  static immutable int BLOCK_SIZE = 8;

  struct stencil_CTX {
    int bs_y;
  };

  FLOAT_PRECISION[] coef;
  FLOAT_PRECISION[] v;
  FLOAT_PRECISION[] u;
  stencil_CTX stencil_ctx;
  int[3] shape;

  uint xdim, ydim, zdim;

  cl_kernel kernel_stencil_7pt_noblocks;
  cl_kernel kernel_stencil_7pt_shared;
  cl_kernel kernel_stencil_7pt_shared_3d;

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
    coef = new FLOAT_PRECISION[xdim * ydim * zdim]; assert( null != coef, "Can't allocate array coef" );
    v    = new FLOAT_PRECISION[xdim * ydim * zdim]; assert( null != v,    "Can't allocate array v" );
    u    = new FLOAT_PRECISION[xdim * ydim * zdim]; assert( null != u,    "Can't allocate array u" );

    debug ( DEBUG )
    {
      writefln( "Stencil dimensions x:%d, y:%d, z:%d, total memory size allocated %d bytes",
                xdim, ydim, zdim, 3 * xdim * ydim * zdim * FLOAT_PRECISION.sizeof );
    }

    Mt19937 gen;
    gen.seed( SEED );
    foreach ( i; 0 .. xdim * ydim * zdim )
    {
      coef[i] = uniform( 0.0, 1.0, gen );
      v[i]    = uniform( 0.0, 1.0, gen );
    }

    /**
     *  7pt stencil example implemented as kernel computing a single
     *  element of the entire space.
     **/
    char[] code = q{
      #define FLOAT_PRECISION float
      #define BLOCK_SIZE 8

      __kernel void stencil_7pt_noblocks( __global       FLOAT_PRECISION * u,
                                          __global const FLOAT_PRECISION * v,
                                          __global const FLOAT_PRECISION * coef,
                                          const int xdim, const int ydim )
      {
        int tx = get_global_id( 0 );
        u[tx] = coef[tx] * v[tx    ]
              + coef[tx] * v[tx - 1]
              + coef[tx] * v[tx + 1]
              + coef[tx] * v[tx - xdim]
              + coef[tx] * v[tx + xdim]
              + coef[tx] * v[tx - ydim * xdim]
              + coef[tx] * v[tx + ydim * xdim];
      } /* stencil_7pt_noblocks */

      __kernel void stencil_7pt_shared( __global       FLOAT_PRECISION * u,
                                        __global const FLOAT_PRECISION * v,
                                        __global const FLOAT_PRECISION * coef,
                                        const int xdim, const int ydim,
                                        __local        FLOAT_PRECISION * lv,
                                        __local        FLOAT_PRECISION * lc )
      {
        size_t bx = get_group_id( 0 );
        size_t ix = get_local_id( 0 );
        size_t tx = get_global_id( 0 );

        size_t zb = (bx * BLOCK_SIZE * BLOCK_SIZE) / (xdim * ydim);
        size_t yb = (bx % ((ydim / BLOCK_SIZE) * (xdim / BLOCK_SIZE))) * (BLOCK_SIZE / xdim);
        size_t xb = (bx % ((ydim / BLOCK_SIZE) * (xdim / BLOCK_SIZE))) % (xdim / BLOCK_SIZE);

        size_t z0 = zb * BLOCK_SIZE;
        size_t y0 = yb * BLOCK_SIZE;
        size_t x0 = xb * BLOCK_SIZE;

        lv[ix] = v[tx];
        lc[ix] = coef[tx];
        barrier( CLK_LOCAL_MEM_FENCE );

        u[tx] = lc[ix] * lv[ix]
              + lc[ix] * lv[ix - 1]
              + lc[ix] * lv[ix + 1]
              + lc[ix] * lv[ix - BLOCK_SIZE]
              + lc[ix] * lv[ix + BLOCK_SIZE]
              + lc[ix] * lv[ix - BLOCK_SIZE * BLOCK_SIZE]
              + lc[ix] * lv[ix + BLOCK_SIZE * BLOCK_SIZE];
      } /* stencil_7pt_shared */

      __kernel void stencil_7pt_shared_3d( __global       FLOAT_PRECISION * u,
                                           __global const FLOAT_PRECISION * v,
                                           __global const FLOAT_PRECISION * coef,
                                           __local        FLOAT_PRECISION * lv,
                                           __local        FLOAT_PRECISION * lc )
      {
        size_t bx = get_group_id( 0 );
        size_t by = get_group_id( 1 );
        size_t bz = get_group_id( 2 );
        size_t gx = get_local_size( 0 );
        size_t gy = get_local_size( 1 );
        size_t gz = get_local_size( 2 );
        size_t ix = get_local_id( 0 );
        size_t iy = get_local_id( 1 );
        size_t iz = get_local_id( 2 );
        size_t sx = get_global_size( 0 );
        size_t sy = get_global_size( 1 );
        size_t sz = get_global_size( 2 );
        size_t tx = get_global_id( 0 );
        size_t ty = get_global_id( 1 );
        size_t tz = get_global_id( 2 );

        lv[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1] =    v[tz * sy * sx + ty * sx + tx];
        lc[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1] = coef[tz * sy * sx + ty * sx + tx];
        if ( iz == 0 )
        {
          lv[iz * gy * gx + (iy + 1) * gx + ix + 1] =    v[(tz - 1) * sy * sx + ty * sx + tx];
          lc[iz * gy * gx + (iy + 1) * gx + ix + 1] = coef[(tz - 1) * sy * sx + ty * sx + tx];
        }
        if ( iz + 1 == gz )
        {
          lv[(iz + 2) * gy * gx + (iy + 1) * gx + ix + 1] =    v[(tz + 1) * sy * sx + ty * sx + tx];
          lc[(iz + 2) * gy * gx + (iy + 1) * gx + ix + 1] = coef[(tz + 1) * sy * sx + ty * sx + tx];
        }
        barrier( CLK_LOCAL_MEM_FENCE );

        u[tz * sy * sx + ty * sx + tx] = lc[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1] * lv[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1]
                                       + lc[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1] * lv[(iz + 1) * gy * gx + (iy + 1) * gx + ix    ]
                                       + lc[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1] * lv[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 2]
                                       + lc[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1] * lv[(iz + 1) * gy * gx + (iy    ) * gx + ix + 1]
                                       + lc[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1] * lv[(iz + 1) * gy * gx + (iy + 2) * gx + ix + 1]
                                       + lc[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1] * lv[(iz    ) * gy * gx + (iy + 1) * gx + ix + 1]
                                       + lc[(iz + 1) * gy * gx + (iy + 1) * gx + ix + 1] * lv[(iz + 2) * gy * gx + (iy + 1) * gx + ix + 1];
      } /* stencil_7pt_shared_3d */
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( runtime.context, 1, strs.ptr, &size, &status );      assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
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
    kernel_stencil_7pt_noblocks  = clCreateKernel( program, "stencil_7pt_noblocks" , &status );    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_stencil_7pt_shared    = clCreateKernel( program, "stencil_7pt_shared"   , &status );    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_stencil_7pt_shared_3d = clCreateKernel( program, "stencil_7pt_shared_3d", &status );    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clReleaseProgram( program );                                                          assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );

    static if ( false )
    { // an example of getting the correct maximum work group
      // size. device info on OS X reports an incorrect value.
      size_t max_group_size;
      clGetKernelWorkGroupInfo( kernel_stencil_7pt_noblocks, runtime.device, CL_KERNEL_WORK_GROUP_SIZE, max_group_size.sizeof, &max_group_size, null );
    }
  } // this()

  void stencil_7pt( const int shape[3],
                    const int xb, const int yb, const int zb,
                    const int xe, const int ye, const int ze,
                    const FLOAT_PRECISION * coef,
                    const FLOAT_PRECISION * v,
                          FLOAT_PRECISION * u,
                          stencil_CTX stencil_ctx )
  {
    int i, j, k, jb, je;
    int nnx = shape[0];
    int nny = shape[1];
    int ln_domain = shape[0] * shape[1] * shape[2];

    for ( jb = yb; jb < ye; jb += stencil_ctx.bs_y ) // blocking in Y
    {
      je = ( jb + stencil_ctx.bs_y ) < ( ye ) ? ( jb + stencil_ctx.bs_y ) : ( ye );
      {
        for ( k = zb; k < ze; k++ )
        {
          for ( j = jb; j < je; j++ )
          {
            for ( i = xb; i < xe; i++ )
            {
              u[( k * nny + j ) * nnx + i] = coef[( k * nny + j ) * nnx + i                    ] * v[(  k      * nny + j     ) * nnx + i    ]
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain     )] * v[(  k      * nny + j     ) * nnx + i - 1]
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain * 2 )] * v[(  k      * nny + j     ) * nnx + i + 1]
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain * 3 )] * v[(  k      * nny + j - 1 ) * nnx + i    ]
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain * 4 )] * v[(  k      * nny + j + 1 ) * nnx + i    ]
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain * 5 )] * v[( (k - 1) * nny + j     ) * nnx + i    ]
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain * 6 )] * v[( (k + 1) * nny + j     ) * nnx + i    ];
            }
          }
        }
      }
    }
  }

  void stencil_25pt( const int shape[3],
                     const int xb, const int yb, const int zb,
                     const int xe, const int ye, const int ze,
                     const FLOAT_PRECISION * coef,
                     const FLOAT_PRECISION * v,
                           FLOAT_PRECISION * u,
                           stencil_CTX stencil_ctx )
  {
    int i, j, k, jb, je;
    int nnx = shape[0];
    int nny = shape[1];
    int ln_domain = shape[0] * shape[1] * shape[2];

    for ( jb = yb; jb < ye; jb += stencil_ctx.bs_y ) // blocking in Y
    {
      je = ( jb + stencil_ctx.bs_y ) < ( ye ) ? ( jb + stencil_ctx.bs_y ) : ( ye );
      {
        for ( k = zb; k < ze; k++ )
        {
          for ( j = jb; j < je; j++ )
          {
            for ( i = xb; i < xe; i++ )
            {
              u[( k * nny + j ) * nnx + i] = coef[( k * nny + j ) * nnx + i                     ] * ( v[( (k    ) * nny + j     ) * nnx + i    ]                                              )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain      )] * ( v[( (k    ) * nny + j     ) * nnx + i + 1] + v[( (k    ) * nny + j     ) * nnx + i - 1] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain *  2 )] * ( v[( (k    ) * nny + j + 1 ) * nnx + i    ] + v[( (k    ) * nny + j - 1 ) * nnx + i    ] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain *  3 )] * ( v[( (k + 1) * nny + j     ) * nnx + i    ] + v[( (k - 1) * nny + j     ) * nnx + i    ] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain *  4 )] * ( v[( (k    ) * nny + j     ) * nnx + i + 2] + v[( (k    ) * nny + j     ) * nnx + i - 2] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain *  5 )] * ( v[( (k    ) * nny + j + 2 ) * nnx + i    ] + v[( (k    ) * nny + j - 2 ) * nnx + i    ] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain *  6 )] * ( v[( (k + 2) * nny + j     ) * nnx + i    ] + v[( (k - 2) * nny + j     ) * nnx + i    ] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain *  7 )] * ( v[( (k    ) * nny + j     ) * nnx + i + 3] + v[( (k    ) * nny + j     ) * nnx + i - 3] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain *  8 )] * ( v[( (k    ) * nny + j + 3 ) * nnx + i    ] + v[( (k    ) * nny + j - 3 ) * nnx + i    ] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain *  9 )] * ( v[( (k + 3) * nny + j     ) * nnx + i    ] + v[( (k - 3) * nny + j     ) * nnx + i    ] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain * 10 )] * ( v[( (k    ) * nny + j     ) * nnx + i + 4] + v[( (k    ) * nny + j     ) * nnx + i - 4] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain * 11 )] * ( v[( (k    ) * nny + j + 4 ) * nnx + i    ] + v[( (k    ) * nny + j - 4 ) * nnx + i    ] )
                                           + coef[( k * nny + j ) * nnx + i + ( ln_domain * 12 )] * ( v[( (k + 4) * nny + j     ) * nnx + i    ] + v[( (k - 4) * nny + j     ) * nnx + i    ] );
            }
          }
        }
      }
    }
  }

  void stencil_7pt_sequential()
  {
    shape[0] = BLOCK_SIZE;
    shape[1] = BLOCK_SIZE;
    shape[2] =          0;
    stencil_ctx.bs_y = BLOCK_SIZE + 1;
    for ( int zb = 0; zb < zdim; zb += BLOCK_SIZE )
    {
      for ( int yb = 0; yb < ydim; yb += BLOCK_SIZE )
      {
        for ( int xb = 0; xb < xdim; xb += BLOCK_SIZE )
        {
          stencil_7pt( shape, xb, yb, zb, xb + BLOCK_SIZE, yb + BLOCK_SIZE, zb + BLOCK_SIZE, coef.ptr, v.ptr, u.ptr, stencil_ctx );
        }
      }
    }
  }

  void opencl_stencil_7pt_noblocks()
  {
    try
    {
      cl_int status;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY | CL_MEM_USE_HOST_PTR;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, u.ptr, &status );                   assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );                   assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );             assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 0, cl_mem.sizeof, &du   );                                           assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 1, cl_mem.sizeof, &dv   );                                           assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 2, cl_mem.sizeof, &dc   );                                           assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 3, cl_int.sizeof, &xdim );                                           assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 4, cl_int.sizeof, &ydim );                                           assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );

      size_t global = zdim * ydim * zdim;
      status = clEnqueueNDRangeKernel( runtime.queue, kernel_stencil_7pt_noblocks, 1, null, &global, null, 0, null, null );      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );

      status = clEnqueueReadBuffer( runtime.queue, du, CL_TRUE, 0, FLOAT_PRECISION.sizeof * u.length, u.ptr, 0, null, null );    assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( du );                                                                                         assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );                                                                                         assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );                                                                                         assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_7pt_noblocks );                                                                   assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
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
      cl_mem_flags flags = CL_MEM_WRITE_ONLY | CL_MEM_USE_HOST_PTR;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, u.ptr, &status );                   assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );                   assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );             assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 0, cl_mem.sizeof, &du   );                                             assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 1, cl_mem.sizeof, &dv   );                                             assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 2, cl_mem.sizeof, &dc   );                                             assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 3, cl_int.sizeof, &xdim );                                             assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 4, cl_int.sizeof, &ydim );                                             assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 5,
                               (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * FLOAT_PRECISION.sizeof, null );          assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 6,
                               (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * FLOAT_PRECISION.sizeof, null );          assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );

      size_t global = zdim * ydim * zdim;
      size_t local  = BLOCK_SIZE * BLOCK_SIZE * BLOCK_SIZE;
      status = clEnqueueNDRangeKernel( runtime.queue, kernel_stencil_7pt_shared, 1, null, &global, &local, 0, null, null );      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );

      status = clEnqueueReadBuffer( runtime.queue, du, CL_TRUE, 0, FLOAT_PRECISION.sizeof * u.length, u.ptr, 0, null, null );    assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( du );                                                                                         assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );                                                                                         assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );                                                                                         assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_7pt_shared );                                                                     assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
  }

  void opencl_stencil_7pt_shared_3d()
  {
    try
    {
      cl_int status;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY | CL_MEM_USE_HOST_PTR;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, u.ptr, &status );                   assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );                   assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );             assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared_3d, 0, cl_mem.sizeof, &du );                                            assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared_3d, 1, cl_mem.sizeof, &dv );                                            assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared_3d, 2, cl_mem.sizeof, &dc );                                            assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared_3d, 3,
                               (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * FLOAT_PRECISION.sizeof, null );          assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared_3d, 4,
                               (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * FLOAT_PRECISION.sizeof, null );          assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );

      size_t[3] global = [xdim, ydim, zdim];
      size_t[3] local  = [BLOCK_SIZE, BLOCK_SIZE, BLOCK_SIZE];
      status = clEnqueueNDRangeKernel( runtime.queue               , // cl_command_queue command_queue
                                       kernel_stencil_7pt_shared_3d, // cl_kernel        kernel
                                       3                           , // cl_uint          work_dim
                                       null                        , // const size_t*    global_work_offset
                                       global.ptr                  , // const size_t*    global_work_size
                                       local.ptr                   , // const size_t*    local_work_size
                                       0                           , // cl_uint          num_events_in_wait_list
                                       null                        , // const cl_event*  event_wait_list
                                       null );                       // cl_event*        event
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );

      status = clEnqueueReadBuffer( runtime.queue, du, CL_TRUE, 0, FLOAT_PRECISION.sizeof * u.length, u.ptr, 0, null, null );    assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      status = clReleaseMemObject( du );                                                                                         assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );                                                                                         assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );                                                                                         assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_7pt_shared_3d );                                                                  assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared_3d " ~ cl_strerror( status ) );
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
    stencil_7pt_sequential();
    timer.stop();
    ticks = timer.peek();
    writeln( "SEQUENTIAL ", ticks.usecs / 1E6, " [s]" );

    timer.reset();
    timer.start();
    opencl_stencil_7pt_noblocks();
    timer.stop();
    ticks = timer.peek();
    writeln( "NO BLOCKS  ", ticks.usecs / 1E6, " [s]" );

    timer.reset();
    timer.start();
    opencl_stencil_7pt_shared();
    timer.stop();
    ticks = timer.peek();
    writeln( "SHARED     ", ticks.usecs / 1E6, " [s]" );

    timer.reset();
    timer.start();
    opencl_stencil_7pt_shared_3d();
    timer.stop();
    ticks = timer.peek();
    writeln( "SHARED 3D  ", ticks.usecs / 1E6, " [s]" );
  }
}

int
main( string[] args )
{
  try
  {
    runtime.init( 0, 1 );
    auto app = new Application( args );
    app.run();
    runtime.shutdown();
  }
  catch ( Exception msg )
  {
    writeln( "STENCIL: ", msg );
    return -1;
  }
  return 0;
}
