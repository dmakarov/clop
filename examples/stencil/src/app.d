module clop.examples.stencil;

import std.conv;
import std.datetime;
import std.getopt;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;

/**
 */
class Application {
  static immutable int SEED  =  1;
  static immutable int BLOCK_SIZE = 16;

  int[] A; // reconstructed aligned sequence A
  int[] B; // reconstructed aligned sequence B
  int[] M; // characters of sequence A
  int[] N; // characters of sequence B
  int[] F; // matrix of computed scores
  int[] G; // copy of F for validation
  int[] S; // matrix of matches
  int[] I; // map of reads
  int[] O; // map of writes
  int[] Z; // map of shared memory accesses
  int rows;
  int cols;
  string outfile;

  cl_kernel kernel_noblocks;

  /**
   */
  this( string[] args )
  {
    getopt( args, "output|o", &outfile );
    if ( args.length != 3 )
    {
      throw new Exception( "ERROR: invalid args # " ~ to!(string)(args.length - 1) );
    }
    rows    = to!(int)( args[1] ) + 1;
    cols    = to!(int)( args[1] ) + 1;
    penalty = to!(int)( args[2] );
    if ( ( rows - 1 ) % 16 != 0 )
    {
      auto r = to!(string)(rows - 1);
      auto b = to!(string)(BLOCK_SIZE);
      throw new Exception( "ERROR: rows # (" ~ r ~ ") must be a multiple of " ~ b );
    }
    S = new int[rows * cols]; assert( null != S, "Can't allocate array S" );
    F = new int[rows * cols]; assert( null != F, "Can't allocate array F" );
    G = new int[rows * cols]; assert( null != G, "Can't allocate array G" );
    M = new int[rows];        assert( null != M, "Can't allocate array M" );
    N = new int[cols];        assert( null != N, "Can't allocate array N" );
    A = new int[rows];        assert( null != A, "Can't allocate array A" );
    B = new int[cols];        assert( null != B, "Can't allocate array B" );

    debug ( DEBUG )
    {
      I = new int[rows * cols]; assert( null != I, "Can't allocate array I" );
      O = new int[rows * cols]; assert( null != O, "Can't allocate array O" );
      Z = new int[(BLOCK_SIZE + 1) * (BLOCK_SIZE + 2)]; assert( null != Z, "Can't allocate array Z" );
    }

    Mt19937 gen;
    gen.seed( SEED );
    foreach ( r; 1 .. rows )
    {
      F[r * cols] = -penalty * r;
      M[r] = uniform( 1, CHARS, gen );
    }
    foreach ( c; 1 .. cols )
    {
      F[c] = -penalty * c;
      N[c] = uniform( 1, CHARS, gen );
    }
    foreach ( r; 1 .. rows )
      foreach ( c; 1 .. cols )
        S[r * cols + c] = BLOSUM62[M[r] * CHARS + N[c]];

    /**
     *
     **/
    char[] code = q{
      #define BLOCK_SIZE 16
      #define CHARS      24
      #define I(r,c)     ((r) * cols + (c))

      int max3( int a, int b, int c );

      int max3( int a, int b, int c )
      {
        int k = a > b ? a : b;
        return k > c ? k : c;
      }

      __kernel void nw_noblocks( __global const int* S, __global int* F, int cols, int penalty, int diagonal )
      {
        int tx = get_global_id( 0 );
        int c =            tx + 1;
        int r = diagonal - tx - 1;
        if ( diagonal >= cols )
        {
          c = diagonal - cols + tx + 1;
          r =            cols - tx - 1;
        }
        int m = F[I(r - 1, c - 1)] + S[I(r, c)];
        int d = F[I(r - 1, c    )] - penalty;
        int i = F[I(r    , c - 1)] - penalty;
        F[I(r, c)] = max3( m, d, i );
      } /* nw_noblocks */
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( runtime.context, 1, strs.ptr, &size, &status );      assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clBuildProgram( program, 1, &runtime.device, "", null, null );                        assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_noblocks             = clCreateKernel( program, "nw_noblocks"             , &status );  assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clReleaseProgram( program );                                                          assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );

    static if ( false )
    { // an example of getting the correct maximum work group
      // size. device info on OS X reports an incorrect value.
      size_t max_group_size;
      clGetKernelWorkGroupInfo( kernel_rectangles, runtime.device, CL_KERNEL_WORK_GROUP_SIZE, max_group_size.sizeof, &max_group_size, null );
    }
  } // this()

  void
  stencil_7pt( const int shape[3],
               const int xb, const int yb, const int zb,
               const int xe, const int ye, const int ze,
               const FLOAT_PRECISION * restrict coef,
               const FLOAT_PRECISION * restrict v,
                     FLOAT_PRECISION * restrict u,
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
                     const FLOAT_PRECISION * restrict coef,
                     const FLOAT_PRECISION * restrict v,
                           FLOAT_PRECISION * restrict u,
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

  /**
   */
  void run()
  {
    StopWatch timer;
    TickDuration ticks;

    timer.start();
    baseline_sequential();
    timer.stop();
    ticks = timer.peek();
    writeln( "SEQUENTIAL ", ticks.usecs / 1E6, " [s]" );
    G[] = F[];

    reset();
    timer.reset();
    timer.start();
    rectangles();
    timer.stop();
    ticks = timer.peek();
    writeln( "RECTANGLES ", ticks.usecs / 1E6, " [s]" );
    validate();
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
