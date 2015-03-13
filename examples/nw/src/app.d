module clop.examples.nw;

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
  static immutable int CHARS = 24;
  static immutable int BLOCK_SIZE = 16;
  static int[CHARS * CHARS] BLOSUM62 = [
   4, -1, -2, -2,  0, -1, -1,  0, -2, -1, -1, -1, -1, -2, -1,  1,  0, -3, -2,  0, -2, -1,  0, -4,
  -1,  5,  0, -2, -3,  1,  0, -2,  0, -3, -2,  2, -1, -3, -2, -1, -1, -3, -2, -3, -1,  0, -1, -4,
  -2,  0,  6,  1, -3,  0,  0,  0,  1, -3, -3,  0, -2, -3, -2,  1,  0, -4, -2, -3,  3,  0, -1, -4,
  -2, -2,  1,  6, -3,  0,  2, -1, -1, -3, -4, -1, -3, -3, -1,  0, -1, -4, -3, -3,  4,  1, -1, -4,
   0, -3, -3, -3,  9, -3, -4, -3, -3, -1, -1, -3, -1, -2, -3, -1, -1, -2, -2, -1, -3, -3, -2, -4,
  -1,  1,  0,  0, -3,  5,  2, -2,  0, -3, -2,  1,  0, -3, -1,  0, -1, -2, -1, -2,  0,  3, -1, -4,
  -1,  0,  0,  2, -4,  2,  5, -2,  0, -3, -3,  1, -2, -3, -1,  0, -1, -3, -2, -2,  1,  4, -1, -4,
   0, -2,  0, -1, -3, -2, -2,  6, -2, -4, -4, -2, -3, -3, -2,  0, -2, -2, -3, -3, -1, -2, -1, -4,
  -2,  0,  1, -1, -3,  0,  0, -2,  8, -3, -3, -1, -2, -1, -2, -1, -2, -2,  2, -3,  0,  0, -1, -4,
  -1, -3, -3, -3, -1, -3, -3, -4, -3,  4,  2, -3,  1,  0, -3, -2, -1, -3, -1,  3, -3, -3, -1, -4,
  -1, -2, -3, -4, -1, -2, -3, -4, -3,  2,  4, -2,  2,  0, -3, -2, -1, -2, -1,  1, -4, -3, -1, -4,
  -1,  2,  0, -1, -3,  1,  1, -2, -1, -3, -2,  5, -1, -3, -1,  0, -1, -3, -2, -2,  0,  1, -1, -4,
  -1, -1, -2, -3, -1,  0, -2, -3, -2,  1,  2, -1,  5,  0, -2, -1, -1, -1, -1,  1, -3, -1, -1, -4,
  -2, -3, -3, -3, -2, -3, -3, -3, -1,  0,  0, -3,  0,  6, -4, -2, -2,  1,  3, -1, -3, -3, -1, -4,
  -1, -2, -2, -1, -3, -1, -1, -2, -2, -3, -3, -1, -2, -4,  7, -1, -1, -4, -3, -2, -2, -1, -2, -4,
   1, -1,  1,  0, -1,  0,  0,  0, -1, -2, -2,  0, -1, -2, -1,  4,  1, -3, -2, -2,  0,  0,  0, -4,
   0, -1,  0, -1, -1, -1, -1, -2, -2, -1, -1, -1, -1, -2, -1,  1,  5, -2, -2,  0, -1, -1,  0, -4,
  -3, -3, -4, -4, -2, -2, -3, -2, -2, -3, -2, -3, -1,  1, -4, -3, -2, 11,  2, -3, -4, -3, -2, -4,
  -2, -2, -2, -3, -2, -1, -2, -3,  2, -1, -1, -2, -1,  3, -3, -2, -2,  2,  7, -1, -3, -2, -1, -4,
   0, -3, -3, -3, -1, -2, -2, -3, -3,  3,  1, -2,  1, -1, -2, -2,  0, -3, -1,  4, -3, -2, -1, -4,
  -2, -1,  3,  4, -3,  0,  1, -1,  0, -3, -4,  0, -3, -3, -2,  0, -1, -4, -3, -3,  4,  1, -1, -4,
  -1,  0,  0,  1, -3,  3,  4, -2,  0, -3, -3,  1, -1, -3, -1,  0, -1, -3, -2, -2,  1,  4, -1, -4,
   0, -1, -1, -1, -2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -2,  0,  0, -2, -1, -1, -1, -1, -1, -4,
  -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4,  1];

  int[] A; // reconstructed aligned sequence A
  int[] B; // reconstructed aligned sequence B
  int[] M; // characters of sequence A
  int[] N; // characters of sequence B
  //int[] F; // matrix of computed scores
  NDArray!int F;
  int[] G; // copy of F for validation
  int[] S; // matrix of matches
  int[] I; // map of reads
  int[] O; // map of writes
  int[] Z; // map of shared memory accesses
  int rows;
  int cols;
  int penalty;
  string outfile;

  cl_kernel kernel_noblocks;
  cl_kernel kernel_noblocks_indirectS;
  cl_kernel kernel_rectangles;
  cl_kernel kernel_rectangles_indirectS;
  cl_kernel kernel_diamonds;
  cl_kernel kernel_diamonds_indirectS;


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
    //F = new int[rows * cols]; assert( null != F, "Can't allocate array F" );
    F = new NDArray!int( cols, rows );

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

      /**
       */
      __kernel void nw_noblocks_indirectS( __global const int* S, __global const int* M, __global const int* N, __global int* F, int cols, int penalty, int diagonal )
      {
        int tx = get_global_id( 0 );
        int c =            tx + 1;
        int r = diagonal - tx - 1;
        if ( diagonal >= cols )
        {
          c = diagonal - cols + tx + 1;
          r =            cols - tx - 1;
        }
        int m = F[I(r - 1, c - 1)] + S[M[r] * CHARS + N[c]];
        int d = F[I(r - 1, c    )] - penalty;
        int i = F[I(r    , c - 1)] - penalty;
        F[I(r, c)] = max3( m, d, i );
      } /* nw_noblocks_indirectS */

      /**
       */
      __kernel void nw_rectangles( __global const int* S, __global int* F, int cols, int penalty, int br, int bc, __local int* s, __local int* t )
      {
        int bx = get_group_id( 0 );
        int tx = get_local_id( 0 );
        int rr = ( br - bx ) * BLOCK_SIZE + 1; // these are the indexes of
        int cc = ( bc + bx ) * BLOCK_SIZE + 1; // the block's top-left element
        // 1.
        int index   = cols * rr + cc + tx;
        int index_n = index - cols;
        int index_w = cols * ( rr + tx ) + cc - 1;

        for ( int k = 0; k < BLOCK_SIZE; ++k )
          s[k * BLOCK_SIZE + tx] = S[k * cols + index];

        if ( tx == 0 ) t[0] = F[index_n - 1];        // copy the NW element into the TL corner of t
        t[(tx + 1) * (BLOCK_SIZE + 1)] = F[index_w]; // copy the column of W elements into the left column of t
        t[(tx + 1)                   ] = F[index_n]; // copy the row of N elements into the top row of t
        barrier( CLK_LOCAL_MEM_FENCE );
        // 2.
        for ( int k = 0; k < BLOCK_SIZE; ++k )
        {
          if ( tx <= k )
          {
            int x =     tx + 1;
            int y = k - tx + 1;
            int m = t[(y-1) * (BLOCK_SIZE + 1) + x-1] + s[(y - 1) * BLOCK_SIZE + x - 1];
            int d = t[(y-1) * (BLOCK_SIZE + 1) + x  ] - penalty;
            int i = t[(y  ) * (BLOCK_SIZE + 1) + x-1] - penalty;
            t[y * (BLOCK_SIZE + 1) + x] = max3( m, d, i );
          }
          barrier( CLK_LOCAL_MEM_FENCE );
        }
        // 3.
        for ( int k = BLOCK_SIZE - 2; k >= 0; --k )
        {
          if ( tx <= k )
          {
            int x = BLOCK_SIZE + tx - k;
            int y = BLOCK_SIZE - tx;
            int m = t[(y-1) * (BLOCK_SIZE + 1) + x-1] + s[(y - 1) * BLOCK_SIZE + x - 1];
            int d = t[(y-1) * (BLOCK_SIZE + 1) + x  ] - penalty;
            int i = t[(y  ) * (BLOCK_SIZE + 1) + x-1] - penalty;
            t[y * (BLOCK_SIZE + 1) + x] = max3( m, d, i );
          }
          barrier( CLK_LOCAL_MEM_FENCE );
        }
        // 4.
        for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
          F[index + ty * cols] = t[(ty + 1) * (BLOCK_SIZE + 1) + tx + 1];
      } /* nw_rectangles */

      /**
       */
      __kernel void nw_rectangles_indirectS( __global const int* S, __global const int* M, __global const int* N, __global int* F, int cols, int penalty, int br, int bc, __local int* s, __local int* t )
      {
        int bx = get_group_id( 0 );
        int tx = get_local_id( 0 );
        int rr = ( br - bx ) * BLOCK_SIZE + 1; // these are the indexes of
        int cc = ( bc + bx ) * BLOCK_SIZE + 1; // the block's top-left element
        // 1.
        int index   = cols * rr + cc + tx;
        int index_n = index - cols;
        int index_w = cols * ( rr + tx ) + cc - 1;

        // Copy BLOSUM array into shared memory.  Each thread copies
        // one or more columns.  The next columns are copied when the
        // number of threads in the group is smaller than the number
        // of columns in BLOSUM.
        int ii = 0;
        while ( ii + tx < CHARS )
        {
          for ( int ty = 0; ty < CHARS; ++ty )
            s[ty * CHARS + ii + tx] = S[ty * CHARS + ii + tx];
          ii += BLOCK_SIZE;
        }
        if ( tx == 0 ) t[0] = F[index_n - 1];        // copy the NW element into the TL corner of t
        t[(tx + 1) * (BLOCK_SIZE + 1)] = F[index_w]; // copy the column of W elements into the left column of t
        t[(tx + 1)                   ] = F[index_n]; // copy the row of N elements into the top row of t
        barrier( CLK_LOCAL_MEM_FENCE );
        // 2.
        for ( int k = 0; k < BLOCK_SIZE; ++k )
        {
          if ( tx <= k )
          {
            int x =     tx + 1;
            int y = k - tx + 1;
            int m = t[(y-1) * (BLOCK_SIZE + 1) + x-1] + s[M[rr + y - 1] * CHARS + N[cc + x - 1]];
            int d = t[(y-1) * (BLOCK_SIZE + 1) + x  ] - penalty;
            int i = t[(y  ) * (BLOCK_SIZE + 1) + x-1] - penalty;
            t[y * (BLOCK_SIZE + 1) + x] = max3( m, d, i );
          }
          barrier( CLK_LOCAL_MEM_FENCE );
        }
        // 3.
        for ( int k = BLOCK_SIZE - 2; k >= 0; --k )
        {
          if ( tx <= k )
          {
            int x = BLOCK_SIZE + tx - k;
            int y = BLOCK_SIZE - tx;
            int m = t[(y-1) * (BLOCK_SIZE + 1) + x-1] + s[M[rr + y - 1] * CHARS + N[cc + x - 1]];
            int d = t[(y-1) * (BLOCK_SIZE + 1) + x  ] - penalty;
            int i = t[(y  ) * (BLOCK_SIZE + 1) + x-1] - penalty;
            t[y * (BLOCK_SIZE + 1) + x] = max3( m, d, i );
          }
          barrier( CLK_LOCAL_MEM_FENCE );
        }
        // 4.
        for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
          F[index + ty * cols] = t[(ty + 1) * (BLOCK_SIZE + 1) + tx + 1];
      } /* nw_rectangles_indirectS */

      /**
       */
      __kernel void nw_diamonds( __global const int* S, __global int* F, int cols, int penalty, int br, int bc, __local int* s, __local int* t )
      {
        int bx = get_group_id( 0 );
        int tx = get_local_id( 0 );
        int rr = ( br -     bx ) * BLOCK_SIZE + 1;
        int cc = ( bc + 2 * bx ) * BLOCK_SIZE + 1;

        int index   = cols * ( rr + tx ) + cc - tx;
        int index_n = cols * ( rr - 1 ) + cc + tx;
        int index_w = index - 1;

        if ( bc == 1 && bx == 0 )
          for ( int m = 0; m < BLOCK_SIZE; ++m )
          {
            int x = m - tx + 1;
            if ( x > 0 )
              F[(rr + tx) * cols + x] = max3( F[(rr + tx - 1) * cols + x - 1] + S[(rr + tx) * cols + x],
                                              F[(rr + tx - 1) * cols + x    ] - penalty,
                                              F[(rr + tx    ) * cols + x - 1] - penalty );
            barrier( CLK_GLOBAL_MEM_FENCE );
          }

        // 1.
        for ( int k = 0; k < BLOCK_SIZE; ++k )
          s[tx * BLOCK_SIZE + k] = S[index + k];
        if ( tx == 0 ) t[0] = F[index_n - 1];
        t[ tx + 1                        ] = F[index_n];
        t[(tx + 1) * (BLOCK_SIZE + 2)    ] = F[index_w - 1];
        t[(tx + 1) * (BLOCK_SIZE + 2) + 1] = F[index_w];
        barrier( CLK_LOCAL_MEM_FENCE );
        // 2.
        for ( int k = 0; k < BLOCK_SIZE; ++k )
        {
          int x =  k + 2;
          int y =  tx + 1;
          int m = t[(y-1) * (BLOCK_SIZE + 2) + x-2] + s[tx * BLOCK_SIZE + k];
          int d = t[(y-1) * (BLOCK_SIZE + 2) + x-1] - penalty;
          int i = t[(y  ) * (BLOCK_SIZE + 2) + x-1] - penalty;
          t[y * (BLOCK_SIZE + 2) + x] = max3( m, d, i );
          barrier( CLK_LOCAL_MEM_FENCE );
        }
        // 3.
        for ( int k = 0; k < BLOCK_SIZE; ++k )
          F[index + k] = t[(tx + 1) * (BLOCK_SIZE + 2) + k + 2];
        barrier( CLK_GLOBAL_MEM_FENCE );

        if ( cc + BLOCK_SIZE == cols )
          for ( int m = 0; m < BLOCK_SIZE; ++m )
          {
            int x = cc + BLOCK_SIZE + m - tx;
            if ( x < cols )
              F[(rr + tx) * cols + x] = max3( F[(rr + tx - 1) * cols + x - 1] + S[(rr + tx) * cols + x],
                                              F[(rr + tx - 1) * cols + x    ] - penalty,
                                              F[(rr + tx    ) * cols + x - 1] - penalty );
            barrier( CLK_GLOBAL_MEM_FENCE );
          }
      } /* nw_diamonds */

      /**
       */
      __kernel void nw_diamonds_indirectS( __global const int* S      , //
                                           __global const int* M      , //
                                           __global const int* N      , //
                                           __global       int* F      , //
                                                          int  cols   , //
                                                          int  penalty, //
                                                          int  br     , //
                                                          int  bc     , //
                                           __local        int* s      , //
                                           __local        int* t      ) //
      {
        int bx = get_group_id( 0 );
        int tx = get_local_id( 0 );
        int rr = ( br -     bx ) * BLOCK_SIZE + 1;
        int cc = ( bc + 2 * bx ) * BLOCK_SIZE + 1;

        int index   = cols * ( rr + tx ) + cc - tx;
        int index_n = cols * ( rr - 1 ) + cc + tx;
        int index_w = index - 1;
        int ii = 0;

        while ( ii + tx < CHARS )
        {
          for ( int ty = 0; ty < CHARS; ++ty )
            s[ty * CHARS + ii + tx] = S[ty * CHARS + ii + tx];
          ii += BLOCK_SIZE;
        }
        barrier( CLK_LOCAL_MEM_FENCE );

        if ( bc == 1 && bx == 0 )
          for ( int m = 0; m < BLOCK_SIZE; ++m )
          {
            int x = m - tx + 1;
            if ( x > 0 )
              F[(rr + tx) * cols + x] = max3( F[(rr + tx - 1) * cols + x - 1] + s[M[rr + tx] * CHARS + N[x]],
                                              F[(rr + tx - 1) * cols + x    ] - penalty,
                                              F[(rr + tx    ) * cols + x - 1] - penalty );
            barrier( CLK_GLOBAL_MEM_FENCE );
          }

        if ( tx == 0 ) t[0] = F[index_n - 1];
        t[ tx + 1                        ] = F[index_n];
        t[(tx + 1) * (BLOCK_SIZE + 2)    ] = F[index_w - 1];
        t[(tx + 1) * (BLOCK_SIZE + 2) + 1] = F[index_w];
        barrier( CLK_LOCAL_MEM_FENCE );

        int nextN = N[cc - tx];
        for ( int k = 0; k < BLOCK_SIZE; ++k )
        {
          int x =  k + 2;
          int y =  tx + 1;
          int currN = nextN;
          nextN = N[cc - tx + k + 1];
          int m = t[(y-1) * (BLOCK_SIZE + 2) + x-2] + s[M[rr + tx] * CHARS + currN];
          int d = t[(y-1) * (BLOCK_SIZE + 2) + x-1] - penalty;
          int i = t[(y  ) * (BLOCK_SIZE + 2) + x-1] - penalty;
          t[y * (BLOCK_SIZE + 2) + x] = max3( m, d, i );
          barrier( CLK_LOCAL_MEM_FENCE );
        }

        for ( int k = 0; k < BLOCK_SIZE; ++k )
          F[index + k] = t[(tx + 1) * (BLOCK_SIZE + 2) + k + 2];
        barrier( CLK_GLOBAL_MEM_FENCE );

        if ( cc + BLOCK_SIZE == cols )
          for ( int m = 0; m < BLOCK_SIZE; ++m )
          {
            int x = cc + BLOCK_SIZE + m - tx;
            if ( x < cols )
              F[(rr + tx) * cols + x] = max3( F[(rr + tx - 1) * cols + x - 1] + s[M[rr + tx] * CHARS + N[x]],
                                              F[(rr + tx - 1) * cols + x    ] - penalty,
                                              F[(rr + tx    ) * cols + x - 1] - penalty );
            barrier( CLK_GLOBAL_MEM_FENCE );
          }
      } /* nw_diamonds_indirectS */
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( runtime.context, 1, strs.ptr, &size, &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clBuildProgram( program, 1, &runtime.device, "", null, null );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_noblocks             = clCreateKernel( program, "nw_noblocks"             , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_rectangles           = clCreateKernel( program, "nw_rectangles"           , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_diamonds             = clCreateKernel( program, "nw_diamonds"             , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_noblocks_indirectS   = clCreateKernel( program, "nw_noblocks_indirectS"   , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_rectangles_indirectS = clCreateKernel( program, "nw_rectangles_indirectS" , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_diamonds_indirectS   = clCreateKernel( program, "nw_diamonds_indirectS"   , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clReleaseProgram( program );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );

    static if ( false )
    { // an example of getting the correct maximum work group
      // size. device info on OS X reports an incorrect value.
      size_t max_group_size;
      clGetKernelWorkGroupInfo( kernel_rectangles, runtime.device, CL_KERNEL_WORK_GROUP_SIZE, max_group_size.sizeof, &max_group_size, null );
    }
  } // this()

  /**
   */
  void save()
  {
    if ( null != outfile )
    {
      auto fp = File( outfile, "w" );
      for ( int ii = 1; ii < rows - 1; ++ii ) fp.writef( "%c", to!(char)(M[ii] + 'A') ); fp.writeln();
      for ( int ii = 1; ii < cols - 1; ++ii ) fp.writef( "%c", to!(char)(N[ii] + 'A') ); fp.writeln( "\n" );
      for ( int ii = 1; ii < rows - 1; ++ii ) fp.writef( "%c", to!(char)(A[ii] + 'A') ); fp.writeln();
      for ( int ii = 1; ii < cols - 1; ++ii ) fp.writef( "%c", to!(char)(B[ii] + 'A') ); fp.writeln( "\n" );
      fp.close();
    }
  }

  /**
   */
  void validate()
  {
    int diff = 0;
    foreach ( ii; 0 .. F.length )
      if ( F[ii] != G[ii] )
        ++diff;
    if ( diff > 0 )
      writeln( "DIFFs ", diff );
  }

  /**
   */
  void reset()
  {
    F[] = 0;
    foreach ( c; 0 .. cols ) F[c       ] = -penalty * c;
    foreach ( r; 1 .. rows ) F[r * cols] = -penalty * r;
  }

  /**
   */
  void print( const string msg, const int[] A ) const
  {
    writeln( msg );
    foreach ( r; 0 .. rows )
    {
      writef( "%4d", A[r * cols] );
      foreach ( c; 1 .. cols )
        writef( " %4d", A[r * cols + c] );
      writeln();
    }
  }

  /**
   */
  void visualize()
  {
    // dumps.
    foreach ( int r; 0 .. rows )
    {
      foreach ( int c; 0 .. cols )
        if ( I[r * cols + c] == 0 ) write( " ." );
        else                        write( " ", I[r * cols + c] );
      writeln();
    }
    foreach ( int j; 0 .. cols ) write( "~~" ); writeln();
    foreach ( r; 0 .. BLOCK_SIZE + 1 )
    {
      foreach ( c; 0 .. BLOCK_SIZE + 2 )
        if ( Z[r * (BLOCK_SIZE + 2) + c] == 0 ) write( " ." );
        else                                    write( " ", Z[r * (BLOCK_SIZE + 2) + c] );
      writeln();
    }
    foreach ( int j; 0 .. cols ) write( "~~" ); writeln();
    foreach ( int r; 0 .. rows )
    {
      foreach ( int c; 0 .. cols )
        if ( O[r * cols + c] == 0 ) write( " ." );
        else                        write( " ", O[r * cols + c] );
      writeln();
    }
    foreach ( int j; 0 .. cols ) write( "XX" ); writeln( "\n" );
  }

  /**
   * Maximum of three numbers.
   **/
  int max3( immutable int a, immutable int b, immutable int c ) const
  {
    auto k = a > b ? a : b;
    return k > c ? k : c;
  }

  /**
   * baseline_sequential:
   * implements sequential computation of the alignment scores matrix.
   *
   * The algorithm of filling in the matrix F
   * <p>
   * <code>
   *    for i=0 to length(A) F(i,0) ← d*i
   *    for j=0 to length(B) F(0,j) ← d*j
   *    for i=1 to length(A)
   *    for j=1 to length(B)
   *    {
   *      Match  ← F( i-1, j-1) + S(Ai, Bj)
   *      Delete ← F( i-1, j  ) + d
   *      Insert ← F( i  , j-1) + d
   *      F(i,j) ← max(Match, Insert, Delete)
   *    }
   * </code>
   */
  void baseline_sequential()
  {
    foreach ( r; 1 .. rows )
      foreach ( c; 1 .. cols )
        F[r * cols + c] = max3( F[(r - 1) * cols + c - 1] + BLOSUM62[M[r] * CHARS + N[c]],
                                F[(r - 1) * cols + c    ] - penalty,
                                F[ r      * cols + c - 1] - penalty );
  }

  /**
   * rectangular_block:
   * implements sequential computation of the alignment scores matrix
   * for a single rectangular block of fixed size #BLOCK_SIZE.
   * @br: block row, the vertical location of the block from the top of the
   *      matrix, starting with 0 block.
   * @bc: block column, the horizontal location of the block from the
   *      left side of the matrix, starting with 0 block.
   */
  void rectangular_block( int br, int bc )
  {
    while ( br >= 0 && bc < (cols - 1) / BLOCK_SIZE )
    {
      auto rr = br * BLOCK_SIZE + 1;
      auto cc = bc * BLOCK_SIZE + 1;
      foreach ( r; rr .. rr + BLOCK_SIZE )
        foreach ( c; cc .. cc + BLOCK_SIZE )
          F[r * cols + c] = max3( F[(r - 1) * cols + c - 1] + BLOSUM62[M[r] * CHARS + N[c]],
                                  F[(r - 1) * cols + c    ] - penalty,
                                  F[ r      * cols + c - 1] - penalty );
      br--;
      bc++;
    }
  }

  /**
   */
  void rectangles()
  {
    auto max_blocks = (cols - 1) / BLOCK_SIZE;
    for ( int i = 0; i < 2 * max_blocks - 1; ++i )
    {
      int br = ( i < max_blocks ) ? i :     max_blocks - 1;
      int bc = ( i < max_blocks ) ? 0 : i - max_blocks + 1;
      rectangular_block( br, bc );
    }
  }

  /**
   */
  void diamond_blocks( int br, int bc  )
  {
    while ( br >= 0 && bc < (cols - 1) / BLOCK_SIZE )
    {
      int rr = br * BLOCK_SIZE + 1;
      int cc = bc * BLOCK_SIZE + 1;
      if ( bc == 1 )
        foreach ( r; 0 .. BLOCK_SIZE )
          foreach ( c; 1 .. BLOCK_SIZE + 1 - r )
            F[(rr + r) * cols + c] = max3( F[(rr + r - 1) * cols + c - 1] + BLOSUM62[M[rr + r] * CHARS + N[c]],
                                           F[(rr + r - 1) * cols + c    ] - penalty,
                                           F[(rr + r    ) * cols + c - 1] - penalty );
      int k = cc;
      foreach ( r; 0 .. BLOCK_SIZE )
      {
        foreach ( c; 0 .. BLOCK_SIZE )
        {
          F[(rr + r) * cols + k + c] = max3( F[(rr + r - 1) * cols + k + c - 1] + BLOSUM62[M[rr + r] * CHARS + N[k + c]],
                                             F[(rr + r - 1) * cols + k + c    ] - penalty,
                                             F[(rr + r    ) * cols + k + c - 1] - penalty );
        }
        k--;
      }
      if ( cc + BLOCK_SIZE == cols )
      {
        cc += BLOCK_SIZE - 2;
        foreach ( r; 1 .. BLOCK_SIZE )
        {
          foreach ( c; 1 .. r + 1 )
            F[(rr + r) * cols + cc + c] = max3( F[(rr + r - 1) * cols + cc + c - 1] + BLOSUM62[M[rr + r] * CHARS + N[cc + c]],
                                                F[(rr + r - 1) * cols + cc + c    ] - penalty,
                                                F[(rr + r    ) * cols + cc + c - 1] - penalty );
          cc--;
        }
      }
      br--;
      bc += 2;
    }
  }

  /**
   */
  void diamonds()
  {
    for ( int i = 0; i < rows / BLOCK_SIZE - 1; ++i )
    {
      diamond_blocks( i, 1 );
      debug ( DEBUG ) { foreach ( int j; 0 .. 2 * cols ) write( "--" ); writeln(); }
      diamond_blocks( i, 2 );
      debug ( DEBUG ) { foreach ( int j; 0 .. 2 * cols ) write( "==" ); writeln(); }
    }
    for ( int i = 1; i < cols / BLOCK_SIZE; ++i )
    {
      diamond_blocks( rows / BLOCK_SIZE - 1, i );
      debug ( DEBUG ) { foreach ( int j; 0 .. 2 * cols ) write( "==" ); writeln(); }
    }
  }

  /**
   * <code>
   *    AlignmentA ← ""
   *    AlignmentB ← ""
   *    i ← length(A)
   *    j ← length(B)
   *    while (i > 0 or j > 0)
   *    {
   *      if (i > 0 and j > 0 and F(i,j) == F(i-1,j-1) + S(Ai, Bj))
   *      {
   *        AlignmentA ← Ai + AlignmentA
   *        AlignmentB ← Bj + AlignmentB
   *        i ← i - 1
   *        j ← j - 1
   *      }
   *      else if (i > 0 and F(i,j) == F(i-1,j) + d)
   *      {
   *        AlignmentA ← Ai + AlignmentA
   *        AlignmentB ← "-" + AlignmentB
   *        i ← i - 1
   *      }
   *      else (j > 0 and F(i,j) == F(i,j-1) + d)
   *      {
   *        AlignmentA ← "-" + AlignmentA
   *        AlignmentB ← Bj + AlignmentB
   *        j ← j - 1
   *      }
   *    }
   * </code>
   */
  void compute_alignments()
  {
    int i = rows - 2;
    int j = cols - 2;
    int m = i;
    int n = j;
    for ( ; i != 0 && j != 0 && m > -1 && n > -1; --m, --n )
    {
      if ( i > 0 && j > 0 && F[i * cols + j] == F[(i - 1) * cols + j - 1] + S[i * cols + j] )
      {
        A[m] = M[i];
        B[n] = N[j];
        --i;
        --j;
      }
      else if ( i > 0 && F[i * cols + j] == F[(i - 1) * cols + j] - penalty )
      {
        A[m] = M[i];
        B[n] = 30;
        --i;
      }
      else if ( j > 0 && F[i * cols + j] == F[i * cols + j - 1] - penalty )
      {
        A[m] = 30;
        B[n] = N[j];
        --j;
      }
      else
      {
        writefln( "i %d, j %d\n", i, j );
      }
    }
  }

  /**
   */
  double opencl_noblocks()
  {
    double result = 0.0;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer( runtime.context, flags, cl_int.sizeof * S.length, S.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer( runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks, 0, cl_mem.sizeof, &dS      );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks, 1, cl_mem.sizeof, &dF      );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks, 2, cl_int.sizeof, &cols    );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks, 3, cl_int.sizeof, &penalty );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      foreach ( i; 2 .. 2 * cols - 1 )
      {
        size_t global = (i < cols) ? i - 1 : 2 * cols - i - 1;
        status = clSetKernelArg( kernel_noblocks, 4, cl_int.sizeof, &i );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_noblocks, 1, null, &global, null, 0, null, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        cl_ulong start_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        cl_ulong end_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
      }
      status = clEnqueueReadBuffer( runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dS );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dF );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_noblocks );
      assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
    }
    catch (Exception e)
    {
      write( e );
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_noblocks_indirectS()
  {
    double result = 0.0;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer( runtime.context, flags, cl_int.sizeof * BLOSUM62.length, BLOSUM62.ptr, &status );              assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      cl_mem dM = clCreateBuffer( runtime.context, flags, cl_int.sizeof * M.length, M.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      cl_mem dN = clCreateBuffer( runtime.context, flags, cl_int.sizeof * N.length, N.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer( runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks_indirectS, 0, cl_mem.sizeof, &dS      );                                          assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks_indirectS, 1, cl_mem.sizeof, &dM      );                                          assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks_indirectS, 2, cl_mem.sizeof, &dN      );                                          assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks_indirectS, 3, cl_mem.sizeof, &dF      );                                          assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks_indirectS, 4, cl_int.sizeof, &cols    );                                          assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_noblocks_indirectS, 5, cl_int.sizeof, &penalty );                                          assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      foreach ( i; 2 .. 2 * cols - 1 )
      {
        size_t global = (i < cols) ? i - 1 : 2 * cols - i - 1;
        status = clSetKernelArg( kernel_noblocks_indirectS, 6, cl_int.sizeof, &i );                                              assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_noblocks_indirectS, 1, null, &global, null, 0, null, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        cl_ulong start_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        cl_ulong end_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
      }
      status = clEnqueueReadBuffer( runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );             assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dF );                                                                                         assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dN );                                                                                         assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dM );                                                                                         assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dS );                                                                                         assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_noblocks_indirectS );                                                                     assert( status == CL_SUCCESS, "opencl_noblocks_indirectS " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_rectangles()
  {
    double result = 0.0;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer( runtime.context, flags, cl_int.sizeof * S.length, S.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer( runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles, 0, cl_mem.sizeof, &dS      );                                                  assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles, 1, cl_mem.sizeof, &dF      );                                                  assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles, 2, cl_int.sizeof, &cols    );                                                  assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles, 3, cl_int.sizeof, &penalty );                                                  assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles, 6,  BLOCK_SIZE      *  BLOCK_SIZE      * cl_int.sizeof, null );                assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles, 7, (BLOCK_SIZE + 1) * (BLOCK_SIZE + 1) * cl_int.sizeof, null );                assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      auto max_blocks = (cols - 1) / BLOCK_SIZE;
      auto cur_blocks = 1;
      auto inc_blocks = 1;
      for ( int i = 0; i < 2 * max_blocks - 1; ++i )
      {
        size_t wgroup = BLOCK_SIZE;
        size_t global = BLOCK_SIZE * cur_blocks;
        cl_int br = ( i < max_blocks ) ? i :     max_blocks - 1;
        cl_int bc = ( i < max_blocks ) ? 0 : i - max_blocks + 1;
        status = clSetKernelArg( kernel_rectangles, 4, cl_int.sizeof, &br );                                                     assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
        status = clSetKernelArg( kernel_rectangles, 5, cl_int.sizeof, &bc );                                                     assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_rectangles, 1, null, &global, &wgroup, 0, null, &event );
        assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        cl_ulong start_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        cl_ulong end_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
        if ( i == max_blocks - 1 ) inc_blocks = -1;
        cur_blocks += inc_blocks;
      }
      status = clEnqueueReadBuffer( runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );             assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dS );                                                                                         assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dF );                                                                                         assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_rectangles );                                                                             assert( status == CL_SUCCESS, "opencl_rectangles " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_rectangles_indirectS()
  {
    double result = 0.0;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer( runtime.context, flags, cl_int.sizeof * BLOSUM62.length, BLOSUM62.ptr, &status );              assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      cl_mem dM = clCreateBuffer( runtime.context, flags, cl_int.sizeof * M.length, M.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      cl_mem dN = clCreateBuffer( runtime.context, flags, cl_int.sizeof * N.length, N.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer( runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles_indirectS, 0, cl_mem.sizeof, &dS      );                                        assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles_indirectS, 1, cl_mem.sizeof, &dM      );                                        assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles_indirectS, 2, cl_mem.sizeof, &dN      );                                        assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles_indirectS, 3, cl_mem.sizeof, &dF      );                                        assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles_indirectS, 4, cl_int.sizeof, &cols    );                                        assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles_indirectS, 5, cl_int.sizeof, &penalty );                                        assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles_indirectS, 8, CHARS * CHARS * cl_int.sizeof, null );                            assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_rectangles_indirectS, 9, (BLOCK_SIZE + 1) * (BLOCK_SIZE + 1) * cl_int.sizeof, null );      assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      auto max_blocks = (cols - 1) / BLOCK_SIZE;
      auto cur_blocks = 1;
      auto inc_blocks = 1;
      for ( int i = 0; i < 2 * max_blocks - 1; ++i )
      {
        size_t wgroup = BLOCK_SIZE;
        size_t global = BLOCK_SIZE * cur_blocks;
        cl_int br = ( i < max_blocks ) ? i :     max_blocks - 1;
        cl_int bc = ( i < max_blocks ) ? 0 : i - max_blocks + 1;
        status = clSetKernelArg( kernel_rectangles_indirectS, 6, cl_int.sizeof, &br );                                           assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
        status = clSetKernelArg( kernel_rectangles_indirectS, 7, cl_int.sizeof, &bc );                                           assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue,
                                         kernel_rectangles_indirectS,
                                         1,
                                         null,
                                         &global,
                                         &wgroup,
                                         0,
                                         null,
                                         &event );
        assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        cl_ulong start_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        cl_ulong end_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
        if ( i == max_blocks - 1 ) inc_blocks = -1;
        cur_blocks += inc_blocks;
      }
      status = clEnqueueReadBuffer( runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );             assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dF );                                                                                         assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dN );                                                                                         assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dM );                                                                                         assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dS );                                                                                         assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_rectangles_indirectS );                                                                   assert( status == CL_SUCCESS, "opencl_rectangles_indirectS " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_diamonds()
  {
    double result = 0.0;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer( runtime.context, flags, cl_int.sizeof * S.length, S.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer( runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status );                            assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds, 0, cl_mem.sizeof, &dS      );                                                    assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds, 1, cl_mem.sizeof, &dF      );                                                    assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds, 2, cl_int.sizeof, &cols    );                                                    assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds, 3, cl_int.sizeof, &penalty );                                                    assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds, 6,  BLOCK_SIZE      *  BLOCK_SIZE      * cl_int.sizeof, null );                  assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds, 7, (BLOCK_SIZE + 1) * (BLOCK_SIZE + 2) * cl_int.sizeof, null );                  assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      size_t wgroup = BLOCK_SIZE;
      auto groups = ( cols - 1 ) / BLOCK_SIZE;
      for ( int i = 0; i < rows / BLOCK_SIZE; ++i )
      {
        cl_int br = i;
        cl_int bc = 1;
        size_t global = ( ( 2 * i + bc ) * BLOCK_SIZE < cols - 1 ) ? BLOCK_SIZE * ( i + 1 ) : ( cols - 1 ) / 2;
        status = clSetKernelArg( kernel_diamonds, 4, cl_int.sizeof, &br );                                                       assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
        status = clSetKernelArg( kernel_diamonds, 5, cl_int.sizeof, &bc );                                                       assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_diamonds, 1, null, &global, &wgroup, 0, null, &event );
        assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        cl_ulong start_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        cl_ulong end_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
        bc = 2;
        global = ( ( 2 * i + bc ) * BLOCK_SIZE < cols - 1 ) ? BLOCK_SIZE * ( i + 1 ) : ( cols - 1 ) / 2 - BLOCK_SIZE;
        if ( global == 0 ) continue;
        status = clSetKernelArg( kernel_diamonds, 5, cl_int.sizeof, &bc );                                                       assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_diamonds, 1, null, &global, &wgroup, 0, null, &event );
        assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
      }
      for ( int i = 1; i < cols / BLOCK_SIZE; ++i )
      {
        cl_int br = rows / BLOCK_SIZE - 1;
        cl_int bc = i;
        size_t global = BLOCK_SIZE * ( ( groups - bc + 1 ) / 2 );
        status = clSetKernelArg( kernel_diamonds, 4, cl_int.sizeof, &br );                                                       assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
        status = clSetKernelArg( kernel_diamonds, 5, cl_int.sizeof, &bc );                                                       assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_diamonds, 1, null, &global, &wgroup, 0, null, &event );
        assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        cl_ulong start_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        cl_ulong end_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
      }
      status = clEnqueueReadBuffer( runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );             assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dS );                                                                                         assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dF );                                                                                         assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_diamonds );                                                                               assert( status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_diamonds_indirectS()
  {
    double result = 0.0;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer( runtime.context, flags, cl_int.sizeof * BLOSUM62.length, BLOSUM62.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      cl_mem dM = clCreateBuffer( runtime.context, flags, cl_int.sizeof * M.length, M.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      cl_mem dN = clCreateBuffer( runtime.context, flags, cl_int.sizeof * N.length, N.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer( runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds_indirectS, 0, cl_mem.sizeof, &dS      );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds_indirectS, 1, cl_mem.sizeof, &dM      );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds_indirectS, 2, cl_mem.sizeof, &dN      );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds_indirectS, 3, cl_mem.sizeof, &dF      );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds_indirectS, 4, cl_int.sizeof, &cols    );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds_indirectS, 5, cl_int.sizeof, &penalty );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds_indirectS, 8, CHARS * CHARS * cl_int.sizeof, null );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_diamonds_indirectS, 9, (BLOCK_SIZE + 1) * (BLOCK_SIZE + 2) * cl_int.sizeof, null );
      assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      size_t wgroup = BLOCK_SIZE;
      auto groups = ( cols - 1 ) / BLOCK_SIZE;
      for ( int i = 0; i < rows / BLOCK_SIZE - 1; ++i )
      {
        cl_int br = i;
        cl_int bc = 1;
        size_t global = ( ( 2 * i + bc ) * BLOCK_SIZE < cols - 1 ) ? BLOCK_SIZE * ( i + 1 ) : ( cols - 1 ) / 2;
        status = clSetKernelArg( kernel_diamonds_indirectS, 6, cl_int.sizeof, &br );
        assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
        status = clSetKernelArg( kernel_diamonds_indirectS, 7, cl_int.sizeof, &bc );
        assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_diamonds_indirectS, 1, null, &global, &wgroup, 0, null, &event );
        assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        cl_ulong start_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        cl_ulong end_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
        bc = 2;
        global = ( ( 2 * i + bc ) * BLOCK_SIZE < cols - 1 ) ? BLOCK_SIZE * ( i + 1 ) : ( cols - 1 ) / 2 - BLOCK_SIZE;
        if ( global == 0 ) continue;
        status = clSetKernelArg( kernel_diamonds_indirectS, 7, cl_int.sizeof, &bc );
        assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_diamonds_indirectS, 1, null, &global, &wgroup, 0, null, &event );
        assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
      }
      for ( int i = 1; i < cols / BLOCK_SIZE; ++i )
      {
        cl_int br = rows / BLOCK_SIZE - 1;
        cl_int bc = i;
        size_t global = BLOCK_SIZE * ( ( groups - bc + 1 ) / 2 );
        status = clSetKernelArg( kernel_diamonds_indirectS, 6, cl_int.sizeof, &br );                                             assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
        status = clSetKernelArg( kernel_diamonds_indirectS, 7, cl_int.sizeof, &bc );                                             assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_diamonds_indirectS, 1, null, &global, &wgroup, 0, null, &event );
        assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
        status = clWaitForEvents( 1, &event );
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );

        cl_ulong start_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &start_time               , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        cl_ulong end_time;
        status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                           CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                           cl_ulong.sizeof           , // size_t            param_value_size
                                           &end_time                 , // void*             param_value
                                           null                     ); // size_t*           param_value_size_ret
        assert( status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror( status ) );
        result += ( end_time - start_time ) / 1E9;
      }
      status = clEnqueueReadBuffer( runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );             assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dF );                                                                                         assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dN );                                                                                         assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dM );                                                                                         assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dS );                                                                                         assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_diamonds_indirectS );                                                                     assert( status == CL_SUCCESS, "opencl_diamonds_indirectS " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
    return result;
  }

  /**
   */
  void clop_dsl()
  {
    // use CLOP DSL to generate OpenCL kernel and API calls.
    mixin( compile(
    q{
      int max3( int a, int b, int c )
      {
        int k = a > b ? a : b;
        return k > c ? k : c;
      }
      Antidiagonal NDRange( c : 1 .. cols, r : 1 .. rows ) {
        F[c, r] = max3( F[c - 1, r - 1] + S[c, r], F[c - 1, r] - penalty, F[c, r - 1] - penalty );
      } apply( rectangular_blocking( 8, 8 ) )
    } ) );
  }

  /**
   */
  void clop_dsl_indirectS()
  {
    mixin( compile(
    q{
      int max3( int a, int b, int c )
      {
        int k = a > b ? a : b;
        return k > c ? k : c;
      }
      Antidiagonal NDRange( c : 1 .. cols, r : 1 .. rows ) {
        F[c, r] = max3( F[c - 1, r - 1] + BLOSUM62[M[r] * CHARS + N[c]], F[c - 1, r] - penalty, F[c, r - 1] - penalty );
      } apply( rectangular_blocking( 8, 8 ), prefetching() )
    } ) );
  }

  /**
   */
  void run()
  {
    size_t size;
    StopWatch timer;
    TickDuration ticks;
    double benchmark = runtime.benchmark( (rows - 1) * (cols - 1) );
    double time;

    timer.start();
    baseline_sequential();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2.0f MI SEQUENTIAL %5.3f [s]",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6 );
    G[] = F[];

    static if ( false )
    {
    reset();
    timer.reset();
    timer.start();
    rectangles();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2.0f MI RECTANGLES %5.3f [s]",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6 );
    validate();

    reset();
    timer.reset();
    timer.start();
    diamonds();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2.0f MI DIAMONDS   %5.3f [s]",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6 );
    validate();

    reset();
    timer.reset();
    timer.start();
    time = opencl_noblocks();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2.0f MI CL NOBLOCK %5.3f (%5.3f) [s], %7.2f MI/s, estimated %7.2f MI/s",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6, time,
              (rows - 1) * (cols - 1) / (1024 * 1024 * time),
              2 * benchmark / 5 );
    validate();

    reset();
    timer.reset();
    timer.start();
    time = opencl_noblocks_indirectS();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2.0f MI CL NB INDI %5.3f (%5.3f) [s], %7.2f MI/s",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6, time,
              (rows - 1) * (cols - 1) / (1024 * 1024 * time) );
    validate();
    }

    clGetKernelWorkGroupInfo( kernel_rectangles,
                              runtime.device,
                              CL_KERNEL_WORK_GROUP_SIZE,
                              size.sizeof,
                              &size,
                              null );
    if ( size >= BLOCK_SIZE )
    {
      reset();
      timer.reset();
      timer.start();
      time = opencl_rectangles();
      timer.stop();
      ticks = timer.peek();
      writefln( "%2.0f MI CL BLOCKS  %5.3f (%5.3f) [s], %7.2f MI/s",
                (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
                ticks.usecs / 1E6, time,
                (rows - 1) * (cols - 1) / (1024 * 1024 * time) );
      validate();

      reset();
      timer.reset();
      timer.start();
      time = opencl_rectangles_indirectS();
      timer.stop();
      ticks = timer.peek();
      writefln( "%2.0f MI CL BL INDI %5.3f (%5.3f) [s], %7.2f MI/s",
                (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
                ticks.usecs / 1E6, time,
                (rows - 1) * (cols - 1) / (1024 * 1024 * time) );
      validate();

      reset();
      timer.reset();
      timer.start();
      time = opencl_diamonds();
      timer.stop();
      ticks = timer.peek();
      writefln( "%2.0f MI CL DIAMOND %5.3f (%5.3f) [s], %7.2f MI/s",
                (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
                ticks.usecs / 1E6, time,
                (rows - 1) * (cols - 1) / (1024 * 1024 * time) );
      validate();

      reset();
      timer.reset();
      timer.start();
      time = opencl_diamonds_indirectS();
      timer.stop();
      ticks = timer.peek();
      writefln( "%2.0f MI CL D INDIR %5.3f (%5.3f) [s], %7.2f MI/s",
                (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
                ticks.usecs / 1E6, time,
                (rows - 1) * (cols - 1) / (1024 * 1024 * time) );
      validate();

      reset();
      timer.reset();
      timer.start();
      clop_dsl();
      timer.stop();
      ticks = timer.peek();
      writefln( "%2.0f MI CLOP DSL   %5.3f [s]",
                (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
                ticks.usecs / 1E6 );
      validate();

      reset();
      timer.reset();
      timer.start();
      clop_dsl_indirectS();
      timer.stop();
      ticks = timer.peek();
      writefln( "%2.0f MI CLOP DSL I %5.3f [s]",
                (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
                ticks.usecs / 1E6 );
      validate();
    }
    save();
  }
}

int
main(string[] args)
{
  auto platforms = runtime.get_platforms();
  foreach (p; 0 .. platforms.length)
    foreach (d; 0 .. platforms[p])
    {
      try
      {
        runtime.init(p, d);
        writeln("==================================================");
        auto app = new Application(args);
        app.run();
        writeln("==================================================");
      }
      catch (Exception msg)
      {
        writeln("BP: ", msg);
        return -1;
      }
      finally
      {
        runtime.shutdown();
      }
    }
  return 0;
}
