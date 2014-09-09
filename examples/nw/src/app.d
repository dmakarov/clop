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
  static immutable int[CHARS][CHARS] BLOSUM62 = [
  [ 4, -1, -2, -2,  0, -1, -1,  0, -2, -1, -1, -1, -1, -2, -1,  1,  0, -3, -2,  0, -2, -1,  0, -4],
  [-1,  5,  0, -2, -3,  1,  0, -2,  0, -3, -2,  2, -1, -3, -2, -1, -1, -3, -2, -3, -1,  0, -1, -4],
  [-2,  0,  6,  1, -3,  0,  0,  0,  1, -3, -3,  0, -2, -3, -2,  1,  0, -4, -2, -3,  3,  0, -1, -4],
  [-2, -2,  1,  6, -3,  0,  2, -1, -1, -3, -4, -1, -3, -3, -1,  0, -1, -4, -3, -3,  4,  1, -1, -4],
  [ 0, -3, -3, -3,  9, -3, -4, -3, -3, -1, -1, -3, -1, -2, -3, -1, -1, -2, -2, -1, -3, -3, -2, -4],
  [-1,  1,  0,  0, -3,  5,  2, -2,  0, -3, -2,  1,  0, -3, -1,  0, -1, -2, -1, -2,  0,  3, -1, -4],
  [-1,  0,  0,  2, -4,  2,  5, -2,  0, -3, -3,  1, -2, -3, -1,  0, -1, -3, -2, -2,  1,  4, -1, -4],
  [ 0, -2,  0, -1, -3, -2, -2,  6, -2, -4, -4, -2, -3, -3, -2,  0, -2, -2, -3, -3, -1, -2, -1, -4],
  [-2,  0,  1, -1, -3,  0,  0, -2,  8, -3, -3, -1, -2, -1, -2, -1, -2, -2,  2, -3,  0,  0, -1, -4],
  [-1, -3, -3, -3, -1, -3, -3, -4, -3,  4,  2, -3,  1,  0, -3, -2, -1, -3, -1,  3, -3, -3, -1, -4],
  [-1, -2, -3, -4, -1, -2, -3, -4, -3,  2,  4, -2,  2,  0, -3, -2, -1, -2, -1,  1, -4, -3, -1, -4],
  [-1,  2,  0, -1, -3,  1,  1, -2, -1, -3, -2,  5, -1, -3, -1,  0, -1, -3, -2, -2,  0,  1, -1, -4],
  [-1, -1, -2, -3, -1,  0, -2, -3, -2,  1,  2, -1,  5,  0, -2, -1, -1, -1, -1,  1, -3, -1, -1, -4],
  [-2, -3, -3, -3, -2, -3, -3, -3, -1,  0,  0, -3,  0,  6, -4, -2, -2,  1,  3, -1, -3, -3, -1, -4],
  [-1, -2, -2, -1, -3, -1, -1, -2, -2, -3, -3, -1, -2, -4,  7, -1, -1, -4, -3, -2, -2, -1, -2, -4],
  [ 1, -1,  1,  0, -1,  0,  0,  0, -1, -2, -2,  0, -1, -2, -1,  4,  1, -3, -2, -2,  0,  0,  0, -4],
  [ 0, -1,  0, -1, -1, -1, -1, -2, -2, -1, -1, -1, -1, -2, -1,  1,  5, -2, -2,  0, -1, -1,  0, -4],
  [-3, -3, -4, -4, -2, -2, -3, -2, -2, -3, -2, -3, -1,  1, -4, -3, -2, 11,  2, -3, -4, -3, -2, -4],
  [-2, -2, -2, -3, -2, -1, -2, -3,  2, -1, -1, -2, -1,  3, -3, -2, -2,  2,  7, -1, -3, -2, -1, -4],
  [ 0, -3, -3, -3, -1, -2, -2, -3, -3,  3,  1, -2,  1, -1, -2, -2,  0, -3, -1,  4, -3, -2, -1, -4],
  [-2, -1,  3,  4, -3,  0,  1, -1,  0, -3, -4,  0, -3, -3, -2,  0, -1, -4, -3, -3,  4,  1, -1, -4],
  [-1,  0,  0,  1, -3,  3,  4, -2,  0, -3, -3,  1, -1, -3, -1,  0, -1, -3, -2, -2,  1,  4, -1, -4],
  [ 0, -1, -1, -1, -2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -2,  0,  0, -2, -1, -1, -1, -1, -1, -4],
  [-4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4,  1]];
  static immutable int BLOCK_SIZE = 16;

  int Z[BLOCK_SIZE + 1][BLOCK_SIZE + 2];

  int[] A;
  int[] B;
  int[] M;
  int[] N;
  int[] F;
  int[] G;
  int[] S;
  int[] I;
  int[] O;
  int rows;
  int cols;
  int penalty;
  string outfile;

  cl_kernel kernel_noblocks;
  cl_kernel kernel_rectangles;
  cl_kernel kernel_diamonds;

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
    M = new int[rows]; assert( null != M, "Can't allocate array M" );
    N = new int[cols]; assert( null != N, "Can't allocate array N" );

    I.length = rows * cols; assert( I !is null );
    O.length = rows * cols; assert( O !is null );
    A.length = rows;        assert( A !is null );
    B.length = cols;        assert( B !is null );

    Mt19937 gen;
    gen.seed( SEED );
    foreach ( r; 1 .. rows )
    {
      F[r * cols] = -penalty * r;
      G[r * cols] = -penalty * r;
      M[r] = uniform( 1, CHARS, gen );
    }
    foreach ( c; 1 .. cols )
    {
      F[c] = -penalty * c;
      G[c] = -penalty * c;
      N[c] = uniform( 1, CHARS, gen );
    }
    foreach ( r; 1 .. rows )
      foreach ( c; 1 .. cols )
        S[r * cols + c] = BLOSUM62[M[r]][N[c]];

    /**
     */
    char[] code = q{
      #define BLOCK_SIZE 16

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
        int m = F[(r - 1) * cols + c - 1] + S[r * cols + c];
        int d = F[(r - 1) * cols + c    ] - penalty;
        int i = F[(r    ) * cols + c - 1] - penalty;
        F[r * cols + c] = max3( m, d, i );
      }

      __kernel void nw_rectangles( __global const int* S, __global int* F, int cols, int penalty, int i, int is_upper, __local int* s, __local int* t )
      {
        int bx = get_group_id( 0 );
        int tx = get_local_id( 0 );
        int xx =         bx; if ( is_upper == 1 ) xx = (cols - 1) / BLOCK_SIZE + bx - i;
        int yy = i - 1 - bx; if ( is_upper == 1 ) yy = (cols - 1) / BLOCK_SIZE - bx - 1;
        // 1.
        int index    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx + cols + 1;
        int index_n  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx + 1;
        int index_w  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + cols;
        int index_nw = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx;

        if ( tx == 0 ) t[0] = F[index_nw];
        for ( int ty = 0; ty < BLOCK_SIZE; ++ty ) s[ty * BLOCK_SIZE + tx] = S[ty * cols + index];

        t[(tx + 1) * (BLOCK_SIZE + 1)] = F[tx * cols + index_w];
        t[(tx + 1)                   ] = F[            index_n];
        barrier( CLK_LOCAL_MEM_FENCE );
        // 2.
        for ( int m = 0; m < BLOCK_SIZE; ++m )
        {
          if ( tx <= m )
          {
            int x = tx + 1;
            int y = m - tx + 1;
            int m = t[(y-1) * (BLOCK_SIZE + 1) + x-1] + s[(y-1) * BLOCK_SIZE + x-1];
            int d = t[(y-1) * (BLOCK_SIZE + 1) + x  ] - penalty;
            int i = t[(y  ) * (BLOCK_SIZE + 1) + x-1] - penalty;
            t[y * (BLOCK_SIZE + 1) + x] = max3( m, d, i );
          }
          barrier( CLK_LOCAL_MEM_FENCE );
        }
        // 3.
        for ( int m = BLOCK_SIZE - 2; m >= 0; --m )
        {
          if ( tx <= m )
          {
            int x = BLOCK_SIZE + tx - m;
            int y = BLOCK_SIZE - tx;
            int m = t[(y-1) * (BLOCK_SIZE + 1) + x-1] + s[(y-1) * BLOCK_SIZE + x-1];
            int d = t[(y-1) * (BLOCK_SIZE + 1) + x  ] - penalty;
            int i = t[(y  ) * (BLOCK_SIZE + 1) + x-1] - penalty;
            t[y * (BLOCK_SIZE + 1) + x] = max3( m, d, i );
          }
          barrier( CLK_LOCAL_MEM_FENCE );
        }
        // 4.
        for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
          F[ty * cols + index] = t[(ty + 1) * (BLOCK_SIZE + 1) + tx + 1];
      }

      __kernel void nw_diamonds( __global const int* S, __global int* F, int Y, int X, int cols, int penalty, __local int* s, __local int* t )
      {
        int bx = get_group_id( 0 );
        int tx = get_local_id( 0 );
        int xx = X + 2 * bx;
        int yy = Y -     bx;

        if ( xx == 0 )
        {
          // 1.
          int index    = cols * BLOCK_SIZE * yy + tx * cols + cols + 1;
          int index_w  = cols * BLOCK_SIZE * yy + tx * cols + cols;
          int index_nw = cols * BLOCK_SIZE * yy + tx * cols;
          int index_n0 = cols * BLOCK_SIZE * yy + 1;

          if ( tx == 0 ) t[0] = F[index_nw];
          for ( int ty = 0; ty < BLOCK_SIZE; ++ty ) s[tx * BLOCK_SIZE + ty] = S[index + ty];

          t[tx + 1] = F[index_n0 + tx];
          t[(tx + 1) * (BLOCK_SIZE + 2)] = F[index_w];
          barrier( CLK_LOCAL_MEM_FENCE );
          // 2.
          for ( int m = 0; m < BLOCK_SIZE; ++m )
          {
            if ( tx <= m )
            {
              int x = tx + 1;
              int y = m - tx + 1;
              int m = t[(y-1) * (BLOCK_SIZE + 2) + x-1] + s[(y-1) * BLOCK_SIZE + x-1];
              int d = t[(y-1) * (BLOCK_SIZE + 2) + x  ] - penalty;
              int i = t[(y  ) * (BLOCK_SIZE + 2) + x-1] - penalty;
              t[y * (BLOCK_SIZE + 2) + x] = max3( m, d, i );
            }
            barrier( CLK_LOCAL_MEM_FENCE );
          }
          // 3.
          for ( int ty = 0; ty < BLOCK_SIZE - tx; ++ty )
            F[index + ty] = t[(tx+1) * (BLOCK_SIZE + 2) + ty+1];
        }
        else if ( xx < cols / BLOCK_SIZE )
        {
          // 1.
          int index    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols + 1;
          int index_w  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols;
          int index_nw = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1);
          int index_n0 = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + 1;

          if ( tx == 0 ) t[0] = F[index_nw];

          for ( int ty = 0; ty < BLOCK_SIZE; ++ty ) s[tx * BLOCK_SIZE + ty] = S[index + ty];
          t[tx + 1] = F[index_n0 + tx];
          t[(tx + 1) * (BLOCK_SIZE + 2)] = F[index_w - 1];
          t[(tx + 1) * (BLOCK_SIZE + 2) + 1] = F[index_w];
          barrier( CLK_LOCAL_MEM_FENCE );
          // 2.
          for ( int m = 0; m < BLOCK_SIZE; ++m )
          {
            int x =  m + 2;
            int y =  tx + 1;
            int m = t[(y-1) * (BLOCK_SIZE + 2) + x-2] + s[(y-1) * BLOCK_SIZE + x-2];
            int d = t[(y-1) * (BLOCK_SIZE + 2) + x-1] - penalty;
            int i = t[(y  ) * (BLOCK_SIZE + 2) + x-1] - penalty;
            t[y * (BLOCK_SIZE + 2) + x] = max3( m, d, i );
            barrier( CLK_LOCAL_MEM_FENCE );
          }
          // 3.
          for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
            F[index + ty] = t[(tx+1) * (BLOCK_SIZE + 2) + ty+2];
        }
        else if ( xx == cols / BLOCK_SIZE )
        {
          // 1.
          int index    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols + 1;
          int index_w  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols;

          for ( int ty = 0; ty < tx; ++ty ) s[tx * BLOCK_SIZE + ty] = S[index + ty];
          t[(tx + 1) * (BLOCK_SIZE + 2)] = F[index_w - 1];
          t[(tx + 1) * (BLOCK_SIZE + 2) + 1] = F[index_w];
          barrier( CLK_LOCAL_MEM_FENCE );
          // 2.
          for ( int m = 0; m < BLOCK_SIZE; ++m )
          {
            if ( m < tx )
            {
              int x =  m + 2;
              int y =  tx + 1;
              int m = t[(y-1) * (BLOCK_SIZE + 2) + x-2] + s[(y-1) * BLOCK_SIZE + x-2];
              int d = t[(y-1) * (BLOCK_SIZE + 2) + x-1] - penalty;
              int i = t[(y  ) * (BLOCK_SIZE + 2) + x-1] - penalty;
              t[y * (BLOCK_SIZE + 2) + x] = max3( m, d, i );
            }
            barrier( CLK_LOCAL_MEM_FENCE );
          }
          // 3.
          for ( int ty = 0; ty < tx; ++ty )
            F[index + ty] = t[(tx+1) * (BLOCK_SIZE + 2) + ty+2];
        }
      }
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( runtime.context, 1, strs.ptr, &size, &status );
    assert( status == CL_SUCCESS, "this: clCreateProgramWithSource" );
    status = clBuildProgram( program, 1, &runtime.device, "", null, null );
    assert( status == CL_SUCCESS, "this: clBuildProgram" );
    kernel_noblocks = clCreateKernel( program, "nw_noblocks", &status );
    assert( status == CL_SUCCESS, "this: clCreateKernel nw_noblocks" );
    kernel_rectangles = clCreateKernel( program, "nw_rectangles", &status );
    assert( status == CL_SUCCESS );
    kernel_diamonds = clCreateKernel( program, "nw_diamonds", &status );
    assert( status == CL_SUCCESS );
    status = clReleaseProgram( program );
    assert( status == CL_SUCCESS );
  }

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
  void dump()
  {
    // dumps.
    foreach ( int r; 0 .. rows )
    {
      foreach ( int c; 0 .. cols )
        if ( I[r * cols + c] == 0 ) write( " ." );
        else write( " ", I[r * cols + c] );
      writeln();
    }
    foreach ( int j; 0 .. cols ) write( "~~" ); writeln();
    foreach ( r; 0 .. BLOCK_SIZE + 1 )
    {
      foreach ( c; 0 .. BLOCK_SIZE + 2 )
        if ( Z[r][c] == 0 ) write( " ." );
        else write( " ", Z[r][c] );
      writeln();
    }
    foreach ( int j; 0 .. cols ) write( "~~" ); writeln();
    foreach ( int r; 0 .. rows )
    {
      foreach ( int c; 0 .. cols )
        if ( O[r * cols + c] == 0 ) write( " ." );
        else write( " ", O[r * cols + c] );
      writeln();
    }
    foreach ( int j; 0 .. cols ) write( "XX" ); writeln( "\n" );
  }

  /**
   * Maximum of three numbers.
   */
  int max3( immutable int a, immutable int b, immutable int c ) const
  {
    auto k = a > b ? a : b;
    return k > c ? k : c;
  }

  /**
     The algorithm of filling in F matrix

     for i=0 to length(A) F(i,0) ← d*i
     for j=0 to length(B) F(0,j) ← d*j
     for i=1 to length(A)
     for j=1 to length(B)
     {
       Match  ← F( i-1, j-1) + S(Ai, Bj)
       Delete ← F( i-1, j  ) + d
       Insert ← F( i  , j-1) + d
       F(i,j) ← max(Match, Insert, Delete)
     }
  */
  void baseline_sequential()
  {
    foreach ( r; 1 .. rows )
      foreach ( c; 1 .. cols )
      {
        F[r * cols + c] = max3( F[(r - 1) * cols + c - 1] + BLOSUM62[M[r]][N[c]],
                                F[(r - 1) * cols + c    ] - penalty,
                                F[ r      * cols + c - 1] - penalty );
      }
  }

  /**
   */
  void rectangular_blocks( int i, bool is_upper )
  {
    immutable int block_width = ( cols - 1 ) / BLOCK_SIZE;
    int t[BLOCK_SIZE + 1][BLOCK_SIZE + 1];
    int s[BLOCK_SIZE][BLOCK_SIZE];
    foreach ( int bx; 0 .. i )
    {
      int index[BLOCK_SIZE];
      int index_n[BLOCK_SIZE];
      int index_w[BLOCK_SIZE];
      int index_nw[BLOCK_SIZE];
      // 1.
      foreach ( int tx; 0 .. BLOCK_SIZE )
      {
        int xx = ( is_upper ) ?         bx : block_width + bx - i;
        int yy = ( is_upper ) ? i - 1 - bx : block_width - bx - 1;
        index[tx]    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx + cols + 1;
        index_n[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx + 1;
        index_w[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + cols;
        index_nw[tx] = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx;

        if ( tx == 0 ) t[tx][0] = F[index_nw[tx]];

        for ( int ty = 0 ; ty < BLOCK_SIZE ; ty++) s[ty][tx] = S[index[tx] + cols * ty];
      }
      // 2.
      foreach ( int tx; 0 .. BLOCK_SIZE ) t[tx + 1][0] = F[index_w[tx] + cols * tx];
      foreach ( int tx; 0 .. BLOCK_SIZE ) t[0][tx + 1] = F[index_n[tx]];
      // 3.
      for ( int m = 0; m < BLOCK_SIZE; ++m )
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          if ( tx <= m )
          {
            int x =  tx + 1;
            int y =  m - tx + 1;

            t[y][x] = max3( t[y-1][x-1] + s[y-1][x-1], t[y][x-1] - penalty, t[y-1][x] - penalty );
          }
        }
      // 4.
      for ( int m = BLOCK_SIZE - 2; m >=0; --m )
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          if ( tx <= m )
          {
            int x =  tx + BLOCK_SIZE - m;
            int y =  BLOCK_SIZE - tx;

            t[y][x] = max3( t[y-1][x-1] + s[y-1][x-1], t[y][x-1] - penalty, t[y-1][x] - penalty );
          }
        }
      // 5.
      foreach ( int tx; 0 .. BLOCK_SIZE )
        foreach ( int ty; 0 .. BLOCK_SIZE )
          F[index[tx] + ty * cols] = t[ty+1][tx+1];
    }
  }

  /**
   */
  void rectangles()
  {
    immutable int block_width = ( cols - 1 ) / BLOCK_SIZE;
    for ( int i = 1; i <= block_width; ++i )     rectangular_blocks( i, true );
    for ( int i = block_width - 1; i >= 1; --i ) rectangular_blocks( i, false );
  }

  /**
   */
  void diamond_blocks_debug( int Y, int X  )
  {
    int t[BLOCK_SIZE + 1][BLOCK_SIZE + 2];
    int s[BLOCK_SIZE][BLOCK_SIZE];

    debug ( DEBUG )
    foreach ( i; 0 .. BLOCK_SIZE + 1 )
      foreach ( j; 0 .. BLOCK_SIZE + 2 )
        Z[i][j] = 0;

    foreach ( int bx; 0 .. Y + 1 )
    {
      int xx = 2 * bx + X;
      int yy = Y - bx;

      debug ( DEBUG ) writeln( "block #", bx, " [", yy, ",", xx, "]" );

      int index[BLOCK_SIZE];
      int index_n[BLOCK_SIZE];
      int index_w[BLOCK_SIZE];
      int index_nw[BLOCK_SIZE];

      if ( xx == 0 )
      {
        debug ( DEBUG ) writeln( "case 1" );
        // 1.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          index[tx]    = cols * BLOCK_SIZE * yy + tx * cols + cols + 1;
          index_w[tx]  = cols * BLOCK_SIZE * yy + tx * cols + cols;
          index_n[tx]  = cols * BLOCK_SIZE * yy + tx * cols + 1;
          index_nw[tx] = cols * BLOCK_SIZE * yy + tx * cols;

          debug ( DEBUG ) writeln( "thread ", tx, " index ", index[tx], " [", index[tx] / cols, ",", index[tx] % cols, "]" );

          if ( tx == 0 )
          {
            debug ( DEBUG )
            {
              writeln( "t[", tx, "][", 0, "] <- F[", index_nw[tx] / cols, "][", index_nw[tx] % cols, "]" );
              I[index_nw[tx]] += 1;
              Z[tx][0] += 1;
            }
            t[tx][0] = F[index_nw[tx]];
          }

          for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
            s[tx][ty] = S[index[tx] + ty];
        }
        // 2.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          debug ( DEBUG )
          {
            writeln( "t[", 0, "][", tx + 1, "] <- F[", (index_n[0] + tx) / cols, "][", (index_n[0] + tx) % cols, "]" );
            I[index_n[0] + tx] += 1;
            Z[0][tx + 1] += 1;
          }

          t[0][tx + 1] = F[index_n[0] + tx];

          debug ( DEBUG )
          {
            writeln( "t[", tx + 1, "][", 0, "] <- F[", (index_w[tx]) / cols, "][", (index_w[tx]) % cols, "]" );
            I[index_w[tx]] += 1;
            Z[tx + 1][0] += 1;
          }

          t[tx + 1][0] = F[index_w[tx]];

          debug ( DEBUG )
          {
            foreach ( r; 0 .. BLOCK_SIZE + 1 )
            {
              foreach ( c; 0 .. BLOCK_SIZE + 2 ) if ( Z[r][c] == 0 ) write( " ." ); else write( " ", Z[r][c] );
              writeln();
            }
            foreach ( int j; 0 .. cols ) write( "~~" ); writeln();
          }
        }
        // 3.
        for ( int m = 0; m < BLOCK_SIZE; ++m )
          foreach ( int tx; 0 .. BLOCK_SIZE )
          {
            if ( m < BLOCK_SIZE - tx )
            {
              int x = m + 1;
              int y = tx + 1;

              t[y][x] = max3( t[y-1][x-1] + s[y-1][x-1], t[y-1][x] - penalty, t[y][x-1] - penalty );

              debug ( DEBUG )
              {
                writeln( "t[", y, "][", x, "] <- t[", y-1, "][", x-1, "], t[", y, "][", x-1, "], t[", y-1, "][", x, "]" );
                Z[y][x] += 1;
              }
            }
          }
        // 4.
        foreach ( int tx; 0 .. BLOCK_SIZE )
          foreach ( int ty; 0 .. BLOCK_SIZE - tx )
          {
            F[index[tx] + ty] = t[tx+1][ty+1];
            debug ( DEBUG ) O[index[tx] + ty] += 1;
          }
      }
      else if ( xx < cols / BLOCK_SIZE )
      {
        debug ( DEBUG ) writeln( "case 2" );
        // 1.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          index[tx]    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols + 1;
          index_w[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols;
          index_n[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + 1;
          index_nw[tx] = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1);

          //writeln( "thread ", tx, " index ", index[tx], " [", index[tx] / cols, ",", index[tx] % cols, "]" );

          if ( tx == 0 )
          {
            debug ( DEBUG )
            {
              writeln( "t[", tx, "][", 0, "] <- F[", index_nw[tx] / cols, "][", index_nw[tx] % cols, "]" );
              I[index_nw[tx]] += 1;
              Z[tx][0] += 1;
            }

            t[tx][0] = F[index_nw[tx]];
          }

          for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
            s[tx][ty] = S[index[tx] + ty];
        }
        // 2.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          debug ( DEBUG )
          {
            writeln( "t[", 0, "][", tx + 1, "] <- F[", (index_n[0] + tx) / cols, "][", (index_n[0] + tx) % cols, "]" );
            I[index_n[0] + tx] += 1;
            Z[0][tx + 1] += 1;
          }

          t[0][tx + 1] = F[index_n[0] + tx];

          debug ( DEBUG )
          {
            writeln( "t[", tx + 1, "][", 0, "] <- F[", (index_w[tx] - 1) / cols, "][", (index_w[tx] - 1) % cols, "]" );
            I[index_w[tx] - 1] += 1;
            Z[tx + 1][0] += 1;
          }

          t[tx + 1][0] = F[index_w[tx] - 1];

          debug ( DEBUG )
          {
            writeln( "t[", tx + 1, "][", 1, "] <- F[", (index_w[tx]) / cols, "][", (index_w[tx]) % cols, "]" );
            I[index_w[tx]] += 1;
            Z[tx + 1][1] += 1;
          }

          t[tx + 1][1] = F[index_w[tx]];
        }
        // 3.
        for ( int m = 0; m < BLOCK_SIZE; ++m )
          foreach ( int tx; 0 .. BLOCK_SIZE )
          {
            int x =  m + 2;
            int y =  tx + 1;

            debug ( DEBUG )
            {
              writeln( "t[", y, "][", x, "] <- t[", y-1, "][", x-2, "], t[", y-1, "][", x-1, "], t[", y, "][", x-1, "]" );
              Z[y][x] += 1;
            }

            t[y][x] = max3( t[y-1][x-2] + s[y-1][x-2], t[y-1][x-1] - penalty, t[y][x-1] - penalty );
          }
        // 4.
        foreach ( int tx; 0 .. BLOCK_SIZE )
          foreach ( int ty; 0 .. BLOCK_SIZE )
          {
            F[index[tx] + ty] = t[tx+1][ty+2];
            debug ( DEBUG ) O[index[tx] + ty] += 1;
          }
      }
      else if ( xx == cols / BLOCK_SIZE )
      {
        debug ( DEBUG ) writeln( "case 3" );
        // 1.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          index[tx]    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols + 1;
          index_w[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols;
          index_n[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + 1;
          index_nw[tx] = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1);

          //writeln( "thread ", tx, " index ", index[tx], " [", index[tx] / cols, ",", index[tx] % cols, "]" );

          for ( int ty = 0; ty < tx; ++ty )
            s[tx][ty] = S[index[tx] + ty];
        }
        // 2.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          debug ( DEBUG )
          {
            writeln( "thread ", tx, " read F[", (index_w[tx] - 1) / cols, ",", (index_w[tx] - 1) % cols, "]" );
            I[index_w[tx] - 1] += 1;
            Z[tx + 1][0] += 1;
          }

          t[tx + 1][0] = F[index_w[tx] - 1];

          debug ( DEBUG )
          {
            writeln( "thread ", tx, " read F[", (index_w[tx]) / cols, ",", (index_w[tx] ) % cols, "]" );
            I[index_w[tx]] += 1;
            Z[tx + 1][1] += 1;
          }

          t[tx + 1][1] = F[index_w[tx]];
        }
        // 3.
        for ( int m = 0; m < BLOCK_SIZE; ++m )
          foreach ( int tx; 0 .. BLOCK_SIZE )
          {
            if ( m < tx )
            {
              int x =  m + 2;
              int y =  tx + 1;

              t[y][x] = max3( t[y-1][x-2] + s[y-1][x-2], t[y-1][x-1] - penalty, t[y][x-1] - penalty );
              debug ( DEBUG )
              {
                Z[y][x] += 1;
              }
            }
          }
        // 4.
        foreach ( int tx; 0 .. BLOCK_SIZE )
          foreach ( int ty; 0 .. tx )
          {
            //writeln( "thread ", tx, " write F[", (index[tx] + ty) / cols, ",", (index[tx] + ty ) % cols, "]" );
            F[index[tx] + ty] = t[tx+1][ty+2];
            debug ( DEBUG ) O[index[tx] + ty] += 1;
          }
      }
      else continue;
      debug ( DEBUG ) dump();
    }
  }

  /**
   */
  void diamond_blocks( int Y, int X  )
  {
    int t[BLOCK_SIZE + 1][BLOCK_SIZE + 2];
    int s[BLOCK_SIZE][BLOCK_SIZE];

    foreach ( int bx; 0 .. Y + 1 )
    {
      int xx = X + 2 * bx;
      int yy = Y -     bx;

      int index[BLOCK_SIZE];
      int index_n[BLOCK_SIZE];
      int index_w[BLOCK_SIZE];
      int index_nw[BLOCK_SIZE];

      if ( xx == 0 )
      {
        // 1.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          index[tx]    = cols * BLOCK_SIZE * yy + tx * cols + cols + 1;
          index_w[tx]  = cols * BLOCK_SIZE * yy + tx * cols + cols;
          index_n[tx]  = cols * BLOCK_SIZE * yy + tx * cols + 1;
          index_nw[tx] = cols * BLOCK_SIZE * yy + tx * cols;

          if ( tx == 0 ) t[tx][0] = F[index_nw[tx]];

          for ( int ty = 0; ty < BLOCK_SIZE; ++ty ) s[tx][ty] = S[index[tx] + ty];
        }
        // 2.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          t[0][tx + 1] = F[index_n[0] + tx];
          t[tx + 1][0] = F[index_w[tx]];
        }
        // 3.
        for ( int m = 0; m < BLOCK_SIZE; ++m )
          foreach ( int tx; 0 .. BLOCK_SIZE )
          {
            if ( m < BLOCK_SIZE - tx )
            {
              int x = m + 1;
              int y = tx + 1;

              t[y][x] = max3( t[y-1][x-1] + s[y-1][x-1], t[y-1][x] - penalty, t[y][x-1] - penalty );
            }
          }
        // 4.
        foreach ( int tx; 0 .. BLOCK_SIZE )
          foreach ( int ty; 0 .. BLOCK_SIZE - tx )
            F[index[tx] + ty] = t[tx+1][ty+1];
      }
      else if ( xx < cols / BLOCK_SIZE )
      {
        // 1.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          index[tx]    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols + 1;
          index_w[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols;
          index_n[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + 1;
          index_nw[tx] = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1);

          if ( tx == 0 ) t[tx][0] = F[index_nw[tx]];

          for ( int ty = 0; ty < BLOCK_SIZE; ++ty ) s[tx][ty] = S[index[tx] + ty];
        }
        // 2.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          t[0][tx + 1] = F[index_n[0] + tx];
          t[tx + 1][0] = F[index_w[tx] - 1];
          t[tx + 1][1] = F[index_w[tx]];
        }
        // 3.
        for ( int m = 0; m < BLOCK_SIZE; ++m )
          foreach ( int tx; 0 .. BLOCK_SIZE )
          {
            int x =  m + 2;
            int y =  tx + 1;

            t[y][x] = max3( t[y-1][x-2] + s[y-1][x-2], t[y-1][x-1] - penalty, t[y][x-1] - penalty );
          }
        // 4.
        foreach ( int tx; 0 .. BLOCK_SIZE )
          foreach ( int ty; 0 .. BLOCK_SIZE )
            F[index[tx] + ty] = t[tx+1][ty+2];
      }
      else if ( xx == cols / BLOCK_SIZE )
      {
        // 1.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          index[tx]    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols + 1;
          index_w[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols;
          index_n[tx]  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + 1;
          index_nw[tx] = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1);

          for ( int ty = 0; ty < tx; ++ty ) s[tx][ty] = S[index[tx] + ty];
        }
        // 2.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          t[tx + 1][0] = F[index_w[tx] - 1];
          t[tx + 1][1] = F[index_w[tx]];
        }
        // 3.
        for ( int m = 0; m < BLOCK_SIZE; ++m )
          foreach ( int tx; 0 .. BLOCK_SIZE )
          {
            if ( m < tx )
            {
              int x =  m + 2;
              int y =  tx + 1;

              t[y][x] = max3( t[y-1][x-2] + s[y-1][x-2], t[y-1][x-1] - penalty, t[y][x-1] - penalty );
            }
          }
        // 4.
        foreach ( int tx; 0 .. BLOCK_SIZE )
          foreach ( int ty; 0 .. tx )
            F[index[tx] + ty] = t[tx+1][ty+2];
      }
    }
  }

  /**
   */
  void diamonds()
  {
    for ( int i = 0; i < rows / BLOCK_SIZE; ++i )
    {
      diamond_blocks( i, 0 );
      debug ( DEBUG ) { foreach ( int j; 0 .. 2 * cols ) write( "--" ); writeln(); }
      diamond_blocks( i, 1 );
      debug ( DEBUG ) { foreach ( int j; 0 .. 2 * cols ) write( "==" ); writeln(); }
    }
    for ( int i = 2; i <= cols / BLOCK_SIZE; ++i )
    {
      diamond_blocks( rows / BLOCK_SIZE - 1, i );
      debug ( DEBUG ) { foreach ( int j; 0 .. 2 * cols ) write( "==" ); writeln(); }
    }
  }

  /**
     AlignmentA ← ""
     AlignmentB ← ""
     i ← length(A)
     j ← length(B)
     while (i > 0 or j > 0)
     {
       if (i > 0 and j > 0 and F(i,j) == F(i-1,j-1) + S(Ai, Bj))
       {
         AlignmentA ← Ai + AlignmentA
         AlignmentB ← Bj + AlignmentB
         i ← i - 1
         j ← j - 1
       }
       else if (i > 0 and F(i,j) == F(i-1,j) + d)
       {
         AlignmentA ← Ai + AlignmentA
         AlignmentB ← "-" + AlignmentB
         i ← i - 1
       }
       else (j > 0 and F(i,j) == F(i,j-1) + d)
       {
         AlignmentA ← "-" + AlignmentA
         AlignmentB ← Bj + AlignmentB
         j ← j - 1
       }
     }
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
  void opencl_noblocks()
  {
    try
    {
      cl_int status;
      cl_mem bS = clCreateBuffer( runtime.context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, cl_int.sizeof * S.length, S.ptr, &status );
      assert( status == CL_SUCCESS );
      cl_mem bF = clCreateBuffer( runtime.context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR, cl_int.sizeof * F.length, F.ptr, &status );
      assert( status == CL_SUCCESS );
      clSetKernelArg( kernel_noblocks, 0, cl_mem.sizeof, &bS      );
      clSetKernelArg( kernel_noblocks, 1, cl_mem.sizeof, &bF      );
      clSetKernelArg( kernel_noblocks, 2, cl_int.sizeof, &cols    );
      clSetKernelArg( kernel_noblocks, 3, cl_int.sizeof, &penalty );
      foreach ( i; 2 .. 2 * cols - 1 )
      {
        size_t global = (i < cols) ? i - 1 : 2 * cols - i - 1;
        clSetKernelArg( kernel_noblocks, 4, cl_int.sizeof, &i );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_noblocks, 1, null, &global, null, 0, null, null );
        assert( status == CL_SUCCESS );
      }
      status = clEnqueueReadBuffer( runtime.queue, bF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );
      assert( status == CL_SUCCESS );
      status = clReleaseMemObject( bS );
      assert( status == CL_SUCCESS );
      status = clReleaseMemObject( bF );
      assert( status == CL_SUCCESS );
      status = clReleaseKernel( kernel_noblocks );
      assert( status == CL_SUCCESS );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
  }

  /**
   */
  void opencl_rectangles()
  {
    try
    {
      cl_int status;
      cl_mem bS = clCreateBuffer( runtime.context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, cl_int.sizeof * S.length, S.ptr, &status );
      assert( status == CL_SUCCESS );
      cl_mem bF = clCreateBuffer( runtime.context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR, cl_int.sizeof * F.length, F.ptr, &status );
      assert( status == CL_SUCCESS );
      clSetKernelArg( kernel_rectangles, 0, cl_mem.sizeof, &bS      );
      clSetKernelArg( kernel_rectangles, 1, cl_mem.sizeof, &bF      );
      clSetKernelArg( kernel_rectangles, 2, cl_int.sizeof, &cols    );
      clSetKernelArg( kernel_rectangles, 3, cl_int.sizeof, &penalty );
      size_t aS =  BLOCK_SIZE      *  BLOCK_SIZE      * cl_int.sizeof;
      size_t aT = (BLOCK_SIZE + 1) * (BLOCK_SIZE + 1) * cl_int.sizeof;
      clSetKernelArg( kernel_rectangles, 6, aS, null );
      clSetKernelArg( kernel_rectangles, 7, aT, null );
      size_t wgroup = BLOCK_SIZE;
      for ( int i = 1; i <= (cols - 1) / BLOCK_SIZE; ++i )
      {
        int u = 0;
        size_t global = BLOCK_SIZE * i;
        clSetKernelArg( kernel_rectangles, 4, cl_int.sizeof, &i );
        clSetKernelArg( kernel_rectangles, 5, cl_int.sizeof, &u );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_rectangles, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS );
      }
      for ( int i = (cols - 1) / BLOCK_SIZE - 1; i > 0; --i )
      {
        int u = 1;
        size_t global = BLOCK_SIZE * i;
        clSetKernelArg( kernel_rectangles, 4, cl_int.sizeof, &i );
        clSetKernelArg( kernel_rectangles, 5, cl_int.sizeof, &u );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_rectangles, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS );
      }
      status = clEnqueueReadBuffer( runtime.queue, bF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );
      assert( status == CL_SUCCESS );
      status = clReleaseMemObject( bS );
      assert( status == CL_SUCCESS );
      status = clReleaseMemObject( bF );
      assert( status == CL_SUCCESS );
      status = clReleaseKernel( kernel_rectangles );
      assert( status == CL_SUCCESS );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
  }

  /**
   */
  void opencl_diamonds()
  {
    try
    {
      cl_int status;
      cl_mem bS = clCreateBuffer( runtime.context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, cl_int.sizeof * S.length, S.ptr, &status );
      assert( status == CL_SUCCESS );
      cl_mem bF = clCreateBuffer( runtime.context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR, cl_int.sizeof * F.length, F.ptr, &status );
      assert( status == CL_SUCCESS );
      clSetKernelArg( kernel_diamonds, 0, cl_mem.sizeof, &bS      );
      clSetKernelArg( kernel_diamonds, 1, cl_mem.sizeof, &bF      );
      clSetKernelArg( kernel_diamonds, 4, cl_int.sizeof, &cols    );
      clSetKernelArg( kernel_diamonds, 5, cl_int.sizeof, &penalty );
      size_t aS =  BLOCK_SIZE      *  BLOCK_SIZE      * cl_int.sizeof;
      size_t aT = (BLOCK_SIZE + 1) * (BLOCK_SIZE + 2) * cl_int.sizeof;
      clSetKernelArg( kernel_diamonds, 6, aS, null );
      clSetKernelArg( kernel_diamonds, 7, aT, null );
      size_t wgroup = BLOCK_SIZE;
      for ( int i = 0; i < rows / BLOCK_SIZE; ++i )
      {
        int Y = 0;
        size_t global = BLOCK_SIZE * (i + 1);
        clSetKernelArg( kernel_diamonds, 2, cl_int.sizeof, &i );
        clSetKernelArg( kernel_diamonds, 3, cl_int.sizeof, &Y );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_diamonds, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS, "opencl_diamonds: clEnqueueNDRangeKernel 1" );

        Y = 1;
        clSetKernelArg( kernel_diamonds, 3, cl_int.sizeof, &Y );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_diamonds, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS, "opencl_diamonds: clEnqueueNDRangeKernel 2" );
      }
      for ( int i = 2; i <= cols / BLOCK_SIZE; ++i )
      {
        size_t global = rows - 1;
        int X = rows / BLOCK_SIZE - 1;
        clSetKernelArg( kernel_diamonds, 2, cl_int.sizeof, &X );
        clSetKernelArg( kernel_diamonds, 3, cl_int.sizeof, &i );
        status = clEnqueueNDRangeKernel( runtime.queue, kernel_diamonds, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS );
      }
      status = clEnqueueReadBuffer( runtime.queue, bF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );
      assert( status == CL_SUCCESS );
      status = clReleaseMemObject( bS );
      assert( status == CL_SUCCESS );
      status = clReleaseMemObject( bF );
      assert( status == CL_SUCCESS );
      status = clReleaseKernel( kernel_diamonds );
      assert( status == CL_SUCCESS );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
  }

  /**
   */
  void clop_dsl()
  {
    try
    {
      // use CLOP DSL to generate the loops around the computation
      mixin( compile(
      q{
        int max3( int a, int b, int c )
        {
          int k = a > b ? a : b;
          return k > c ? k : c;
        }
        NDRange( r ; 1 .. rows, c ; 1 .. cols );
        F[r * cols + c] = max3( F[(r - 1) * cols + c - 1] + S[r * cols + c],
                                F[(r - 1) * cols + c    ] - penalty,
                                F[ r      * cols + c - 1] - penalty );
      } ) );
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

    reset();
    timer.reset();
    timer.start();
    diamonds();
    timer.stop();
    ticks = timer.peek();
    writeln( "DIAMONDS   ", ticks.usecs / 1E6, " [s]" );
    validate();

    reset();
    timer.reset();
    timer.start();
    opencl_noblocks();
    timer.stop();
    ticks = timer.peek();
    writeln( "CL NOBLOCK ", ticks.usecs / 1E6, " [s]" );
    validate();

    reset();
    timer.reset();
    timer.start();
    opencl_rectangles();
    timer.stop();
    ticks = timer.peek();
    writeln( "CL BLOCKS  ", ticks.usecs / 1E6, " [s]" );
    validate();

    reset();
    timer.reset();
    timer.start();
    opencl_diamonds();
    timer.stop();
    ticks = timer.peek();
    writeln( "CL DIAMOND ", ticks.usecs / 1E6, " [s]" );
    validate();

    reset();
    timer.reset();
    timer.start();
    clop_dsl();
    timer.stop();
    ticks = timer.peek();
    writeln( "CLOP DSL   ", ticks.usecs / 1E6, " [s]" );
    validate();

    save();
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
    writeln( "NW: ", msg );
    return -1;
  }
  return 0;
}
