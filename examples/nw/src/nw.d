module nw;

import std.conv;
import std.datetime;
import std.getopt;
import std.random;
import std.stdio;

import derelict.opencl.cl;

class Application {
  static immutable int SEED  =  1;
  static immutable int CHARS = 24;
  static immutable int BLOSUM62[CHARS][CHARS] = [
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
  cl_context context;
  cl_command_queue queue;
  cl_device_id device;

  this( string[] args )
  {
    getopt( args, "output|o", &outfile );
    rows    = to!(int)( args[1] ) + 1;
    cols    = to!(int)( args[1] ) + 1;
    penalty = to!(int)( args[2] );
    if ( ( rows - 1 ) % 16 != 0 )
    {
      auto r = to!(string)(rows - 1);
      auto b = to!(string)(BLOCK_SIZE);
      throw new Exception( "ERROR: rows # (" ~ r ~ ") must be a multiple of " ~ b );
    }
    F = new int[rows * cols]; assert( null != F );
    G = new int[rows * cols]; assert( null != G );
    S = new int[rows * cols]; assert( null != S );
    I = new int[rows * cols]; assert( null != I );
    O = new int[rows * cols]; assert( null != O );
    A = new int[rows];        assert( null != A );
    B = new int[cols];        assert( null != B );
    M = new int[rows];        assert( null != M );
    N = new int[cols];        assert( null != N );
    Mt19937 gen;
    gen.seed( SEED );
    for ( int ii = 1; ii < rows; ++ii ) M[ii] = uniform( 1, CHARS, gen );
    for ( int jj = 1; jj < cols; ++jj ) N[jj] = uniform( 1, CHARS, gen );
    for ( int ii = 1; ii < rows; ++ii )
      for ( int jj = 1; jj < cols; ++jj )
        S[ii * cols + jj] = BLOSUM62[M[ii]][N[jj]];
    foreach ( r; 1 .. rows ) F[r * cols] = -penalty * r;
    foreach ( c; 0 .. cols ) F[c       ] = -penalty * c;

    DerelictCL.load();
    cl_uint num_platforms;
    cl_int status = clGetPlatformIDs( 0, null, &num_platforms );
    assert( status == CL_SUCCESS && num_platforms > 0 );
    writeln( "# platforms ", num_platforms );
    cl_platform_id[] platforms = new cl_platform_id[num_platforms];
    assert( platforms != null );
    status = clGetPlatformIDs( num_platforms, platforms.ptr, null );
    assert( status == CL_SUCCESS );
    cl_platform_id platform = platforms[0];
    cl_uint num_devices;
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, 0, null, &num_devices );
    assert( status == CL_SUCCESS && num_devices > 0 );
    writeln( "# devices ", num_devices );
    cl_device_id[] devices = new cl_device_id[num_devices];
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, num_devices, devices.ptr, null );
    assert( status == CL_SUCCESS );
    device = devices[1];
    context = clCreateContext( null, 1, &device, null, null, &status );
    assert( status == CL_SUCCESS );
    queue = clCreateCommandQueue( context, device, 0, &status );
    assert( status == CL_SUCCESS );
  }

  void save()
  {
    if ( null != outfile )
    {
      auto fp = File( outfile, "w" );
      for ( int ii = 1; ii < rows - 1; ++ii )
        fp.writef( "%c", to!(char)(M[ii] + 'A') );
      fp.writeln();
      for ( int ii = 1; ii < cols - 1; ++ii )
        fp.writef( "%c", to!(char)(N[ii] + 'A') );
      fp.writeln( "\n" );
      for ( int ii = 1; ii < rows - 1; ++ii )
        fp.writef( "%c", to!(char)(A[ii] + 'A') );
      fp.writeln();
      for ( int ii = 1; ii < cols - 1; ++ii )
        fp.writef( "%c", to!(char)(B[ii] + 'A') );
      fp.writeln( "\n" );
      fp.close();
    }
  }

  int max3( int a, int b, int c )
  {
    int k = a > b ? a : b;
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
  void compute_scores_serial()
  {
    foreach ( r; 1 .. rows )
      foreach ( c; 1 .. cols )
      {
        int m = F[(r - 1) * cols + c - 1] + S[r * cols + c];
        int d = F[(r - 1) * cols + c    ] - penalty;
        int i = F[ r      * cols + c - 1] - penalty;
        F[r * cols + c] = max3( m, d, i );
      }
  }

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

  void run()
  {
    StopWatch timer;
    TickDuration ticks;
    timer.start();
    compute_scores_serial();
    timer.stop();
    ticks = timer.peek();
    writeln( "SERIAL   ", ticks.usecs / 1E6, " [s]" );
    G[] = F[];
    int diff = 0;
    opencl_diamonds();
    foreach( ii; 0 .. F.length ) if ( F[ii] != G[ii] ) ++diff;
    if ( diff > 0 ) writeln( "DIFFs ", diff );
    opencl_rectangles();
    diff = 0;
    foreach( ii; 0 .. F.length ) if ( F[ii] != G[ii] ) ++diff;
    if ( diff > 0 ) writeln( "DIFFs ", diff );
    auto status = clReleaseCommandQueue( queue );
    assert( status == CL_SUCCESS );
    status = clReleaseContext( context );
    assert( status == CL_SUCCESS );
  }

  void opencl_rectangles()
  {
    try
    {
      F[] = 0;
      foreach ( r; 1 .. rows ) F[r * cols] = -penalty * r;
      foreach ( c; 0 .. cols ) F[c       ] = -penalty * c;
      char[] code = q{
          #define BLOCK_SIZE 16
          int max3( int a, int b, int c );
          int max3( int a, int b, int c )
          {
            int k = a > b ? a : b;
            return k > c ? k : c;
          }
          __kernel void rectangles( __global const int* S, __global int* F,
                                    int cols, int penalty, int i, int is_upper,
                                    __local int* s, __local int* t )
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
      }.dup;
      cl_int status;
      size_t size = code.length;
      char*[] strs = [code.ptr];
      auto program = clCreateProgramWithSource( context, 1, strs.ptr, &size, &status );
      assert( status == CL_SUCCESS );
      status = clBuildProgram( program, 1, &device, "", null, null );
      assert( status == CL_SUCCESS );
      cl_kernel kernel = clCreateKernel( program, "rectangles", &status );
      assert( status == CL_SUCCESS );

      StopWatch timer;
      timer.reset();
      timer.start();
      cl_mem bS = clCreateBuffer( context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, cl_int.sizeof * S.length, S.ptr, &status );
      assert( status == CL_SUCCESS );
      cl_mem bF = clCreateBuffer( context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR, cl_int.sizeof * F.length, F.ptr, &status );
      assert( status == CL_SUCCESS );
      clSetKernelArg( kernel, 0, cl_mem.sizeof, &bS      );
      clSetKernelArg( kernel, 1, cl_mem.sizeof, &bF      );
      clSetKernelArg( kernel, 2, cl_int.sizeof, &cols    );
      clSetKernelArg( kernel, 3, cl_int.sizeof, &penalty );
      size_t aS =  BLOCK_SIZE      *  BLOCK_SIZE      * cl_int.sizeof;
      size_t aT = (BLOCK_SIZE + 1) * (BLOCK_SIZE + 1) * cl_int.sizeof;
      clSetKernelArg( kernel, 6, aS, null );
      clSetKernelArg( kernel, 7, aT, null );
      size_t wgroup = BLOCK_SIZE;
      for ( int i = 1; i <= (cols - 1) / BLOCK_SIZE; ++i )
      {
        int u = 0;
        size_t global = BLOCK_SIZE * i;
        clSetKernelArg( kernel, 4, cl_int.sizeof, &i );
        clSetKernelArg( kernel, 5, cl_int.sizeof, &u );
        status = clEnqueueNDRangeKernel( queue, kernel, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS );
      }
      for ( int i = (cols - 1) / BLOCK_SIZE - 1; i > 0; --i )
      {
        int u = 1;
        size_t global = BLOCK_SIZE * i;
        clSetKernelArg( kernel, 4, cl_int.sizeof, &i );
        clSetKernelArg( kernel, 5, cl_int.sizeof, &u );
        status = clEnqueueNDRangeKernel( queue, kernel, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS );
      }
      status = clEnqueueReadBuffer( queue, bF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );
      assert( status == CL_SUCCESS );
      timer.stop();
      TickDuration ticks = timer.peek();
      writeln( "RECT     ", ticks.usecs / 1E6, "  [s]" );
      status = clReleaseMemObject( bS );
      assert( status == CL_SUCCESS );
      status = clReleaseMemObject( bF );
      assert( status == CL_SUCCESS );
      status = clReleaseKernel( kernel );
      assert( status == CL_SUCCESS );
      status = clReleaseProgram( program );
      assert( status == CL_SUCCESS );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
  }

  void opencl_diamonds()
  {
    try
    {
      F[] = 0;
      foreach ( r; 1 .. rows ) F[r * cols] = -penalty * r;
      foreach ( c; 0 .. cols ) F[c       ] = -penalty * c;
      char[] code = q{
          #define BLOCK_SIZE 16
          int max3( int a, int b, int c );
          int max3( int a, int b, int c )
          {
            int k = a > b ? a : b;
            return k > c ? k : c;
          }
          __kernel void diamond( __global const int* S, __global int* F,
                                 int Y, int X, int cols, int penalty,
                                 __local int* s, __local int* t )
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
      auto program = clCreateProgramWithSource( context, 1, strs.ptr, &size, &status );
      assert( status == CL_SUCCESS );
      status = clBuildProgram( program, 1, &device, "", null, null );
      assert( status == CL_SUCCESS );
      cl_kernel kernel = clCreateKernel( program, "diamond", &status );
      assert( status == CL_SUCCESS );

      StopWatch timer;
      timer.reset();
      timer.start();
      cl_mem bS = clCreateBuffer( context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, cl_int.sizeof * S.length, S.ptr, &status );
      assert( status == CL_SUCCESS );
      cl_mem bF = clCreateBuffer( context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR, cl_int.sizeof * F.length, F.ptr, &status );
      assert( status == CL_SUCCESS );
      clSetKernelArg( kernel, 0, cl_mem.sizeof, &bS      );
      clSetKernelArg( kernel, 1, cl_mem.sizeof, &bF      );
      clSetKernelArg( kernel, 4, cl_int.sizeof, &cols    );
      clSetKernelArg( kernel, 5, cl_int.sizeof, &penalty );
      size_t aS =  BLOCK_SIZE      *  BLOCK_SIZE      * cl_int.sizeof;
      size_t aT = (BLOCK_SIZE + 1) * (BLOCK_SIZE + 2) * cl_int.sizeof;
      clSetKernelArg( kernel, 6, aS, null );
      clSetKernelArg( kernel, 7, aT, null );
      size_t wgroup = BLOCK_SIZE;
      for ( int i = 0; i < rows / BLOCK_SIZE; ++i )
      {
        int Y = 0;
        size_t global = BLOCK_SIZE * (i + 1);
        clSetKernelArg( kernel, 2, cl_int.sizeof, &i );
        clSetKernelArg( kernel, 3, cl_int.sizeof, &Y );
        status = clEnqueueNDRangeKernel( queue, kernel, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS );

        Y = 1;
        clSetKernelArg( kernel, 3, cl_int.sizeof, &Y );
        status = clEnqueueNDRangeKernel( queue, kernel, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS );
      }
      for ( int i = 2; i <= cols / BLOCK_SIZE; ++i )
      {
        size_t global = rows - 1;
        int X = rows / BLOCK_SIZE - 1;
        clSetKernelArg( kernel, 2, cl_int.sizeof, &X );
        clSetKernelArg( kernel, 3, cl_int.sizeof, &i );
        status = clEnqueueNDRangeKernel( queue, kernel, 1, null, &global, &wgroup, 0, null, null );
        assert( status == CL_SUCCESS );
      }
      status = clEnqueueReadBuffer( queue, bF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null );
      assert( status == CL_SUCCESS );
      timer.stop();
      TickDuration ticks = timer.peek();
      writeln( "RHOMBUS  ", ticks.usecs / 1E6, "  [s]" );
      status = clReleaseMemObject( bS );
      assert( status == CL_SUCCESS );
      status = clReleaseMemObject( bF );
      assert( status == CL_SUCCESS );
      status = clReleaseKernel( kernel );
      assert( status == CL_SUCCESS );
      status = clReleaseProgram( program );
      assert( status == CL_SUCCESS );
    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
  }
}

int
main( string[] args )
{
  try
  {
    auto app = new Application( args );
    app.run();
  }
  catch ( Exception msg )
  {
    writeln( "NW: ", msg );
    return -1;
  }
  return 0;
}
