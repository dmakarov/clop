import opencl.all;

import std.conv;
import std.datetime;
import std.getopt;
import std.random;
import std.stdio;

class Application {
  static immutable DEBUG = false;
  static immutable int CHARS =   24;
  static immutable int LIMIT = -999;
  static immutable int SEED  =    1;
  static immutable int BLOCK_SIZE = 16;
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
  [-4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4,  1]
                                                 ];
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
  CLContext context;
  CLCommandQueue queue;

  this( string[] args )
  {
    getopt( args, "output|o", &outfile );
    rows    = to!(int)( args[1] ) + 1;
    cols    = to!(int)( args[1] ) + 1;
    penalty = to!(int)( args[2] );
    if ( ( rows - 1 ) % 16 != 0 )
    {
      throw new Exception( "ERROR the dimension value (" ); // << rows << ") must be a multiple of " << BLOCK;
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

    CLDevices devices;
    CLPlatforms platforms = CLHost.getPlatforms();
    if ( platforms.length < 1 )
    {
      writeln( "No platforms available." );
      return;
    }
    foreach ( CLPlatform platform; platforms )
    {
      static if ( DEBUG )
        writeln( "Platform ", platform.name, "\nvendor\t\t", platform.vendor, ", ", platform.clversion, "\nprofile\t\t", platform.profile, "\nextensions\t", platform.extensions, "\n" );
      devices = platform.allDevices;
      if ( devices.length < 1 )
      {
        writeln( "No devices available." );
        return;
      }
      static if ( DEBUG )
        foreach ( CLDevice device; devices )
        {
          writeln( "Device ", device.name );
          writeln( "vendor\t\t", device.vendor, " ", device.driverVersion, ", ", device.clVersion, "\nprofile\t\t", device.profile, "\nextensions\t", device.extensions, "\n" );
        }
      }
    context = CLContext( platforms[0], CL_DEVICE_TYPE_GPU );
    queue = CLCommandQueue( context, devices[1] );
  }

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

  int maximum( int a, int b, int c )
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
        F[r * cols + c] = maximum( m, d, i );
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

  void diamond_blocks_debug( int Y, int X  )
  {
    int t[BLOCK_SIZE + 1][BLOCK_SIZE + 2];
    int s[BLOCK_SIZE][BLOCK_SIZE];

    static if ( DEBUG )
    foreach ( i; 0 .. BLOCK_SIZE + 1 )
      foreach ( j; 0 .. BLOCK_SIZE + 2 )
        Z[i][j] = 0;

    foreach ( int bx; 0 .. Y + 1 )
    {
      int xx = 2 * bx + X;
      int yy = Y - bx;

      static if ( DEBUG ) writeln( "block #", bx, " [", yy, ",", xx, "]" );

      int index[BLOCK_SIZE];
      int index_n[BLOCK_SIZE];
      int index_w[BLOCK_SIZE];
      int index_nw[BLOCK_SIZE];

      if ( xx == 0 )
      {
        static if ( DEBUG ) writeln( "case 1" );
        // 1.
        foreach ( int tx; 0 .. BLOCK_SIZE )
        {
          index[tx]    = cols * BLOCK_SIZE * yy + tx * cols + cols + 1;
          index_w[tx]  = cols * BLOCK_SIZE * yy + tx * cols + cols;
          index_n[tx]  = cols * BLOCK_SIZE * yy + tx * cols + 1;
          index_nw[tx] = cols * BLOCK_SIZE * yy + tx * cols;

          static if ( DEBUG ) writeln( "thread ", tx, " index ", index[tx], " [", index[tx] / cols, ",", index[tx] % cols, "]" );

          if ( tx == 0 )
          {
            static if ( DEBUG )
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
          static if ( DEBUG )
          {
            writeln( "t[", 0, "][", tx + 1, "] <- F[", (index_n[0] + tx) / cols, "][", (index_n[0] + tx) % cols, "]" );
            I[index_n[0] + tx] += 1;
            Z[0][tx + 1] += 1;
          }

          t[0][tx + 1] = F[index_n[0] + tx];

          static if ( DEBUG )
          {
            writeln( "t[", tx + 1, "][", 0, "] <- F[", (index_w[tx]) / cols, "][", (index_w[tx]) % cols, "]" );
            I[index_w[tx]] += 1;
            Z[tx + 1][0] += 1;
          }

          t[tx + 1][0] = F[index_w[tx]];

          static if ( DEBUG )
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

              t[y][x] = maximum( t[y-1][x-1] + s[y-1][x-1], t[y-1][x] - penalty, t[y][x-1] - penalty );

              static if ( DEBUG )
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
            static if ( DEBUG ) O[index[tx] + ty] += 1;
          }
      }
      else if ( xx < cols / BLOCK_SIZE )
      {
        static if ( DEBUG ) writeln( "case 2" );
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
            static if ( DEBUG )
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
          static if ( DEBUG )
          {
            writeln( "t[", 0, "][", tx + 1, "] <- F[", (index_n[0] + tx) / cols, "][", (index_n[0] + tx) % cols, "]" );
            I[index_n[0] + tx] += 1;
            Z[0][tx + 1] += 1;
          }

          t[0][tx + 1] = F[index_n[0] + tx];

          static if ( DEBUG )
          {
            writeln( "t[", tx + 1, "][", 0, "] <- F[", (index_w[tx] - 1) / cols, "][", (index_w[tx] - 1) % cols, "]" );
            I[index_w[tx] - 1] += 1;
            Z[tx + 1][0] += 1;
          }

          t[tx + 1][0] = F[index_w[tx] - 1];

          static if ( DEBUG )
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

            static if ( DEBUG )
            {
              writeln( "t[", y, "][", x, "] <- t[", y-1, "][", x-2, "], t[", y-1, "][", x-1, "], t[", y, "][", x-1, "]" );
              Z[y][x] += 1;
            }

            t[y][x] = maximum( t[y-1][x-2] + s[y-1][x-2], t[y-1][x-1] - penalty, t[y][x-1] - penalty );
          }
        // 4.
        foreach ( int tx; 0 .. BLOCK_SIZE )
          foreach ( int ty; 0 .. BLOCK_SIZE )
          {
            F[index[tx] + ty] = t[tx+1][ty+2];
            static if ( DEBUG ) O[index[tx] + ty] += 1;
          }
      }
      else if ( xx == cols / BLOCK_SIZE )
      {
        static if ( DEBUG ) writeln( "case 3" );
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
          static if ( DEBUG )
          {
            writeln( "thread ", tx, " read F[", (index_w[tx] - 1) / cols, ",", (index_w[tx] - 1) % cols, "]" );
            I[index_w[tx] - 1] += 1;
            Z[tx + 1][0] += 1;
          }

          t[tx + 1][0] = F[index_w[tx] - 1];

          static if ( DEBUG )
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

              t[y][x] = maximum( t[y-1][x-2] + s[y-1][x-2], t[y-1][x-1] - penalty, t[y][x-1] - penalty );
              static if ( DEBUG )
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
            static if ( DEBUG ) O[index[tx] + ty] += 1;
          }
      }
      else continue;
      static if ( DEBUG ) dump();
    }
  }

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

              t[y][x] = maximum( t[y-1][x-1] + s[y-1][x-1], t[y-1][x] - penalty, t[y][x-1] - penalty );
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

            t[y][x] = maximum( t[y-1][x-2] + s[y-1][x-2], t[y-1][x-1] - penalty, t[y][x-1] - penalty );
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

              t[y][x] = maximum( t[y-1][x-2] + s[y-1][x-2], t[y-1][x-1] - penalty, t[y][x-1] - penalty );
            }
          }
        // 4.
        foreach ( int tx; 0 .. BLOCK_SIZE )
          foreach ( int ty; 0 .. tx )
            F[index[tx] + ty] = t[tx+1][ty+2];
      }
    }
  }

  void usingdiamonds()
  {
    for ( int i = 0; i < rows / BLOCK_SIZE; ++i )
    {
      diamond_blocks( i, 0 );

      static if ( DEBUG )
      {
        foreach ( int j; 0 .. 2 * cols ) write( "--" ); writeln();
      }
      diamond_blocks( i, 1 );

      static if ( DEBUG )
      {
        foreach ( int j; 0 .. 2 * cols ) write( "==" ); writeln();
      }
    }
    for ( int i = 2; i <= cols / BLOCK_SIZE; ++i )
    {
      diamond_blocks( rows / BLOCK_SIZE - 1, i );

      static if ( DEBUG )
      {
        foreach ( int j; 0 .. 2 * cols ) write( "==" ); writeln();
      }
    }
  }

  void serial_kernel( int i, bool is_upper )
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

              t[y][x] = maximum( t[y-1][x-1] + s[y-1][x-1], t[y][x-1] - penalty, t[y-1][x] - penalty );
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

              t[y][x] = maximum( t[y-1][x-1] + s[y-1][x-1], t[y][x-1] - penalty, t[y-1][x] - penalty );
            }
          }
        // 5.
        foreach ( int tx; 0 .. BLOCK_SIZE )
          foreach ( int ty; 0 .. BLOCK_SIZE )
            F[index[tx] + ty * cols] = t[ty+1][tx+1];
      }
  }

  void upperleftbottomright()
  {
    immutable int block_width = ( cols - 1 ) / BLOCK_SIZE;
    for ( int i = 1; i <= block_width; ++i )     serial_kernel( i, true );
    for ( int i = block_width - 1; i >= 1; --i ) serial_kernel( i, false );
  }

  void thread_kernel( int i, bool is_upper )
  {
    immutable int block_width = ( cols - 1 ) / BLOCK_SIZE;
    int bx = 0;
    int tx = 0;
    int t[BLOCK_SIZE + 1][BLOCK_SIZE + 1];
    int s[BLOCK_SIZE][BLOCK_SIZE];
    // 1.
    int xx = ( is_upper ) ?         bx : block_width + bx - i;
    int yy = ( is_upper ) ? i - 1 - bx : block_width - bx - 1;
    int index    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx + cols + 1;
    int index_n  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx + 1;
    int index_w  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + cols;
    int index_nw = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx;

    if ( tx == 0 ) t[tx][0] = F[index_nw];

    for ( int ty = 0 ; ty < BLOCK_SIZE ; ty++) s[ty][tx] = S[index + cols * ty];

    //barrier();

    // 2.
    t[tx + 1][0] = F[index_w + cols * tx];
    t[0][tx + 1] = F[index_n];

    //barrier();

    // 3.
    for ( int m = 0; m < BLOCK_SIZE; ++m )
    {
      if ( tx <= m )
      {
        int x =  tx + 1;
        int y =  m - tx + 1;

        t[y][x] = maximum( t[y-1][x-1] + s[y-1][x-1], t[y][x-1] - penalty, t[y-1][x] - penalty );
      }
      //barrier();
    }

    // 4.
    for ( int m = BLOCK_SIZE - 2; m >=0; --m )
    {
      if ( tx <= m )
      {
        int x =  tx + BLOCK_SIZE - m ;
        int y =  BLOCK_SIZE - tx;

        t[y][x] = maximum( t[y-1][x-1] + s[y-1][x-1], t[y][x-1] - penalty, t[y-1][x] - penalty );
      }
      //barrier();
    }

    // 5.
    foreach ( int ty; 0 .. BLOCK_SIZE ) F[index + ty * cols] = t[ty+1][tx+1];
  }

  void compute_scores_parallel( bool is_upper )
  {
    immutable int block_width = ( cols - 1 ) / BLOCK_SIZE;
    for ( int i = 1; i <= block_width;     ++i ) thread_kernel( i, true  );
    for ( int i = block_width - 1; i >= 1; --i ) thread_kernel( i, false );
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
    upperleftbottomright();
    timer.stop();
    ticks = timer.peek();
    writeln( "BLOCKED  ", ticks.usecs / 1E6, " [s]" );
    timer.reset();
    timer.start();
    usingdiamonds();
    timer.stop();
    ticks = timer.peek();
    writeln( "DIAMONDS ", ticks.usecs / 1E6, " [s]" );
    G[] = F[];
    timer.reset();
    timer.start();
    //compute_scores_parallel();
    compute_scores_serial();
    timer.stop();
    ticks = timer.peek();
    writeln( "SERIAL   ", ticks.usecs / 1E6, " [s]" );
    int diff = 0;
    foreach( ii; 0 .. F.length ) if ( F[ii] != G[ii] ) ++diff;
    if ( diff > 0 ) writeln( "DIFFs ", diff );
    //compute_alignments();
    opencl_diamonds();
    diff = 0;
    foreach( ii; 0 .. F.length ) if ( F[ii] != G[ii] ) ++diff;
    if ( diff > 0 ) writeln( "DIFFs ", diff );
    opencl_rectangles();
    diff = 0;
    foreach( ii; 0 .. F.length ) if ( F[ii] != G[ii] ) ++diff;
    if ( diff > 0 ) writeln( "DIFFs ", diff );
    save();
  }

  void opencl_rectangles()
  {
    try
    {
      F[] = 0;
      foreach ( r; 1 .. rows ) F[r * cols] = -penalty * r;
      foreach ( c; 0 .. cols ) F[c       ] = -penalty * c;
      auto program = context.createProgram( mixin( CL_PROGRAM_STRING_DEBUG_INFO ) ~ q{
          #define BLOCK_SIZE 16
          int maximum( int a, int b, int c );
          int maximum( int a, int b, int c )
          {
            int k = a > b ? a : b;
            return k > c ? k : c;
          }
          __kernel void rectangles( __global const int* S, __global int* F, int cols, int penalty, int i, int is_upper, __local int* s, __local int* t )
          {
            int bx = get_group_id( 0 );
            int tx = get_local_id( 0 );
            int xx =         bx;
            if ( is_upper == 1 ) xx = (cols - 1) / BLOCK_SIZE + bx - i;
            int yy = i - 1 - bx;
            if ( is_upper == 1 ) yy = (cols - 1) / BLOCK_SIZE - bx - 1;
            // 1.
            int index    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx + cols + 1;
            int index_n  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx + 1;
            int index_w  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + cols;
            int index_nw = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx;

            if ( tx == 0 ) t[0] = F[index_nw];
            for ( int ty = 0; ty < BLOCK_SIZE; ++ty ) s[ty * BLOCK_SIZE + tx] = S[ty * cols + index];

            t[(tx + 1) * (BLOCK_SIZE + 1)] = F[tx * cols + index_w];
            t[(tx + 1)]                    = F[index_n];
            barrier( CLK_LOCAL_MEM_FENCE );
            // 2.
            for ( int m = 0; m < BLOCK_SIZE; ++m )
            {
              if ( tx <= m )
              {
                int x = tx + 1;
                int y = m - tx + 1;

                t[y * (BLOCK_SIZE + 1) + x] = maximum( t[(y-1) * (BLOCK_SIZE + 1) + x-1] + s[(y-1) * BLOCK_SIZE + x-1],
                                                       t[(y-1) * (BLOCK_SIZE + 1) + x  ] - penalty,
                                                       t[(y  ) * (BLOCK_SIZE + 1) + x-1] - penalty );
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

                t[y * (BLOCK_SIZE + 1) + x] = maximum( t[(y-1) * (BLOCK_SIZE + 1) + x-1] + s[(y-1) * BLOCK_SIZE + x-1],
                                                       t[(y-1) * (BLOCK_SIZE + 1) + x  ] - penalty,
                                                       t[(y  ) * (BLOCK_SIZE + 1) + x-1] - penalty );
              }
              barrier( CLK_LOCAL_MEM_FENCE );
            }
            // 4.
            for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
              F[ty * cols + index] = t[(ty + 1) * (BLOCK_SIZE + 1) + tx + 1];
          }
        } );
      program.build( "" );
      auto kernel = CLKernel( program, "rectangles" );
      StopWatch timer;
      timer.reset();
      timer.start();
      auto aS = LocalArgSize( BLOCK_SIZE * BLOCK_SIZE * cl_int.sizeof );
      auto aT = LocalArgSize( (BLOCK_SIZE + 1) * (BLOCK_SIZE + 1) * cl_int.sizeof );
      auto bS = CLBuffer( context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, cl_int.sizeof * S.length, S.ptr );
      auto bF = CLBuffer( context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR, cl_int.sizeof * F.length, F.ptr );
      kernel.setArg( 0, bS );
      kernel.setArg( 1, bF );
      kernel.setArg( 2, cols );
      kernel.setArg( 3, penalty );
      kernel.setArg( 6, aS );
      kernel.setArg( 7, aT );
      auto wgroup = NDRange( BLOCK_SIZE );
      for ( int i = 1; i <= (cols - 1) / BLOCK_SIZE; ++i )
      {
        auto global = NDRange( BLOCK_SIZE * i );
        kernel.setArg( 4, i );
        kernel.setArg( 5, 0 );
        queue.enqueueNDRangeKernel( kernel, global, wgroup );
      }
      for ( int i = (cols - 1) / BLOCK_SIZE - 1; i > 0; --i )
      {
        auto global = NDRange( BLOCK_SIZE * i );
        kernel.setArg( 4, i );
        kernel.setArg( 5, 1 );
        queue.enqueueNDRangeKernel( kernel, global, wgroup );
      }
      queue.enqueueReadBuffer( bF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr );
      timer.stop();
      TickDuration ticks = timer.peek();
      writeln( "BLOCKEDC ", ticks.usecs, " [us]" );
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
      auto program = context.createProgram( mixin( CL_PROGRAM_STRING_DEBUG_INFO ) ~ q{
          #define BLOCK_SIZE 16
          int maximum( int a, int b, int c );
          int maximum( int a, int b, int c )
          {
            int k = a > b ? a : b;
            return k > c ? k : c;
          }
          __kernel void diamond( __global const int* S, __global int* F, int Y, int X, int cols, int penalty, __local int* s, __local int* t )
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

                  t[y * (BLOCK_SIZE + 2) + x] = maximum( t[(y-1) * (BLOCK_SIZE + 2) + x-1] + s[(y-1) * BLOCK_SIZE + x-1],
                                                         t[(y-1) * (BLOCK_SIZE + 2) + x] - penalty,
                                                         t[y * (BLOCK_SIZE + 2) + x-1] - penalty );
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

                t[y * (BLOCK_SIZE + 2) + x] = maximum( t[(y-1) * (BLOCK_SIZE + 2) + x-2] + s[(y-1) * BLOCK_SIZE + x-2], t[(y-1) * (BLOCK_SIZE + 2) + x-1] - penalty, t[y * (BLOCK_SIZE + 2) + x-1] - penalty );
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

                  t[y * (BLOCK_SIZE + 2) + x] = maximum( t[(y-1) * (BLOCK_SIZE + 2) + x-2] + s[(y-1) * BLOCK_SIZE + x-2], t[(y-1) * (BLOCK_SIZE + 2) + x-1] - penalty, t[y * (BLOCK_SIZE + 2) + x-1] - penalty );
                }
                barrier( CLK_LOCAL_MEM_FENCE );
              }
              // 3.
              for ( int ty = 0; ty < tx; ++ty )
                F[index + ty] = t[(tx+1) * (BLOCK_SIZE + 2) + ty+2];
            }
          }
        } );
      program.build( "" );
      auto kernel = CLKernel( program, "diamond" );
      StopWatch timer;
      timer.reset();
      timer.start();
      auto aS = LocalArgSize( BLOCK_SIZE * BLOCK_SIZE * cl_int.sizeof );
      auto aT = LocalArgSize( (BLOCK_SIZE + 1) * (BLOCK_SIZE + 2) * cl_int.sizeof );
      auto bS = CLBuffer( context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, cl_int.sizeof * S.length, S.ptr );
      auto bF = CLBuffer( context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR, cl_int.sizeof * F.length, F.ptr );
      kernel.setArg( 0, bS );
      kernel.setArg( 1, bF );
      kernel.setArg( 4, cols );
      kernel.setArg( 5, penalty );
      kernel.setArg( 6, aS );
      kernel.setArg( 7, aT );
      auto wgroup = NDRange( BLOCK_SIZE );
      for ( int i = 0; i < rows / BLOCK_SIZE; ++i )
      {
        auto global = NDRange( BLOCK_SIZE * (i + 1) );
        kernel.setArg( 2, i );
        kernel.setArg( 3, 0 );
        queue.enqueueNDRangeKernel( kernel, global, wgroup );

        kernel.setArg( 3, 1 );
        queue.enqueueNDRangeKernel( kernel, global, wgroup );
      }
      for ( int i = 2; i <= cols / BLOCK_SIZE; ++i )
      {
        auto global = NDRange( rows - 1 );
        kernel.setArg( 2, rows / BLOCK_SIZE - 1);
        kernel.setArg( 3, i );
        queue.enqueueNDRangeKernel( kernel, global, wgroup );
      }
      queue.enqueueReadBuffer( bF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr );
      timer.stop();
      TickDuration ticks = timer.peek();
      writeln( "PARALLEL ", ticks.usecs, " [us]" );
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
    write( "nw: " );
    writeln( msg );
    return -1;
  }
  return 0;
}
