module clop.app;

import std.conv;
import std.random;
import std.stdio;
import std.string;

import derelict.opencl.cl;

import clop.compiler;

/**
*/
class Application
{
  static immutable int SEED  =  1;
  static immutable int BLOCK = 16;
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

  int[] F;
  int[] G;
  int[] S;
  int rows;
  int cols;
  int penalty;

  this( string[] args )
  {
    rows    = to!(int)( args[1] ) + 1;
    cols    = to!(int)( args[1] ) + 1;
    penalty = to!(int)( args[2] );
    if ( ( rows - 1 ) % BLOCK != 0 )
    {
      auto r = to!(string)(rows - 1);
      auto b = to!(string)(BLOCK);
      throw new Exception( "ERROR: rows # (" ~ r ~ ") must be a multiple of " ~ b );
    }
    Mt19937 gen;
    gen.seed( SEED );
    S = new int[rows * cols]; assert( null != S );
    F = new int[rows * cols]; assert( null != F );
    G = new int[rows * cols]; assert( null != G );
    auto M = new int[rows]; assert( null != M );
    auto N = new int[cols]; assert( null != N );
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
  }

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

  int max3( immutable int a, immutable int b, immutable int c ) const
  {
    immutable k = a > b ? a : b;
    return k > c ? k : c;
  }

  void run()
  {
    int x;
    // use CLOP DSL to generate the loops around the computation
    mixin( compile(
    q{
       NDRange( r ; 1 .. rows, c ; 1 .. cols );
       F[r * cols + c] = max3( F[(r - 1) * cols + c - 1] + S[r * cols + c],
                               F[(r - 1) * cols + c    ] - penalty,
                               F[ r      * cols + c - 1] - penalty );
     } ) );

    print( "The scores matrix:", F );

    // compute the same matrix explicitly
    foreach ( r; 1 .. rows )
      foreach ( c; 1 .. cols )
        G[r * cols + c] = max3( G[(r - 1) * cols + c - 1] + S[r * cols + c],
                                G[(r - 1) * cols + c    ] - penalty,
                                G[ r      * cols + c - 1] - penalty );

    // compare the results and count differences
    int diffs = 0;
    foreach ( r; 0 .. rows )
      foreach ( c; 0 .. cols )
        if ( F[r * cols + c] != G[r * cols + c] )
          ++diffs;

    writeln( "\nDIFFS ", diffs );
    if ( diffs != 0 ) print( "The correct results:", G );
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
