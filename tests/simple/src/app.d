module clop.tests.simple;

import std.stdio;

/**
 * simple test of compiler backdoor.
 */
int
main( string[] args )
{
  writefln( "Hello, %s!", __traits( backdoorForCLOP, "CLOP" ) );
  return 0;
}
