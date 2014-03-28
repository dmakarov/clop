import std.stdio;

template
Kernel( string name, string code )
{
  const char[] Kernel = "writeln( q{__kernel void " ~ name ~ "()\n{" ~ code ~ "}\n} );";
}

int
main( string[] args )
{
  mixin( Kernel!( "kernel",
  q{
    c[tx] = a[tx] + b[tx];
  }));
  return 0;
}
