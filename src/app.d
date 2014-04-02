import opencl.all;
import pegged.grammar;

import std.stdio;
import std.string;

template
Kernel( string name, string code )
{
  const char[] Kernel = "writeln( q{__kernel void " ~ name ~ "()\n{" ~ code.outdent() ~ "}\n} );";
}

mixin( grammar(q{
CLOP:
    Term     < Factor (Add / Sub)*
    Add      < "+" Factor
    Sub      < "-" Factor
    Factor   < Primary (Mul / Div)*
    Mul      < "*" Primary
    Div      < "/" Primary
    Primary  < Parens / Neg / Number / Variable
    Parens   < :"(" Term :")"
    Neg      < "-" Primary
    Number   < ~([0-9]+)
    Variable <- identifier
}));

float
interpreter( string expr )
{
  auto p = CLOP( expr );

  float value( ParseTree p )
  {
      switch ( p.name )
      {
      case "CLOP":         return value(p.children[0]);
      case "CLOP.Term":
          float v = 0.0;
          foreach(child; p.children) v += value(child);
          return v;
      case "CLOP.Add":     return value(p.children[0]);
      case "CLOP.Sub":     return -value(p.children[0]);
      case "CLOP.Factor":
          float v = 1.0;
          foreach(child; p.children) v *= value(child);
          return v;
      case "CLOP.Mul":     return value(p.children[0]);
      case "CLOP.Div":     return 1.0/value(p.children[0]);
      case "CLOP.Primary": return value(p.children[0]);
      case "CLOP.Parens":  return value(p.children[0]);
      case "CLOP.Neg":     return -value(p.children[0]);
      case "CLOP.Number":  return to!float(p.matches[0]);
      default: return float.nan;
      }
  }

  return value(p);
}

string toString( float v )
{
  int d = to!(int)(v);
  static if ( false )
  return format( "%f", v );
  else
  switch( d )
  {
  case 0: return "0";
  case 1: return "1";
  case 2: return "2";
  case 3: return "3";
  case 4: return "4";
  case 5: return "5";
  case 6: return "6";
  case 7: return "7";
  case 8: return "8";
  case 9: return "9";
  default: return "";
  }
}

string result( string expr )
{
  static if (true)
  return "\"writefln( \\\"%d\\\", " ~ toString( interpreter( expr ) ) ~ " );\"";
  else
  return "writefln( \\\"%d\\\", " ~ toString( interpreter( expr ) ) ~ " );";
}

int
main( string[] args )
{

  mixin( Kernel!( "kernel",
  q{
       c[tx] = a[tx] + b[tx];
   } ) );
  static if ( true )
  {
    string s = mixin( result( q{
          (3 + 5 - 1 + 2) / 3
    } ) );
    writefln( "%s", s );
  }
  else
  mixin( result(
       q{
          (3 + 5 - 1 + 2) / 3
        } ) );
  return 0;
}
