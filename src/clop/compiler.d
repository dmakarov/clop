module clop.compiler;

import std.traits;

import pegged.grammar;

public import clop.grammar;

struct Compiler
{
  uint[string] symtable;
  uint[string] localtab;

  void add_symbol( string symbol )
  {
    if ( symtable.length == 0 )
      symtable = [symbol:1];
    else if ( symtable.get( symbol, 0 ) == 0 )
      symtable[symbol] = 1;
    else
      ++symtable[symbol];
  }

  void add_local( string symbol )
  {
    if ( localtab.length == 0 )
      localtab = [symbol:1];
    else if ( localtab.get( symbol, 0 ) == 0 )
      localtab[symbol] = 1;
    else
      ++localtab[symbol];
  }

  string ct_itoa(T)(T x)
  {
    string s = "";
    static if ( is ( T == byte ) || is ( T == short ) || is ( T == int ) || is ( T == long ) )
    {
      if ( x < 0 )
      {
        s = "-";
        x = -x;
      }
    }
    do
    {
      s = cast(char)( '0' + ( x % 10 ) ) ~ s;
      x /= 10;
    } while ( x > 0 );
    return s;
  }

  string set_args( string kernel )
  {
    uint i = 0;
    string result = "uint arg = 0;\n";
    foreach( sym; symtable.keys )
    {
      if ( localtab.get( sym, 0 ) == 0 )
      {
        static if ( false )
        result ~= "mixin( set_kernel_arg!(" ~ sym ~ ")(\"" ~ kernel ~ "\",\"" ~ ct_itoa!(typeof(i))(i) ~ "\",\"" ~ sym ~ "\") );\n";
        else
        result ~= "mixin( set_kernel_arg!(typeof(" ~ sym ~ "))(\"" ~ kernel ~ "\",\"arg\",\"" ~ sym ~ "\") );\n";
        ++i;
      }
    }
    return result;
  }

  string set_params()
  {
    string result = "string param;\n";
    uint i = 0;
    foreach( sym; symtable.keys )
    {
      if ( localtab.get( sym, 0 ) == 0 )
      {
        result ~= "param = set_kernel_param!(typeof(" ~ sym ~ "))(\"" ~ sym ~ "\");\n";
        if ( i == 0 )
        {
          result ~= "string kernel_params = param;\n";
        }
        else
        {
          result ~= "if ( param != \"\" ) kernel_params ~= \",\" ~ param;\n";
        }
        ++i;
      }
    }
    return result;
  }
}

immutable string
compile( immutable string expr )
{
  auto c = Compiler();
  auto p = CLOP( expr );

  string value( ParseTree p )
  {
    switch ( p.name )
    {
    case "CLOP":
      return value( p.children[0] );
    case "CLOP.Identifier":
      c.add_symbol( p.matches[0] );
      return p.matches[0];
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
      return p.matches[0];
    case "CLOP.ArrayIndex":
      return "[" ~ value( p.children[0] ) ~ "]";
    case "CLOP.FunctionCall":
      if ( p.children.length > 0 )
        return "(" ~ value( p.children[0] ) ~ ")";
      return "()";
    case "CLOP.PrimaryExpr":
      string s = value( p.children[0] );
      if ( p.matches[0] == "(" ) return "(" ~ s ~ ")";
      return s;
    case "CLOP.Factor":
    case "CLOP.UnaryExpr":
    case "CLOP.Expression":
      string s = value( p.children[0] );
      for ( auto c = 1; c < p.children.length; ++c )
        s ~= value( p.children[c] );
      return s;
    case "CLOP.ArgumentList":
      string s = value( p.children[0] );
      for ( auto c = 1; c < p.children.length; ++c )
        s ~= "," ~ value( p.children[c] );
      return s;
    case "CLOP.MulExpr":
    case "CLOP.AddExpr":
      return p.matches[0] ~ value( p.children[0] );
    case "CLOP.AssignExpr":
      return value( p.children[0] ) ~ "=" ~ value( p.children[1] );
    case "CLOP.Statement":
      return value( p.children[0] ) ~ ";";
    case "CLOP.StatementList":
      string s = "\n";
      foreach ( c; p.children ) s ~= "  " ~ value( c ) ~ "\n";
      return s ~ "}";
    case "CLOP.RangeSpec":
      c.add_local( p.matches[0] );
      /+
      string s = "foreach (";
      foreach (c ; p.matches) s ~= " " ~ c;
      return s ~ " )\n";
      +/
      return "  int " ~ p.matches[0] ~ " = 0;\n";
    case "CLOP.RangeList":
      string s = value( p.children[0] );
      for ( auto c = 1; c < p.children.length; ++c )
        s ~= value( p.children[c] );
      return s;
    case "CLOP.RangeDecl":
      return "{\n" ~ value( p.children[0] );
    case "CLOP.TranslationUnit":
      return value( p.children[0] ) ~ value( p.children[1] );
    default: return p.name;
    }
  }

  string kernel = value( p );
  string params = c.set_params();
  string max3 = q{
  int max3( int a, int b, int c );
  int max3( int a, int b, int c )
  {
    int k = a > b ? a : b;
    return k > c ? k : c;
  }
  };
  string code = params ~ "char[] code = (q{\n" ~ max3 ~ "} ~ \"__kernel void kernel1(\" ~ kernel_params ~ \")\" ~\n";
  code ~= "q{\n" ~ kernel ~ "\n}).dup;" ~ q{
  writeln( code );
  size_t size = code.length;
  char*[] strs = [code.ptr];
  auto program = clCreateProgramWithSource( context, 1, strs.ptr, &size, &status );
  assert( status == CL_SUCCESS, "clCreateProgramWithSource" );
  status = clBuildProgram( program, 1, &device, "", null, null );
  if ( status != CL_SUCCESS )
  {
    char[1024] log;
    clGetProgramBuildInfo( program, device, CL_PROGRAM_BUILD_LOG, 1024, log.ptr, null );
    writeln( log );
  }
  assert( status == CL_SUCCESS, "clBuildProgram" );
  cl_kernel kernel = clCreateKernel( program, "kernel1", &status );
  assert( status == CL_SUCCESS, "clCreateKernel" );
  } ~ c.set_args( "kernel" );
  return "writefln( \"Generated code:\\n%s\\n\", q{" ~ code ~ "} );\n" ~ code;
}

// clSetKernelArg( kernel, 2, cl_int.sizeof, &cols );
// set_kernel_arg(alias expr)( string kernel, string arg, string name )
immutable string
set_kernel_arg( T )( string kernel, string arg, string name )
{
  string code;
  static if ( false )
  {
  static      if ( is ( typeof( expr ) == bool ) )
    code = "clSetKernelArg(" ~ kernel ~ "," ~ arg  ~ "++,cl_char.sizeof,&" ~ name ~ ");";
  else static if ( is ( typeof( expr ) == int ) )
    code = "clSetKernelArg(" ~ kernel ~ "," ~ arg  ~ "++,cl_int.sizeof,&" ~ name ~ ");";
  else static if ( is ( typeof( expr ) == uint ) )
    code = "clSetKernelArg(" ~ kernel ~ "," ~ arg  ~ "++,cl_uint.sizeof,&" ~ name ~ ");";
  else
    code = "";
  } else {
  static      if ( is ( T == bool ) )
    code = "clSetKernelArg(" ~ kernel ~ "," ~ arg  ~ "++,cl_char.sizeof,&" ~ name ~ ");";
  else static if ( is ( T == int ) )
    code = "clSetKernelArg(" ~ kernel ~ "," ~ arg  ~ "++,cl_int.sizeof,&" ~ name ~ ");";
  else static if ( is ( T == uint ) )
    code = "clSetKernelArg(" ~ kernel ~ "," ~ arg  ~ "++,cl_uint.sizeof,&" ~ name ~ ");";
  else static if ( isDynamicArray!T )
    code = "cl_mem b" ~ name ~ " = clCreateBuffer( context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR, typeid( *"
           ~ name ~ ".ptr ).tsize * " ~ name ~ ".length, " ~ name ~ ".ptr, &status );\nclSetKernelArg( "
           ~ kernel ~ ", " ~ arg ~ "++, cl_mem.sizeof, &b" ~ name ~ " );";
  else
    code = "";
  }
  return "writefln( \"%s\\n\", q{" ~ code ~ "} );\n" ~ code;
}

immutable string
set_kernel_param( T )( string name )
{
  string code;
  static      if ( is ( T == bool ) )
    code = "int " ~ name;
  else static if ( is ( T == int ) )
    code = "int " ~ name;
  else static if ( is ( T == uint ) )
    code = "uint " ~ name;
  else static if ( isDynamicArray!T )
    code = "__global int* " ~ name;
  else
    code = "";
  return code;
}
