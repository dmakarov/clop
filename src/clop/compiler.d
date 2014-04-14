module clop.compiler;

import std.traits;

import pegged.grammar;

public import clop.grammar;
public import clop.runtime;

struct Compiler
{
  uint[string] symtable;
  uint[string] localtab;
  string declarations;
  string range;

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

  bool is_local( string sym )
  {
    return ( localtab.get( sym, 0 ) != 0 );
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

  string set_params()
  {
    string result = "string param;\n";
    uint i = 0;
    foreach( sym; symtable.keys )
    {
      if ( localtab.get( sym, 0 ) == 0 )
      {
        result ~= "param = mixin( set_kernel_param!(typeof(" ~ sym ~ "))(\"" ~ sym ~ "\") );\n";
        if ( i == 0 )
        {
          result ~= "string kernel_params = param;\n";
        }
        else
        {
          result ~= "if ( param != \"\" ) kernel_params ~= \", \" ~ param;\n";
        }
        ++i;
      }
    }
    result ~= "kernel_params ~= \", int diagonal\";\n";
    return result;
  }

  string set_args( string kernel )
  {
    string result = "uint clop_opencl_kernel_arg = 0;\n";
    foreach( sym; symtable.keys )
    {
      if ( localtab.get( sym, 0 ) == 0 )
      {
        result ~= "mixin( set_kernel_arg!(typeof(" ~ sym ~ "))(\"" ~
        kernel ~ "\",\"clop_opencl_kernel_arg\",\"" ~ sym ~ "\") );\n";
      }
    }
    return result;
  }

  string code_to_read_data_from_device()
  {
    string result = "";
    foreach( sym; symtable.keys )
    {
      if ( localtab.get( sym, 0 ) == 0 )
      {
        result ~= "mixin( read_device_buffer!(typeof(" ~ sym ~ "))(\"" ~ sym ~ "\") );\n";
      }
    }
    return result;
  }

  string code_to_invoke_kernel()
  {
    return "foreach ( clop_diagonal; 2 .. 2 * (" ~ range ~ ") - 1 )
    {
      size_t global = (clop_diagonal < (" ~ range ~ ")) ? clop_diagonal - 1 : 2 * (" ~ range ~ ") - clop_diagonal - 1;
      clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg, cl_int.sizeof, &clop_diagonal );
      runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
      assert( runtime.status == CL_SUCCESS, \"clEnqueueNDRangeKernel failed.\" );
    }\n";
  }
}

immutable string
compile( immutable string expr )
{
  auto c = Compiler();
  auto p = CLOP( expr );
  bool internal = false;
  bool global_scope = false;

  string value( ParseTree p )
  {
    switch ( p.name )
    {
    case "CLOP":
      return value( p.children[0] );
    case "CLOP.Identifier":
      if ( global_scope && !c.is_local( p.matches[0] ) )
        c.add_symbol( p.matches[0] );
      else
        c.add_local( p.matches[0] );
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
      if ( p.children.length == 2 )
        return value( p.children[0] ) ~ " = " ~ value( p.children[1] );
      else
        return value( p.children[0] );
    case "CLOP.EqualityExpression":
    case "CLOP.RelationalExpression":
      string s = value( p.children[0] );
      for ( auto i = 1; i < p.children.length; i += 2 )
        s ~= " " ~ p.children[i].matches[0] ~ " " ~ value( p.children[i + 1] );
      return s;
    case "CLOP.ANDExpression":
      string s = value( p.children[0] );
      foreach ( i; 1 .. p.children.length )
        s ~= "&" ~ value( p.children[i] );
      return s;
    case "CLOP.ExclusiveORExpression":
      string s = value( p.children[0] );
      foreach ( i; 1 .. p.children.length )
        s ~= "^" ~ value( p.children[i] );
      return s;
    case "CLOP.InclusiveORExpression":
      string s = value( p.children[0] );
      foreach ( i; 1 .. p.children.length )
        s ~= "|" ~ value( p.children[i] );
      return s;
    case "CLOP.LogicalANDExpression":
      string s = value( p.children[0] );
      foreach ( i; 1 .. p.children.length )
        s ~= "&&" ~ value( p.children[i] );
      return s;
    case "CLOP.LogicalORExpression":
      string s = value( p.children[0] );
      foreach ( i; 1 .. p.children.length )
        s ~= "||" ~ value( p.children[i] );
      return s;
    case "CLOP.ConditionalExpression":
      if ( p.children.length == 3 )
        return "(" ~ value( p.children[0] ) ~ " ? " ~ value( p.children[1] ) ~
                                              " : " ~ value( p.children[2] ) ~ ")";
      else
        return value( p.children[0] );
    case "CLOP.ExpressionStatement":
      string s = ( p.children.length > 0 ) ? value( p.children[0] ) ~ ";" : ";";
      return s;
    case "CLOP.ReturnStatement":
      return "return " ~ value( p.children[0] ) ~ ";";
    case "CLOP.Statement":
      return value( p.children[0] );
    case "CLOP.StatementList":
      string s = "";
      foreach ( c; p.children ) s ~= "  " ~ value( c ) ~ "\n";
      return s;
    case "CLOP.CompoundStatement":
      string s = "{\n";
      foreach ( c; p.children ) s ~= value( c );
      return s ~ "}\n";
    case "CLOP.RangeSpec":
      c.add_local( p.matches[0] );
      c.range = p.matches[4].dup;
      /+
      string s = "foreach (";
      foreach (c ; p.matches) s ~= " " ~ c;
      return s ~ " )\n";
      +/
      string result;
      if ( internal )
        result = "  int " ~ p.matches[0] ~ " = (diagonal >= cols) ? diagonal - cols + tx + 1 : tx + 1;\n";
      else
        result = "  int " ~ p.matches[0] ~ " = (diagonal >= cols) ? cols - tx - 1 : diagonal - tx - 1;\n";
      internal = true;
      return result;
    case "CLOP.RangeList":
      string s = value( p.children[0] );
      for ( auto c = 1; c < p.children.length; ++c )
        s ~= value( p.children[c] );
      return s;
    case "CLOP.RangeDecl":
      return "{\n  int tx = get_global_id( 0 );\n" ~ value( p.children[0] );
    case "CLOP.TypeSpecifier":
      return p.matches[0];
    case "CLOP.ParameterDeclaration":
      return value( p.children[0] ) ~ " " ~ value( p.children[1] );
    case "CLOP.ParameterList":
      string s = value( p.children[0] );
      foreach ( i; 1 .. p.children.length )
        s ~= ", " ~ value( p.children[i] );
      return s;
    case "CLOP.Initializer":
      return value( p.children[0] );
    case "CLOP.InitDeclarator":
      string s = value( p.children[0] );
      if ( p.children.length > 1 )
        s ~= " = " ~ value( p.children[1] );
      return s;
    case "CLOP.InitDeclaratorList":
      string s = value( p.children[0] );
      foreach ( i; 1 .. p.children.length )
        s ~= ", " ~ value( p.children[i] );
      return s;
    case "CLOP.Declarator":
      string s = value( p.children[0] );
      if ( p.children.length > 1 )
        s ~= "( " ~ value( p.children[1] ) ~ " )";
      return s;
    case "CLOP.Declaration":
      string s = value( p.children[0] );
      if ( p.children.length > 1 )
        s ~= " " ~ value( p.children[1] );
      return s ~ ";";
    case "CLOP.DeclarationList":
      string s = "";
      foreach ( c; p.children )
        s ~= "  " ~ value( c ) ~ "\n";
      return s;
    case "CLOP.FunctionDefinition":
      return value( p.children[0] ) ~ " " ~ value( p.children[1] ) ~ " " ~ value( p.children[2] );
    case "CLOP.ExternalDeclaration":
      return value( p.children[0] );
    case "CLOP.ExternalDeclarations":
      string s = "";
      foreach ( c; p.children )
        s ~= value( c );
      return s;
    case "CLOP.TranslationUnit":
      if ( p.children.length == 2 )
      {
        global_scope = true;
        return value( p.children[0] ) ~ value( p.children[1] ) ~ "}";
      }
      else
      {
        //foreach ( m; p.children[0].matches ) c.declarations ~= " " ~ m;
        c.declarations = value( p.children[0] );
        global_scope = true;
        return value( p.children[1] ) ~ value( p.children[2] ) ~ "}";
      }
    default: return p.name;
    }
  }

  string kernel = value( p );
  string params = c.set_params();
  string code = params ~ "char[] clop_opencl_program_source = (q{\n" ~ c.declarations ~
  "} ~ \"__kernel void clop_opencl_kernel_main( \" ~ kernel_params ~ \" )\" ~\n";
  code ~= "q{\n" ~ kernel ~ "\n}).dup;" ~ q{
  writeln( "OpenCL program:\n", clop_opencl_program_source, "EOF" );
  size_t clop_opencl_program_source_size = clop_opencl_program_source.length;
  char* clop_opencl_program_source_pointer = clop_opencl_program_source.ptr;
  auto program = clCreateProgramWithSource( runtime.context, 1, &clop_opencl_program_source_pointer,
                                            &clop_opencl_program_source_size, &runtime.status );
  assert( runtime.status == CL_SUCCESS, "clCreateProgramWithSource failed." );
  runtime.status = clBuildProgram( program, 1, &runtime.device, "", null, null );
  if ( runtime.status != CL_SUCCESS )
  {
    char[1024] log;
    clGetProgramBuildInfo( program, runtime.device, CL_PROGRAM_BUILD_LOG, 1024, log.ptr, null );
    writeln( log );
  }
  assert( runtime.status == CL_SUCCESS, "clBuildProgram failed." );
  auto clop_opencl_kernel = clCreateKernel( program, "clop_opencl_kernel_main", &runtime.status );
  assert( runtime.status == CL_SUCCESS, "clCreateKernel failed." );
  } ~ c.set_args( "clop_opencl_kernel" ) ~ c.code_to_invoke_kernel() ~ c.code_to_read_data_from_device();
  return "writefln( \"DSL MIXIN:\\n%sEOD\", q{" ~ code ~ "} );\n" ~ code;
}

immutable string
set_kernel_param( T )( string name )
{
  string code;
  static      if ( is ( T == bool ) )
    code = "\"int " ~ name ~ "\"";
  else static if ( is ( T == int ) )
    code = "\"int " ~ name ~ "\"";
  else static if ( is ( T == uint ) )
    code = "\"uint " ~ name ~ "\"";
  else static if ( isDynamicArray!T )
    code = "\"__global \" ~ typeid( *" ~ name ~ ".ptr ).toString() ~ \"* " ~ name ~ "\"";
  else
    code = "\"\"";
  return code;
}

immutable string
set_kernel_arg( T )( string kernel, string arg, string name )
{
  string code;
  static      if ( is ( T == bool ) )
    code = "clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_char.sizeof, &" ~ name ~ " );";
  else static if ( is ( T == int ) )
    code = "clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_int.sizeof, &" ~ name ~ " );";
  else static if ( is ( T == uint ) )
    code = "clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_uint.sizeof, &" ~ name ~ " );";
  else static if ( isDynamicArray!T )
    code = "cl_mem clop_opencl_device_buffer_" ~ name ~
           " = clCreateBuffer( runtime.context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, typeid( *"
           ~ name ~ ".ptr ).tsize * " ~ name ~ ".length, "
           ~ name ~ ".ptr, &runtime.status );\nclSetKernelArg( "
           ~ kernel ~ ", " ~ arg ~ "++, cl_mem.sizeof, &clop_opencl_device_buffer_" ~ name ~ " );";
  else
    code = "";
  return "writefln( \"%s\\n\", q{" ~ code ~ "} );\n" ~ code;
}

immutable string
read_device_buffer( T )( string name )
{
  string code;
  static if ( isDynamicArray!T )
    code = "runtime.status = clEnqueueReadBuffer( runtime.queue, clop_opencl_device_buffer_"
           ~ name ~ ", CL_TRUE, 0, typeid( *" ~ name ~ ".ptr ).tsize * " ~ name ~ ".length, " ~ name ~
           ".ptr, 0, null, null );\nassert( runtime.status == CL_SUCCESS, \"clEnqueueReadBuffer failed.\" );";
  else
    code = "";
  return "writefln( \"%s\\n\", q{" ~ code ~ "} );\n" ~ code;
}
