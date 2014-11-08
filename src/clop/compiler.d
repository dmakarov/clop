module clop.compiler;

import std.traits;

import pegged.grammar;

public import clop.grammar;
public import clop.runtime;

struct Compiler
{
  immutable string source;
  ParseTree AST;
  uint[string] symtable;
  uint[string] localtab;
  string declarations;
  string range;
  bool global_scope;
  bool internal;

  this( immutable string expr )
  {
    //    symtable.length = 0;
    //    localtab.length = 0;
    declarations = null;
    range = null;
    source = expr;
    internal = false;
    global_scope = false;
    AST = CLOP( source );
  }

  string generate()
  {
    return evaluate( AST );
  }

  string evaluate( ParseTree t )
  {
    switch ( t.name )
    {
    case "CLOP":
      return evaluate( t.children[0] );
    case "CLOP.Identifier":
      if ( global_scope && !is_local( t.matches[0] ) )
        add_symbol( t.matches[0] );
      else
        add_local( t.matches[0] );
      return t.matches[0];
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
      return t.matches[0];
    case "CLOP.ArrayIndex":
      return "[" ~ evaluate( t.children[0] ) ~ "]";
    case "CLOP.FunctionCall":
      if ( t.children.length > 0 )
        return "(" ~ evaluate( t.children[0] ) ~ ")";
      return "()";
    case "CLOP.PrimaryExpr":
      string s = evaluate( t.children[0] );
      if ( t.matches[0] == "(" ) return "(" ~ s ~ ")";
      return s;
    case "CLOP.Factor":
    case "CLOP.UnaryExpr":
    case "CLOP.Expression":
      string s = evaluate( t.children[0] );
      for ( auto c = 1; c < t.children.length; ++c )
        s ~= evaluate( t.children[c] );
      return s;
    case "CLOP.ArgumentList":
      string s = evaluate( t.children[0] );
      for ( auto c = 1; c < t.children.length; ++c )
        s ~= "," ~ evaluate( t.children[c] );
      return s;
    case "CLOP.MulExpr":
    case "CLOP.AddExpr":
      return t.matches[0] ~ evaluate( t.children[0] );
    case "CLOP.AssignExpr":
      if ( t.children.length == 2 )
        return evaluate( t.children[0] ) ~ " = " ~ evaluate( t.children[1] );
      else
        return evaluate( t.children[0] );
    case "CLOP.EqualityExpression":
    case "CLOP.RelationalExpression":
      string s = evaluate( t.children[0] );
      for ( auto i = 1; i < t.children.length; i += 2 )
        s ~= " " ~ t.children[i].matches[0] ~ " " ~ evaluate( t.children[i + 1] );
      return s;
    case "CLOP.ANDExpression":
      string s = evaluate( t.children[0] );
      foreach ( i; 1 .. t.children.length )
        s ~= "&" ~ evaluate( t.children[i] );
      return s;
    case "CLOP.ExclusiveORExpression":
      string s = evaluate( t.children[0] );
      foreach ( i; 1 .. t.children.length )
        s ~= "^" ~ evaluate( t.children[i] );
      return s;
    case "CLOP.InclusiveORExpression":
      string s = evaluate( t.children[0] );
      foreach ( i; 1 .. t.children.length )
        s ~= "|" ~ evaluate( t.children[i] );
      return s;
    case "CLOP.LogicalANDExpression":
      string s = evaluate( t.children[0] );
      foreach ( i; 1 .. t.children.length )
        s ~= "&&" ~ evaluate( t.children[i] );
      return s;
    case "CLOP.LogicalORExpression":
      string s = evaluate( t.children[0] );
      foreach ( i; 1 .. t.children.length )
        s ~= "||" ~ evaluate( t.children[i] );
      return s;
    case "CLOP.ConditionalExpression":
      if ( t.children.length == 3 )
        return "(" ~ evaluate( t.children[0] ) ~ " ? " ~ evaluate( t.children[1] ) ~
                                              " : " ~ evaluate( t.children[2] ) ~ ")";
      else
        return evaluate( t.children[0] );
    case "CLOP.ExpressionStatement":
      string s = ( t.children.length > 0 ) ? evaluate( t.children[0] ) ~ ";" : ";";
      return s;
    case "CLOP.ReturnStatement":
      return "return " ~ evaluate( t.children[0] ) ~ ";";
    case "CLOP.Statement":
      return evaluate( t.children[0] );
    case "CLOP.StatementList":
      string s = "";
      foreach ( c; t.children ) s ~= "  " ~ evaluate( c ) ~ "\n";
      return s;
    case "CLOP.CompoundStatement":
      string s = "{\n";
      foreach ( c; t.children ) s ~= evaluate( c );
      return s ~ "}\n";
    case "CLOP.RangeSpec":
      add_local( t.matches[0] );
      range = t.matches[4].dup;
      /+
      string s = "foreach (";
      foreach (c ; t.matches) s ~= " " ~ c;
      return s ~ " )\n";
      +/
      string result;
      if ( internal )
        result = "  int " ~ t.matches[0] ~ " = (diagonal >= cols) ? diagonal - cols + tx + 1 : tx + 1;\n";
      else
        result = "  int " ~ t.matches[0] ~ " = (diagonal >= cols) ? cols - tx - 1 : diagonal - tx - 1;\n";
      internal = true;
      return result;
    case "CLOP.RangeList":
      string s = evaluate( t.children[0] );
      for ( auto c = 1; c < t.children.length; ++c )
        s ~= evaluate( t.children[c] );
      return s;
    case "CLOP.RangeDecl":
      return "{\n  int tx = get_global_id( 0 );\n" ~ evaluate( t.children[0] );
    case "CLOP.TypeSpecifier":
      return t.matches[0];
    case "CLOP.ParameterDeclaration":
      return evaluate( t.children[0] ) ~ " " ~ evaluate( t.children[1] );
    case "CLOP.ParameterList":
      string s = evaluate( t.children[0] );
      foreach ( i; 1 .. t.children.length )
        s ~= ", " ~ evaluate( t.children[i] );
      return s;
    case "CLOP.Initializer":
      return evaluate( t.children[0] );
    case "CLOP.InitDeclarator":
      string s = evaluate( t.children[0] );
      if ( t.children.length > 1 )
        s ~= " = " ~ evaluate( t.children[1] );
      return s;
    case "CLOP.InitDeclaratorList":
      string s = evaluate( t.children[0] );
      foreach ( i; 1 .. t.children.length )
        s ~= ", " ~ evaluate( t.children[i] );
      return s;
    case "CLOP.Declarator":
      string s = evaluate( t.children[0] );
      if ( t.children.length > 1 )
        s ~= "( " ~ evaluate( t.children[1] ) ~ " )";
      return s;
    case "CLOP.Declaration":
      string s = evaluate( t.children[0] );
      if ( t.children.length > 1 )
        s ~= " " ~ evaluate( t.children[1] );
      return s ~ ";";
    case "CLOP.DeclarationList":
      string s = "";
      foreach ( c; t.children )
        s ~= "  " ~ evaluate( c ) ~ "\n";
      return s;
    case "CLOP.FunctionDefinition":
      return evaluate( t.children[0] ) ~ " " ~ evaluate( t.children[1] ) ~ " " ~ evaluate( t.children[2] );
    case "CLOP.ExternalDeclaration":
      return evaluate( t.children[0] );
    case "CLOP.ExternalDeclarations":
      string s = "";
      foreach ( c; t.children )
        s ~= evaluate( c );
      return s;
    case "CLOP.TranslationUnit":
      if ( t.children.length == 2 )
      {
        global_scope = true;
        return evaluate( t.children[0] ) ~ evaluate( t.children[1] ) ~ "}";
      }
      else
      {
        //foreach ( m; t.children[0].matches ) c.declarations ~= " " ~ m;
        declarations = evaluate( t.children[0] );
        global_scope = true;
        return evaluate( t.children[1] ) ~ evaluate( t.children[2] ) ~ "}";
      }
    default: return t.name;
    }
  }

  void add_symbol( string symbol )
  {
    if ( symtable.length == 0 )
      symtable[symbol] = 1;
    else if ( symtable.get( symbol, 0 ) == 0 )
      symtable[symbol] = 1;
    else
      ++symtable[symbol];
  }

  void add_local( string symbol )
  {
    if ( localtab.length == 0 )
      localtab[symbol] = 1;
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
    static assert( is ( T == byte  ) || is ( T == ubyte  ) ||
                   is ( T == short ) || is ( T == ushort ) ||
                   is ( T == int   ) || is ( T == uint   ) ||
                   is ( T == long  ) || is ( T == ulong  ),
                   "not an integer value" );
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
      runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg, cl_int.sizeof, &clop_diagonal );
      assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );
      runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
      assert( runtime.status == CL_SUCCESS, \"clEnqueueNDRangeKernel failed.\" );
    }\n";
  }
}

string
compile( immutable string expr )
{
  auto c = new Compiler( expr );
  auto kernel = c.generate();
  auto params = c.set_params();
  string code = params ~ "char[] clop_opencl_program_source = (q{\n" ~ c.declarations ~
  "} ~ \"__kernel void clop_opencl_kernel_main( \" ~ kernel_params ~ \" )\" ~\n";
  code ~= "q{\n" ~ kernel ~ "\n}).dup;" ~ q{
    debug ( DEBUG ) writeln( "OpenCL program:\n", clop_opencl_program_source, "EOF" );
    size_t clop_opencl_program_source_size = clop_opencl_program_source.length;
    char* clop_opencl_program_source_pointer = clop_opencl_program_source.ptr;
    auto program = clCreateProgramWithSource( runtime.context, 1, &clop_opencl_program_source_pointer, &clop_opencl_program_source_size, &runtime.status );
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
  return "debug ( DEBUG ) writefln( \"DSL MIXIN:\\n%sEOD\", q{" ~ code ~ "} );\n" ~ code;
}

string
set_kernel_param( T )( string name )
{
  string code;
  static      if ( is ( T == bool   ) ) code = "\"uchar "  ~ name ~ "\"";
  else static if ( is ( T == char   ) ) code = "\"char "   ~ name ~ "\"";
  else static if ( is ( T == byte   ) ) code = "\"char "   ~ name ~ "\"";
  else static if ( is ( T == ubyte  ) ) code = "\"uchar "  ~ name ~ "\"";
  else static if ( is ( T == short  ) ) code = "\"short "  ~ name ~ "\"";
  else static if ( is ( T == ushort ) ) code = "\"ushort " ~ name ~ "\"";
  else static if ( is ( Unqual!(T) == int    ) ) code = "\"int "    ~ name ~ "\"";
  else static if ( is ( T == uint   ) ) code = "\"uint "   ~ name ~ "\"";
  else static if ( is ( T == long   ) ) code = "\"long "   ~ name ~ "\"";
  else static if ( is ( T == ulong  ) ) code = "\"ulong "  ~ name ~ "\"";
  else static if ( is ( T == float  ) ) code = "\"float "  ~ name ~ "\"";
  else static if ( is ( T == double ) ) code = "\"double " ~ name ~ "\"";
  else static if ( isDynamicArray!T || isStaticArray!T )
    code = "\"__global \" ~ typeid( *" ~ name ~ ".ptr ).toString() ~ \"* " ~ name ~ "\"";
  else
    code = "\"\"";
  return code;
}

string
set_kernel_arg( T )( string kernel, string arg, string name )
{
  string code;
  static      if ( is ( T == bool ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_char.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == char ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_char.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == byte ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_char.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == ubyte ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_uchar.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == short ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_short.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == ushort ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_ushort.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( Unqual!(T) == int ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_int.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == uint ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_uint.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == long ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_long.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == ulong ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_ulong.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == float ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_float.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( is ( T == double ) )
    code = "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg  ~ "++, cl_double.sizeof, &" ~ name ~ " );\n"
         ~ "assert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else static if ( isDynamicArray!T || isStaticArray!T )
    code = "cl_mem clop_opencl_device_buffer_" ~ name ~
           " = clCreateBuffer( runtime.context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, typeid( *"
           ~ name ~ ".ptr ).tsize * " ~ name ~ ".length, " ~ name ~ ".ptr, &runtime.status );\n"
           ~ "assert( runtime.status == CL_SUCCESS, \"clCreateBuffer failed.\" );\n"
           ~ "runtime.status = clSetKernelArg( " ~ kernel ~ ", " ~ arg ~ "++, cl_mem.sizeof, &clop_opencl_device_buffer_"
           ~ name ~ " );\nassert( runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\" );";
  else
    code = "";
  return "debug ( DEBUG ) writefln( \"%s\", q{" ~ code ~ "} );\n" ~ code;
}

string
read_device_buffer( T )( string name )
{
  string code;
  static if ( isDynamicArray!T || isStaticArray!T )
    code = "runtime.status = clEnqueueReadBuffer( runtime.queue, clop_opencl_device_buffer_"
           ~ name ~ ", CL_TRUE, 0, typeid( *" ~ name ~ ".ptr ).tsize * " ~ name ~ ".length, " ~ name ~
           ".ptr, 0, null, null );\nassert( runtime.status == CL_SUCCESS, \"clEnqueueReadBuffer failed.\" );";
  else
    code = "";
  return "debug ( DEBUG ) writefln( \"%s\", q{" ~ code ~ "} );\n" ~ code;
}
