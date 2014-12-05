module clop.compiler;

import std.string;
import std.traits;

import pegged.grammar;

import clop.analysis;
public import clop.grammar;
public import clop.runtime;

class Compiler
{
  immutable string source;
  ParseTree AST;
  Interval[string] intervals;
  uint[string] symtable;
  uint[string] localtab;
  uint[string] arraytab;
  string declarations;
  string pattern;
  string transformations;
  bool global_scope = false;
  bool internal     = false;
  uint depth        = 0;

  this( immutable string expr )
  {
    source = expr;
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
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return evaluate( t.children[0] );
      }
    case "CLOP.Identifier":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_leaf( t );
      }
      else
      {
        string s = t.matches[0];
        if ( global_scope && !is_local( s ) )
          add_symbol( s );
        else
          add_local( s );
        return s;
      }
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_leaf( t );
      }
      else
      {
        return t.matches[0];
      }
    case "CLOP.ArrayIndex":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = "/*" ~ interval_apply( t.children[0] ).toString() ~ "*/";
        return "[" ~ evaluate( t.children[0] ) ~  s ~ "]";
      }
    case "CLOP.FunctionCall":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        if ( t.children.length > 0 )
        {
          return "(" ~ evaluate( t.children[0] ) ~ ")";
        }
        return "()";
      }
    case "CLOP.PrimaryExpr":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        if ( t.matches[0] == "(" ) return "(" ~ s ~ ")";
        return s;
      }
    case "CLOP.Factor":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        for ( auto c = 1; c < t.children.length; ++c )
          s ~= evaluate( t.children[c] );
        return s;
      }
    case "CLOP.UnaryExpr":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        // this primary expression
        string s = evaluate( t.children[0] );
        string symbol = s;
        // this can be array index or function call
        for ( auto c = 1; c < t.children.length; ++c )
        {
          if ( "CLOP.ArrayIndex" == t.children[c].name )
          {
            // analyze index expression
            add_array( symbol );
          }
          s ~= evaluate( t.children[c] );
        }
        return s;
      }
    case "CLOP.Expression":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        for ( auto c = 1; c < t.children.length; ++c )
          s ~= evaluate( t.children[c] );
        return s;
      }
    case "CLOP.ArgumentList":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        for ( auto c = 1; c < t.children.length; ++c )
          s ~= "," ~ evaluate( t.children[c] );
        return s;
      }
    case "CLOP.MulExpr":
    case "CLOP.AddExpr":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return t.matches[0] ~ evaluate( t.children[0] );
      }
    case "CLOP.AssignExpr":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        if ( t.children.length == 2 )
        {
          return evaluate( t.children[0] ) ~ " = " ~ evaluate( t.children[1] );
        }
        else
        {
          return evaluate( t.children[0] );
        }
      }
    case "CLOP.EqualityExpression":
    case "CLOP.RelationalExpression":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        for ( auto i = 1; i < t.children.length; i += 2 )
          s ~= " " ~ t.children[i].matches[0] ~ " " ~ evaluate( t.children[i + 1] );
        return s;
      }
    case "CLOP.ANDExpression":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "&" ~ evaluate( t.children[i] );
        return s;
      }
    case "CLOP.ExclusiveORExpression":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "^" ~ evaluate( t.children[i] );
        return s;
      }
    case "CLOP.InclusiveORExpression":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "|" ~ evaluate( t.children[i] );
        return s;
      }
    case "CLOP.LogicalANDExpression":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "&&" ~ evaluate( t.children[i] );
        return s;
      }
    case "CLOP.LogicalORExpression":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "||" ~ evaluate( t.children[i] );
        return s;
      }
    case "CLOP.ConditionalExpression":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        if ( t.children.length == 3 )
        {
          string c = evaluate( t.children[0] );
          string a = evaluate( t.children[1] );
          string b = evaluate( t.children[2] );
          return "(" ~ c ~ " ? " ~ a ~ " : " ~ b ~ ")";
        }
        else
        {
          return evaluate( t.children[0] );
        }
      }
    case "CLOP.ExpressionStatement":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = ( t.children.length > 0 ) ? evaluate( t.children[0] ) ~ ";" : ";";
        return s;
      }
    case "CLOP.ReturnStatement":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return "return " ~ evaluate( t.children[0] ) ~ ";";
      }
    case "CLOP.Statement":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return evaluate( t.children[0] );
      }
    case "CLOP.StatementList":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = "";
        foreach ( c; t.children ) s ~= "  " ~ evaluate( c ) ~ "\n";
        return s;
      }
    case "CLOP.CompoundStatement":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = "{\n";
        foreach ( c; t.children ) s ~= evaluate( c );
        return s ~ "}\n";
      }
    /+
     + specification of the ND range over which the kernel will be executed.
     +/
    case "CLOP.SyncPattern":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_leaf( t );
      }
      else
      {
        return t.matches[0];
      }
    case "CLOP.RangeSpec":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return null;
      }
    case "CLOP.RangeList":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        switch ( pattern )
        {
        case "Antidiagonal": return generate_antidiagonal_range( t );
        case "Horizontal":   return generate_horizontal_range( t );
        case "Stencil":      return generate_stencil_range( t );
        default:             return generate_ndrange( t );
        }
      }
    case "CLOP.RangeDecl":
      debug( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return evaluate( t.children[0] );
      }
    case "CLOP.Transformations":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return evaluate( t.children[0] );
      }
    case "CLOP.TransList":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        for ( auto c = 1; c < t.children.length; ++c )
          s ~= evaluate( t.children[c] );
        return s;
      }
    case "CLOP.TransSpec":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return evaluate( t.children[0] );
      }
    case "CLOP.TypeSpecifier":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_leaf( t );
      }
      else
      {
        return t.matches[0];
      }
    case "CLOP.ParameterDeclaration":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return evaluate( t.children[0] ) ~ " " ~ evaluate( t.children[1] );
      }
    case "CLOP.ParameterList":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= ", " ~ evaluate( t.children[i] );
        return s;
      }
    case "CLOP.Initializer":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return evaluate( t.children[0] );
      }
    case "CLOP.InitDeclarator":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        if ( t.children.length > 1 )
          s ~= " = " ~ evaluate( t.children[1] );
        return s;
      }
    case "CLOP.InitDeclaratorList":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= ", " ~ evaluate( t.children[i] );
        return s;
      }
    case "CLOP.Declarator":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        if ( t.children.length > 1 )
          s ~= "( " ~ evaluate( t.children[1] ) ~ " )";
        return s;
      }
    case "CLOP.Declaration":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = evaluate( t.children[0] );
        if ( t.children.length > 1 )
          s ~= " " ~ evaluate( t.children[1] );
        return s ~ ";";
      }
    case "CLOP.DeclarationList":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = "";
        foreach ( c; t.children )
          s ~= "  " ~ evaluate( c ) ~ "\n";
        return s;
      }
    case "CLOP.FunctionDefinition":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        bool saved_global_scope_value = global_scope;
        global_scope = false;
        string y = evaluate( t.children[0] );
        string d = evaluate( t.children[1] );
        string s = evaluate( t.children[2] );
        global_scope = saved_global_scope_value;
        return y ~ " " ~ d ~ " " ~ s;
      }
    case "CLOP.ExternalDeclaration":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        return evaluate( t.children[0] );
      }
    case "CLOP.ExternalDeclarations":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        string s = "";
        foreach ( c; t.children )
          s ~= evaluate( c );
        return s;
      }
    case "CLOP.TranslationUnit":
      debug ( DEBUG_GRAMMAR )
      {
        return debug_node( t );
      }
      else
      {
        ulong i = 0;
        if ( t.children[i].name == "CLOP.ExternalDeclarations" )
        {
          declarations = evaluate( t.children[i++] );
        }
        if ( t.children[i].name == "CLOP.SyncPattern" )
        {
          pattern = evaluate( t.children[i++] );
        }
        ulong n = t.children.length;
        if ( t.children[n - 1].name == "CLOP.Transformations" )
        {
          transformations = evaluate( t.children[n - 1] );
        }
        global_scope = true;
        string r = evaluate( t.children[i++] );
        string s = evaluate( t.children[i++] );
        return "q{\n" ~ declarations ~ "} ~ \"__kernel void clop_opencl_kernel_main( \" ~ kernel_params ~ \" )\" ~\nq{\n{\n" ~ r ~ s ~ "}\n}";
      }
    default: return t.name;
    }
  }

  Interval interval_apply( ParseTree t )
  {
    switch ( t.name )
    {
    case "CLOP.Expression":
      auto r = interval_apply( t.children[0] );
      for ( auto c = 1; c < t.children.length; ++c )
        if ( "+" == t.children[c].matches[0] )
          r = interval_arithmetic_operation( r, "+", interval_apply( t.children[c] ) );
        else
          r = interval_arithmetic_operation( r, "-", interval_apply( t.children[c] ) );
      return r;
    case "CLOP.Factor":
      auto r = interval_apply( t.children[0] );
      for ( auto c = 1; c < t.children.length; ++c )
        if ( "*" == t.children[c].matches[0] )
          r = interval_arithmetic_operation( r, "*", interval_apply( t.children[c] ) );
        else
          r = interval_arithmetic_operation( r, "/", interval_apply( t.children[c] ) );
      return r;
    case "CLOP.AddExpr":
    case "CLOP.MulExpr":
      return interval_apply( t.children[0] );
    case "CLOP.UnaryExpr":
      return interval_apply( t.children[0] );
    case "CLOP.PrimaryExpr":
      return interval_apply( t.children[0] );
    case "CLOP.Identifier":
      return intervals.get( t.matches[0], Interval( t, t ) );
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
      return Interval( t, t );
    default:
      return Interval();
    }
  }

  string generate_antidiagonal_range( ParseTree t )
  {
    /*
        int bx = get_group_id( 0 );
        int tx = get_local_id( 0 );
        int rr = ( br - bx ) * BLOCK_SIZE + 1; // these are the indexes of
        int cc = ( bc + bx ) * BLOCK_SIZE + 1; // the block's top-left element
        // 1.
        int index   = cols * rr + cc + tx;
        int index_n = index - cols;
        int index_w = cols * ( rr + tx ) + cc - 1;

        for ( int k = 0; k < BLOCK_SIZE; ++k )
          s[k * BLOCK_SIZE + tx] = S[k * cols + index];

        if ( tx == 0 ) F_shared[0] = F[index_n - 1];        // copy the NW element into the TL corner of t
        F_shared[(tx + 1) * (BLOCK_SIZE + 1)] = F[index_w]; // copy the column of W elements into the left column of t
        F_shared[(tx + 1)                   ] = F[index_n]; // copy the row of N elements into the top row of t
        barrier( CLK_LOCAL_MEM_FENCE );
    */
    string s = "  int tx = get_global_id( 0 );\n";
    for ( auto c = 0; c < t.children.length; ++c )
    {
      ParseTree u = t.children[c];
      auto name = u.matches[0];
      auto interval = Interval( u.children[1], u.children[2] );
      add_interval( name, interval );
      add_local( name );
      s ~= ( internal )
           ? "  int " ~ u.matches[0] ~ " = c0 + tx;\n"
           : "  int " ~ u.matches[0] ~ " = r0 - tx;\n";
      internal = true;
    }
    return s;
  }

  string generate_horizontal_range( ParseTree t )
  {
    return null;
  }

  string generate_stencil_range( ParseTree t )
  {
    return generate_ndrange( t );
  }

  string generate_ndrange( ParseTree t )
  {
    string s = "";
    ulong n = t.children.length;
    for ( auto c = 0; c < n; ++c )
    {
      ParseTree u = t.children[c];
      add_local( u.matches[0] );
      s ~= "  int " ~ u.matches[0] ~ " = get_global_id( " ~ i2s( n - 1 - c ) ~ " );\n";
    }
    return s;
  }

  void add_array( string symbol )
  {
    if ( arraytab.length == 0 )
      arraytab[symbol] = 1;
    else if ( arraytab.get( symbol, 0 ) == 0 )
      arraytab[symbol] = 1;
    else
      ++arraytab[symbol];
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

  void add_interval( string name, Interval interval )
  {
    intervals[name] = interval;
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
    result ~= "kernel_params ~= \", int r0, int c0\";\n";
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
    debug ( DEBUG_GRAMMAR )
    {
      return "";
    }
    else
    {
      Interval outter = intervals.get( intervals.keys[0], Interval() );
      Interval inner  = intervals.get( intervals.keys[1], Interval() );
      return format( q{
        uint clop_c0 = %s;
        runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg + 1, cl_int.sizeof, &clop_c0 );
        assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
        uint clop_r0;
        for ( clop_r0 = %s; clop_r0 < %s; ++clop_r0 )
        {
          size_t global = clop_r0;
          runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg, cl_int.sizeof, &clop_r0 );
          assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
          runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
          assert( runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel failed." );
        }
        for ( clop_c0 = %s + 1, clop_r0 -= 1; clop_c0 < %s; ++clop_c0 )
        {
          size_t global = clop_r0 - clop_c0 + 1;
          runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg + 1, cl_int.sizeof, &clop_c0 );
          assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
          runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
          assert( runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel failed." );
        }
      }, inner.min.matches[0],
         outter.min.matches[0],
         outter.max.matches[0],
         inner.min.matches[0],
         inner.max.matches[0] );
    }
  }

  string dump_arraytab()
  {
    string result = "// Recognized the following array variables:\n";
    foreach ( sym; arraytab.keys )
    {
      result ~= "// " ~ sym ~ "\n";
    }
    return result;
  }

  string dump_intervals()
  {
    string result = "// The intervals:\n";
    foreach ( k; intervals.keys )
    {
      result ~= "// " ~ k ~ ": " ~ intervals[k].toString() ~ "\n";
    }
    return result;
  }

  string debug_node( ParseTree t )
  {
    string s = "\n";
    string indent = "";
    foreach ( x; 0 .. depth ) indent ~= "  ";
    ++depth;
    foreach ( i, c; t.children )
      s ~= indent ~ i2s( i ) ~ ": " ~ evaluate( c ) ~ "\n";
    --depth;
    string m = "";
    foreach ( i, x; t.matches )
      m ~= " " ~ i2s( i ) ~ ":\"" ~ x ~ "\"";
    return format( "<%s, input[%d,%d]:\"%s\" matches%s>%s%s</%s>", t.name, t.begin, t.end, t.input[t.begin .. t.end], m, s, indent, t.name );
  }

  string debug_leaf( ParseTree t )
  {
    return "<" ~ t.name ~ ">" ~ t.matches[0] ~ "</" ~ t.name ~ ">";
  }
}

string
compile( immutable string expr )
{
  auto c = new Compiler( expr );
  auto kernel = c.generate();
  auto params = c.set_params();
  string code = params ~ "try\n{\n  char[] clop_opencl_program_source = (" ~ kernel ~ ").dup;";
  code ~= q{
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
  code ~= "\n// transformations <" ~ c.transformations ~ ">\n";
  code ~= c.dump_arraytab();
  code ~= c.dump_intervals();
  code ~= "}\ncatch( Exception e )\n{\n  writeln( e );\n}\n";
  debug ( DEBUG_GRAMMAR )
    return "debug ( DEBUG ) writefln( \"DSL MIXIN:\\n%sEOD\", q{" ~ code ~ "} );\n";
  else
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

string
i2s( ulong arg )
{
  switch ( arg )
  {
  case  0: return "0";
  case  1: return "1";
  case  2: return "2";
  case  3: return "3";
  case  4: return "4";
  case  5: return "5";
  case  6: return "6";
  case  7: return "7";
  case  8: return "8";
  case  9: return "9";
  case 10: return "10";
  case 11: return "11";
  default: return "TOO_BIG";
  }
}
