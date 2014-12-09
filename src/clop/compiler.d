module clop.compiler;

import std.string;
import std.traits;

import pegged.grammar;

import clop.analysis;
import clop.symbol;
public import clop.grammar;
public import clop.runtime;

class Compiler
{
  immutable string source;
  ParseTree AST;
  Symbol[string] symtable;
  Interval[string] intervals;
  string declarations;
  string pattern;
  string transformations;
  string block_size;
  bool global_scope = false;
  bool eval_def     = false;
  bool internal     = false;
  uint depth        = 0;

  this( immutable string expr )
  {
    source = expr;
    AST = CLOP( source );
  }

  string generate()
  {
    debug ( DEBUG_GRAMMAR )
    {
      return debug_tree( AST );
    }
    else
    {
      analyze( AST );
      return translate( AST );
    }
  }

  void analyze( ParseTree t )
  {
    switch ( t.name )
    {
    case "CLOP":
      {
        analyze( t.children[0] );
        break;
      }
    case "CLOP.TranslationUnit":
      {
        ulong i = 0;
        if ( t.children[i].name == "CLOP.ExternalDeclarations" )
        {
          analyze( t.children[i++] );
        }
        if ( t.children[i].name == "CLOP.SyncPattern" )
        {
          analyze( t.children[i++] );
        }
        ulong n = t.children.length;
        if ( t.children[n - 1].name == "CLOP.Transformations" )
        {
          analyze( t.children[n - 1] );
        }
        global_scope = true;
        for ( ; i < t.children.length; ++i )
          analyze( t.children[i] );
        break;
      }
    case "CLOP.ExternalDeclarations":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.ExternalDeclaration":
      {
        analyze( t.children[0] );
        break;
      }
    case "CLOP.FunctionDefinition":
      {
        bool saved_global_scope_value = global_scope;
        global_scope = false;
        foreach ( c; t.children )
          analyze( c );
        global_scope = saved_global_scope_value;
        break;
      }
    case "CLOP.DeclarationList":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.Declaration":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.Declarator":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.TypeSpecifier":
      {
        break;
      }
    case "CLOP.ParameterList":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.ParameterDeclaration":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.IdentifierList":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.InitDeclaratorList":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.InitDeclarator":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.Initializer":
      {
        analyze( t.children[0] );
        break;
      }
    case "CLOP.InitializerList":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.SyncPattern":
      {
        pattern = t.matches[0];
        break;
      }
    case "CLOP.RangeDecl":
      {
        analyze( t.children[0] );
        break;
      }
    case "CLOP.RangeList":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.RangeSpec":
      {
        break;
      }
    case "CLOP.Transformations":
      {
        analyze( t.children[0] );
        break;
      }
    case "CLOP.TransList":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.TransSpec":
      {
        if ( t.matches.length > 3 )
        {
          block_size = t.matches[$-2];
        }
        transformations = t.matches[0];
        break;
      }
    case "CLOP.PrimaryExpr":
      {
        analyze( t.children[0] );
        break;
      }
    case "CLOP.UnaryExpr":
      {
        analyze( t.children[0] );
        // this is primary expression
        string symbol = t.matches[0];
        // this can be array index or function call
        for ( auto c = 1; c < t.children.length; ++c )
        {
          if ( "CLOP.ArrayIndex" == t.children[c].name )
          {
            // analyze index expression
            symtable[symbol].is_array = true;
            if ( eval_def )
            {
              symtable[symbol].defs ~= t.children[c].children[0];
            }
            else
            {
              symtable[symbol].uses ~= t.children[c].children[0];
            }
          }
          analyze( t.children[c] );
        }
        break;
      }
    case "CLOP.ArrayIndex":
      {
        analyze( t.children[0] );
        break;
      }
    case "CLOP.FunctionCall":
      {
        if ( t.children.length > 0 )
        {
          analyze( t.children[0] );
        }
        break;
      }
    case "CLOP.ArgumentList":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.MulExpr":
    case "CLOP.Factor":
    case "CLOP.AddExpr":
    case "CLOP.Expression":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.AssignExpr":
      {
        if ( t.children.length == 2 )
        {
          eval_def = true;
          analyze( t.children[0] );
          eval_def = false;
          analyze( t.children[1] );
        }
        else
        {
          analyze( t.children[0] );
        }
        break;
      }
    case "CLOP.RelationalExpression":
    case "CLOP.EqualityExpression":
      {
        analyze( t.children[0] );
        for ( auto i = 1; i < t.children.length; i += 2 )
          analyze( t.children[i + 1] );
        break;
      }
    case "CLOP.ANDExpression":
    case "CLOP.ExclusiveORExpression":
    case "CLOP.InclusiveORExpression":
    case "CLOP.LogicalANDExpression":
    case "CLOP.LogicalORExpression":
    case "CLOP.ConditionalExpression":
    case "CLOP.ConstantExpression":
    case "CLOP.CompoundStatement":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.ExpressionStatement":
      {
        if ( t.children.length > 0 )
          analyze( t.children[0] );
        break;
      }
    case "CLOP.ReturnStatement":
    case "CLOP.Statement":
      {
        analyze( t.children[0] );
        break;
      }
    case "CLOP.StatementList":
      {
        foreach ( c; t.children )
          analyze( c );
        break;
      }
    case "CLOP.Identifier":
      {
        string s = t.matches[0];
        if ( symtable.get( s, Symbol() ) == Symbol() )
          symtable[s] = Symbol( s, t, !global_scope, false, [], [] );
        break;
      }
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
      {
        break;
      }
    default:
      {
        break;
      }
    }
  }

  string translate( ParseTree t )
  {
    switch ( t.name )
    {
    case "CLOP":
      {
        return translate( t.children[0] );
      }
    case "CLOP.TranslationUnit":
      {
        ulong i = 0;
        if ( t.children[i].name == "CLOP.ExternalDeclarations" )
        {
          declarations = translate( t.children[i++] );
        }
        if ( t.children[i].name == "CLOP.SyncPattern" )
        {
          i++;
        }
        global_scope = true;
        string r = translate( t.children[i++] );
        string s = translate( t.children[i] );
        string k = generate_kernel_name();
        return "q{\n" ~ declarations ~ "} ~ \"__kernel void " ~ k ~ "( \" ~ kernel_params ~ \" )\" ~\nq{\n{\n" ~ r ~ s ~ "}\n}";
      }
    case "CLOP.ExternalDeclarations":
      {
        string s = "";
        foreach ( c; t.children )
          s ~= translate( c );
        return s;
      }
    case "CLOP.ExternalDeclaration":
      {
        return translate( t.children[0] );
      }
    case "CLOP.FunctionDefinition":
      {
        bool saved_global_scope_value = global_scope;
        global_scope = false;
        string y = translate( t.children[0] );
        string d = translate( t.children[1] );
        string s = translate( t.children[2] );
        global_scope = saved_global_scope_value;
        return y ~ " " ~ d ~ " " ~ s;
      }
    case "CLOP.DeclarationList":
      {
        string s = "";
        foreach ( c; t.children )
          s ~= "  " ~ translate( c ) ~ "\n";
        return s;
      }
    case "CLOP.Declaration":
      {
        string s = translate( t.children[0] );
        if ( t.children.length > 1 )
          s ~= " " ~ translate( t.children[1] );
        return s ~ ";";
      }
    case "CLOP.Declarator":
      {
        string s = translate( t.children[0] );
        if ( t.children.length > 1 )
          s ~= "( " ~ translate( t.children[1] ) ~ " )";
        return s;
      }
    case "CLOP.TypeSpecifier":
      {
        return t.matches[0];
      }
    case "CLOP.ParameterList":
      {
        string s = translate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= ", " ~ translate( t.children[i] );
        return s;
      }
    case "CLOP.ParameterDeclaration":
      {
        return translate( t.children[0] ) ~ " " ~ translate( t.children[1] );
      }
    case "CLOP.InitDeclaratorList":
      {
        string s = translate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= ", " ~ translate( t.children[i] );
        return s;
      }
    case "CLOP.InitDeclarator":
      {
        string s = translate( t.children[0] );
        if ( t.children.length > 1 )
          s ~= " = " ~ translate( t.children[1] );
        return s;
      }
    case "CLOP.Initializer":
      {
        return translate( t.children[0] );
      }
    /+
     + specification of the ND range over which the kernel will be executed.
     +/
    case "CLOP.SyncPattern":
      {
        return t.matches[0];
      }
    case "CLOP.RangeDecl":
      {
        return translate( t.children[0] );
      }
    case "CLOP.RangeList":
      {
        switch ( pattern )
        {
        case "Antidiagonal": return generate_antidiagonal_range( t );
        case "Horizontal":   return generate_horizontal_range( t );
        case "Stencil":      return generate_stencil_range( t );
        default:             return generate_ndrange( t );
        }
      }
    case "CLOP.Transformations":
      {
        return translate( t.children[0] );
      }
    case "CLOP.TransList":
      {
        string s = translate( t.children[0] );
        for ( auto c = 1; c < t.children.length; ++c )
          s ~= translate( t.children[c] );
        return s;
      }
    case "CLOP.TransSpec":
      {
        if ( t.matches.length > 3 )
        {
          block_size = t.matches[$-2];
        }
        return translate( t.children[0] );
      }
    case "CLOP.PrimaryExpr":
      {
        string s = translate( t.children[0] );
        if ( t.matches[0] == "(" ) return "(" ~ s ~ ")";
        return s;
      }
    case "CLOP.UnaryExpr":
      {
        // this primary expression
        string s = translate( t.children[0] );
        // this can be array index or function call
        for ( auto c = 1; c < t.children.length; ++c )
        {
          if ( "CLOP.ArrayIndex" == t.children[c].name )
          {
            // translate index expression
            if ( eval_def )
            {
              s = "/*def*/" ~ s;
            }
            else
            {
              s = "/*use*/" ~ s;
            }
          }
          s ~= translate( t.children[c] );
        }
        return s;
      }
    case "CLOP.ArrayIndex":
      {
        return "[" ~ translate( t.children[0] ) ~ "]";
      }
    case "CLOP.FunctionCall":
      {
        if ( t.children.length > 0 )
        {
          return "(" ~ translate( t.children[0] ) ~ ")";
        }
        return "()";
      }
    case "CLOP.ArgumentList":
      {
        string s = translate( t.children[0] );
        for ( auto c = 1; c < t.children.length; ++c )
          s ~= "," ~ translate( t.children[c] );
        return s;
      }
    case "CLOP.MulExpr":
    case "CLOP.AddExpr":
      {
        return t.matches[0] ~ translate( t.children[0] );
      }
    case "CLOP.Factor":
      {
        string s = translate( t.children[0] );
        for ( auto c = 1; c < t.children.length; ++c )
          s ~= translate( t.children[c] );
        return s;
      }
    case "CLOP.Expression":
      {
        string s = translate( t.children[0] );
        for ( auto c = 1; c < t.children.length; ++c )
          s ~= translate( t.children[c] );
        return s;
      }
    case "CLOP.AssignExpr":
      {
        if ( t.children.length == 2 )
        {
          eval_def = true;
          string lhs = translate( t.children[0] );
          eval_def = false;
          string rhs = translate( t.children[1] );
          return lhs ~ " = " ~ rhs;
        }
        else
        {
          return translate( t.children[0] );
        }
      }
    case "CLOP.RelationalExpression":
    case "CLOP.EqualityExpression":
      {
        string s = translate( t.children[0] );
        for ( auto i = 1; i < t.children.length; i += 2 )
          s ~= " " ~ t.children[i].matches[0] ~ " " ~ translate( t.children[i + 1] );
        return s;
      }
    case "CLOP.ANDExpression":
      {
        string s = translate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "&" ~ translate( t.children[i] );
        return s;
      }
    case "CLOP.ExclusiveORExpression":
      {
        string s = translate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "^" ~ translate( t.children[i] );
        return s;
      }
    case "CLOP.InclusiveORExpression":
      {
        string s = translate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "|" ~ translate( t.children[i] );
        return s;
      }
    case "CLOP.LogicalANDExpression":
      {
        string s = translate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "&&" ~ translate( t.children[i] );
        return s;
      }
    case "CLOP.LogicalORExpression":
      {
        string s = translate( t.children[0] );
        foreach ( i; 1 .. t.children.length )
          s ~= "||" ~ translate( t.children[i] );
        return s;
      }
    case "CLOP.ConditionalExpression":
      {
        if ( t.children.length == 3 )
        {
          string c = translate( t.children[0] );
          string a = translate( t.children[1] );
          string b = translate( t.children[2] );
          return "(" ~ c ~ " ? " ~ a ~ " : " ~ b ~ ")";
        }
        else
        {
          return translate( t.children[0] );
        }
      }
    case "CLOP.CompoundStatement":
      {
        string s = "{\n";
        foreach ( c; t.children ) s ~= translate( c );
        return s ~ "}\n";
      }
    case "CLOP.ExpressionStatement":
      {
        string s = ( t.children.length > 0 ) ? translate( t.children[0] ) ~ ";" : ";";
        return s;
      }
    case "CLOP.ReturnStatement":
      {
        return "return " ~ translate( t.children[0] ) ~ ";";
      }
    case "CLOP.Statement":
      {
        return translate( t.children[0] );
      }
    case "CLOP.StatementList":
      {
        string s = "";
        foreach ( c; t.children ) s ~= "  " ~ translate( c ) ~ "\n";
        return s;
      }
    case "CLOP.Identifier":
      {
        return t.matches[0];
      }
    case "CLOP.IntegerLiteral":
    case "CLOP.FloatLiteral":
      {
        return t.matches[0];
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

  string generate_kernel_name()
  {
    return format( "clop_kernel_main_%s_%s", pattern, transformations );
  }

  string select_array_for_shared_memory()
  {
    return "F";
  }

  string generate_shared_memory_variable( string v )
  {
    return v ~ "_in_shared_memory";
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
    string s = "";
    if ( transformations == "rectangular_blocking" )
    {
      s ~= "  int bx = get_group_id( 0 );\n";
      s ~= "  int tx = get_local_id( 0 );\n";
      string inner;
      string outter;
      for ( auto c = 0; c < t.children.length; ++c )
      {
        ParseTree u = t.children[c];
        auto name = u.matches[0];
        auto interval = Interval( u.children[1], u.children[2] );
        add_interval( name, interval );
        symtable[name] = Symbol( name, t, true );
        string v = u.matches[0];
        if ( internal )
        {
          inner = v;
          s ~= format( "  int b%s = b%s0 + bx;\n", v, v );
        }
        else
        {
          outter = v;
          s ~= format( "  int b%s = b%s0 - bx;\n", v, v );
        }
        s ~= format( "  int %s = b%s * %s + 1;\n", v, v, block_size );
        internal = true;
      }
      string v = select_array_for_shared_memory();
      string u = generate_shared_memory_variable( v );
      string w = "1";
      string h = "1";
      string nw = format( "(%s - %s) * %s + %s - %s", outter, h, "cols", inner, w );
      string wi = format( "(%s - %s) * %s + %s - %s", outter, h, "cols", inner, w );
      string ni = format( "(%s - %s) * %s + %s - %s", outter, h, "cols", inner, w );
      s ~= format( q{
        if ( tx == 0 )
          for ( int i = 0; i < %s; ++i ) // h
            for ( int j = 0; j < %s; ++j ) // w
              %s[i * (%s + %s)] = %s[%s]; // u, bs, w, v, nw
        for ( int i = 0; i < %s; ++i ) // w
              %s[(tx + %s) * (%s + %s)] = %s[%s]; // u, h, bs, w, v, wi
        for ( int i = 0; i < %s; ++i ) // h
              %s[(tx + %s)] = %s[%s]; // u, w, v, ni
        barrier( CLK_LOCAL_MEM_FENCE );
      }, h, w, u, block_size, w, v, nw, w, u, h, block_size, w, v, wi, h, u, w, v, ni );
    }
    else
    {
      s ~= "  int tx = get_global_id( 0 );\n";
      for ( auto c = 0; c < t.children.length; ++c )
      {
        ParseTree u = t.children[c];
        auto name = u.matches[0];
        auto interval = Interval( u.children[1], u.children[2] );
        add_interval( name, interval );
        symtable[name] = Symbol( name, t, true );
        s ~= ( internal )
             ? "  int " ~ u.matches[0] ~ " = c0 + tx;\n"
             : "  int " ~ u.matches[0] ~ " = r0 - tx;\n";
        internal = true;
      }
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
      string name = u.matches[0];
      symtable[name] = Symbol( name, t, true );
      s ~= "  int " ~ name ~ " = get_global_id( " ~ i2s( n - 1 - c ) ~ " );\n";
    }
    return s;
  }

  void add_interval( string name, Interval interval )
  {
    intervals[name] = interval;
  }

  bool is_local( string sym )
  {
    return symtable.get( sym, Symbol() ).is_local;
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
      if ( !symtable[sym].is_local )
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
    result ~= "kernel_params ~= \", int br0, int bc0, __local int* F_in_shared_memory\";\n";
    return result;
  }

  string set_args( string kernel )
  {
    string result = "uint clop_opencl_kernel_arg = 0;\n";
    foreach( sym; symtable.keys )
    {
      if ( !symtable[sym].is_local )
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
      if ( !symtable[sym].is_local )
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
          //runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg, cl_int.sizeof, &clop_r0 );
          assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
          //runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
          assert( runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel failed." );
        }
        for ( clop_c0 = %s + 1, clop_r0 -= 1; clop_c0 < %s; ++clop_c0 )
        {
          size_t global = clop_r0 - clop_c0 + 1;
          //runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg + 1, cl_int.sizeof, &clop_c0 );
          assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
          //runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
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
    foreach ( sym; symtable.keys )
    {
      if ( symtable[sym].is_array )
      {
        string s = "// defs " ~ i2s(symtable[sym].defs.length) ~ "\n";
        foreach ( a; symtable[sym].defs )
          s ~= "// " ~ interval_apply( a ).toString() ~ "\n";
        s ~= "// uses " ~ i2s(symtable[sym].uses.length) ~ "\n";
        foreach ( a; symtable[sym].uses )
          s ~= "// " ~ interval_apply( a ).toString() ~ "\n";
        result ~= "// " ~ sym ~ ":\n" ~ s;
      }
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
      s ~= indent ~ i2s( i ) ~ ": " ~ debug_tree( c ) ~ "\n";
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
  
  string debug_tree( ParseTree t )
  {
    switch ( t.name )
    {
    case "CLOP":
    case "CLOP.ArrayIndex":
    case "CLOP.FunctionCall":
    case "CLOP.PrimaryExpr":
    case "CLOP.Factor":
    case "CLOP.UnaryExpr":
    case "CLOP.Expression":
    case "CLOP.ArgumentList":
    case "CLOP.MulExpr":
    case "CLOP.AddExpr":
    case "CLOP.AssignExpr":
    case "CLOP.EqualityExpression":
    case "CLOP.RelationalExpression":
    case "CLOP.ANDExpression":
    case "CLOP.ExclusiveORExpression":
    case "CLOP.InclusiveORExpression":
    case "CLOP.LogicalANDExpression":
    case "CLOP.LogicalORExpression":
    case "CLOP.ConditionalExpression":
    case "CLOP.ExpressionStatement":
    case "CLOP.ReturnStatement":
    case "CLOP.Statement":
    case "CLOP.StatementList":
    case "CLOP.CompoundStatement":
    case "CLOP.SyncPattern":
    case "CLOP.RangeSpec":
    case "CLOP.RangeList":
    case "CLOP.RangeDecl":
    case "CLOP.Transformations":
    case "CLOP.TransList":
    case "CLOP.TransSpec":
    case "CLOP.TypeSpecifier":
    case "CLOP.ParameterDeclaration":
    case "CLOP.ParameterList":
    case "CLOP.Initializer":
    case "CLOP.InitDeclarator":
    case "CLOP.InitDeclaratorList":
    case "CLOP.Declarator":
    case "CLOP.Declaration":
    case "CLOP.DeclarationList":
    case "CLOP.FunctionDefinition":
    case "CLOP.ExternalDeclaration":
    case "CLOP.ExternalDeclarations":
    case "CLOP.TranslationUnit":
        return debug_node( t );
    case "CLOP.Identifier":
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
        return debug_leaf( t );
    default: return t.name;
    }
  }

}

string
compile( immutable string expr )
{
  auto c = new Compiler( expr );
  auto kernel = c.generate();
  auto params = c.set_params();
  string code = params ~ "try\n{\n  char[] clop_opencl_program_source = (" ~ kernel ~ ").dup;";
  code ~= format( q{
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
    auto clop_opencl_kernel = clCreateKernel( program, "%s", &runtime.status );
    assert( runtime.status == CL_SUCCESS, "clCreateKernel failed." );
  }, c.generate_kernel_name() );
  code ~= c.set_args( "clop_opencl_kernel" ) ~ c.code_to_invoke_kernel() ~ c.code_to_read_data_from_device();
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
