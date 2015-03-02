module clop.program;

import std.string;

import pegged.grammar;

import clop.analysis;
import clop.symbol;
import clop.transform;

/++
 +  The container of all information related to a specific variant
 +  a CLOP program fragment.
 +/
struct Program
{
  /++
   + Return the string that is this program's kernel source code.
   +/
  string generate_code ()
  {
    auto t = optimize(AST);
    auto c = translate(t);
    auto k = generate_kernel_name();
    return "\"__kernel void " ~ k ~ "(\" ~ kernel_params ~ \")\" ~\nq{\n  " ~ c ~ "}";
  }

  private:

  Symbol[string] symtable;
  Transformation[] trans;
  ParseTree AST;
  Box range;
  string pattern;

  string kernel;
  bool use_shadow = false;

  ParseTree optimize(ParseTree t)
  {
    auto r = t;
    foreach (f; trans)
      r = apply_trans(f, r);
    return r;
  }

  ParseTree apply_trans(Transformation f, ParseTree t)
  {
    switch (f.get_name())
    {
    case "rectangular_blocking":
      return apply_rectangular_blocking(t);
    default:
      return t.dup;
    }
  }

  auto get_applied_transformations ()
  {
    return reduce!((a, b) => a ~ "_" ~ b)("", map!(a => a.toString)(trans));
  }

  auto patch_index_expression_for_shared_memory (string s, ParseTree t)
  {
    auto r = Box( [], [] );
    foreach ( c, v; range.symbols )
    {
      r.symbols ~= [v];
      r.s2i[v] = c;
      auto n = "0";
      auto x = "8"; // block_size;
      symtable[v].shadow = s ~ "_" ~ generate_shared_memory_variable( v );
      r.intervals ~= [
                      Interval(
                               ParseTree( "CLOP.IntegerLiteral", true, [n], "", 0, 0, [] ),
                               ParseTree( "CLOP.IntegerLiteral", true, [x], "", 0, 0, [] ) )
                      ];
    }
    auto uses = symtable[s].uses;
    Interval[3] local_box;
    foreach ( i, c; uses[0].children )
      local_box[i] = interval_apply( c, r );
    for ( auto ii = 1; ii < uses.length; ++ii )
      foreach ( i, c; uses[ii].children )
        local_box[i] = interval_union( local_box[i], interval_apply( c, r ) );
    auto u = t.children[0];
    auto bsz0 = local_box[0].get_size();
    auto bsz1 = local_box[1].get_size();
    auto x = "";
    switch ( u.children.length )
    {
      case 1:
        x = translate( u.children[0] );
        break;
      case 2:
        x = format( "(%s) * (%s) + (%s)", translate( u.children[1] ), bsz0,
                    translate( u.children[0] ) );
        break;
      case 3:
        x = format( "(%s) * (%s) * (%s) + (%s) * (%s) + (%s)",
                    translate( u.children[2] ), bsz1, bsz0,
                    translate( u.children[1] ), bsz1,
                    translate( u.children[0] ) );
        break;
      default:
        x = "0";
    }
    return format( "[%s]", x );
  }

  auto generate_shared_memory_variable(string v)
  {
    return v ~ "_in_shared_memory";
  }

  auto generate_kernel_name ()
  {
    auto suffix = get_applied_transformations();
    return format ("clop_kernel_main_%s_%s", pattern, suffix);
  }

  auto apply_rectangular_blocking(ParseTree t)
  {
    ParseTree stmts;
    if (t.name != "CLOP.CompoundStatement")
    {
      return t;
    }
    if (t.children.length == 1 && t.children[0].name == "CLOP.StatementList")
    {
      stmts = t.children[0];
    }
    else if (t.children.length == 2 && t.children[1].name == "CLOP.StatementList")
    {
      stmts = t.children[1];
    }
    else
    {
      return t;
    }
    ParseTree n;
    ulong i = 0;
    do n = stmts.children[i++];
    while (n.name != "CLOP.InternalNode" && i < stmts.children.length);
    if (i == stmts.children.length)
    {
      i = 0;
    }
    n = stmts.children[i];

    // collect all array variables we want to put in shared memory.
    auto shadowed = SList!string();

    foreach (v; symtable)
      if (v.is_array && v.shadow != null)
        shadowed.insert(v.name);

    foreach (v; shadowed)
    {
      Interval[3] local_box;
      Interval[3] global_box;
      auto uses = symtable[v].uses;
      foreach (i, c; uses[0].children)
      {
        local_box[i] = interval_apply(c, r);
        global_box[i] = interval_apply(c, g);
      }
      for (auto ii = 1; ii < uses.length; ++ii)
      {
        foreach (i, c; uses[ii].children)
        {
          local_box[i] = interval_union(local_box[i], interval_apply(c, r));
          global_box[i] = interval_union(global_box[i], interval_apply(c, g));
        }
      }
      // size of current array block
      auto bsz0 = local_box[0].get_size();
      auto bsz1 = local_box[1].get_size();
      // global index of current array
      auto gi0 = format("(%s) + (%s)", global_box[0].get_min(), li0);
      auto gi1 = format("(%s) + (%s)", global_box[1].get_min(), li1);
      // size of current global array
      auto gsz0 = symtable[v].box[0].get_max();
      // global memory array name
      auto gma = v;
      // shared memory array name
      auto sma = generate_shared_memory_variable(v);
      s ~= format(template_shared_memory_data_init,
                  li1, li1, bsz1, li1,
                  li0, li0, bsz0, li0, lsz0,
                  li0, tid0, bsz0,
                  sma, li1, bsz0, li0, tid0,
                  gma, gi1, gsz0, gi0, tid0);
      parameters ~= Argument(sma, "", "__local", format("(%s) * (%s)", bsz0, bsz1), gma);
    }
  }

  auto generate_horizontal_range (ParseTree t)
  {
    return "";
  }

  auto generate_stencil_range (ParseTree t)
  {
    return "";
  }

  auto generate_ndrange (ParseTree t)
  {
    return "";
  }

  string translate (ParseTree t)
  {
    switch (t.name)
    {
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
    case "CLOP.PrimaryExpr":
      {
        string s = translate( t.children[0] );
        return "(" == t.matches[0] ? "(" ~ s ~ ")" : s;
      }
    case "CLOP.UnaryExpr":
      {
        // this is a primary expression
        string s = translate(t.children[0]);
        // this can be array index or function call
        if (t.children.length == 2)
        {
          // if ( "CLOP.ArrayIndex" == t.children[1].name &&
          //      symtable.get( s, Symbol() ) != Symbol() &&
          //      symtable[s].shadow != null )
          // {
          //   use_shadow = true;
          //   s = symtable[s].shadow ~
          //       patch_index_expression_for_shared_memory( s, t.children[1] );
          //   use_shadow = false;
          // }
          // else
          {
            s ~= translate(t.children[1]);
          }
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
          string lhs = translate( t.children[0] );
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
          s ~= " " ~ t.children[i].matches[0] ~
               " " ~ translate( t.children[i + 1] );
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
        string s = ( t.children.length > 0 )
                 ? translate( t.children[0] ) ~ ";" : ";";
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
    case "CLOP.IfStatement":
      {
        string s = "if (" ~ translate(t.children[0]) ~ ")\n  " ~ translate(t.children[1]);
        if (t.children.length == 3)
          s ~= "\nelse\n  " ~ translate(t.children[2]);
        return s;
      }
    case "CLOP.Identifier":
      {
        string s = t.matches[0];
        if ( use_shadow &&
             symtable.get( s, Symbol() ) != Symbol() &&
             symtable[s].shadow != null )
          return symtable[s].shadow;
        return s;
      }
    case "CLOP.IntegerLiteral":
    case "CLOP.FloatLiteral":
      {
        return t.matches[0];
      }
    case "CLOP.InternalNode":
      {
        return "";
      }
    default: return t.name;
    }
  }
} // Program

/+

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
        string r = translate( t.children[i] );
        string q;
        switch ( pattern )
        {
        case "Antidiagonal": q = finish_antidiagonal_range( t.children[i++] ); break;
        case "Horizontal":   q = finish_horizontal_range( t.children[i++] ); break;
        case "Stencil":      q = finish_stencil_range( t.children[i++] ); break;
        default:             q = finish_ndrange( t.children[i++] ); break;
        }
        string s = translate( t.children[i] );
        string k = generate_kernel_name();
        return "q{\n" ~ declarations ~ "} ~ \"__kernel void " ~ k ~ "( \" ~ kernel_params ~ \" )\" ~\nq{\n  {\n" ~ r ~ s ~ q ~ "  }\n}";
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
        string y = translate( t.children[0] );
        string d = translate( t.children[1] );
        string s = translate( t.children[2] );
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
        return "(" == t.matches[0] ? "(" ~ s ~ ")" : s;
      }
    case "CLOP.UnaryExpr":
      {
        // this is a primary expression
        string s = translate( t.children[0] );
        // this can be array index or function call
        if ( t.children.length == 2 )
        {
          if ( "CLOP.ArrayIndex" == t.children[1].name && symtable.get( s, Symbol() ) != Symbol() && symtable[s].shadow != null )
          {
            use_shadow = true;
            s = symtable[s].shadow ~ patch_index_expression_for_shared_memory( s, t.children[1] );
            use_shadow = false;
          }
          else
          {
            s ~= translate( t.children[1] );
          }
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
          string lhs = translate( t.children[0] );
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
        string s = t.matches[0];
        if ( use_shadow && symtable.get( s, Symbol() ) != Symbol() && symtable[s].shadow != null )
          return symtable[s].shadow;
        return s;
      }
    case "CLOP.IntegerLiteral":
    case "CLOP.FloatLiteral":
      {
        return t.matches[0];
      }
    default: return t.name;
    }
  }
+/
