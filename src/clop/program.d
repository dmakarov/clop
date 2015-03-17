/*
  The MIT License (MIT)
  =====================

  Copyright (c) 2015 Dmitri Makarov <dmakarov@alumni.stanford.edu>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*/
module clop.program;

import std.algorithm : reduce;
import std.conv;
import std.container;
import std.string;

import pegged.grammar;

import clop.analysis;
import clop.structs;
import clop.symbol;
import clop.templates;
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
    auto t = optimize(AST); // apply the optimizing transformations on the AST
    auto kbody = translate(t);  // generate OpenCL kernel source code from the AST
    auto kname = generate_kernel_name(); // we need to give the kernel a name
    auto params = set_params();
    auto kernel = "\"__kernel void " ~ kname ~ "(\" ~ kernel_params ~ \")\" ~\nq{\n" ~ kbody ~ "}";
    auto boiler = format(template_create_opencl_kernel, generate_kernel_name());
    boiler ~= set_args("clop_opencl_kernel") ~ code_to_invoke_kernel() ~ code_to_read_data_from_device();
    auto diagnostics = format("pragma (msg, \"%s\");\n", errors);
    return diagnostics ~ format(template_clop_unit, params, external, kernel, boiler);
  }

  private:

  Symbol[string] symtable;
  Transformation[] trans;
  Argument[] parameters;
  ParseTree AST;
  Box range;
  string pattern;
  string external;
  string errors;

  string kernel;
  string indent = "  ";
  string block_size = "8";
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
    {
      ulong i = 0;
      do
      {
        n = stmts.children[i++];
      } while (n.name != "CLOP.InternalNode" && i < stmts.children.length);
      n = i < stmts.children.length ? stmts.children[i] : stmts.children[0];
    }


    auto r = Box([], []);
    auto g = Box([], []);
    string[3] bix;
    foreach (c; 0 .. range.symbols.length)
    {
      auto v = range.symbols[c]; // variable name in NDRange specification
      r.symbols ~= [v];
      r.s2i[v] = c;
      auto m = "0";
      auto x = "8"; // FIXME block_size;
      r.intervals ~= [
                      Interval(
                               ParseTree("CLOP.IntegerLiteral", true, [m], "", 0, 0, []),
                               ParseTree("CLOP.IntegerLiteral", true, [x], "", 0, 0, [])
                              )
                      ];
      // n = range.intervals[c].min + b%d * block_size;
      // x = n + block_size;
      bix[c] = format("b%d", c);
      auto y = ParseTree("CLOP.Identifier", true, [bix[c]], "", 0, 0, []);
      auto w = ParseTree("CLOP.IntegerLiteral", true, [x], "", 0, 0, []);
      auto f = create_mul_expr(y, w, "*");
      auto a = create_add_expr(range.intervals[c].min, f, "+");
      g.intervals ~= [Interval(a, create_add_expr(a, w, "+"))];
      g.symbols ~= [v];
      g.s2i[v] = c;
    }

    debug(NEVER) {
    auto iv = "iv";
    // block or work group id
    auto bid0 = "bid0";
    // thread or work item id
    auto tid0 = "tid0";
    // local index variables
    auto li0 = "li0";
    auto li1 = "li1";
    // size of local index space
    auto block_size = "8";
    auto lsz0 = block_size;
    auto lsz1 = block_size;

    // collect all array variables we want to put in shared memory.
    auto shadowed = SList!string();

    foreach (v; symtable)
      v.is_array && v.shadow != null && shadowed.insert(v.name);

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
      auto s = format(template_shared_memory_data_init,
                      li1, li1, bsz1, li1,
                      li0, li0, bsz0, li0, lsz0,
                      li0, tid0, bsz0,
                      sma, li1, bsz0, li0, tid0,
                      gma, gi1, gsz0, gi0, tid0);
      parameters ~= Argument(sma, "", "__local", format("(%s) * (%s)", bsz0, bsz1), gma);
    }
    }
    return t;
  }

  string generate_antidiagonal_range(ParseTree t)
  {
    auto s = "";
    auto num_dimensions = t.children.length;
    auto range_arg = new string[num_dimensions];
    auto shadowed = SList!string();

    foreach (v; symtable)
      if (v.is_array && v.shadow != null)
        shadowed.insert(v.name);

    auto r = Box([], []);
    auto g = Box([], []);
    string[3] bix;
    foreach (c, u; t.children)
    {
      auto v = u.matches[0];              // variable name in NDRange specification
      r.symbols ~= [v];
      r.s2i[v] = c;
      auto n = "0";
      auto x = block_size;
      r.intervals ~= [
                      Interval(
                               ParseTree("CLOP.IntegerLiteral", true, [n], "", 0, 0, []),
                               ParseTree("CLOP.IntegerLiteral", true, [x], "", 0, 0, [])
                              )
                      ];
      // n = range.intervals[c].min + b%d * block_size;
      // x = n + block_size;
      bix[c] = format("b%d", c);
      auto y = ParseTree("CLOP.Identifier", true, [bix[c]], "", 0, 0, []);
      auto w = ParseTree("CLOP.IntegerLiteral", true, [x], "", 0, 0, []);
      auto f = create_mul_expr(y, w, "*");
      auto a = create_add_expr(range.intervals[c].min, f, "+");
      g.intervals ~= [Interval(a, create_add_expr(a, w, "+"))];
      g.symbols ~= [v];
      g.s2i[v] = c;
    }

    if (true /*trans.contains("rectangular_blocking")*/)
    {
      auto iv = "iv";
      // block or work group id
      auto bid0 = "bid0";
      // thread or work item id
      auto tid0 = "tid0";
      // local index variables
      auto li0 = "li0";
      auto li1 = "li1";
      // size of local index space
      auto lsz0 = block_size;
      auto lsz1 = block_size;

      s ~= format("  int %s = get_group_id(0);\n", bid0);
      s ~= format("  int %s = get_local_id(0);\n", tid0);
      auto factor = false;
      foreach (c, u; t.children)
      {
        auto v = u.matches[0];        // variable name in NDRange specification
        auto o = factor ? "-" : "+";  // arithmetic operation
        factor = !factor;
        range_arg[c] = v;
        auto n = format("b%s0", v); // generated variable to pass the block index.
        s ~= format("  int b%d = %s %s %s;\n", c, n, o, bid0);
        parameters ~= [Argument(n, "int", "", "", "", true)];
      }

      auto recalculations = "";
      auto safeguards = "";

      foreach (v; shadowed)
      {
        auto uses = symtable[v].uses;
        Interval[3] local_box;
        Interval[3] global_box;
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
        parameters ~= Argument(sma, "", "__local",format("(%s) * (%s)", bsz0, bsz1), gma);
        s ~= format(template_shared_memory_data_init,
                     li1, li1, bsz1, li1,
                     li0, li0, bsz0, li0, lsz0,
                     li0, tid0, bsz0,
                     sma, li1, bsz0, li0, tid0,
                     gma, gi1, gsz0, gi0, tid0);

        // margin size
        auto msz0 = bsz0 ~ "-" ~ lsz0;
        auto msz1 = bsz1 ~ "-" ~ lsz1;

        auto iv0 = v ~ "_" ~ generate_shared_memory_variable(range_arg[0]);
        auto iv1 = v ~ "_" ~ generate_shared_memory_variable(range_arg[1]);
        recalculations ~= format(template_shared_index_recalculation, iv0, msz0, tid0, iv, lsz0, iv, lsz0,  iv1, msz1, tid0, iv, lsz1, iv, lsz1);
        if (safeguards != "") safeguards ~= " && ";
        safeguards ~= format("(%s) < (%s) && (%s) >= (%s)", iv0, bsz0, iv1, msz1);
      }
      s ~= format(template_antidiagonal_loop_prefix, iv, iv, lsz1, lsz0, iv);
      s ~= format("    int %s = (%s) + tid0 + ((%s) < (%s) ?   0  : (%s) - (%s) + 1);\n", t.children[0].matches[0], g.intervals[0].get_min(), iv, lsz0, iv, lsz0);
      s ~= format("    int %s = (%s) - tid0 + ((%s) < (%s) ? (%s) : (%s)        - 1);\n", t.children[1].matches[0], g.intervals[1].get_min(), iv, lsz1, iv, lsz1);
      s ~= recalculations;
      s ~= format("\n    if (%s)\n", safeguards);
    }
    else
    {
      s ~= "  int tid0 = get_global_id(0);\n";
      auto internal = false;
      for (auto c = 0; c < t.children.length; ++c)
      {
        ParseTree u = t.children[c];
        s ~= (internal)
             ? "  int " ~ u.matches[0] ~ " = c0 + tid0;\n"
             : "  int " ~ u.matches[0] ~ " = r0 - tid1;\n";
        internal = true;
      }
    }
    return s;
  }

  string finish_antidiagonal_range(ParseTree t)
  {
    auto s = "";
    auto shadowed = SList!string();

    foreach (v; symtable)
      if (v.is_array && v.shadow != null)
      {
        auto defs = v.defs;
        if (defs.length > 0)
          shadowed.insert(v.name);
      }

    if (true /*trans.contains("rectangular_blocking")*/)
    {
      s ~= template_antidiagonal_loop_suffix;
      string[3] bix;
      auto r = Box([], []);
      foreach (c, u; range.symbols)
      {
        bix[c] = format("b%d", c);
        r.symbols ~= [u];
        r.s2i[u] = c;
        auto n = "0";
        auto x = block_size;
        r.intervals ~= [
                        Interval(ParseTree("CLOP.IntegerLiteral", true, [n], "", 0, 0, []), ParseTree("CLOP.IntegerLiteral", true, [x], "", 0, 0, []))];
      }
      foreach (v; shadowed)
      {
        auto uses = symtable[v].uses;
        Interval[3] local_box;
        foreach (i, c; uses[0].children)
        {
          local_box[i] = interval_apply(c, r);
        }
        for (auto ii = 1; ii < uses.length; ++ii)
          foreach (i, c; uses[ii].children)
          {
            local_box[i] = interval_union(local_box[i], interval_apply(c, r));
          }
        // thread or work item id
        auto tid0 = "tid0";
        // local index variables
        auto li0 = "li0";
        auto li1 = "li1";
        // size of local index space
        auto lsz0 = block_size;
        auto lsz1 = block_size;
        // size of current array block
        auto bsz0 = local_box[0].get_size();
        auto bsz1 = local_box[1].get_size();
        // global index of current array
        auto gi0 = format("(%s) * (%s) + (%s)", bix[0], lsz0, li0);
        auto gi1 = format("(%s) * (%s) + (%s)", bix[1], lsz1, li1);
        // size of current global array
        auto gsz0 = symtable[v].box[0].get_max();
        // global memory array name
        auto gma = v;
        // shared memory array name
        auto sma = generate_shared_memory_variable(v);
        s ~= format(template_shared_memory_fini,
                     li1, li1, bsz1, li1,
                     li0, li0, bsz0, li0, lsz0,
                     li0, tid0, bsz0,
                     gma, gi1, gsz0, gi0, tid0,
                     sma, li1, bsz0, li0, tid0);
      }
    }
    return s;
  }

  auto generate_horizontal_range (ParseTree t)
  {
    return "";
  }

  auto finish_horizontal_range(ParseTree t)
  {
    return "";
  }

  auto generate_stencil_range (ParseTree t)
  {
    return "";
  }

  auto finish_stencil_range(ParseTree t)
  {
    return "";
  }

  auto generate_ndrange (ParseTree t)
  {
    return "";
  }

  string generate_ndrange(ParseTree t)
  {
    string s = "";
    ulong n = t.children.length;
    for (auto c = 0; c < n; ++c)
    {
      ParseTree u = t.children[c];
      string name = u.matches[0];
      symtable[name] = Symbol(name, "int", t, null, null, null, null, true, false, false);
      s ~= "  int " ~ name ~ " = get_global_id(" ~ to!string(n - 1 - c) ~ ");\n";
    }
    return s;
  }

  string finish_ndrange(ParseTree t)
  {
    return "";
  }

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

  string set_params()
  {
    string result = "string param;\n";
    auto issued_declaration = false;
    foreach(p; parameters)
    {
      if (p.type == "")
      {
        if (p.back == "")
        {
          result ~= format("param = mixin(set_kernel_param!(typeof(%s))(\"%s\",\"%s\"));\n", p.name, p.name, p.back);
        }
        else
        {
          result ~= format("param = mixin(set_kernel_param!(typeof(%s))(\"%s\",\"%s\"));\n", p.back, p.name, p.back);
        }
      }
      else
      {
        result ~= format("param = \"%s %s\";\n", p.type, p.name);
      }
      if (issued_declaration)
      {
        result ~= "if (param != \"\") kernel_params ~= \", \" ~ param;\n";
      }
      else
      {
        result ~= "string kernel_params = param;\n";
        issued_declaration = true;
      }
    }
    return result;
  }

  string set_args(string kernel)
  {
    auto result = "";
    foreach(i, p; parameters)
    {
      if (!p.skip)
      {
        if (p.back == "")
        {
          result ~= format("mixin(set_kernel_arg!(typeof(%s))(\"%s\",\"%d\",\"%s\",\"\",\"\"));\n", p.name, kernel, i, p.name);
        }
        else
        {
          result ~= format("mixin(set_kernel_arg!(typeof(%s))(\"%s\",\"%d\",\"null\",\"%s\",\"%s\"));\n", p.back, kernel, i, p.back, p.size);
        }
      }
    }
    return result;
  }

  string code_to_read_data_from_device()
  {
    string result = "";
    foreach (k, v; symtable)
    {
      if (!v.is_local && v.type == "" && v.defs.length > 0)
      {
        result ~= "mixin(read_device_buffer!(typeof(" ~ k ~ "))(\"" ~ k ~ "\"));\n";
      }
    }
    return result;
  }

  string code_to_invoke_kernel()
  {
    debug (DEBUG_GRAMMAR)
    {
      return "";
    }
    else
    {
      auto gsz0 = range.intervals[0].get_max();
      auto bsz0 = block_size;
      auto bsz1 = block_size;
      auto name = "clop_opencl_kernel";
      auto n = 0;
      ulong argindex[2];
      foreach (i, p; parameters)
        if (p.skip)
          argindex[n++] = i;
      return format(template_antidiagonal_rectangular_blocks_invoke_kernel, gsz0, bsz0, bsz0, bsz0, name, argindex[0], name, argindex[1], name);
    }
  }

  /++
   +/
  string translate_index_expression(string v, ParseTree t)
  {
    if (t.children.length == 1 || t.children.length != range.get_dimensionality() || v !in symtable || symtable[v].box is null)
    {
      return translate(t);
    }
    auto s = "(" ~ translate(t.children[0]) ~ ")";
    auto m = "(" ~ symtable[v].box[0].get_size() ~ ")";
    foreach (i, c; t.children[1 .. $])
    {
      s ~= " + " ~ m ~ " * (" ~ translate(c) ~ ")";
      m ~= " * (" ~ symtable[v].box[i + 1].get_size() ~ ")";
    }
    return s;
  }

  string translate(ParseTree t)
  {
    switch (t.name)
    {
    case "CLOP.Declaration":
      {
        auto s = indent ~ translate( t.children[0] );
        if ( t.children.length > 1 )
          s ~= " " ~ translate( t.children[1] );
        return s ~ ";\n";
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
        return reduce!((a, b) => a ~ ", " ~ translate(b))(translate(t.children[0]), t.children[1 .. $]);
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
    case "CLOP.StatementList":
      {
        return reduce!((a, b) => a ~ translate(b))("", t.children);
      }
    case "CLOP.Statement":
      {
        return translate(t.children[0]);
      }
    case "CLOP.CompoundStatement":
      {
        indent ~= "  ";
        auto s = translate(t.children[0]);
        indent = indent[0 .. $ - 2];
        return t.children.length == 1 ? indent ~ "{\n" ~ s ~ indent ~ "}\n" : "{}\n";
      }
    case "CLOP.ExpressionStatement":
      {
        return indent ~ translate(t.children[0]) ~ ";\n";
      }
    case "CLOP.IfStatement":
      {
        auto s = indent ~ "if (" ~ translate(t.children[0]) ~ ")\n" ~ translate(t.children[1]);
        if (t.children.length == 3)
          s ~= indent ~ "else\n" ~ translate(t.children[2]);
        return s;
      }
    case "CLOP.ReturnStatement":
      {
        return indent ~ (t.children.length == 1 ? "return " ~ translate(t.children[0]) ~ ";\n" : "return;\n");
      }
    case "CLOP.PrimaryExpr":
      {
        auto s = translate(t.children[0]);
        return "(" == t.matches[0] ? "(" ~ s ~ ")" : s;
      }
    case "CLOP.PostfixExpr":
      {
        auto s = translate(t.children[0]);
        if (t.children.length == 2)
        {
          if (t.children[1].name == "CLOP.Expression")
          {
            if (t.children[0].name == "CLOP.PrimaryExpr" && t.children[0].children[0].name == "CLOP.Identifier")
            {
              s ~= "[" ~ translate_index_expression(t.children[0].children[0].matches[0], t.children[1]) ~ "]";
            }
            else
            {
              s ~= "[" ~ translate(t.children[1]) ~ "]";
            }
          }
          else if (t.children[1].name == "CLOP.ArgumentExprList")
          {
            s ~= "(" ~ translate(t.children[1]) ~ ")";
          }
          else
          {
            s ~= "." ~ translate(t.children[1]);
          }
        }
        else
        {
          if (")" == t.matches[$ - 1])
          {
            s ~= "()";
          }
          else if ("++" == t.matches[$ - 1])
          {
            s ~= "++";
          }
          else if ("--" == t.matches[$ - 1])
          {
            s ~= "--";
          }
        }
        return s;
      }
    case "CLOP.ArgumentExprList":
      {
        return reduce!((a, b) => a ~ ", " ~ translate(b))(translate(t.children[0]), t.children[1 .. $]);
      }
    case "CLOP.UnaryExpr":
      {
        return t.children.length == 1 ? translate(t.children[0])
          : t.children[0].matches[0] ~ translate(t.children[1]);
      }
    case "CLOP.IncrementExpr":
      {
        return "++" ~ translate(t.children[0]);
      }
    case "CLOP.DecrementExpr":
      {
        return "--" ~ translate(t.children[0]);
      }
    case "CLOP.CastExpr":
      {
        return t.children.length == 1 ? translate(t.children[0])
          : "(" ~ translate(t.children[0]) ~ ")" ~ translate(t.children[1]);
      }
    case "CLOP.MultiplicativeExpr", "CLOP.AdditiveExpr", "CLOP.ShiftExpr":
    case "CLOP.RelationalExpr", "CLOP.EqualityExpr":
      {
        auto s = translate(t.children[0]);
        for (auto i = 1; i < t.children.length; i += 2)
          s ~= " " ~ t.children[i].matches[0] ~ " " ~ translate(t.children[i + 1]);
        return s;
      }
    case "CLOP.ANDExpr":
      {
        return reduce!((a, b) => a ~ " & " ~ translate(b))(translate(t.children[0]), t.children[1 .. $]);
      }
    case "CLOP.ExclusiveORExpr":
      {
        return reduce!((a, b) => a ~ " ^ " ~ translate(b))(translate(t.children[0]), t.children[1 .. $]);
      }
    case "CLOP.InclusiveORExpr":
      {
        return reduce!((a, b) => a ~ " | " ~ translate(b))(translate(t.children[0]), t.children[1 .. $]);
      }
    case "CLOP.LogicalANDExpr":
      {
        return reduce!((a, b) => a ~ " && " ~ translate(b))(translate(t.children[0]), t.children[1 .. $]);
      }
    case "CLOP.LogicalORExpr":
      {
        return reduce!((a, b) => a ~ " || " ~ translate(b))(translate(t.children[0]), t.children[1 .. $]);
      }
    case "CLOP.ConditionalExpr":
      {
        auto s = translate(t.children[0]);
        if (t.children.length == 3)
        {
          s ~= " ? " ~ translate(t.children[1]) ~ " : " ~ translate(t.children[2]);
        }
        return s;
      }
    case "CLOP.AssignmentExpr":
      {
        if (t.children.length == 3)
        {
          string lhs = translate(t.children[0]);
          string rhs = translate(t.children[2]);
          return lhs ~ " " ~ t.children[1].matches[0] ~ " " ~ rhs;
        }
        else
        {
          return translate(t.children[0]);
        }
      }
    case "CLOP.Expression":
      {
        return reduce!((a, b) => a ~ ", " ~ translate(b))(translate(t.children[0]), t.children[1 .. $]);
      }
    case "CLOP.Identifier":
      {
        string s = t.matches[0];
        if (use_shadow && s in symtable && symtable[s].shadow != null)
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
