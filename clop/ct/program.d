/*
 *  The MIT License (MIT)
 *  =====================
 *
 *  Copyright (c) 2015 Dmitri Makarov <dmakarov@alumni.stanford.edu>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in all
 *  copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *  SOFTWARE.
 */
module clop.ct.program;

import std.algorithm : reduce;
import std.container;
import std.conv;
import std.string;
import std.traits;
debug (UNITTEST_DEBUG) import std.stdio;

import pegged.grammar;

import clop.ct.analysis;
import clop.ct.structs;
import clop.ct.symbol;
static import clop.ct.templates;
import clop.ct.transform;

/++
 +  The container of all information related to a specific variant
 +  a CLOP program fragment.
 +/
struct Program
{
  /++
   + Return the string that is this program's kernel source code.
   +/
  string generate_code()
  {
    auto t = optimize(AST); // apply the optimizing transformations on the AST
    auto kbody = translate(t);  // generate OpenCL kernel source code from the AST
    auto kname = generate_kernel_name(); // we need to give the kernel a name
    generate_code_setting_kernel_parameters("instance_" ~ suffix ~ ".kernel");
    auto kernel = "macros ~ q{" ~ external ~ "} ~ \n"
                ~ "          \"__kernel void " ~ kname
                ~ "(\" ~ kernel_params ~ \")\" ~q{\n" ~ kbody ~ "          }";
    auto clhost = format(clop.ct.templates.template_create_opencl_kernel,
                         suffix, suffix, suffix, suffix, suffix,
                         suffix, kernel_parameter_list_code, suffix, kname , kernel);
    clhost ~= kernel_set_args_code
            ~ generate_code_to_invoke_kernel()
            ~ generate_code_to_pull_from_device()
            ~ generate_code_to_release_buffers();
    auto diagnostics = format("static if (\"%s\" != \"\")\n  pragma (msg, \"%s\");\n", errors, errors);
    return diagnostics ~ format(clop.ct.templates.template_clop_unit, clhost, toString());
  }

  @property
  string toString()
  {
    import std.algorithm : map, reduce;
    auto prefix = suffix;
    if (pattern !is null)
    {
      prefix ~= "_" ~ pattern;
    }
    return prefix ~ reduce!((a, b) => a ~ "_" ~ b)("", map!(a => a.toString)(trans));
  }

 private:

  Symbol[string] symtable; ///
  Transformation[] trans;  ///
  Argument[] parameters;   ///
  ParseTree AST;           ///
  Box range;               ///
  string[] lws;            ///
  string pattern;          ///
  string external;         ///
  string errors;           ///
  string suffix;           ///

  string indent = "            ";
  string block_size = "8"; ///
  bool use_shadow = false; ///
  string kernel_parameter_list_code;
  string kernel_set_args_code;
  uint temporary_id;

  struct translation_result
  {
    string stmts;
    string value;
  }

  /++
   +
   +/
  ParseTree optimize(ParseTree t)
  {
    auto r = t;
    foreach (f; trans)
      r = apply_trans(f, r);
    return r;
  }

  ParseTree apply_trans(Transformation f, ParseTree t)
  {
    switch (f.name)
    {
    case "rectangular_blocking":
      return apply_rectangular_blocking(t);
    default:
      return t.dup;
    }
  }

  auto generate_shared_memory_variable(string v)
  {
    return v ~ "_in_shared_memory";
  }

  auto generate_kernel_name()
  {
    return format("clop_kernel_main_%s", toString());
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
        parameters ~= [Argument(n, `"int "`, "", "", "", true)];
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
        parameters ~= Argument(sma, "", "__local", format("(%s) * (%s)", bsz0, bsz1), gma);
        s ~= format(clop.ct.templates.template_shared_memory_data_init,
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
        recalculations ~= format(clop.ct.templates.template_shared_index_recalculation, iv0, msz0, tid0, iv, lsz0, iv, lsz0,  iv1, msz1, tid0, iv, lsz1, iv, lsz1);
        if (safeguards != "") safeguards ~= " && ";
        safeguards ~= format("(%s) < (%s) && (%s) >= (%s)", iv0, bsz0, iv1, msz1);
      }
      s ~= format(clop.ct.templates.template_antidiagonal_loop_prefix, iv, iv, lsz1, lsz0, iv);
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
      s ~= clop.ct.templates.template_antidiagonal_loop_suffix;
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
        s ~= format(clop.ct.templates.template_shared_memory_fini,
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

  /++
   + Given a string that represents a name of a variable, return a
   + string that represents a parameter declaration that contains the
   + type of the parameter matching the variable's type and the
   + parameter name matching the variable's name.
   +/
  void generate_code_setting_kernel_parameters(string kernel)
  {
    auto i = 0U;
    auto comma = "";
    kernel_parameter_list_code = q{
          auto kernel_params = "", macros = "";};
    kernel_set_args_code = "          // setting up buffers and kernel arguments";
    foreach(j, p; parameters)
    {
      if (!p.is_macro)
      {
        parameters[j].number = i;
        kernel_parameter_list_code ~= format(q{
          kernel_params ~= "%s%s" ~ %s ~ "%s";},
          comma, p.qual, p.type, p.name);
        if (p.to_push != "")
        {
          kernel_set_args_code ~= p.to_push;
        }
        if (!p.skip)
        {
          kernel_set_args_code ~= format(q{
          runtime.status = clSetKernelArg(%s, %s, %s, %s);
          assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: %s"));
          }, kernel, i, p.size, p.address, i);
        }
        comma = ", ";
        ++i;
      }
      else
      {
        kernel_parameter_list_code ~= format(q{
          macros ~= "#define %s " ~ std.format.format("%%s\n", %s);},
          p.name, p.name);
      }
    }
  }

  string generate_code_to_pull_from_device()
  {
    auto result = "";
    foreach (p; parameters)
    {
      auto v = symtable[p.name];
      if (p.to_pull != "" && v.defs.length > 0)
      {
        result ~= p.to_pull;
      }
    }
    return result;
  }

  string generate_code_to_release_buffers()
  {
    auto result = "";
    foreach (p; parameters)
    {
      if (p.to_release != "")
      {
        result ~= p.to_release;
      }
    }
    return result;
  }

  /++
   + Generate the host code that invokes the OpenCL kernel with proper
   + synchronization pattern.  The code defines the parameters of
   + the kernel's NDRange and then uses the clEnqueueNDRangeKernel
   + call to launch the kernel.
   +/
  string generate_code_to_invoke_kernel()
  {
    if (pattern is null)
    {
      auto wgsize = lws is null ? "null" : "[" ~ reduce!`a ~ "," ~ b`(lws[0], lws[1 .. $]) ~ "]";
      auto global = "[" ~ range.get_interval_sizes() ~ "]";
      auto offset = "[" ~ range.get_lower_bounds() ~ "]";
      auto kernel = "instance_" ~ suffix ~ ".kernel";
      auto dimensions = range.get_dimensions();
      return format(clop.ct.templates.template_plain_invoke_kernel, offset, global, wgsize, kernel, dimensions);
    }
    if (pattern == "Antidiagonal")
    {
      if (true) // plain anti-diagonal pattern, no blocking.
      {
        auto gsz0 = range.intervals[0].get_max();
        auto name = "instance_" ~ suffix ~ ".kernel";
        return format(clop.ct.templates.template_antidiagonal_invoke_kernel,
                      gsz0, gsz0, gsz0, name, parameters[$ - 1].number, name);
      }
      else
      {
        auto gsz0 = range.intervals[0].get_max();
        auto bsz0 = block_size;
        auto bsz1 = block_size;
        auto name = "instance_" ~ suffix ~ ".kernel";
        auto n = 0;
        ulong[2] argindex;
        foreach (i, p; parameters)
          if (p.skip)
            argindex[n++] = i;
        return format(clop.ct.templates.template_antidiagonal_rectangular_blocks_invoke_kernel,
                      gsz0, bsz0, bsz0, bsz0, name, argindex[0], name, argindex[1], name);
      }
    }
    return "";
  }

  /++
   +  For multidimensional arrays linearize the index expressions.
   +/
  translation_result translate_index_expression(string v, ParseTree t)
  {
    string value = "", stmts = "";
    if (t.children.length == 1 || t.children.length != range.get_dimensions() || v !in symtable || symtable[v].box is null)
    {
      return translate_expression(t);
    }
    auto r = translate_expression(t.children[0]);
    value = r.value;
    stmts = r.stmts;
    foreach (i, c; t.children[1 .. $])
    {
      auto q = translate_expression(c);
      stmts ~= q.stmts;
      value = "(" ~ value ~ ") * (" ~ symtable[v].box[i + 1].get_size() ~ ") + " ~ q.value;
    }
    return translation_result(stmts, value);
  }

  /++
   +
   +/
  Symbol create_new_temporary(string lhs)
  {
    auto n = format("clop_tmp_%s", temporary_id++);
    // FIXME: get the type from lhs.
    auto t = "float";
    auto s = Symbol(n, t);
    symtable[n] = s;
    return s;
  }

  /++
   +
   +/
  translation_result translate_expression(ParseTree t)
  {
    string stmts = "", value = "";
    switch (t.name)
    {
    case "CLOP.InitDeclaratorList":
      {
        foreach (i, c; t.children)
        {
          auto r = translate_expression(c);
          stmts ~= r.stmts;
          value ~= (i == 0) ? r.value : ", " ~ r.value;
        }
        break;
      }
    case "CLOP.InitDeclarator":
      {
        value = translate(t.children[0]);
        if (t.children.length > 1)
        {
          auto r = translate_expression(t.children[1]);
          value ~= " = " ~ r.value;
          stmts = r.stmts;
        }
        break;
      }
    case "CLOP.Initializer":
      {
        auto r = translate_expression(t.children[0]);
        value = r.value;
        stmts = r.stmts;
        break;
      }
    case "CLOP.PrimaryExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = "(" == t.matches[0] ? "(" ~ r.value ~ ")" : r.value;
        stmts = r.stmts;
        break;
      }
    case "CLOP.PostfixExpr":
      {
        auto is_snippet = t.children[0].children.length == 2;
        if (is_snippet)
        {
          switch (t.children[0].children[0].matches[0])
          {
          case "reduce":
            {
              auto func_name = "clop_binary_function";
              auto type = "float";
              auto length = "n - 1";
              value = "reduce_result";
              stmts = "              " ~ type ~ " " ~ value ~ ";";
              stmts ~= clop.ct.templates.reduce.instantiate_template(func_name, t.children[1], value, length);
              debug (UNITTEST_DEBUG) writefln("UT:%s template expanded to %s", suffix, stmts);
            }
            break;
          default: {/+ do nothing +/}
          }
        }
        else if (t.children.length == 2)
        {
          auto r = translate_expression(t.children[0]);
          value = r.value;
          stmts = r.stmts;
          if (t.children[1].name == "CLOP.Expression")
          {
            if (t.children[0].name == "CLOP.PrimaryExpr" &&
                t.children[0].children[0].name == "CLOP.Identifier")
            {
              auto q = translate_index_expression(t.children[0].children[0].matches[0], t.children[1]);
              value ~= "[" ~ q.value ~ "]";
              stmts ~= q.stmts;
            }
            else
            {
              r = translate_expression(t.children[1]);
              value ~= "[" ~ r.value ~ "]";
              stmts = r.stmts;
            }
          }
          else if (t.children[1].name == "CLOP.ArgumentExprList")
          {
            r = translate_expression(t.children[1]);
            value ~= "(" ~ r.value ~ ")";
            stmts = r.stmts;
          }
          else
          {
            r = translate_expression(t.children[1]);
            value ~= "." ~ r.value;
            stmts = r.stmts;
          }
        }
        else
        {
          auto r = translate_expression(t.children[0]);
          value = r.value;
          stmts = r.stmts;
          if (t.matches.length > 2 && "(" == t.matches[$ - 2] && ")" == t.matches[$ - 1])
          {
            value ~= "()";
          }
          else if ("++" == t.matches[$ - 1])
          {
            value ~= "++";
          }
          else if ("--" == t.matches[$ - 1])
          {
            value ~= "--";
          }
        }
        break;
      }
    case "CLOP.ArgumentExprList":
      {
        foreach (i, c; t.children)
        {
          auto r = translate_expression(c);
          value ~= i == 0 ? r.value : ", " ~ r.value;
          stmts ~= r.stmts;
        }
        break;
      }
    case "CLOP.UnaryExpr":
      {
        if (t.children.length == 1)
        {
          auto r = translate_expression(t.children[0]);
          value = r.value;
          stmts = r.stmts;
        }
        else
        {
          auto r = translate_expression(t.children[1]);
          value = t.children[0].matches[0] ~ r.value;
          stmts = r.stmts;
        }
        break;
      }
    case "CLOP.IncrementExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = "++" ~ r.value;
        stmts = r.stmts;
        break;
      }
    case "CLOP.DecrementExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = "--" ~ r.value;
        stmts = r.stmts;
        break;
      }
    case "CLOP.CastExpr":
      {
        if (t.children.length == 1)
        {
          auto r = translate_expression(t.children[0]);
          value = r.value;
          stmts = r.stmts;
        }
        else
        {
          auto r = translate_expression(t.children[1]);
          value = "(" ~ translate(t.children[0]) ~ ")" ~ r.value;
          stmts = r.stmts;
        }
        break;
      }
    case "CLOP.MultiplicativeExpr", "CLOP.AdditiveExpr", "CLOP.ShiftExpr":
    case "CLOP.RelationalExpr", "CLOP.EqualityExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = r.value;
        stmts = r.stmts;
        for (auto i = 1; i < t.children.length; i += 2)
        {
          r = translate_expression(t.children[i + 1]);
          value ~= " " ~ t.children[i].matches[0] ~ " " ~ r.value;
          stmts ~= r.stmts;
        }
        break;
      }
    case "CLOP.ANDExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = r.value;
        stmts = r.stmts;
        foreach (i; 1 .. t.children.length)
        {
          r = translate_expression(t.children[i]);
          value ~= " & " ~ r.value;
          stmts ~= r.stmts;
        }
        break;
      }
    case "CLOP.ExclusiveORExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = r.value;
        stmts = r.stmts;
        foreach (i; 1 .. t.children.length)
        {
          r = translate_expression(t.children[i]);
          value ~= " ^ " ~ r.value;
          stmts ~= r.stmts;
        }
        break;
      }
    case "CLOP.InclusiveORExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = r.value;
        stmts = r.stmts;
        foreach (i; 1 .. t.children.length)
        {
          r = translate_expression(t.children[i]);
          value ~= " | " ~ r.value;
          stmts ~= r.stmts;
        }
        break;
      }
    case "CLOP.LogicalANDExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = r.value;
        stmts = r.stmts;
        foreach (i; 1 .. t.children.length)
        {
          r = translate_expression(t.children[i]);
          value ~= " && " ~ r.value;
          stmts ~= r.stmts;
        }
        break;
      }
    case "CLOP.LogicalORExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = r.value;
        stmts = r.stmts;
        foreach (i; 1 .. t.children.length)
        {
          r = translate_expression(t.children[i]);
          value ~= " || " ~ r.value;
          stmts ~= r.stmts;
        }
        break;
      }
    case "CLOP.ConditionalExpr":
      {
        auto r = translate_expression(t.children[0]);
        value = r.value;
        stmts = r.stmts;
        if (t.children.length == 3)
        {
          auto then_part = translate_expression(t.children[1]);
          auto else_part = translate_expression(t.children[2]);
          value ~= " ? " ~ then_part.value ~ " : " ~ else_part.value;
          stmts ~= then_part.stmts ~ else_part.stmts;
        }
        break;
      }
    case "CLOP.AssignmentExpr":
      {
        if (t.children.length == 3)
        {
          auto lhs = translate_expression(t.children[0]);
          stmts = lhs.stmts;
          auto rhs = translate_expression(t.children[2]);
          stmts ~= rhs.stmts;
          value = lhs.value ~ " " ~ t.children[1].matches[0] ~ " " ~ rhs.value;
        }
        else
        {
          auto r = translate_expression(t.children[0]);
          value = r.value;
          stmts = r.stmts;
        }
        break;
      }
    case "CLOP.Expression":
      {
        foreach (i, c; t.children)
        {
          auto r = translate_expression(c);
          value ~= i == 0 ? r.value : ", " ~ r.value;
          stmts ~= r.stmts;
        }
        break;
      }
    case "CLOP.Identifier":
      {
        value = t.matches[0];
        if (use_shadow && value in symtable && symtable[value].shadow != null)
          value = symtable[value].shadow;
        break;
      }
    case "CLOP.IntegerLiteral":
    case "CLOP.FloatLiteral":
      {
        value = t.matches[0];
        break;
      }
    default:
      break;
    }
    return translation_result(stmts, value);
  }

  /++
   +
   +/
  string translate(ParseTree t)
  {
    switch (t.name)
    {
    case "CLOP.Declaration":
      {
        auto s = indent ~ translate(t.children[0]);
        if (t.children.length > 1)
        {
          auto r = translate_expression(t.children[1]);
          s = r.stmts ~ s ~ " " ~ r.value;
        }
        return s ~ ";\n";
      }
    case "CLOP.Declarator":
      {
        string s = translate(t.children[0]);
        string b = t.matches[$ - 1] == "]" ? "[" : "(";
        if (t.children.length > 1)
        {
          if (t.children[1].name == "CLOP.ConditionalExpr")
          {
            auto r = translate_expression(t.children[1]);
            s = r.stmts ~ s ~ b ~ r.value ~ t.matches[$ - 1];
          }
          else
          {
            s ~= b ~ translate(t.children[1]) ~ t.matches[$ - 1];
          }
        }
        return s;
      }
    case "CLOP.DeclarationSpecifiers":
      {
        string s = translate(t.children[0]);
        if (t.children.length > 1)
          s ~= " " ~ translate(t.children[1]);
        return s;
      }
    case "CLOP.StorageClassSpecifier":
      {
        return "//";
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
        return translate(t.children[0]) ~ " " ~ translate(t.children[1]);
      }
    case "CLOP.StatementList":
      {
        return reduce!((a, b) => a ~ translate(b))("", t.children);
      }
    case "CLOP.Statement":
      {
        debug (UNITTEST_DEBUG) writefln("UT:%s Program.translate case CLOP.Statement: %s", suffix, t.matches);
        return translate(t.children[0]);
      }
    case "CLOP.CompoundStatement":
      {
        debug (UNITTEST_DEBUG)
        {
          writefln("UT:%s Program.translate case CLOP.CompoundStatement: %s", suffix, t.matches);
        }
        indent ~= "  ";
        auto s = translate(t.children[0]);
        indent = indent[0 .. $ - 2];
        return t.children.length == 1 ? indent ~ "{\n" ~ s ~ indent ~ "}\n" : "{}\n";
      }
    case "CLOP.ExpressionStatement":
      {
        debug (UNITTEST_DEBUG) writefln("UT:%s Program.translate case CLOP.ExpressionStatement: %s", suffix, t.matches);
        auto r = translate_expression(t.children[0]);
        return r.stmts ~ indent ~ r.value ~ ";\n";
      }
    case "CLOP.IfStatement":
      {
        auto r = translate_expression(t.children[0]);
        indent ~= "  ";
        auto then_part = translate(t.children[1]);
        auto else_part = "";
        if (t.children.length == 3)
          else_part = translate(t.children[2]);
        indent = indent[0 .. $ - 2];
        auto s = format("%s%sif (%s)\n%s", r.stmts, indent, r.value, then_part);
        if (else_part != "")
          s ~= indent ~ "else\n" ~ else_part;
        return s;
      }
    case "CLOP.IterationStatement":
      {
        return translate(t.children[0]);
      }
    case "CLOP.ForStatement":
      {
        auto s = indent;
        // FIXME: each of the children is optional. check they exist.
        auto num_children = t.children.length;
        auto num_of_matches = t.children[0].matches.length;
        auto has_init_part = num_children > 1 && t.matches[2] != ";";
        auto has_cond_part = has_init_part && num_children > 2 && t.matches[3 + num_of_matches] != ";" ||
                            !has_init_part && num_children > 1 && t.matches[3] != ";";
        auto has_step_part = has_init_part &&  has_cond_part && num_children == 4 ||
                             has_init_part && !has_cond_part && num_children == 3 ||
                            !has_init_part &&  has_cond_part && num_children == 3 ||
                            !has_init_part && !has_cond_part && num_children == 2;
        auto init_part = "";
        auto cond_part = "";
        auto step_part = "";
        auto index = 0;
        if (has_init_part)
        {
          auto r = translate_expression(t.children[index++]);
          s ~= r.stmts;
          init_part = r.value;
        }
        if (has_cond_part)
        {
          auto r = translate_expression(t.children[index++]);
          s ~= r.stmts;
          cond_part = r.value;
        }
        if (has_step_part)
        {
          auto r = translate_expression(t.children[index++]);
          s ~= r.stmts;
          step_part = r.value;
        }
        indent ~= "  ";
        auto loop_body = translate(t.children[index]);
        indent = indent[0 .. $ - 2];
        s ~= indent ~ "for (" ~ init_part ~ ";" ~ cond_part ~ ";" ~ step_part ~ ")\n"
          ~ indent ~ loop_body;
        return s;
      }
    case "CLOP.ReturnStatement":
      {
        return indent ~ (t.children.length == 1 ? "return " ~ translate(t.children[0]) ~ ";\n" : "return;\n");
      }
    case "CLOP.Identifier":
      {
        auto s = t.matches[0];
        if (use_shadow && s in symtable && symtable[s].shadow != null)
          s = symtable[s].shadow;
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

} // Program struct

unittest
{
  static import clop.ct.parser;
  static import clop.ct.stage2;
  static import clop.rt.ndarray;

  auto t = clop.ct.parser.CLOP(q{
      NDRange(i : 1 .. h_n, j : 0 .. i_n)
      {
        local float t[i_n];
        t[j] = inputs[j] * weights[i, j];
        float s = reduce!"a + b"(0, t);
        if (j == 0)
        {
          hidden[i] = ONEF / (ONEF + exp(-s));
        }
      }});
  alias A = clop.rt.ndarray.NDArray!float;
  alias T = std.typetuple.TypeTuple!(size_t, float*, size_t, A, A, A);
  auto be = clop.ct.stage2.Backend!T(t, __FILE__, __LINE__, ["h_n", "t", "i_n", "inputs", "weights", "hidden"]);
  be.analyze(t);
  be.update_parameters();
  be.lower(t);
  be.compute_intervals();
  auto p = Program(be.symtable, [], be.parameters, be.KBT, be.range, be.lws, be.pattern, "", "", be.suffix);
  auto k = p.translate(be.KBT);
  debug (UNITTEST_DEBUG) writefln("UT:%s k:\n%s", be.suffix, k);
  auto c = p.generate_code();
  debug (UNITTEST_DEBUG) writefln("UT:%s c:\n%s", be.suffix, c);
}

// Local Variables:
// compile-command: "../../tests/test_module clop.ct.program"
// flycheck-dmd-include-path: ("~/.dub/packages/pegged-0.2.1")
// End:
