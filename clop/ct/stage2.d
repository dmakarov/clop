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
module clop.ct.stage2;

import std.algorithm : reduce;
import std.container;
import std.conv;
import std.string;
import std.traits;

import clop.ct.analysis;
import clop.ct.parser;
import clop.ct.program;
import clop.ct.structs;
import clop.ct.symbol;
import clop.ct.templates;
import clop.ct.transform;
import clop.rt.ndarray;

template Backend(TList...)
{
  import std.typetuple;
  alias TT = TypeTuple!(TList[0 .. $]);

  /++
   +  The CLOP compiler backend.
   +/
  struct Backend
  {
    Box range;                /// box of intervals for this piece
    ParseTree AST;            /// the tree for this piece
    ParseTree KBT;            /// kernel body compound statement AST
    Program[] variants;       /// one variant for each set of transformations
    Argument[] parameters;    /// kernel parameters
    Symbol[string] symtable;  /// the symbol table
    Set!Transformation trans; /// set of optimizing transformations

    string suffix;
    string errors;
    string pattern;
    string external;
    string source_file;
    size_t source_line;

    bool global_scope = false;
    bool eval_def     = false;
    uint depth        = 0;

    /++
     +
     +/
    this(ParseTree t, string file, size_t line, immutable(string[]) params)
    {
      import std.path : baseName, stripExtension;
      suffix = format("%s_%s", baseName(stripExtension(file)), line);
      errors = "";
      external = "";
      source_file = file;
      source_line = line;
      range = Box();
      AST   = t;
      trans = Set!Transformation();
      foreach (p; params)
        parameters ~= [Argument(p)];
    }

    /++
     +
     +/
    string generate_code()
    {
      analyze(AST);
      compute_intervals();
      update_parameters();
      // before we generate optimized variants, we need to transform
      // the AST for a specific synchronization pattern.
      auto t = transform_by_pattern(KBT);
      apply_optimizations(t);
      auto code = "";
      debug (VERBOSE)
      {
        code ~= "// Transformations <" ~ pattern ~ trans.toString ~ ">\n";
        code ~= dump_parameters();
        code ~= dump_symtable();
        code ~= dump_arraytab();
        code ~= dump_intervals();
      }
      // @FIXME we need to backup the data that can be overwritten by
      // each new variant. If an array is read from device and written
      // to host we must copy the array before the variant starts and
      // restore the array before the next variant starts.
      foreach (v; variants)
      {
        code ~= v.generate_code();
      }
      return code;
    }

    // The following methods are internal to the struct and not used outside.

  private:

    /++
     +
     +/
    void analyze(ParseTree t)
    {
      switch (t.name)
      {
      case "CLOP":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.TranslationUnit":
        {
          if (t.children[0].name == "CLOP.ExternalDeclarations")
          {
            auto x = t.children[0];
            foreach (m; x.matches)
            {
              external ~= " " ~ m;
            }
            analyze(t.children[0]);
            analyze(t.children[1]);
          }
          else
          {
            analyze(t.children[0]);
          }
          break;
        }
      case "CLOP.KernelBlock":
        {
          uint i = 0;
          if (t.children[i].name == "CLOP.SyncPattern")
          {
            pattern = t.children[i++].matches[0];
          }
          if (t.children[$ - 1].name == "CLOP.Transformations")
          {
            analyze(t.children[$ - 1]);
          }
          analyze(t.children[i++]);
          global_scope = true;
          if (t.children[i].name == "CLOP.CompoundStatement")
          {
            KBT = t.children[i];
          }
          analyze(t.children[i]);
          break;
        }
      case "CLOP.ExternalDeclarations":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.ExternalDeclaration":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.FunctionDefinition":
        {
          // add function name to the symbol table
          analyze(t.children[1]);
          break;
        }
      case "CLOP.RangeDecl":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.RangeList":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.RangeSpec":
        {
          auto s = t.matches[0];
          auto d = range.get_dimensions();
          symtable[s] = Symbol(s, "int", t, null, null, null, "clop_local_index_" ~ s, true, false, false);
          range.intervals ~= [Interval(t.children[1], t.children[2])];
          range.symbols ~= [s];
          range.s2i[s] = d;
          bool saved_global_scope_value = global_scope;
          global_scope = true;
          analyze(t.children[1]);
          analyze(t.children[2]);
          global_scope = saved_global_scope_value;
          break;
        }
      case "CLOP.Transformations":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.TransList":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.TransSpec":
        {
          string[] params;
          if (t.children.length > 1)
            foreach (c; t.children[1].children)
              params ~= c.matches[0];
          trans.insert(Transformation(t.matches[0], params));
          break;
        }
      case "CLOP.Declaration":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.Declarator":
        {
          foreach (c; t.children)
          {
            auto saved_global_scope_value = global_scope;
            global_scope = false;
            analyze(c);
          global_scope = saved_global_scope_value;
          }
          break;
        }
      case "CLOP.TypeSpecifier":
        {
          break;
        }
      case "CLOP.StructSpecifier":
        {
          break;
        }
      case "CLOP.InitDeclaratorList":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.InitDeclarator":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.InitializerList":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.Initializer":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.ParameterList":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.ParameterDeclaration":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.TypeName":
        {
          break;
        }
      case "CLOP.StatementList":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.Statement":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.CompoundStatement":
        {
          t.children.length == 1 && analyze(t.children[0]);
          break;
        }
      case "CLOP.ExpressionStatement":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.IfStatement":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.IterationStatement":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.ForStatement":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.ReturnStatement":
        {
          t.children.length == 1 && analyze(t.children[0]);
          break;
        }
      case "CLOP.PrimaryExpr":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.PostfixExpr":
        {
          analyze(t.children[0]);
          if (t.children.length == 1)
          {
            return;
          }
          analyze(t.children[1]);
          if (t.children[1].name != "CLOP.Expression" || t.children[0].children[0].name != "CLOP.Identifier")
          {
            return;
          }
          // this is PostfixExpr.PrimaryExpr.Identifier
          auto symbol = t.children[0].children[0].matches[0];
          // analyze index expression
          if (!symtable[symbol].is_array)
          {
            symtable[symbol].is_array = true;
            // test PostfixExpr.Expression for transformability
            if (can_transform_index_expression(t.children[1]))
            {
              symtable[symbol].can_cache = true;
            }
          }
          eval_def ? symtable[symbol].defs : symtable[symbol].uses ~= t.children[1];
          break;
        }
      case "CLOP.ArgumentExprList":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.UnaryExpr":
      case "CLOP.IncrementExpr":
      case "CLOP.DecrementExpr":
        {
          analyze(t.children[0]);
          break;
        }
      case "CLOP.CastExpr":
      case "CLOP.MultiplicativeExpr":
      case "CLOP.AdditiveExpr":
      case "CLOP.ShiftExpr":
      case "CLOP.RelationalExpr":
      case "CLOP.EqualityExpr":
      case "CLOP.ANDExpr":
      case "CLOP.ExclusiveORExpr":
      case "CLOP.InclusiveORExpr":
      case "CLOP.LogicalANDExpr":
      case "CLOP.LogicalORExpr":
      case "CLOP.ConditionalExpr":
      case "CLOP.Expression":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.AssignmentExpr":
        {
          if (t.children.length == 3)
          {
            eval_def = true;
            analyze(t.children[0]);
            eval_def = false;
            analyze(t.children[2]);
          }
          else
          {
            analyze(t.children[0]);
          }
          break;
        }
      case "CLOP.IdentifierList":
        {
          foreach (c; t.children)
          {
            analyze(c);
          }
          break;
        }
      case "CLOP.Identifier":
        {
          auto s = t.matches[0];
          if (s !in symtable)
          {
            symtable[s] = Symbol(s, "", t, [], [], null, null, !global_scope, false, false);
          }
          break;
        }
      default:
        {
          break;
        }
      }
    }

    /++
     + @FIXME this doesn't work when not entire index space is used in
     + index expressions.  A possible work-around to assume that arrays
     + indexes always start at 0 and if the lower bound of a computed
     + interval is greater than 0 it should be extended to 0.
     +/
    void compute_intervals()
    {
      foreach (k, v; symtable)
      {
        if (v.is_array && v.can_cache)
        {
          if (v.uses.length > 0)
          {
            auto uses = v.uses;
            auto box = range.map(uses[0]);
            foreach (u; uses[1 .. $])
            {
              auto newbox = range.map(u);
              foreach (i; 0 .. box.length)
                box[i] = interval_union(box[i], newbox[i]);
            }
            symtable[k].box = box;
          }
          else if (v.defs.length > 0)
          {
            auto defs = v.defs;
            auto box = range.map(defs[0]);
            foreach (d; defs[1 .. $])
            {
              auto newbox = range.map(d);
              foreach (i; 0 .. box.length)
                box[i] = interval_union(box[i], newbox[i]);
            }
            symtable[k].box = box;
          }
        }
      }
    }

    /++
     +
     +/
    void update_parameters()
    {
      foreach (i, T; TT)
      {
        auto name = parameters[i].name;
        static      if (is (Unqual!(T) == bool))
        {
          parameters[i].type = `"char "`;
          parameters[i].size = `cl_char.sizeof`;
        }
        else static if (is (Unqual!(T) == char))
        {
          parameters[i].type = `"char "`;
          parameters[i].size = `cl_char.sizeof`;
        }
        else static if (is (Unqual!(T) == byte))
        {
          parameters[i].type = `"char "`;
          parameters[i].size = `cl_char.sizeof`;
        }
        else static if (is (Unqual!(T) == ubyte))
        {
          parameters[i].type = `"uchar "`;
          parameters[i].size = `cl_uchar.sizeof`;
        }
        else static if (is (Unqual!(T) == short))
        {
          parameters[i].type = `"short "`;
          parameters[i].size = `cl_short.sizeof`;
        }
        else static if (is (Unqual!(T) == ushort))
        {
          parameters[i].type = `"ushort "`;
          parameters[i].size = `cl_ushort.sizeof`;
        }
        else static if (is (Unqual!(T) == int))
        {
          parameters[i].type = `"int "`;
          parameters[i].size = `cl_int.sizeof`;
        }
        else static if (is (Unqual!(T) == uint))
        {
          parameters[i].type = `"uint "`;
          parameters[i].size = `cl_uint.sizeof`;
        }
        else static if (is (Unqual!(T) == long))
        {
          parameters[i].type = `"long "`;
          parameters[i].size = `cl_long.sizeof`;
        }
        else static if (is (Unqual!(T) == ulong))
        {
          parameters[i].type = `"ulong "`;
          parameters[i].size = `cl_ulong.sizeof`;
        }
        else static if (is (Unqual!(T) == float))
        {
          parameters[i].type = `"float "`;
          parameters[i].size = `cl_float.sizeof`;
        }
        else static if (is (Unqual!(T) == double))
        {
          parameters[i].type = `"double "`;
          parameters[i].size = `cl_double.sizeof`;
        }
        else static if (isArray!T)
        {
          parameters[i].type = `typeid(*` ~ name ~ `.ptr).toString() ~ "* "`;
          parameters[i].qual = `__global `;
          parameters[i].size = `cl_mem.sizeof`;
          parameters[i].address = "&clop_opencl_device_buffer_" ~ name;
          parameters[i].to_push = format(q{
            cl_mem clop_opencl_device_buffer_%s = clCreateBuffer(runtime.context, CL_MEM_READ_WRITE, typeid(*%s.ptr).tsize * %s.length, null, &runtime.status);
            assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clCreateBuffer"));
            runtime.status = clEnqueueWriteBuffer(runtime.queue, clop_opencl_device_buffer_%s, CL_TRUE, 0, typeid(*%s.ptr).tsize * %s.length, %s.ptr, 0, null, null);
            assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueWriteBuffer"));
            }, name, name, name, name, name, name, name);
          parameters[i].to_pull = format(q{
            runtime.status = clEnqueueReadBuffer(runtime.queue, clop_opencl_device_buffer_%s, CL_TRUE, 0, typeid(*%s.ptr).tsize * %s.length, %s.ptr, 0, null, null);
            assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueReadBuffer"));
            }, name, name, name, name);
          parameters[i].to_release = format(q{
            runtime.status = clReleaseMemObject(clop_opencl_device_buffer_%s);
            assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clReleaseMemObject"));
            }, name);
        }
        else static if (__traits(compiles, TemplateOf!(T)) && __traits(isSame, TemplateOf!(T), NDArray))
        {
          parameters[i].type = `typeid(*` ~ name ~ `.ptr).toString() ~ "* "`;
          parameters[i].qual = `__global `;
          parameters[i].size = `cl_mem.sizeof`;
          parameters[i].address = name ~ ".get_buffer";
          parameters[i].to_push = format(q{
            %s.create_buffer(runtime.context);
            %s.push_buffer(runtime.queue);
            }, name, name);
          parameters[i].to_pull = format(q{
            %s.pull_buffer(runtime.queue);
            }, name);
        }
        parameters[i].is_macro = !isMutable!T;
      }
    }

    /++
     +
     +/
    ParseTree transform_by_pattern(ParseTree t)
    {
      if (pattern is null)
      {
        return Plain(t);
      }
      // check all Compiler member whether we can find a callable one
      // with the same name as the value of pattern
      foreach (member; __traits (allMembers, Backend))
      {
        if (pattern == member)
        {
          // @FIXME the requirement ReturnType!M == ParseTree is to
          // force the D compiler to consider in the following mixin
          // only the methods that can do pattern transformations.
          // Otherwise, the D compiler will check(?) every callable
          // member of the Compiler struct and see if it can be called
          // with a ParseTree parameter and return a ParseTree, which is
          // not true for the majority of Compiler's methods.  This
          // condition prevents a compile-time error, by restricting the
          // set of checked(?) methods to the ones that don't trigger an
          // error.  If we add a method that returns ParseTree but takes
          // different parameters, this will break.  How to do this
          // correctly?
          alias M = typeof (mixin ("this." ~ member));
          static if (isCallable!M && is (ReturnType!M == ParseTree))
            return mixin ("this." ~ member ~ "(t)");
        }
      }
      errors ~= "CLOP: unknown syncronization pattern " ~ pattern ~ "\n";
      return t;
    }

    /++
     + Create all possible variants of optimized programs.  Different
     + orderings of the same optimizations are considered equal.
     + @FIXME more sophisticated logic is needed.  We want to apply one
     + optimization of each kind, find the most beneficial, then apply
     + one of remaining optimizations, find the most beneficial
     + sequence, and so on.
     +/
    void apply_optimizations(ParseTree t)
    {
      auto items = trans.get_items_as_array();
      auto length = items.length;
      auto index = 0;
      variants ~= Program(symtable, [], parameters, t.dup, range, pattern, external, errors,
                          format("%s_%s", suffix, index++));
      for (auto i = 0; i < length; ++i)
      {
        auto set = [items[i]];
        variants ~= Program(symtable, set, parameters, t.dup, range, pattern, external, errors,
                            format("%s_%s", suffix, index++));
        for (auto j = i + 1; j < length; ++j)
        {
          set ~= items[j];
          variants ~= Program(symtable, set, parameters, t.dup, range, pattern, external, errors,
                              format("%s_%s", suffix, index++));
        }
      }
    }

    /++
     +
     +/
    bool can_transform_index_expression(ParseTree t)
    {
      switch (t.name)
      {
      default:
        return reduce!((a, b) => a && b)(true, map!(a => can_transform_index_expression(a))(t.children));
      }
    }

    /++
     + Transforms an AST with no synchronization pattern.
     + Each thread computes a single item in the NDRange.
     + @FIXME can this be generalized and templatized?
     +/
    ParseTree Plain(ParseTree t)
    {
      if (t.children.length == 0 || t.name != "CLOP.CompoundStatement")
      {
        return t;
      }
      ParseTree newt = t.dup;
      ParseTree[] decl;
      for (auto i = 0; i < range.get_dimensions(); ++i)
      {
        decl ~= CLOP.decimateTree(CLOP.Declaration(format("int %s = get_global_id(%d);",
                                                          range.symbols[i], i)));
      }
      newt.children[0].children = decl ~ newt.children[0].children;
      return newt;
    }

    /++
     + Transforms an AST as necessary to implement antidiagonal
     + synchronization pattern.
     + @FIXME can this be generalized and templatized?
     +/
    ParseTree Antidiagonal(ParseTree t)
    {
      if (t.children.length == 0 || t.name != "CLOP.CompoundStatement")
        return t;
      ParseTree newt = t.dup;
      ParseTree[] decl;
      auto thread_index = "tx";
      auto sync_diagonal = "diagonal";
      if (sync_diagonal !in symtable)
      {
        symtable[sync_diagonal] = Symbol(sync_diagonal, "int");
        parameters ~= [Argument(sync_diagonal, `"int "`, "", "", "", true)];
      }
      decl ~= CLOP.decimateTree(CLOP.Declaration(format("int %s = get_global_id(0);", thread_index)));
      decl ~= CLOP.decimateTree(CLOP.Declaration(format("int %s = %s + 1;", range.symbols[0], thread_index)));
      decl ~= CLOP.decimateTree(CLOP.Declaration(format("int %s = %s - %s - 1;", range.symbols[1], sync_diagonal, thread_index)));
      auto max = range.intervals[0].get_max();
      decl ~= CLOP.decimateTree(CLOP.Statement(format("if (%s >= %s) {%s = %s - %s + %s + 1; %s = %s - %s - 1;}",
                                                      sync_diagonal, max,
                                                      range.symbols[0], sync_diagonal, max, thread_index,
                                                      range.symbols[1], max, thread_index)));
      newt.children[0].children = decl ~ newt.children[0].children;
      return newt;
    }

    /+
     + Methods to dump compiler's internal and debug information
     +/

    string dump_symtable()
    {
      auto result = "// Symbol table:\n";
      foreach (s; symtable)
        result ~= format("// %s\n", s.toString());
      return result;
    }

    string dump_arraytab()
    {
      auto result = "// Recognized the following array variables:\n";
      foreach (k, v; symtable)
      {
        if (v.is_array)
        {
          string s = "// defs " ~ to!string(v.defs.length) ~ "\n";
          foreach (a; v.defs)
          {
            s ~= "// " ~ to!string(a.matches) ~ "\n";
            s ~= "// " ~ to!string(map!(p => p.toString())(range.map(a))) ~ "\n";
          }
          s ~= "// uses " ~ to!string(v.uses.length) ~ "\n";
          foreach (a; v.uses)
          {
            s ~= "// " ~ to!string(a.matches) ~ "\n";
            s ~= "// " ~ to!string(map!(p => p.toString())(range.map(a))) ~ "\n";
          }
          result ~= "// " ~ k ~ ":\n" ~ s;
        }
      }
      return result;
    }

    string dump_intervals()
    {
      auto result = "// The intervals:\n";
      foreach (k; 0 .. range.get_dimensions())
      {
        result ~= "// " ~ range.symbols[k] ~ ": " ~ range.intervals[k].toString() ~ "\n";
      }
      return result;
    }

    string dump_parameters()
    {
      auto result = "// The kernel arguments:\n";
      foreach (i, p; parameters)
      {
        result ~= format("// %s: %s\n", i, p.toString());
      }
      return result;
    }

  } // Backend class
} // Backend template

unittest {
  ParseTree t;
  auto c = Backend(t, __FILE__, __LINE__);
  auto p = c.generate_code();
}
