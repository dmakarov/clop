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

import clop.rt.ndarray;
import clop.ct.analysis;
import clop.ct.parser;
import clop.ct.structs;
import clop.ct.symbol;
import clop.ct.transform;
static import clop.ct.program;
static import clop.ct.templates;

alias Program = clop.ct.program.Program;

debug (UNITTEST_DEBUG)
{
  import std.stdio;
}

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
    string[] lws;             /// local work size
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

    string current_type   = "";
    bool external_linkage = false;
    bool global_scope     = false;
    bool eval_def         = false;
    uint depth            = 0;
    uint temporary_index  = 0;

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
      update_parameters();
      lower(AST);
      compute_intervals();
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

  package:

    /++
     + XXX: do we need this pass?
     + builds the symbol table
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
          symtable[s] = Symbol(s, "int", t, null, null, null, null, "clop_local_index_" ~ s, true, false, false);
          range.intervals ~= [Interval(t.children[1], t.children[2])];
          range.symbols ~= [s];
          range.s2i[s] = d;
          bool saved_global_scope_value = global_scope;
          global_scope = true;
          analyze(t.children[1]);
          analyze(t.children[2]);
          global_scope = saved_global_scope_value;
          if (t.children.length == 4)
          {
            lws ~= [reduce!"a ~ b"("", t.children[3].matches[1 .. $])];
          }
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
          current_type = "";
          break;
        }
      case "CLOP.Declarator":
        {
          auto saved_global_scope_value = global_scope;
          global_scope = external_linkage;
          analyze(t.children[0]);
          global_scope = saved_global_scope_value;
          external_linkage = false;
          if (t.children.length > 1)
          {
            analyze(t.children[1]);
            if (t.children[1].name == "CLOP.ConditionalExpr")
            {
              auto symbol = t.children[0].matches[0];
              auto type = current_type ~ "*";
              auto array_size = reduce!"a ~ b"("", t.children[1].matches);
              symtable[symbol].is_array = true;
              symtable[symbol].type = type;
              symtable[symbol].length = array_size;
              foreach (ref p; parameters)
              {
                if (p.name == symbol)
                {
                  p.type = `"` ~ type ~ ` "`;
                  p.qual = "__local ";
                  p.size = array_size ~ " * " ~ current_type ~ ".sizeof";
                  p.address = "null";
                }
              }
            }
          }
          break;
        }
      case "CLOP.DeclarationSpecifiers":
        {
          foreach (c; t.children)
            analyze(c);
          break;
        }
      case "CLOP.StorageClassSpecifier":
        {
          external_linkage = true;
          break;
        }
      case "CLOP.TypeSpecifier":
        {
          current_type = t.matches[0];
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
          auto recognized_template = false;
          if (t.children[0].children[0].name == "CLOP.Identifier" &&
              t.children[0].children.length > 1 &&
              t.children[0].children[1].name == "CLOP.StringLiteral")
          {
            switch (t.children[0].children[0].matches[0])
            {
            case "reduce":
              recognized_template = true;
              break;
            default:
              recognized_template = false;
            }
            /+
            foreach (a; __traits(allMembers, ExpansionPattern))
              if (a == t.children[0].children[0].matches[0])
                recognized_template = true;
            +/
          }
          auto recognized_function_call =
            (t.children.length == 1 && t.matches[$ - 1] == ")" && t.matches[$ - 2] == "(")
            || (t.children.length > 1 && t.children[1].name == "CLOP.ArgumentExprList");
          if (!recognized_template && !recognized_function_call)
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
            debug (UNITTEST_DEBUG) writefln("new identifier %s", s);
            symtable[s] = Symbol(s, current_type, t, [], [], null, null, null, !global_scope, false, false);
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
          symtable[name].type = "int";
          parameters[i].type = `"int "`;
          parameters[i].size = `cl_int.sizeof`;
        }
        else static if (is (Unqual!(T) == uint))
        {
          symtable[name].type = "uint";
          parameters[i].type = `"uint "`;
          parameters[i].size = `cl_uint.sizeof`;
        }
        else static if (is (Unqual!(T) == long))
        {
          symtable[name].type = "long";
          parameters[i].type = `"long "`;
          parameters[i].size = `cl_long.sizeof`;
        }
        else static if (is (Unqual!(T) == ulong))
        {
          debug (UNITTEST_DEBUG) writefln("parameter %s", name);
          symtable[name].type = "ulong";
          parameters[i].type = `"ulong "`;
          parameters[i].size = `cl_ulong.sizeof`;
        }
        else static if (is (Unqual!(T) == float))
        {
          symtable[name].type = "float";
          parameters[i].type = `"float "`;
          parameters[i].size = `cl_float.sizeof`;
        }
        else static if (is (Unqual!(T) == double))
        {
          symtable[name].type = "double";
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
          static      if (is (TemplateArgsOf!(T) == TypeTuple!(bool)))   symtable[name].type = "char*";
          else static if (is (TemplateArgsOf!(T) == TypeTuple!(int)))    symtable[name].type = "int*";
          else static if (is (TemplateArgsOf!(T) == TypeTuple!(uint)))   symtable[name].type = "uint*";
          else static if (is (TemplateArgsOf!(T) == TypeTuple!(long)))   symtable[name].type = "long*";
          else static if (is (TemplateArgsOf!(T) == TypeTuple!(ulong)))  symtable[name].type = "ulong*";
          else static if (is (TemplateArgsOf!(T) == TypeTuple!(float)))  symtable[name].type = "float*";
          else static if (is (TemplateArgsOf!(T) == TypeTuple!(double))) symtable[name].type = "double*";
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
          parameters[i].is_ndarray = true;
        }
        else
        {
        }
        parameters[i].is_macro = !isMutable!T;
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
      auto number_of_parameters = parameters.length;
      string[] clop_ndarray_dimensions_parameters = [];
      string[] clop_ndarray_dimensions_variables = [];
      foreach (k, v; symtable)
      {
        uint j;
        for (j = 0; j < number_of_parameters; ++j)
        {
          if (parameters[j].name == k)
            break;
        }
        if (j < number_of_parameters && parameters[j].is_ndarray)
        {
          Interval[] box;
          for (auto n = 0; n < range.intervals.length; ++n)
            box ~= Interval(CLOP.decimateTree(CLOP.IntegerLiteral("0")),
                            CLOP.decimateTree(CLOP.Expression(format("clop_ndarray_dimensions_%s[%s]", parameters[j].name, n))));
          clop_ndarray_dimensions_parameters ~= ["cl_ulong[] clop_ndarray_dimensions_" ~ parameters[j].name ~ " = " ~ parameters[j].name ~ ".get_dimensions();"];
          clop_ndarray_dimensions_variables ~= [parameters[j].name];
          symtable[k].box = box;
        }
        else if (v.is_array && v.can_cache)
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
      foreach (i, p; clop_ndarray_dimensions_parameters)
      {
        auto name = `clop_ndarray_dimensions_` ~ clop_ndarray_dimensions_variables[i];
        auto buffer = name ~ `_buffer`;
        auto to_push =  format(q{
          %s
          cl_mem %s = clCreateBuffer(runtime.context, CL_MEM_READ_ONLY, cl_ulong.sizeof * %s.length, null, &runtime.status);
          assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clCreateBuffer"));
          runtime.status = clEnqueueWriteBuffer(runtime.queue, %s, CL_TRUE, 0, cl_ulong.sizeof * %s.length, %s.ptr, 0, null, null);
          assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueWriteBuffer"));
          }, p, buffer, name, buffer, name, name);
        auto to_release = format(q{
          runtime.status = clReleaseMemObject(%s);
          assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clReleaseMemObject"));
          }, buffer);
        symtable[name] = Symbol(name, `cl_ulong[]`);
        parameters ~= [Argument(name, `"ulong* "`, `__constant `, `cl_mem.sizeof`, ``, false, false, `&` ~ buffer, to_push, ``, to_release)];
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
          static if (isCallable!M &&
                     is (ReturnType!M == ParseTree) &&
                     is (ParameterTypeTuple!M == TypeTuple!(ParseTree)))
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
      variants ~= Program(symtable, [], parameters, t.dup, range, lws, pattern, external, errors,
                          format("%s_%s", suffix, index++));
      for (auto i = 0; i < length; ++i)
      {
        auto set = [items[i]];
        variants ~= Program(symtable, set, parameters, t.dup, range, lws, pattern, external, errors,
                            format("%s_%s", suffix, index++));
        for (auto j = i + 1; j < length; ++j)
        {
          set ~= items[j];
          variants ~= Program(symtable, set, parameters, t.dup, range, lws, pattern, external, errors,
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
      ParseTree newt = CLOP.decimateTree(CLOP.CompoundStatement("{}"));
      newt.children = [ParseTree("CLOP.StatementList", true, [], "", 0, 0, [])];
      ParseTree[] decl;
      auto thread_index = "tx";
      auto sync_diagonal = "diagonal";
      if (sync_diagonal !in symtable)
      {
        symtable[sync_diagonal] = Symbol(sync_diagonal, "int");
        parameters ~= [Argument(sync_diagonal, `"int "`, "", "", "", true)];
      }
      decl ~= CLOP.decimateTree(CLOP.Declaration(format("int %s = get_global_id(0);", thread_index)));
      decl ~= CLOP.decimateTree(CLOP.Declaration(format("int %s = %s < ((%s) - (%s)) ? %s - %s + (%s) : ((%s) - 1) - %s;",
                                                        range.symbols[0], sync_diagonal,
                                                        range.intervals[0].get_max(),
                                                        range.intervals[0].get_min(),
                                                        sync_diagonal, thread_index,
                                                        range.intervals[0].get_min(),
                                                        range.intervals[0].get_max(),
                                                        thread_index)));
      decl ~= CLOP.decimateTree(CLOP.Declaration(format("int %s = %s < ((%s) - (%s)) ? (%s) + %s : %s - ((%s) - (%s)) + (%s) + %s + 1;",
                                                        range.symbols[1], sync_diagonal,
                                                        range.intervals[0].get_max(),
                                                        range.intervals[0].get_min(),
                                                        range.intervals[0].get_min(),
                                                        thread_index, sync_diagonal,
                                                        range.intervals[0].get_max(),
                                                        range.intervals[0].get_min(),
                                                        range.intervals[0].get_min(),
                                                        thread_index)));
      newt.children[0].children = decl ~ t.dup;
      return newt;
    }

    /++
     +  XXX: can this pass be combined with analyze?
     +
     +  Lower the expressions that need to be evaluated to temporaries
     +  and replaced in translation.  This pass modifies the AST in
     +  place.
     +/
    void lower(ref ParseTree t)
    {
      switch (t.name)
      {
      case "CLOP":
        {
          debug (UNITTEST_DEBUG) writefln("UT:%s in Backend.lower case CLOP", suffix);
          lower(t.children[0]);
          break;
        }
      case "CLOP.TranslationUnit":
        {
          uint i = t.children[0].name == "CLOP.ExternalDeclarations" ? 1 : 0;
          lower(t.children[i]);
          break;
        }
      case "CLOP.KernelBlock":
        {
          uint i = t.children[0].name == "CLOP.SyncPattern" ? 2 : 1;
          lower(t.children[i]);
          break;
        }
      case "CLOP.Declaration":
        {
          foreach (c; t.children)
            lower(c);
          break;
        }
      case "CLOP.Declarator":
        {
          foreach (c; t.children)
            lower(c);
          break;
        }
      case "CLOP.InitDeclaratorList":
        {
          foreach (c; t.children)
            lower(c);
          break;
        }
      case "CLOP.InitDeclarator":
        {
          foreach (c; t.children)
            lower(c);
          break;
        }
      case "CLOP.InitializerList":
        {
          foreach (c; t.children)
            lower(c);
          break;
        }
      case "CLOP.Initializer":
        {
          lower(t.children[0]);
          break;
        }
      case "CLOP.ParameterList":
        {
          foreach (c; t.children)
            lower(c);
          break;
        }
      case "CLOP.ParameterDeclaration":
        {
          foreach (c; t.children)
            lower(c);
          break;
        }
      case "CLOP.TypeName":
        {
          break;
        }
      case "CLOP.StatementList":
        {
          debug (UNITTEST_DEBUG) writefln("UT:%s in Backend.lower case CLOP.StatementList", suffix);
          ParseTree[] newlist = [];
          foreach (i, c; t.children)
          {
            auto s = c.children[0];
            debug (UNITTEST_DEBUG) writefln("UT:%s    child %s %s: %s", suffix, i, s.name, s.matches);
            switch (s.name)
            {
            case "CLOP.Declaration":
              {
                if (s.children.length > 1)
                {
                  foreach (j; s.children[1].children)
                  {
                    if (j.children.length > 1)
                    {
                      auto sl = lower_expression(j.children[1]);
                      newlist ~= sl;
                    }
                  }
                }
              }
              break;
            case "CLOP.ExpressionStatement":
              {
                auto sl = lower_expression(s.children[0]);
                newlist ~= sl;
              }
              break;
            default:
              lower(c);
            }
            newlist ~= c;
          }
          t.children = newlist;
          break;
        }
      case "CLOP.Statement":
        {
          lower(t.children[0]);
          break;
        }
      case "CLOP.CompoundStatement":
        {
          t.children.length == 1 && lower(t.children[0]);
          break;
        }
      case "CLOP.IfStatement":
        {
          foreach (c; t.children)
            lower(c);
          break;
        }
      case "CLOP.IterationStatement":
        {
          lower(t.children[0]);
          break;
        }
      case "CLOP.ForStatement":
        {
          foreach (c; t.children)
            lower(c);
          break;
        }
      case "CLOP.ReturnStatement":
        {
          t.children.length == 1 && lower(t.children[0]);
          break;
        }
      default:
        {
          break;
        }
      }
    }

    /++
     +
     +/
    string infer_type(ParseTree t)
    {
      switch (t.name)
      {
      case "CLOP.PrimaryExpr":
        {
          switch (t.children[0].name)
          {
          case "CLOP.FloatLiteral":
            return "float";
          case "CLOP.IntegerLiteral":
            return "int";
          case "CLOP.Identifier":
            auto id = t.children[0].matches[0];
            if (id in symtable)
            {
              return symtable[id].type;
            }
            else
            {
              return "float";
            }
          default:
            return infer_type(t.children[0]);
          }
        }
      case "CLOP.PostfixExpr":
        {
          // FIXME handle function calls, arrays, etc.
          auto type = infer_type(t.children[0]);
          if (type == "")
            return "float";
          if (type[$ - 1] == '*')
            return type[0 .. $ - 1];
          return type;
        }
      case "CLOP.ArgumentExprList":
        {
          return "float";
        }
      case "CLOP.UnaryExpr":
        {
          return infer_type(t.children[0]);
        }
      case "CLOP.CastExpr":
        {
          return infer_type(t.children[0]);
        }
      case "CLOP.MultiplicativeExpr":
      case "CLOP.AdditiveExpr":
      case "CLOP.ShiftExpr":
      case "CLOP.RelationalExpr":
      case "CLOP.EqualityExpr":
        {
          if (t.children.length > 1)
          {
            auto type1 = infer_type(t.children[0]);
            auto type2 = infer_type(t.children[2]);
            if (type1 == type2)
              return type1;
            if (type1 == "float" || type2 == "float")
              return "float";
            return "int";
          }
          else
          {
            return infer_type(t.children[0]);
          }
        }
      case "CLOP.ANDExpr":
      case "CLOP.ExclusiveORExpr":
      case "CLOP.InclusiveORExpr":
      case "CLOP.LogicalANDExpr":
      case "CLOP.LogicalORExpr":
        {
          if (t.children.length > 1)
          {
            auto type1 = infer_type(t.children[0]);
            auto type2 = infer_type(t.children[1]);
            if (type1 == type2)
              return type1;
            if (type1 == "float" || type2 == "float")
              return "float";
            return "int";
          }
          else
          {
            return infer_type(t.children[0]);
          }
        }
      case "CLOP.ConditionalExpr":
        {
          return infer_type(t.children[0]);
        }
      case "CLOP.AssignmentExpr":
        {
          if (t.children.length == 1)
          {
            return infer_type(t.children[0]);
          }
          return infer_type(t.children[2]);
        }
      case "CLOP.Expression":
        {
          return infer_type(t.children[0]);
        }
      default:
        {
          debug (UNITTEST_DEBUG) writefln("infer_type default case: %s %s", t.name, t.matches);
          return "float";
        }
      }
    }

    /++
     +
     +/
    ParseTree[] lower_expression(ParseTree t)
    {
      // this is an empty list of statements
      ParseTree[] sl;
      lower_expression_internal(t, sl);
      debug (UNITTEST_DEBUG)
        foreach (i, s; sl)
          writefln("%s %s", i, s.matches);
      return sl;
    }

    ParseTree lower_expression_internal(ParseTree t, ref ParseTree[] sl)
    {
      debug (UNITTEST_DEBUG) writefln("lower_expression_internal %s: %s", t.name, t.matches);
      ParseTree nt;
      switch (t.name)
      {
      case "CLOP.Declaration":
        {
          debug (UNITTEST_DEBUG) writefln("lower Declaration %s", t.matches);
          if (t.children.length > 1)
          {
            nt = lower_expression_internal(t.children[1], sl);
            t.children[1] = nt;
            t.matches = t.children[0].matches ~ nt.matches;
          }
        }
        break;
      case "CLOP.InitDeclaratorList":
        {
          string[] matches = [];
          foreach (i; 0 .. t.children.length)
          {
            nt = lower_expression_internal(t.children[i], sl);
            matches ~= (i == 0) ? nt.matches : [","] ~ nt.matches;
            t.children[i] = nt;
          }
          t.matches = matches;
        }
        break;
      case "CLOP.InitDeclarator":
        {
          if (t.children.length > 1)
          {
            nt = lower_expression_internal(t.children[1], sl);
            t.children[1] = nt;
            t.matches = t.children[0].matches ~ "=" ~ nt.matches;
          }
        }
        break;
      case "CLOP.Initializer":
        {
          nt = lower_expression_internal(t.children[0], sl);
          t.children[0] = nt;
          t.matches = nt.matches;
        }
        break;
      case "CLOP.PrimaryExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower PrimaryExpr %s", t.matches);
          if (t.children[0].name == "CLOP.Expression")
          {
            // lower the expression in parenthesis
            // FIXME: auto type = infer_type(t.children[0]);
            auto type = infer_type(t.children[0]);
            debug (UNITTEST_DEBUG) writefln("inferred type %s", type);
            // FIXME: generate new temporary
            auto name = format("clop_temp_%s", temporary_index++);
            auto code = type ~ " " ~ name ~ " = " ~ reduce!"a ~ b"("", t.children[0].matches) ~ ";";
            auto tree = CLOP.decimateTree(CLOP.Declaration(code));
            nt = lower_expression_internal(tree, sl);
            sl ~= nt;
            debug (UNITTEST_DEBUG) writefln("lower PrimaryExpr replace %s with %s", t.matches, name);
            t.matches = [name];
            t.children[0] = CLOP.decimateTree(CLOP.PrimaryExpr(name)).children[0];
          }
          else if (t.children.length > 1)
          {
            auto function_literal = t.children[1].matches[0];
            auto return_type = current_type;
            auto x = clop.ct.templates.SnippetContainer.binary_function(function_literal, return_type);
            debug (UNITTEST_DEBUG) writefln("lower PrimaryExpr add an external %s", x);
            external ~= x;
          }
        }
        break;
      case "CLOP.PostfixExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower PostfixExpr %s", t.matches);
          if (t.children.length > 1)
          {
            if (t.children[1].name == "CLOP.Expression")
            {
              // lower the expression in brackets
              nt = lower_expression_internal(t.children[1], sl);
              t.children[1] = nt;
              t.matches = t.children[0].matches ~ "[" ~ t.children[1].matches ~ "]";
            }
            else if (t.children[1].name == "CLOP.ArgumentExprList")
            {
              // lower the actual parameters
              current_type = infer_type(t.children[1]);
              nt = lower_expression_internal(t.children[0], sl);
              t.children[0] = nt;
              nt = lower_expression_internal(t.children[1], sl);
              t.children[1] = nt;
              t.matches = nt.matches;
            }
          }
          else
          {
            debug (UNITTEST_DEBUG) writefln("lower PostfixExpr replace %s with %s", t.matches, nt.matches);
            nt = lower_expression_internal(t.children[0], sl);
            t.children[0] = nt;
            t.matches = nt.matches;
          }
        }
        break;
      case "CLOP.ArgumentExprList":
        {
          string[] matches = [];
          foreach (i; 0 .. t.children.length)
          {
            nt = lower_expression_internal(t.children[i], sl);
            matches ~= (i == 0) ? nt.matches : [","] ~ nt.matches;
            t.children[i] = nt;
          }
          t.matches = matches;
        }
        break;
      case "CLOP.UnaryExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower UnaryExpr %s", t.matches);
          if (t.children[0].name == "CLOP.PostfixExpr")
          {
            t.children[0] = lower_expression_internal(t.children[0], sl);
            debug (UNITTEST_DEBUG) writefln("lower UnaryExpr replace %s with %s",
                                              t.matches, t.children[0].matches);
            t.matches = t.children[0].matches;
          }
        }
        break;
      case "CLOP.CastExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower CastExpr %s", t.matches);
          if (t.children[0].name == "CLOP.UnaryExpr")
          {
            t.children[0] = lower_expression_internal(t.children[0], sl);
            debug (UNITTEST_DEBUG) writefln("lower CastExpr replace %s with %s",
                                              t.matches, t.children[0].matches);
            t.matches = t.children[0].matches;
          }
        }
        break;
      case "CLOP.MultiplicativeExpr":
      case "CLOP.AdditiveExpr":
      case "CLOP.ShiftExpr":
      case "CLOP.RelationalExpr":
      case "CLOP.EqualityExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower %s %s", t.name, t.matches);
          if (t.children.length > 1)
          {
            auto type1 = infer_type(t.children[0]);
            auto name1 = format("clop_temp_%s", temporary_index++);
            auto code1 = type1 ~ " " ~ name1 ~ " = " ~ reduce!"a ~ b"("", t.children[0].matches) ~ ";";
            debug (UNITTEST_DEBUG) writefln("code1 %s", code1);
            auto tree1 = CLOP.decimateTree(CLOP.Declaration(code1));
            nt = lower_expression_internal(tree1, sl);
            sl ~= nt;
            auto type2 = infer_type(t.children[2]);
            auto name2 = format("clop_temp_%s", temporary_index++);
            auto code2 = type2 ~ " " ~ name2 ~ " = " ~ reduce!"a ~ b"("", t.children[2].matches) ~ ";";
            debug (UNITTEST_DEBUG) writefln("code2 %s", code2);
            auto tree2 = CLOP.decimateTree(CLOP.Declaration(code2));
            nt = lower_expression_internal(tree2, sl);
            sl ~= nt;
            auto code = name1 ~ t.children[1].matches[0] ~ name2;
            ParseTree tree;
            switch (t.name)
            {
            case "CLOP.MultiplicativeExpr": tree = CLOP.decimateTree(CLOP.MultiplicativeExpr(code)); break;
            case "CLOP.AdditiveExpr": tree = CLOP.decimateTree(CLOP.AdditiveExpr(code)); break;
            case "CLOP.ShiftExpr": tree = CLOP.decimateTree(CLOP.ShiftExpr(code)); break;
            case "CLOP.RelationalExpr": tree = CLOP.decimateTree(CLOP.RelationalExpr(code)); break;
            case "CLOP.EqualityExpr": tree = CLOP.decimateTree(CLOP.EqualityExpr(code)); break;
            default: break;
            }
            t.matches = [name1, t.children[1].matches[0], name2];
            t.children[0] = tree.children[0];
            t.children[2] = tree.children[2];
          }
          else
          {
            nt = lower_expression_internal(t.children[0], sl);
            t.children[0] = nt;
            t.matches = nt.matches;
          }
        }
        break;
      case "CLOP.ANDExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower ANDExpr %s", t.matches);
          if (t.children.length > 1)
          {
          }
          else
          {
            nt = lower_expression_internal(t.children[0], sl);
            t.children[0] = nt;
            t.matches = nt.matches;
          }
        }
        break;
      case "CLOP.ExclusiveORExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower ExclusiveORExpr %s", t.matches);
          if (t.children.length > 1)
          {
          }
          else
          {
            nt = lower_expression_internal(t.children[0], sl);
            t.children[0] = nt;
            t.matches = nt.matches;
          }
        }
        break;
      case "CLOP.InclusiveORExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower IclusiveORExpr %s", t.matches);
          if (t.children.length > 1)
          {
          }
          else
          {
            nt = lower_expression_internal(t.children[0], sl);
            t.children[0] = nt;
            t.matches = nt.matches;
          }
        }
        break;
      case "CLOP.LogicalANDExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower LogicalANDExpr %s", t.matches);
          if (t.children.length > 1)
          {
          }
          else
          {
            nt = lower_expression_internal(t.children[0], sl);
            t.children[0] = nt;
            t.matches = nt.matches;
          }
        }
        break;
      case "CLOP.LogicalORExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower LogicalORExpr %s", t.matches);
          if (t.children.length > 1)
          {
          }
          else
          {
            nt = lower_expression_internal(t.children[0], sl);
            t.children[0] = nt;
            t.matches = nt.matches;
          }
        }
        break;
      case "CLOP.ConditionalExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower ConditionalExpr %s", t.matches);
          if (t.children.length > 1)
          {
          }
          else
          {
            nt = lower_expression_internal(t.children[0], sl);
            t.children[0] = nt;
            t.matches = nt.matches;
          }
        }
        break;
      case "CLOP.AssignmentExpr":
        {
          debug (UNITTEST_DEBUG) writefln("lower AssignmentExpr %s", t.matches);
          if (t.children.length > 1)
          {
            nt = lower_expression_internal(t.children[2], sl);
            t.children[2] = nt;
            t.matches = t.children[0].matches ~ t.children[1].matches ~ nt.matches;
          }
          else
          {
            nt = lower_expression_internal(t.children[0], sl);
            t.children[0] = nt;
            t.matches = nt.matches;
          }
        }
        break;
      case "CLOP.Expression":
        {
          debug (UNITTEST_DEBUG) writefln("lower Expression %s", t.matches);
          string[] matches = [];
          foreach (i; 0 .. t.children.length)
          {
            nt = lower_expression_internal(t.children[i], sl);
            matches ~= (i == 0) ? nt.matches : [","] ~ nt.matches;
            t.children[i] = nt;
          }
          t.matches = matches;
        }
        break;
      default: assert(0, t.name);
      }
      return t;
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
          string s = "//   defs " ~ to!string(v.defs.length) ~ "\n";
          foreach (i, a; v.defs)
          {
            s ~= format("//     %s: %s %s\n", i, to!string(a.matches),
                        to!string(map!(p => p.toString())(range.map(a))));
          }
          s ~= "//   uses " ~ to!string(v.uses.length) ~ "\n";
          foreach (i, a; v.uses)
          {
            s ~= format("//     %s: %s %s\n", i, to!string(a.matches),
                        to!string(map!(p => p.toString())(range.map(a))));
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

unittest
{
  alias A = NDArray!float;

  // test 1
  version (DISABLED)
  {
    auto AST = clop.ct.parser.CLOP(q{NDRange(i: 0 .. 8){c = a + b;}});
    alias T1 = std.typetuple.TypeTuple!(int, int, int);
    auto be1 = Backend!T1(AST, __FILE__, __LINE__, ["a", "b", "c"]);
    be1.analyze(AST);
    be1.lower(AST);
    auto output = be1.generate_code();
    assert(output !is null && output != "");
  }

  // test 2
  version (DISABLED)
  {
    auto AST = clop.ct.parser.CLOP(q{
        NDRange(j : 1 .. n)
        {
          s[j - 1] = inputs[j] * weights[i * n + j];
        }});
    alias T2 = std.typetuple.TypeTuple!(size_t, int, A, A, A);
    auto be2 = Backend!T2(AST, __FILE__, __LINE__, ["n", "i", "inputs", "weights", "s"]);
    be2.analyze(AST);
    be2.update_parameters();
    debug (UNITTEST_DEBUG)
    {
      writefln("%s", be2.dump_symtable());
      writeln(be2.dump_parameters());
    }
    be2.lower(AST);
  }

  // test 3
  version (DISABLED)
  {
    auto t3 = clop.ct.parser.CLOP(q{
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
    alias T3 = std.typetuple.TypeTuple!(size_t, float*, size_t, A, A, A);
    auto be3 = clop.ct.stage2.Backend!T3(t3, __FILE__, __LINE__,
                                         ["h_n", "t", "i_n", "inputs", "weights", "hidden"]);
    be3.analyze(t3);
    be3.update_parameters();
    debug (UNITTEST_DEBUG)
    {
      writefln("%s", be3.dump_symtable());
      writeln(be3.dump_parameters());
    }
    be3.lower(t3);
    debug (UNITTEST_DEBUG) writeln(t3.toString());
  }

  // test 4
  {
    auto t4 = clop.ct.parser.CLOP(q{
        NDRange(tid : 0 .. gws $ wgs)
        {
          int group_index = get_group_id(0) + 1;
          int local_index = get_local_id(0);
          local float scratch[wgs];
          float s = 0.0;
          int j;
          for (j = 1; tid + j < input_n + 1; j += gws)
          {
            s += input_units[j] * i2h_weights[group_index * (input_n + 1) + j];
          }
          scratch[local_index] = s;
          s = reduce!"a + b"(0, scratch);
          if (local_index == 0)
          {
            hidden_units[group_index] = s;
          }
        }});
    alias T4 = std.typetuple.TypeTuple!(size_t, size_t, size_t, float*, A, A, A);
    auto be4 = clop.ct.stage2.Backend!T4(t4, __FILE__, __LINE__,
                                         ["gws", "wgs", "input_n", "scratch", "input_units", "i2h_weights", "hidden_units"]);
    be4.analyze(t4);
    be4.update_parameters();
    debug (UNITTEST_DEBUG)
    {
      writefln("%s", be4.dump_symtable());
      writeln(be4.dump_parameters());
    }
    be4.lower(t4);
    debug (UNITTEST_DEBUG) writeln(t4.toString());
  }
}

// Local Variables:
// compile-command: "../../tests/test_module clop.ct.stage2"
// End:
