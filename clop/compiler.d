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
module clop.compiler;

public import clop.rt.ctx;

private:

import std.algorithm : reduce;
import std.container;
import std.conv;
import std.string;
import std.traits;

import clop.ct.analysis;
import clop.ct.grammar;
import clop.ct.program;
import clop.ct.structs;
import clop.ct.symbol;
import clop.ct.templates;
import clop.ct.transform;


/++
 +  The CLOP compiler.
 +/
struct Compiler
{
  Box range;                /// box of intervals for this piece
  ParseTree AST;            /// the tree for this piece
  ParseTree KBT;            /// kernel body compound statement AST
  Program[] variants;       /// one variant for each set of transformations
  Argument[] parameters;    /// kernel parameters
  Symbol[string] symtable;  /// the symbol table
  Set!Transformation trans; /// set of optimizing transformations

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
  this(string expr, string file, size_t line)
  {
    errors = "";
    external = "";
    source_file = file;
    source_line = line;
    range = Box();
    AST   = CLOP(expr);
    trans = Set!Transformation();
  }

  /++
   +
   +/
  string generate_code()
  {
    debug (DEBUG_GRAMMAR)
    {
      debug_tree(AST);
    }
    else
    {
      analyze(AST);
      compute_intervals();
      collect_parameters();
      // before we generate optimized variants, we need to transform
      // the AST for a specific synchronization pattern.
      auto t = transform_by_pattern(KBT);
      apply_optimizations(t);
      auto code = "";
      debug (DEBUG)
      {
        code ~= "// Transformations <" ~ pattern ~ trans.toString ~ ">\n";
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
  }

  // The following methods are internal to the struct and not used outside.

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
          analyze(c);
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
  void collect_parameters()
  {
    foreach (s, v; symtable)
      if (!v.is_local)
        parameters ~= [Argument(s)];
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
    foreach (member; __traits (allMembers, Compiler))
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
    variants ~= Program(symtable, [], parameters, t.dup, range, pattern, external, errors);
    for (auto i = 0; i < length; ++i)
    {
      auto set = [items[i]];
      variants ~= Program(symtable, set, parameters, t.dup, range, pattern, external, errors);
      for (auto j = i + 1; j < length; ++j)
      {
        set ~= items[j];
        variants ~= Program(symtable, set, parameters, t.dup, range, pattern, external, errors);
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
      parameters ~= [Argument(sync_diagonal, "int", "", "", "", true)];
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
    string result = "// Recognized the following array variables:\n";
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
    string result = "// The intervals:\n";
    foreach (k; 0 .. range.get_dimensions())
    {
      result ~= "// " ~ range.symbols[k] ~ ": " ~ range.intervals[k].toString() ~ "\n";
    }
    return result;
  }

  string dump_parameters()
  {
    string result = "// The kernel arguments:\n";
    foreach (i, a; parameters)
    {
      result ~= "// " ~ a.name ~ " " ~ a.type ~ " " ~ a.qual ~ " " ~ a.size ~ "\n";
    }
    return result;
  }

  string debug_node(ParseTree t)
  {
    string s = "\n";
    string indent = "";
    foreach (x; 0 .. depth) indent ~= "  ";
    ++depth;
    foreach (i, c; t.children)
      s ~= indent ~ to!string(i) ~ ": " ~ debug_tree(c) ~ "\n";
    --depth;
    string m = "";
    foreach (i, x; t.matches)
      m ~= " " ~ to!string(i) ~ ":\"" ~ x ~ "\"";
    return format("<%s, input[%d,%d]:\"%s\" matches%s>%s%s</%s>", t.name, t.begin, t.end, t.input[t.begin .. t.end], m, s, indent, t.name);
  }

  string debug_leaf(ParseTree t)
  {
    return "<" ~ t.name ~ ">" ~ t.matches[0] ~ "</" ~ t.name ~ ">";
  }

  string debug_tree(ParseTree t)
  {
    switch (t.name)
    {
    case "CLOP":
    case "CLOP.TranslationUnit":
    case "CLOP.KernelBlock":
    case "CLOP.ExternalDeclarations":
    case "CLOP.ExternalDeclaration":
    case "CLOP.FunctionDefinition":
    case "CLOP.SyncPattern":
    case "CLOP.RangeDecl":
    case "CLOP.RangeList":
    case "CLOP.RangeSpec":
    case "CLOP.Transformations":
    case "CLOP.TransList":
    case "CLOP.TransSpec":
    case "CLOP.Declaration":
    case "CLOP.Declarator":
    case "CLOP.TypeSpecifier":
    case "CLOP.StructSpecifier":
    case "CLOP.StructDeclarationList":
    case "CLOP.StructDeclaration":
    case "CLOP.StructDeclaratorList":
    case "CLOP.StructDeclarator":
    case "CLOP.InitDeclaratorList":
    case "CLOP.InitDeclarator":
    case "CLOP.InitializerList":
    case "CLOP.Initializer":
    case "CLOP.ParameterList":
    case "CLOP.ParameterDeclaration":
    case "CLOP.TypeName":
    case "CLOP.StatementList":
    case "CLOP.Statement":
    case "CLOP.CompoundStatement":
    case "CLOP.ExpressionStatement":
    case "CLOP.IfStatement":
    case "CLOP.IterationStatement":
    case "CLOP.WhileStatement":
    case "CLOP.ForStatement":
    case "CLOP.ReturnStatement":
    case "CLOP.PrimaryExpr":
    case "CLOP.PostfixExpr":
    case "CLOP.ArgumentExprList":
    case "CLOP.UnaryExpr":
    case "CLOP.IncrementExpr":
    case "CLOP.DecrementExpr":
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
    case "CLOP.AssignmentExpr":
    case "CLOP.Expression":
    case "CLOP.IdentifierList":
        return debug_node(t);
    case "CLOP.MultiplicativeOperator":
    case "CLOP.AdditiveOperator":
    case "CLOP.ShiftOperator":
    case "CLOP.RelationalOperator":
    case "CLOP.EqualityOperator":
    case "CLOP.AssignmentOperator":
    case "CLOP.Identifier":
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
        return debug_leaf(t);
    default: return t.name;
    }
  }

} // Compiler class

unittest {
  auto s = "";
  auto c = Compiler(s, __FILE__, __LINE__);
  auto p = c.generate_code();
}

/+
 +  Free functions, not members of Compiler class.  These functions are
 +  evaluated at compile time and used for the second level of mixin
 +  recursion.  The code generated by these functions is compiled in
 +  the final application executable.
 +/

public:

/++
 + Given a string that represents a name of a variable, return a
 + string that represents a parameter declaration that contains the
 + type of the parameter matching the variable's type and the
 + parameter name matching the variable's name.
 +/
auto generate_kernel_parameter(T)(string name, string back)
{
  auto code = "\"\"";
  static      if (is (T == bool  )) code = "\"uchar "  ~ name ~ "\"";
  else static if (is (T == char  )) code = "\"char "   ~ name ~ "\"";
  else static if (is (T == byte  )) code = "\"char "   ~ name ~ "\"";
  else static if (is (T == ubyte )) code = "\"uchar "  ~ name ~ "\"";
  else static if (is (T == short )) code = "\"short "  ~ name ~ "\"";
  else static if (is (T == ushort)) code = "\"ushort " ~ name ~ "\"";
  else static if (is (Unqual!(T) == int)) code = "\"int "    ~ name ~ "\"";
  else static if (is (T == uint  )) code = "\"uint "   ~ name ~ "\"";
  else static if (is (T == long  )) code = "\"long "   ~ name ~ "\"";
  else static if (is (T == ulong )) code = "\"ulong "  ~ name ~ "\"";
  else static if (is (T == float )) code = "\"float "  ~ name ~ "\"";
  else static if (is (T == double)) code = "\"double " ~ name ~ "\"";
  else static if (isDynamicArray!T || isStaticArray!T || __traits(isSame, TemplateOf!(T), NDArray))
  {
    auto qual = (back == "") ? "__global" : "__local";
    auto orig = (back == "") ? name : back;
    code = "\"" ~ qual ~ " \" ~ typeid(*" ~ orig ~ ".ptr).toString() ~ \"* " ~ name ~ "\"";
  }
  return code;
}

/++
 +
 +/
auto set_kernel_arg(T)(string kernel, string arg, string name, string back, string size)
{
  auto code = "";
  static      if (is (T == bool))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_char.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == char))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_char.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == byte))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_char.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == ubyte))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_uchar.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == short))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_short.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == ushort))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_ushort.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (Unqual!(T) == int))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_int.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == uint))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_uint.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == long))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_long.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == ulong))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_ulong.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == float))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_float.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (is (T == double))
    code = "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg  ~ ", cl_double.sizeof, &" ~ name ~ ");\n"
         ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  else static if (isDynamicArray!T || isStaticArray!T || __traits(isSame, TemplateOf!(T), NDArray))
  {
    code = (null == size || size == "")
           ?
           "cl_mem clop_opencl_device_buffer_" ~ name ~
           " = clCreateBuffer(runtime.context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, typeid(*"
           ~ name ~ ".ptr).tsize * " ~ name ~ ".length, " ~ name ~ ".ptr, &runtime.status);\n"
           ~ "assert(runtime.status == CL_SUCCESS, \"clCreateBuffer failed.\");\n"
           ~ "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg ~ ", cl_mem.sizeof, &clop_opencl_device_buffer_"
           ~ name ~ ");\nassert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");\n"
           ~ `debug (DEBUG) writefln("clCreateBuffer(%s at %s)", typeid(*` ~ name ~ `.ptr).tsize * ` ~ name ~ `.length, ` ~ name ~ `.ptr);`
           :
           "runtime.status = clSetKernelArg(" ~ kernel ~ ", " ~ arg ~ ", " ~ size ~ " * typeid(*" ~ back ~ ".ptr).tsize, " ~ name ~ ");\n"
           ~ "assert(runtime.status == CL_SUCCESS, \"clSetKernelArg failed.\");";
  }
  return "debug (DEBUG) writefln(\"%s\", q{" ~ code ~ "});\n" ~ code;
}

/++
 +
 +/
auto read_device_buffer(T)(string name)
{
  auto code = "";
  static if (isDynamicArray!T || isStaticArray!T || __traits(isSame, TemplateOf!(T), NDArray))
    code = "runtime.status = clEnqueueReadBuffer(runtime.queue, clop_opencl_device_buffer_"
           ~ name ~ ", CL_TRUE, 0, typeid(*" ~ name ~ ".ptr).tsize * " ~ name ~ ".length, " ~ name ~
           ".ptr, 0, null, null);\nassert(runtime.status == CL_SUCCESS, \"clEnqueueReadBuffer failed.\");";
  return "debug (DEBUG) writefln(\"%s\", q{" ~ code ~ "});\n" ~ code;
}

/++
 + Main entry point to the compiler.
 + \param expr a string containing the source code of a CLOP fragment
 +             to be compiled into OpenCL kernel and the supporting
 +             OpenCL API calls.
 + \return     a string containing the valid D source code to be mixed
 +             in the invoking application code.  This code includes
 +             the generated OpenCL kernel code and everything needed
 +             to invoke the kernel along with the data movements to
 +             and from the OpenCL device used to execute the kernel.
 +/
string compile(string expr, string file = __FILE__, size_t line = __LINE__)
{
  auto compiler = Compiler(expr, file, line);
  auto code = compiler.generate_code();
  auto flln = file ~ ":" ~ to!string(line);
  auto head = "CLOP MIXIN at " ~ flln;
  auto tail = "END OF CLOP " ~ flln;
  auto dump = format("debug (DEBUG) writeln(\"%s:\\n\",`%s`,\"%s:\");\n", head, code, tail);
  return dump ~ code;
}

// Local Variables:
// hide-functions: ("dump_symtable" "dump_arraytab" "dump_intervals" \
//                  "dump_parameters" "debug_node" "debug_leaf" "debug_tree")
// End:
