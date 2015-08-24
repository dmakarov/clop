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
module clop.ct.stage1;

import clop.ct.parser, clop.ct.symbol;
static import clop.ct.templates;
version (LDC) import std.string;
else import std.format;

debug (UNITTEST_DEBUG)
{
  import std.stdio;
}

/++
 +  The CLOP compiler front end.
 +  Analyze the AST and extract the kernel parameters.
 +/
struct Frontend
{
  /++
   +
   +/
  this(ParseTree t, string file, size_t line)
  {
    import std.path : baseName, stripExtension;
    source_file = file;
    source_line = line;
    suffix = format("%s_%s", baseName(stripExtension(file)), line);
    errors = "";
    AST = t;
  }

  /++
   +
   +/
  string generate_stage2_code()
  {
    debug (GRAMMAR)
    {
      return format(q{
          static bool clop_grammar_instance_%s;
          if (!clop_grammar_instance_%s)
          {
            clop_grammar_instance_%s = true;
            writeln(q{%s});
          }
        }, suffix, suffix, suffix, AST.toString());
    }
    else
    {
      analyze(AST);
      auto types = "";
      auto names = "";
      auto comma = "";
      foreach (s, v; symtable)
        if (!v.is_local)
        {
          types ~= comma ~ (v.type == "" ? "typeof(" ~ v.name ~ ")" : v.type);
          names ~= comma ~ `"` ~ v.name ~ `"`;
          comma = ", ";
        }
      auto code = format(q{
          alias CLOP_parameter_types = std.typetuple.TypeTuple!(%s);
          immutable(string[]) clop_parameter_names = [%s];
          mixin (stage2_codegen!CLOP_parameter_types(clop_parse_tree, "%s", cast(size_t) %s, clop_parameter_names));},
        types, names, source_file, source_line);
      return code;
    }
  }

  @property
  string get_suffix()
  {
    return suffix;
  }

  // The following methods are internal to the struct and not used outside.

 private:

  ParseTree AST;            /// the tree for this piece
  Symbol[string] symtable;  /// the symbol table

  string source_file;
  size_t source_line;
  string suffix;
  string errors;

  string current_type = "";
  bool external_linkage = false;
  bool global_scope = false;
  uint depth = 0;

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
        uint i = (t.children[0].name == "CLOP.SyncPattern") ? 1 : 0;
        analyze(t.children[i++]);
        global_scope = true;
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
        symtable[s] = Symbol(s, "int", t, null, null, null, "clop_local_index_" ~ s, null, true, false, false);
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
        break;
      }
    case "CLOP.Declaration":
      {
        foreach (i, c; t.children)
        {
          debug (UNITTEST_DEBUG) writefln("STAGE1 analyze CLOP.Declaration %s %s", i, c.matches);
          analyze(c);
        }
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
            symtable[symbol].is_array = true;
            symtable[symbol].type = current_type ~ "*";
          }
        }
        break;
      }
    case "CLOP.DeclarationSpecifiers":
      {
        analyze(t.children[0]);
        if (t.children.length > 1)
          analyze(t.children[1]);
        break;
      }
    case "CLOP.StorageClassSpecifier":
      {
        debug (UNITTEST_DEBUG) writeln("STAGE1 analyze CLOP.StorageClassSpecifier");
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
        }
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
          analyze(t.children[0]);
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
          symtable[s] = Symbol(s, "", t, [], [], null, null, null, !global_scope, false, false);
        }
        break;
      }
    default:
      {
        break;
      }
    }
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

  string debug_node(ParseTree t)
  {
    import std.conv : to;
    string s = "\n";
    string indent = "";
    foreach (x; 0 .. depth) indent ~= "  ";
    ++depth;
    foreach (i, c; t.children)
      s ~= indent ~ to!string(i) ~ ": " ~ debug_tree(c) ~ "\n";
    --depth;
    string m = "";
    foreach (i, x; t.matches)
      m ~= " " ~ to!string(i) ~ ":#" ~ x ~ "#";
    return format("<%s, input[%d,%d]:#%s# matches%s>%s%s</%s>",
                  t.name, t.begin, t.end, t.input[t.begin .. t.end], m, s, indent, t.name);
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

} // Frontend class

unittest {
  // test 1
  debug (UNITTEST_DEBUG)
  {
    writeln("STAGE 1 UNIT TESTS");
  }
  auto t1 = clop.ct.parser.CLOP(q{
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
  auto f1 = Frontend(t1, __FILE__, __LINE__);
  auto c1 = f1.generate_stage2_code();
  debug (UNITTEST_DEBUG)
  {
    writefln("%s", f1.dump_symtable());
    writefln("// STAGE 1 GENERATED CODE:%s\n", c1);
  }

  // test 2
  auto t2 = clop.ct.parser.CLOP(q{
      NDRange(tid : 0 .. gws $ wgs)
      {
        int group_index = get_group_id(0) + 1;
        int local_index = get_local_id(0);
        local float t[wgs];
        float s = 0.0;
        int j;
        for (j = 1; tid + j < input_n + 1; j += gws)
        {
          s += input_units[j] * i2h_weights[group_index, j];
        }
        t[local_index] = s;
        s = reduce!"a + b"(0, t);
        if (local_index == 0)
        {
          hidden_units[group_index] = s;
        }
      }});
  auto f2 = Frontend(t2, __FILE__, __LINE__);
  auto c2 = f2.generate_stage2_code();
  debug (UNITTEST_DEBUG)
  {
    writefln("%s", f2.dump_symtable());
    writefln("// STAGE 1 GENERATED CODE:%s\n", c2);
    writefln("The ParseTree:\n%s", t2.toString());
  }
}

// Local Variables:
// compile-command: "../../tests/test_module clop.ct.stage1"
// End:
