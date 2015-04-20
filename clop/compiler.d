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
import std.format, clop.ct.parser;

/++
 + Main entry point to the compiler.
 + The compilation proceeds in parsing and two stages of code
 + generation.  This function arranges for the parser to be invoked,
 + its result stored in a local enum constant to be passed as a
 + parameter to the code generator stages.
 +
 + \param input a string containing the source code of a CLOP fragment
 +              to be compiled into OpenCL kernel and the supporting
 +              OpenCL API calls.
 + \return      a string containing the valid D source code to be
 +              mixed in the invoking application code.
 +              This code includes the CLOP parser and stage 1 codegen
 +              invocations.  The ParseTree returned from the CLOP
 +              parser is used both by the stage1 and stage2 code
 +              generators.
 +/
string compile(string input, string file = __FILE__, size_t line = __LINE__)
{
  return format(q{{
        import clop.ct.stage1, clop.ct.stage2, clop.rt.instance, std.format, std.typetuple;
        import clop.ct.parser:CLOP,ParseTree;
        enum clop_input = q{%s};
        enum clop_parse_tree = CLOP(clop_input);
        mixin (stage1_codegen(clop_parse_tree, "%s", cast(size_t) %s));
      }}, input, file, line);
}

/++
 + Stage 1 of the compiler.
 + \param AST   ParseTree object returned by the CLOP parser.
 + \return      a string containing the valid D source code to be
 +              mixed in the invoking application code.
 +              This code includes the stage 2 codegen invocation.
 +/
string stage1_codegen(ParseTree AST, string file, size_t line)
{
  import std.conv : to;
  import clop.ct.stage1;
  auto frontend = Frontend(AST, file, line);
  auto suffix = frontend.get_suffix();
  auto code = frontend.generate_stage2_code();
  auto flln = file ~ ":" ~ to!string(line);
  auto head = "CLOP STAGE 1 MIX " ~ flln;
  auto tail = "END STAGE 1 CLOP " ~ flln;
  auto dump = format(q{
      debug (VERBOSE)
      {
        static bool clop_stage1_instance_%s;
        if (!clop_stage1_instance_%s)
        {
          clop_stage1_instance_%s = true;
          writeln("%s:",`%s`,"\n%s:");
        }
      }
    }, suffix, suffix, suffix, head, code, tail);
  return dump ~ code;
}

/++
 + Stage 2 the compiler.
 + \param AST   ParseTree object returned by the CLOP parser.
 + \param params
 +              list of parameter names.
 + \param template parameter TList
 +              list of parameter types.
 + \return      a string containing the valid D source code to be mixed
 +              in the invoking application code.  This code includes
 +              the generated OpenCL kernel code and everything needed
 +              to invoke the kernel along with the data movements to
 +              and from the OpenCL device used to execute the kernel.
 +/
template stage2_codegen(TList...)
{
  import std.conv : to;
  import std.typetuple, clop.ct.stage2;
  alias TT = TypeTuple!(TList[0 .. $]);

  string stage2_codegen(ParseTree AST, string file, size_t line, immutable(string[]) params)
  {
    auto backend = Backend!TT(AST, file, line, params);
    auto code = backend.generate_code();
    auto flln = file ~ ":" ~ to!string(line);
    auto head = "CLOP STAGE 2 MIX " ~ flln;
    auto tail = "END STAGE 2 CLOP " ~ flln;
    auto dump = format(q{
        debug (VERBOSE)
        {
          static bool clop_stage2_instance_%s;
          if (!clop_stage2_instance_%s)
          {
            clop_stage2_instance_%s = true;
            writeln("%s:\n",`%s`,"%s:");
          }
        }
      }, backend.suffix, backend.suffix, backend.suffix, head, code, tail);
    return dump ~ code;
  }
}
