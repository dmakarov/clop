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

void main(string[] args)
{
  import std.algorithm.iteration : filter;
  import std.algorithm.searching : endsWith, find;
  import std.array, std.file, std.regex, std.stdio;

  immutable coverage_filename_predicate = `endsWith(a.name, ".lst") && find(a.name, ".dub-packages").empty`;
  auto coverage_files = dirEntries(".", SpanMode.depth).filter!coverage_filename_predicate;
  auto whole_line_regex = regex(`^([0-9 ]+)\|(.*)$`, "g");
  auto empty_coverage_regex = regex(`^ +$`, "g");
  foreach (filename; coverage_files)
  {
    auto input_file = File(filename, "r");
    auto output_file = File(filename ~ "#.cov", "w");
    auto source_filename = filename.replace("-", "/").replaceLast(".lst", ".d");
    auto line_number = 0;
    output_file.writefln("%8s:%8s:Source:%s", "-", line_number, source_filename);
    foreach (line; input_file.byLine())
    {
      auto captures = line.matchFirst(whole_line_regex);
      if (!captures.empty())
      {
        auto line_coverage = captures[1].match(empty_coverage_regex) ? "-" : captures[1];
        auto source_code_text = captures[2];
        output_file.writefln("%8s:%8s:%s", line_coverage, ++line_number, source_code_text);
      }
    }
  }
}
