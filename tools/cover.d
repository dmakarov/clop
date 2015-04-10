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
  import std.array, std.digest.md, std.file, std.format;
  import std.net.curl, std.process, std.range, std.regex, std.stdio;

  immutable coverage_filename_predicate = `endsWith(a.name, ".lst") && find(a.name, ".dub-packages").empty`;
  auto coverage_files = dirEntries(".", SpanMode.depth).filter!coverage_filename_predicate;
  auto whole_line_regex = regex(`^( *)([0-9]*)\|.*$`, "g");
  //auto output_file = File("json_file", "w");
  auto job_id = environment.get("TRAVIS_JOB_ID", "N/A");
  auto output_buffer = format(`json_file: {
  "service_job_id" : "%s",
  "service_name" : "travis-ci",
  "source_files" : [`, job_id);
  auto file_block_comma = "";
  foreach (filename; coverage_files)
  {
    auto input_file = File(filename, "r");
    auto source_filename = filename.replaceFirst("./", "").replace("-", "/").replaceLast(".lst", ".d");
    MD5 source_digest;
    source_digest.start();
    auto source_file = File(source_filename, "rb");
    put(source_digest, source_file.byChunk(1024));
    output_buffer ~= format(`%s
    {
      "name" : "%s",
      "source_digest" : "%s",
      "coverage" : [`, file_block_comma, source_filename, toHexString(source_digest.finish()));
    auto coverage_item_comma = "";
    foreach (line; input_file.byLine())
    {
      auto captures = line.matchFirst(whole_line_regex);
      if (!captures.empty())
      {
        auto line_coverage = captures[2].empty ? "null" : captures[2];
        output_buffer ~= format("%s%s", coverage_item_comma, line_coverage);
      }
      coverage_item_comma = ", ";
    }
    output_buffer ~= format("]\n    }");
    file_block_comma = ",";
  }
  output_buffer ~= format("\n  ]\n}\n");
  auto content = post("https://coveralls.io/api/v1/jobs", output_buffer);
  writeln("coveralls responded: ", content);
}
