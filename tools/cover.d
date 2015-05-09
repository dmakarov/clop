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

import std.stdio;

void save_coverage_in_json_file()
{
  import std.algorithm.iteration : filter;
  import std.algorithm.searching : endsWith, find;
  import std.array, std.conv, std.digest.md, std.file;
  import std.format, std.process, std.range, std.regex;

  immutable coverage_filename_predicate =
    `endsWith(a.name, ".lst") && find(a.name, ".dub-packages").empty && find(a.name, "dub_test_root").empty`;
  auto coverage_files = dirEntries(".", SpanMode.depth).filter!coverage_filename_predicate;
  auto whole_line_regex = regex(`^( *)([0-9]*)\|.*$`, "g");
  auto all_zeros_regex = regex(`^ *0+$`, "g");
  auto output_file = File("json_file", "w");
  auto job_id = environment.get("TRAVIS_JOB_ID", "");
  output_file.writef(`{
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
    output_file.writef(`%s
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
        auto line_coverage = captures[2].empty ? "null" :
          captures[2].match(all_zeros_regex) ? "0" : captures[2];
        output_file.writef("%s%s", coverage_item_comma, line_coverage);
      }
      coverage_item_comma = ", ";
    }
    output_file.writef("]\n    }");
    file_block_comma = ",";
  }
  output_file.writef("\n  ]\n}\n");
  output_file.close();
}

void send_json_file_to_coveralls()
{
  import etc.c.curl;
  import core.sys.posix.sys.time;
  /* Fill in the file upload field. This makes libcurl load data from
     the given file name when curl_easy_perform() is called. */
  curl_httppost *formpost;
  curl_httppost *lastptr;
  const char[] json_file = "json_file";
  curl_formadd(&formpost, &lastptr,
               CurlForm.copyname, json_file.ptr,
               CurlForm.file, json_file.ptr,
               CurlForm.end);
  CURL* curl = curl_easy_init();
  CURLM* multi_handle = curl_multi_init();
  // initalize custom header list stating that Expect: 100
  // - continue is not wanted
  curl_slist* headerlist;
  const char[] expect_header = "Expect:";
  headerlist = curl_slist_append(headerlist, expect_header.ptr);
  if (curl && multi_handle)
  {
    const char[] url = "https://coveralls.io/api/v1/jobs";
    curl_easy_setopt(curl, CurlOption.url, url.ptr);
    //curl_easy_setopt(curl, CurlOption.verbose, 1L);
    curl_easy_setopt(curl, CurlOption.httpheader, headerlist);
    curl_easy_setopt(curl, CurlOption.httppost, formpost);
    curl_multi_add_handle(multi_handle, curl);
    int still_running;
    curl_multi_perform(multi_handle, &still_running);
    do
    {
      // set a suitable timeout to play around with
      timeval timeout = { 1, 0 }; // 1 s
      long curl_timeo = -1;

      curl_multi_timeout(multi_handle, &curl_timeo);

      if (curl_timeo >= 0)
      {
        timeout.tv_sec = curl_timeo / 1000;
        if (timeout.tv_sec > 1)
        {
          timeout.tv_sec = 1;
        }
        else
        {
          timeout.tv_usec = (curl_timeo % 1000) * 1000;
        }
      }
      // get file descriptors from the transfers
      core.sys.posix.sys.select.fd_set fdread;
      core.sys.posix.sys.select.fd_set fdwrite;
      core.sys.posix.sys.select.fd_set fdexcep;
      int maxfd = -1;
      CURLMcode mc = curl_multi_fdset(multi_handle,
      /* */                           cast(int*) &fdread,
      /* */                           cast(int*) &fdwrite,
      /* */                           cast(int*) &fdexcep,
      /* */                           &maxfd);
      if (mc != CurlM.ok)
      {
        writef("curl_multi_fdset() failed, code %d.", mc);
        break;
      }
      /* On success the value of maxfd is guaranteed to be >= -1.
         We call select(maxfd + 1, ...); specially in case of
         (maxfd == -1) there are no fds ready yet so we call
         select(0, ...) to sleep 100 ms, which is the minimum
         suggested value in the curl_multi_fdset() doc. */
      int rc; // select() return code
      if (maxfd == -1)
      {
        // Portable sleep for platforms other than Windows.
        timeval wait = { 0, 100_000 }; // 100 ms
        rc = select(0, null, null, null, &wait);
      }
      else
      {
        /* Note that on some platforms 'timeout' may be modified by
           select().  If you need access to the original value save a
           copy beforehand.  */
        rc = select(maxfd + 1, &fdread, &fdwrite, &fdexcep, &timeout);
      }
      if (rc != -1)
      {
        // timeout or readable/writable sockets
        curl_multi_perform(multi_handle, &still_running);
      }
    } while (still_running);

    curl_multi_cleanup(multi_handle);
    curl_easy_cleanup(curl);
    curl_formfree(formpost);
    curl_slist_free_all(headerlist);
    writeln();
  }
}

void main(string[] args)
{
  save_coverage_in_json_file();
  send_json_file_to_coveralls();
}
