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
module clop.rt.instance;

import std.stdio;
import derelict.opencl.cl;
import clop.rt.ctx;

/++
 +
 +/
final class Instance
{
  string name;
  string program;
  cl_mem[] buffers;
  cl_kernel kernel;

  this(string n, cl_kernel k)
  {
    name = n;
    kernel = k;
  }

  @property override
  string toString()
  {
    return name;
  }

  void release_resources()
  {
    cl_int status;
    foreach (buffer; buffers)
    {
      status = clReleaseMemObject(buffer);
      assert(status == CL_SUCCESS, cl_strerror(status, "clReleaseMemObject"));
    }
    status = clReleaseKernel(kernel);
    assert(status == CL_SUCCESS, cl_strerror(status, "clReleaseKernel"));
    writeln("CLOP Instance " ~ name ~ " released all resources.");
  }
}

// Local Variables:
// flycheck-dmd-include-path: ("~/.dub/packages/derelict-cl-1.2.2/source")
// End:
