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
module clop.examples.vectors;

import std.conv;
import std.datetime;
import std.getopt;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;

/++
 +/
class Application {
  static immutable int SEED  =  1;

  cl_kernel kernel;
  float[] A, B, C;
  size_t size;

  /++
   +/
  this(string[] args)
  {
    if (args.length != 2)
    {
      throw new Exception("ERROR: invalid args # " ~ to!(string)(args.length - 1));
    }
    size = to!(size_t)(args[1]);
    A = new float[size]; assert(A !is null, "Can't allocate array A");
    B = new float[size]; assert(B !is null, "Can't allocate array B");
    C = new float[size]; assert(C !is null, "Can't allocate array C");

    Mt19937 gen;
    gen.seed(SEED);
    foreach (r; 0 .. size)
    {
      A[r] = uniform(0f, 1f, gen);
      B[r] = uniform(0f, 1f, gen);
    }

    /++
     +/
    char[] code = q{
      __kernel void vector_sum(__global float* a, __global float* b, __global float* c)
      {
        int tx = get_global_id(0);
        c[tx] = a[tx] + b[tx];
      } /* vector_sum */
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource(runtime.context, 1, strs.ptr, &size, &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    status = clBuildProgram(program, 1, &runtime.device, "", null, null);

    if (status != CL_SUCCESS)
    {
      char[3072] log;
      clGetProgramBuildInfo(program, runtime.device, CL_PROGRAM_BUILD_LOG, 3071, log.ptr, null);
      writeln("CL_PROGRAM_BUILD_LOG:\n", log, "\nEOD");
    }

    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel = clCreateKernel(program, "vector_sum", &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    status = clReleaseProgram(program);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
  } // this()

  /++
   +/
  void validate()
  {
    uint diff = 0;
    foreach (ii; 0 .. size)
      if (C[ii] != A[ii] + B[ii])
        ++diff;
    if (diff > 0)
      writeln("DIFFs ", diff);
  }

  /++
   +/
  void opencl_compute()
  {
    try
    {
      cl_int status;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dA = clCreateBuffer(runtime.context, flags, cl_float.sizeof * A.length, A.ptr, &status);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      cl_mem dB = clCreateBuffer(runtime.context, flags, cl_float.sizeof * B.length, B.ptr, &status);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      flags = CL_MEM_WRITE_ONLY | CL_MEM_USE_HOST_PTR;
      cl_mem dC = clCreateBuffer(runtime.context, flags, cl_float.sizeof * C.length, C.ptr, &status);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clSetKernelArg(kernel, 0, cl_mem.sizeof, &dA);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clSetKernelArg(kernel, 1, cl_mem.sizeof, &dB);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clSetKernelArg(kernel, 2, cl_mem.sizeof, &dC);

      status = clEnqueueNDRangeKernel(runtime.queue, kernel, 1, null, &size, null, 0, null, null);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));

      status = clEnqueueReadBuffer(runtime.queue, dC, CL_TRUE, 0, cl_float.sizeof * C.length, C.ptr, 0, null, null);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clReleaseMemObject(dA);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clReleaseMemObject(dB);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clReleaseMemObject(dC);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clReleaseKernel(kernel);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
    }
    catch (Exception e)
    {
      writeln(e);
    }
  }

  /++
   +/
  void clop_compute()
  {
    mixin (compile(q{ NDRange(i : 0 .. size) { C[i] = A[i] + B[i]; } }));
  }

  /++
   +/
  void run()
  {
    size_t size;
    StopWatch timer;
    TickDuration ticks;

    timer.reset();
    timer.start();
    opencl_compute();
    timer.stop();
    ticks = timer.peek();
    writefln("OPENCL %5.3f [s]", ticks.usecs / 1E6);
    validate();

    timer.reset();
    timer.start();
    clop_compute();
    timer.stop();
    ticks = timer.peek();
    writefln("CLOP   %5.3f [s]", ticks.usecs / 1E6);
    validate();
  }
}

int main(string[] args)
{
  auto platforms = runtime.get_platforms();
  foreach (p; 0 .. platforms.length)
    foreach (d; 0 .. platforms[p])
    {
      try
      {
        runtime.init(p, d);
        writeln("==================================================");
        auto app = new Application(args);
        app.run();
        writeln("==================================================");
      }
      catch (Exception msg)
      {
        writeln("BP: ", msg);
        return -1;
      }
      finally
      {
        runtime.shutdown();
      }
    }
  return 0;
}
