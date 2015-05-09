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
 + The class examplifies the basic arithmetic operations on vectors
 + and matrices.
 +/
class Application {

  static immutable int SEED = 1;

  NDArray!cl_float A, B, C;
  NDArray!cl_float M, N, R;
  size_t size;

  /++
   + The Application constructor allocates and initializes the vectors
   + and matrices with random numbers in the interval [0,1].
   + @param args an array of command-line arguments passed to the
   +        application by the user. The only accepted argument is an
   +        integer positive value N -- the lentgh of a vector, and
   +        the size N x N of a matrix.
   +/
  this(string[] args)
  {
    if (args.length != 2)
    {
      throw new Exception("ERROR: invalid args # " ~ to!(string)(args.length - 1));
    }
    size = to!(size_t)(args[1]);
    A = new NDArray!cl_float(size);       assert(A !is null, "Can't allocate array A");
    B = new NDArray!cl_float(size);       assert(B !is null, "Can't allocate array B");
    C = new NDArray!cl_float(size);       assert(C !is null, "Can't allocate array C");
    M = new NDArray!cl_float(size, size); assert(M !is null, "Can't allocate array M");
    N = new NDArray!cl_float(size, size); assert(N !is null, "Can't allocate array N");
    R = new NDArray!cl_float(size, size); assert(R !is null, "Can't allocate array R");

    Mt19937 gen;
    gen.seed(SEED);
    foreach (i; 0 .. size)
    {
      A[i] = uniform(0f, 1f, gen);
      B[i] = uniform(0f, 1f, gen);
      foreach (j; 0 .. size)
      {
        M[i, j] = uniform(0f, 1f, gen);
        N[i, j] = uniform(0f, 1f, gen);
      }
    }
  } // this

  /++
   + Compute the sum of two vectors using CLOP.
   +/
  void clop_add_vectors()
  {
    mixin (compile(q{ NDRange(i : 0 .. size) { C[i] = A[i] + B[i]; } }));
  }

  /++
   + Compute the sum of two matrices using CLOP.
   +/
  void clop_add_matrices()
  {
    mixin (compile(q{ NDRange(i : 0 .. size, j : 0 .. size) { R[i, j] = M[i, j] + N[i, j]; } }));
  }

  /++
   + Compute the product of two matrices using CLOP.
   +/
  void clop_mul_matrices()
  {
    mixin (compile(q{
          NDRange(i : 0 .. size, j : 0 .. size)
          {
            int k;
            R[i, j] = 0;
            for (k = 0; k < size; ++k)
              R[i, j] += M[i, k] * N[k, j];
          }
        }));
  }

  /++
   + Compute the sum of two vectors using OpenCL directly.
   +/
  void opencl_add_vectors()
  {
    try
    {
      char[] code = q{
        __kernel void vector_sum(__global float* a, __global float* b, __global float* c)
        {
          int tx = get_global_id(0);
          c[tx] = a[tx] + b[tx];
        } /* vector_sum */
      }.dup;
      cl_int status;
      char*[] strs = [code.ptr];
      size_t code_length = code.length;
      auto program = clCreateProgramWithSource(runtime.context, 1, strs.ptr, &code_length, &status);
      assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
      status = clBuildProgram(program, 1, &runtime.device, "", null, null);
      if (status != CL_SUCCESS)
      {
        char[1024] log;
        clGetProgramBuildInfo(program, runtime.device, CL_PROGRAM_BUILD_LOG, 1023, log.ptr, null);
        writeln("CL_PROGRAM_BUILD_LOG:\n", log, "\nEOD");
      }
      assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
      cl_kernel kernel = clCreateKernel(program, "vector_sum", &status);
      assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
      status = clReleaseProgram(program);
      assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));

      cl_mem dA = clCreateBuffer(runtime.context, CL_MEM_READ_ONLY, cl_float.sizeof * A.length, null, &status);
      assert(status == CL_SUCCESS, "opencl_compute clCreateBuffer A " ~ cl_strerror(status));
      cl_mem dB = clCreateBuffer(runtime.context, CL_MEM_READ_ONLY, cl_float.sizeof * B.length, null, &status);
      assert(status == CL_SUCCESS, "opencl_compute clCreateBuffer B " ~ cl_strerror(status));
      cl_mem dC = clCreateBuffer(runtime.context, CL_MEM_WRITE_ONLY, cl_float.sizeof * C.length, null, &status);
      assert(status == CL_SUCCESS, "opencl_compute clCreateBuffer C " ~ cl_strerror(status));

      status = clEnqueueWriteBuffer(runtime.queue, dA, CL_TRUE, 0, cl_float.sizeof * A.length, A.ptr, 0, null, null);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clEnqueueWriteBuffer(runtime.queue, dB, CL_TRUE, 0, cl_float.sizeof * B.length, B.ptr, 0, null, null);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));

      status = clSetKernelArg(kernel, 0, cl_mem.sizeof, &dA);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clSetKernelArg(kernel, 1, cl_mem.sizeof, &dB);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));
      status = clSetKernelArg(kernel, 2, cl_mem.sizeof, &dC);
      assert(status == CL_SUCCESS, "opencl_compute " ~ cl_strerror(status));

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
  void run()
  {
    StopWatch timer;
    alias check = validate!((a, b) => a + b);

    timer.reset();
    timer.start();
    opencl_add_vectors();
    timer.stop();
    check(C, A, B);
    writefln("OPENCL %5.3f [s]", timer.peek().usecs / 1E6);

    C[] = cl_float.init;
    clop_add_vectors();
    check(C, A, B);

    clop_add_matrices();
    check(R, M, N);

    clop_mul_matrices();
  }
} // Application class

/++
 + Validate that an is a result of applying an operation to two other
 + arrays element-wise.
 + @param fun template parameter is a binary operation
 + @param T template parameter deduced from the actual parameters to
 +          validate
 + @param R the result of applying fun to A and B
 + @param A, B input arrays
 +/
template validate(alias fun)
{
  void validate(T)(NDArray!T R, NDArray!T A, NDArray!T B)
    if (is (typeof (fun(A[0], B[0])) == T))
  {
    uint diff = 0;
    foreach (ii; 0 .. R.length)
      if (R[ii] != fun(A[ii], B[ii]))
        ++diff;
    if (diff > 0)
      writeln("DIFFs ", diff);
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
        writeln("algebra: ", msg);
        return -1;
      }
      finally
      {
        runtime.shutdown();
      }
    }
  return 0;
}
