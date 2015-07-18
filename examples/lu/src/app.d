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
module clop.examples.lu;

import std.conv;
import std.datetime;
import std.format;
import std.getopt;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;

/++
 +/
class Application {
  static immutable auto BLOCK_SIZE = 16;
  cl_kernel     diagonal;
  cl_kernel     perimeter;
  cl_kernel     internal;
  size_t        matrix_dim = 32;
  bool          do_verify = false;
  NDArray!float m;
  NDArray!float mm;

  /++
   +/
  this(string[] args)
  {
    getopt(args, "size|s", &matrix_dim, "verify|v", &do_verify);
    if (args.length > 1 || 0 == matrix_dim)
    {
      writefln("Usage: %s [-v] -s matrix_size", args[0]);
      throw new Exception("invalid command line arguments");
    }
    m = new NDArray!float(matrix_dim, matrix_dim);
    if (do_verify)
    {
      mm = new NDArray!float(matrix_dim, matrix_dim);
    }
    /++
     +/
    char[] code = q{
      __kernel void lud_diagonal(__global float* m, __local float* shadow, int matrix_dim, int offset)
      {
        int tx = get_local_id(0);
        int array_offset = offset * matrix_dim + offset;

        for (int i = 0; i < BLOCK_SIZE; ++i)
        {
          shadow[i * BLOCK_SIZE + tx] = m[array_offset + tx];
          array_offset += matrix_dim;
        }

        barrier(CLK_LOCAL_MEM_FENCE);

        for (int i = 0; i < BLOCK_SIZE-1; ++i)
        {
          if (tx > i)
          {
            for (int j = 0; j < i; ++j)
            {
              shadow[tx * BLOCK_SIZE + i] -= shadow[tx * BLOCK_SIZE + j] * shadow[j * BLOCK_SIZE + i];
            }
            shadow[tx * BLOCK_SIZE + i] /= shadow[i * BLOCK_SIZE + i];
          }

          barrier(CLK_LOCAL_MEM_FENCE);

          if (tx > i)
          {
            for (int j = 0; j < i + 1; ++j)
            {
              shadow[(i + 1) * BLOCK_SIZE + tx] -= shadow[(i + 1) * BLOCK_SIZE + j] * shadow[j * BLOCK_SIZE + tx];
            }
          }
          barrier(CLK_LOCAL_MEM_FENCE);
        }

        array_offset = (offset + 1) * matrix_dim + offset;
        for (int i = 1; i < BLOCK_SIZE; ++i)
        {
          m[array_offset + tx] = shadow[i * BLOCK_SIZE + tx];
          array_offset += matrix_dim;
        }
      }

      __kernel void lud_perimeter(__global float* m, __local float* dia, __local float* peri_row, __local float* peri_col, int matrix_dim, int offset)
      {
        int bx = get_group_id(0);
        int tx = get_local_id(0);

        if (tx < BLOCK_SIZE)
        {
          int idx = tx;
          int array_offset = offset * matrix_dim + offset;
          for (int i = 0; i < BLOCK_SIZE / 2; ++i)
          {
            dia[i * BLOCK_SIZE + idx] = m[array_offset + idx];
            array_offset += matrix_dim;
          }
          array_offset = offset * matrix_dim + offset;
          for (int i = 0; i < BLOCK_SIZE; ++i)
          {
            peri_row[i * BLOCK_SIZE+ idx] = m[array_offset + (bx + 1) * BLOCK_SIZE + idx];
            array_offset += matrix_dim;
          }
        }
        else
        {
          int idx = tx - BLOCK_SIZE;
          int array_offset = (offset + BLOCK_SIZE / 2) * matrix_dim + offset;
          for (int i = BLOCK_SIZE / 2; i < BLOCK_SIZE; ++i)
          {
            dia[i * BLOCK_SIZE + idx] = m[array_offset + idx];
            array_offset += matrix_dim;
          }
          array_offset = (offset + (bx + 1) * BLOCK_SIZE) * matrix_dim + offset;
          for (int i = 0; i < BLOCK_SIZE; ++i)
          {
            peri_col[i * BLOCK_SIZE + idx] = m[array_offset + idx];
            array_offset += matrix_dim;
          }
        }

        barrier(CLK_LOCAL_MEM_FENCE);

        if (tx < BLOCK_SIZE)
        { //peri-row
          int idx = tx;
          for (int i = 1; i < BLOCK_SIZE; ++i)
          {
            for (int j = 0; j < i; ++j)
            {
              peri_row[i * BLOCK_SIZE + idx] -= dia[i * BLOCK_SIZE+ j] * peri_row[j * BLOCK_SIZE + idx];
            }
          }
        }
        else
        { //peri-col
          int idx = tx - BLOCK_SIZE;
          for (int i = 0; i < BLOCK_SIZE; ++i)
          {
            for (int j = 0; j < i; ++j)
            {
              peri_col[idx * BLOCK_SIZE + i] -= peri_col[idx * BLOCK_SIZE+ j] * dia[j * BLOCK_SIZE + i];
            }
            peri_col[idx * BLOCK_SIZE + i] /= dia[i * BLOCK_SIZE + i];
          }
        }

        barrier(CLK_LOCAL_MEM_FENCE);

        if (tx < BLOCK_SIZE)
        { //peri-row
          int idx = tx;
          int array_offset = (offset + 1) * matrix_dim + offset;
          for (int i = 1; i < BLOCK_SIZE; ++i)
          {
            m[array_offset + (bx + 1) * BLOCK_SIZE + idx] = peri_row[i * BLOCK_SIZE + idx];
            array_offset += matrix_dim;
          }
        }
        else
        { //peri-col
          int idx = tx - BLOCK_SIZE;
          int array_offset = (offset + (bx + 1) * BLOCK_SIZE) * matrix_dim + offset;
          for (int i = 0; i < BLOCK_SIZE; ++i)
          {
            m[array_offset + idx] = peri_col[i * BLOCK_SIZE + idx];
            array_offset += matrix_dim;
          }
        }
      }

      __kernel void lud_internal(__global float* m, __local float* peri_row, __local float* peri_col, int matrix_dim, int offset)
      {
        int bx = get_group_id(0);
        int by = get_group_id(1);
        int tx = get_local_id(0);
        int ty = get_local_id(1);
        int global_row_id = offset + (by + 1) * BLOCK_SIZE;
        int global_col_id = offset + (bx + 1) * BLOCK_SIZE;

        peri_row[ty * BLOCK_SIZE + tx] = m[(offset + ty) * matrix_dim + global_col_id + tx];
        peri_col[ty * BLOCK_SIZE + tx] = m[(global_row_id + ty) * matrix_dim + offset + tx];

        barrier(CLK_LOCAL_MEM_FENCE);

        float sum = 0;
        for (int i = 0; i < BLOCK_SIZE; ++i)
        {
          sum += peri_col[ty * BLOCK_SIZE + i] * peri_row[i * BLOCK_SIZE + tx];
        }
        m[(global_row_id + ty) * matrix_dim + global_col_id + tx] -= sum;
      }
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource(runtime.context, 1, strs.ptr, &size, &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    auto clopts = format("-DBLOCK_SIZE=%s", BLOCK_SIZE);
    status = clBuildProgram(program, 1, &runtime.device, clopts.ptr, null, null);
    if (status != CL_SUCCESS)
    {
      char[3072] log;
      clGetProgramBuildInfo(program, runtime.device, CL_PROGRAM_BUILD_LOG, 3071, log.ptr, null);
      writeln("CL_PROGRAM_BUILD_LOG:\n", log, "\nEOD");
    }
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    diagonal  = clCreateKernel(program, "lud_diagonal" , &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    perimeter = clCreateKernel(program, "lud_perimeter", &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    internal  = clCreateKernel(program, "lud_internal" , &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    status = clReleaseProgram(program);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
  } // this()

  // Generate well-conditioned matrix internally by Ke Wang 2013/08/07 22:20:06
  void create_matrix()
  {
    import std.math : exp;
    writefln("Creating matrix internally size = %s", matrix_dim);
    const float lambda = -0.001;
    auto coe = new float[2 * matrix_dim - 1];
    float coe_i = 0.0;
    foreach (i; 0 .. matrix_dim)
    {
      coe_i = 10 * exp(lambda * i);
      coe[matrix_dim - 1 + i] = coe_i;
      coe[matrix_dim - 1 - i] = coe_i;
    }
    foreach (i; 0 .. matrix_dim)
    {
      foreach (j; 0 .. matrix_dim)
      {
        m[i, j] = coe[matrix_dim - 1 - i + j];
      }
    }
    if (do_verify)
    {
      mm[] = m;
    }
  }

  void print_matrix()
  {
    foreach (i; 0 .. matrix_dim)
    {
      writef("%5.2f", m[i, 0]);
      foreach (j; 1 .. matrix_dim)
      {
        writef(" %5.2f", m[i, j]);
      }
      writeln();
    }
  }

  void validate()
  {
    if (do_verify)
    {
      import std.math : fabs;
      writeln(">>>Verify<<<<");
      NDArray!float tmp = new NDArray!float(matrix_dim, matrix_dim);
      foreach (i; 0 .. matrix_dim)
      {
        foreach (j; 0 .. matrix_dim)
        {
          float sum = 0;
          for (int k = 0; k <= i && k <= j; ++k)
          {
            float l = (i == k) ? 1 : m[i, k];
            float u = m[k, j];
            sum += l * u;
          }
          tmp[i, j] = sum;
        }
      }
      foreach (i; 0 .. matrix_dim)
      {
        foreach (j; 0 .. matrix_dim)
        {
          if (fabs(mm[i, j] - tmp[i, j]) > 0.0001)
          {
            writefln("dismatch at (%s, %s): (o)%s (n)%s", i, j, m[i, j], tmp[i, j]);
          }
        }
      }
    }
  }

  void opencl_lud()
  {
    try
    {
    writefln("WG size of kernel = %s X %s", BLOCK_SIZE, BLOCK_SIZE);
    create_matrix();
    cl_int status;
    cl_mem d_m;
    d_m = clCreateBuffer(runtime.context, CL_MEM_READ_WRITE, m.length * cl_float.sizeof, null, &status);
    assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));
    status = clEnqueueWriteBuffer(runtime.queue, d_m, 1, 0, m.length * cl_float.sizeof, m.ptr, 0, null, null);
    assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));
    int i;
    for (i = 0; i < matrix_dim - BLOCK_SIZE; i += BLOCK_SIZE)
    {
      clSetKernelArg(diagonal, 0, cl_mem.sizeof, &d_m);
      clSetKernelArg(diagonal, 1, cl_float.sizeof * BLOCK_SIZE * BLOCK_SIZE, null);
      clSetKernelArg(diagonal, 2, cl_int.sizeof, &matrix_dim);
      clSetKernelArg(diagonal, 3, cl_int.sizeof, &i);

      size_t[3] global_work1 = [BLOCK_SIZE, 1, 1];
      size_t[3] local_work1  = [BLOCK_SIZE, 1, 1];

      status = clEnqueueNDRangeKernel(runtime.queue, diagonal, 2, null, global_work1.ptr, local_work1.ptr, 0, null, null);
      assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));

      clSetKernelArg(perimeter, 0, cl_mem.sizeof, &d_m);
      clSetKernelArg(perimeter, 1, cl_float.sizeof * BLOCK_SIZE * BLOCK_SIZE, null);
      clSetKernelArg(perimeter, 2, cl_float.sizeof * BLOCK_SIZE * BLOCK_SIZE, null);
      clSetKernelArg(perimeter, 3, cl_float.sizeof * BLOCK_SIZE * BLOCK_SIZE, null);
      clSetKernelArg(perimeter, 4, cl_int.sizeof, &matrix_dim);
      clSetKernelArg(perimeter, 5, cl_int.sizeof, &i);

      size_t[3] global_work2 = [BLOCK_SIZE * 2 * ((matrix_dim - i) / BLOCK_SIZE - 1), 1, 1];
      size_t[3] local_work2  = [BLOCK_SIZE * 2, 1, 1];

      status = clEnqueueNDRangeKernel(runtime.queue, perimeter, 2, null, global_work2.ptr, local_work2.ptr, 0, null, null);
      assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));

      clSetKernelArg(internal, 0, cl_mem.sizeof, &d_m);
      clSetKernelArg(internal, 1, cl_float.sizeof * BLOCK_SIZE * BLOCK_SIZE, null);
      clSetKernelArg(internal, 2, cl_float.sizeof * BLOCK_SIZE * BLOCK_SIZE, null);
      clSetKernelArg(internal, 3, cl_int.sizeof, &matrix_dim);
      clSetKernelArg(internal, 4, cl_int.sizeof, &i);

      size_t[3] global_work3 = [BLOCK_SIZE * ((matrix_dim - i) / BLOCK_SIZE - 1), BLOCK_SIZE * ((matrix_dim - i) / BLOCK_SIZE - 1), 1];
      size_t[3] local_work3  = [BLOCK_SIZE, BLOCK_SIZE, 1];

      status = clEnqueueNDRangeKernel(runtime.queue, internal, 2, null, global_work3.ptr, local_work3.ptr, 0, null, null);
      assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));
    }
    clSetKernelArg(diagonal, 0, cl_mem.sizeof, &d_m);
    clSetKernelArg(diagonal, 1, cl_float.sizeof * BLOCK_SIZE * BLOCK_SIZE, null);
    clSetKernelArg(diagonal, 2, cl_int.sizeof, &matrix_dim);
    clSetKernelArg(diagonal, 3, cl_int.sizeof, &i);

    size_t[3] global_work1 = [BLOCK_SIZE, 1, 1];
    size_t[3] local_work1  = [BLOCK_SIZE, 1, 1];
    status = clEnqueueNDRangeKernel(runtime.queue, diagonal, 2, null, global_work1.ptr, local_work1.ptr, 0, null, null);
    assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));

    status = clEnqueueReadBuffer(runtime.queue, d_m, 1, 0, m.length * cl_float.sizeof, m.ptr, 0, null, null);
    assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));
    clFinish(runtime.queue);
    clReleaseMemObject(d_m);

    }
    catch( Exception e )
    {
      write( e );
      writeln();
    }
  }

  /++
   +/
  void clop_lud()
  {
    // use CLOP DSL to generate OpenCL kernel and API calls.
/+
    mixin( compile(
    q{
      Antidiagonal NDRange(r : 1 .. rows, c : 1 .. cols) {
        F[r, c] = max3( F[r - 1, c - 1] + S[r, c], F[r, c - 1] - penalty, F[r - 1, c] - penalty );
      } apply( rectangular_blocking( 8, 8 ) )
    } ) );
+/
  }

  /++
   +/
  void run()
  {
    StopWatch timer;
    TickDuration ticks;

    timer.start();
    opencl_lud();
    timer.stop();
    ticks = timer.peek();
    writefln( "OPENCL %5.3f [s]", ticks.usecs / 1E6 );
    validate();
  }
}

int
main(string[] args)
{
  auto platforms = runtime.get_platforms();
  foreach (p; 0 .. platforms.length)
    foreach (d; 1 .. platforms[p])
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
        writeln("LU: ", msg);
        return -1;
      }
      finally
      {
        runtime.shutdown();
      }
    }
  return 0;
}
