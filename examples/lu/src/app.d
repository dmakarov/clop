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
module clop.examples.lu.app;

import std.conv;
import std.datetime;
import std.format;
import std.getopt;
import std.stdio;

import derelict.opencl.cl;
import clop.compiler;

static auto run_once = true;

/**
 *  LU Decomposition implementation.
 */
class Application {
  const uint    BLOCK_SIZE;
  cl_kernel     diagonal;
  cl_kernel     perimeter;
  cl_kernel     internal;
  cl_kernel     baseline_h;
  cl_kernel     baseline_v;
  size_t        matrix_dim = 32;
  bool          do_verify = false;
  bool          do_animate = false;
  bool          doolittle = false;
  NDArray!float m;
  NDArray!float mm;
  History       history;

  /**
   *  Process the command line arguments, and compile kernels.
   */
  this(string[] args)
  {
    uint block_size = 16;
    getopt(args, "animate|a", &do_animate, "block_size|b", &block_size, "doolittle|d", &doolittle, "verify|v", &do_verify, "size|s", &matrix_dim);
    if (args.length > 1 || 0 == matrix_dim)
    {
      writefln("Usage: %s [-a -b block_size -d -v] -s matrix_size", args[0]);
      throw new Exception("invalid command line arguments");
    }
    m = new NDArray!float(matrix_dim, matrix_dim);
    if (do_verify)
    {
      mm = new NDArray!float(matrix_dim, matrix_dim);
    }
    do_animate = do_animate && run_once;
    doolittle = doolittle && run_once;
    BLOCK_SIZE = block_size;
    /**
     */
    char[] code = q{
      #define G(i,j) ((i) * matrix_dim + (j))
      #define L(i,j) ((i) * BLOCK_SIZE + (j))

      __kernel void lud_diagonal(__global float* m, int matrix_dim, int offset)
      {
        __local float shadow[BLOCK_SIZE * BLOCK_SIZE];
        int tx = get_local_id(0);

        for (int i = 0; i < BLOCK_SIZE; ++i)
        {
          shadow[L(i, tx)] = m[G(offset + i, offset + tx)];
        }

        barrier(CLK_LOCAL_MEM_FENCE);

        for (int k = 0; k < BLOCK_SIZE - 1; ++k)
        {
          if (tx > k)
          {
            for (int p = 0; p < k; ++p)
            {
              shadow[L(tx, k)] -= shadow[L(tx, p)] * shadow[L(p, k)];
            }
            shadow[L(tx, k)] /= shadow[L(k, k)];
          }

          barrier(CLK_LOCAL_MEM_FENCE);

          if (tx > k)
          {
            for (int p = 0; p < k + 1; ++p)
            {
              shadow[L(k + 1, tx)] -= shadow[L(k + 1, p)] * shadow[L(p, tx)];
            }
          }

          barrier(CLK_LOCAL_MEM_FENCE);
        }

        for (int i = 1; i < BLOCK_SIZE; ++i)
        {
          m[G(offset + i, offset + tx)] = shadow[L(i, tx)];
        }
      }

      __kernel void lud_perimeter(__global float* m, int matrix_dim, int offset)
      {
        __local float peri_row[BLOCK_SIZE * BLOCK_SIZE];
        __local float peri_col[BLOCK_SIZE * BLOCK_SIZE];
        __local float dia[BLOCK_SIZE * BLOCK_SIZE];
        int bx = get_group_id(0);
        int tx = get_local_id(0);
        int idx = tx < BLOCK_SIZE ? tx : tx - BLOCK_SIZE;

        if (tx < BLOCK_SIZE)
        {
          for (int i = 0; i < BLOCK_SIZE / 2; ++i)
          {
            dia[L(i, idx)] = m[G(offset + i, offset + idx)];
          }
          for (int i = 0; i < BLOCK_SIZE; ++i)
          {
            peri_row[L(i, idx)] = m[G(offset + i, offset + (bx + 1) * BLOCK_SIZE + idx)];
          }
        }
        else
        {
          for (int i = BLOCK_SIZE / 2; i < BLOCK_SIZE; ++i)
          {
            dia[L(i, idx)] = m[G(offset + i, offset + idx)];
          }
          for (int i = 0; i < BLOCK_SIZE; ++i)
          {
            peri_col[L(i, idx)] = m[G(offset + (bx + 1) * BLOCK_SIZE + i, offset + idx)];
          }
        }

        barrier(CLK_LOCAL_MEM_FENCE);

        if (tx < BLOCK_SIZE)
        {
          for (int i = 1; i < BLOCK_SIZE; ++i)
          {
            for (int j = 0; j < i; ++j)
            {
              peri_row[L(i, idx)] -= dia[L(i, j)] * peri_row[L(j, idx)];
            }
          }
        }
        else
        {
          for (int i = 0; i < BLOCK_SIZE; ++i)
          {
            for (int j = 0; j < i; ++j)
            {
              peri_col[L(idx, i)] -= peri_col[L(idx, j)] * dia[L(j, i)];
            }
            peri_col[L(idx, i)] /= dia[L(i, i)];
          }
        }

        barrier(CLK_LOCAL_MEM_FENCE);

        if (tx < BLOCK_SIZE)
        {
          for (int i = 1; i < BLOCK_SIZE; ++i)
          {
            m[G(offset + i, offset + (bx + 1) * BLOCK_SIZE + idx)] = peri_row[L(i, idx)];
          }
        }
        else
        {
          for (int i = 0; i < BLOCK_SIZE; ++i)
          {
            m[G(offset + (bx + 1) * BLOCK_SIZE + i, offset + idx)] = peri_col[L(i, idx)];
          }
        }
      }

      __kernel void lud_internal(__global float* m, int matrix_dim, int offset)
      {
        __local float peri_row[BLOCK_SIZE * BLOCK_SIZE];
        __local float peri_col[BLOCK_SIZE * BLOCK_SIZE];
        int bx = get_group_id(0);
        int by = get_group_id(1);
        int tx = get_local_id(0);
        int ty = get_local_id(1);
        int global_row_id = offset + (by + 1) * BLOCK_SIZE;
        int global_col_id = offset + (bx + 1) * BLOCK_SIZE;

        peri_row[L(ty, tx)] = m[G(offset + ty, global_col_id + tx)];
        peri_col[L(ty, tx)] = m[G(global_row_id + ty, offset + tx)];

        barrier(CLK_LOCAL_MEM_FENCE);

        float sum = 0;
        for (int i = 0; i < BLOCK_SIZE; ++i)
        {
          sum += peri_col[L(ty, i)] * peri_row[L(i, tx)];
        }
        m[G(global_row_id + ty, global_col_id + tx)] -= sum;
      }

      __kernel void lud_baseline_horizontal(__global float* m, int matrix_dim, int k)
      {
        int j = k + get_global_id(0);
        float sum = 0;
        for (int p = 0; p < k; ++p)
        {
          sum += m[G(k, p)] * m[G(p, j)];
        }
        m[G(k, j)] -= sum;
      }

      __kernel void lud_baseline_vertical(__global float* m, int matrix_dim, int k)
      {
        int i = k + get_global_id(0) + 1;
        float sum = 0;
        for (int p = 0; p < k; ++p)
        {
          sum += m[G(i, p)] * m[G(p, k)];
        }
        m[G(i, k)] -= sum;
        m[G(i, k)] /= m[G(k, k)];
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
    baseline_h = clCreateKernel(program, "lud_baseline_horizontal", &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    baseline_v = clCreateKernel(program, "lud_baseline_vertical", &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    status = clReleaseProgram(program);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    if (do_animate)
    {
      history = new History("a.tex", matrix_dim);
    }
  } // this()

  long I(long i, long j)
  {
    return i * matrix_dim + j;
  }

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
      auto print_header = true;
      foreach (i; 0 .. matrix_dim)
      {
        foreach (j; 0 .. matrix_dim)
        {
          if (fabs(mm[i, j] - tmp[i, j]) > 0.0001)
          {
            if (print_header) writeln(">>>Verify<<<<");
            else print_header = false;
            writefln("dismatch at (%s, %s): (o)%s (n)%s", i, j, m[i, j], tmp[i, j]);
          }
        }
      }
    }
  }

  /**
   *  int Doolittle_LU_Decomposition(double *A, int n)
   *
   *  Description:
   *
   *     This routine uses Doolittle's method to decompose the N x N
   *     matrix A into a unit lower triangular matrix L and an upper
   *     triangular matrix U such that A = LU.  The matrices L and U
   *     replace the matrix A so that the original matrix A is
   *     destroyed.
   *
   *     Note!  In Doolittle's method the diagonal elements of L are 1
   *            and are not stored.
   *
   *     Note!  The determinant of A is the product of the diagonal
   *            elements of U.  (det A = det L * det U = det U).
   *
   *     This routine is suitable for those classes of matrices which
   *     when performing Gaussian elimination do not need to undergo
   *     partial pivoting, e.g. positive definite symmetric matrices,
   *     diagonally dominant band matrices, etc.  For the more general
   *     case in which partial pivoting is needed use
   *     Doolittle_LU_Decomposition_with_Pivoting.
   *
   *     The LU decomposition is convenient when one needs to solve
   *     the linear equation Ax = B for the vector x while the matrix
   *     A is fixed and the vector B is varied.  The routine for
   *     solving the linear system Ax = B after performing the LU
   *     decomposition for A is Doolittle_LU_Solve (see below).
   *
   *     The Doolittle method is given by evaluating, in order, the
   *     following pair of expressions
   *
   *     for k = 0, ... , n - 1:
   *       for j = k, k + 1, ... , n - 1:
   *         U[k][j] = A[k][j] - (L[k][0] * U[0][j] + ...
   *                              + L[k][k-1] * U[k-1][j])
   *       for i = k + 1, ... , n - 1:
   *         L[i][k] = (A[i][k] - (L[i][0] * U[0][k] + ...
   *                               + L[i][k-1] * U[k-1][k])) / U[k][k]
   *
   *       The matrix U forms the upper triangular matrix, and the
   *       matrix L forms the lower triangular matrix.
   *
   *  Arguments:
   *     double *A   Pointer to the first element of the matrix A[n][n].
   *     int     n   The number of rows or columns of the matrix A.
   *
   *  Return Values:
   *     0  Success
   *    -1  Failure - The matrix A is singular.
   *
   *  Example:
   *     #define N
   *     double A[N][N];
   *
   *     (your code to intialize the matrix A)
   *
   *     err = Doolittle_LU_Decomposition(&A[0][0], N);
   *     if (err < 0) printf(" Matrix A is singular\n");
   *     else { printf(" The LU decomposition of A is \n");
   *           ...
   */
  int doolittle_lud()
  {
    auto ts = 0;
    create_matrix();
    foreach (k; 0 .. matrix_dim)
    {
      foreach (j; k .. matrix_dim)
      {
        foreach (p; 0 .. k)
        {
          m[k, j] -= m[k, p] * m[p, j];
          if (do_animate) history.add_event(ts, -1, I(k, j), [I(k, p), I(p, j)]);
        }
        ++ts;
      }
      if (m[k, k] == 0.0) return -1;
      foreach (i; k + 1 .. matrix_dim)
      {
        foreach (p; 0 .. k)
        {
          m[i, k] -= m[i, p] * m[p, k];
          if (do_animate) history.add_event(ts, -1, I(i, k), [I(i, p), I(p, k)]);
        }
        m[i, k] /= m[k, k];
        if (do_animate) history.add_event(ts++, -1, I(i, k), [I(k, k)]);
      }
    }
    return 0;
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
        clSetKernelArg(diagonal, 1, cl_int.sizeof, &matrix_dim);
        clSetKernelArg(diagonal, 2, cl_int.sizeof, &i);
        size_t[1] global_work1 = [BLOCK_SIZE];
        status = clEnqueueNDRangeKernel(runtime.queue, diagonal, 1, null, global_work1.ptr, null, 0, null, null);
        assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));

        clSetKernelArg(perimeter, 0, cl_mem.sizeof, &d_m);
        clSetKernelArg(perimeter, 1, cl_int.sizeof, &matrix_dim);
        clSetKernelArg(perimeter, 2, cl_int.sizeof, &i);
        size_t[1] global_work2 = [BLOCK_SIZE * 2 * ((matrix_dim - i) / BLOCK_SIZE - 1)];
        size_t[1] local_work2  = [BLOCK_SIZE * 2];
        status = clEnqueueNDRangeKernel(runtime.queue, perimeter, 1, null, global_work2.ptr, local_work2.ptr, 0, null, null);
        assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));

        clSetKernelArg(internal, 0, cl_mem.sizeof, &d_m);
        clSetKernelArg(internal, 1, cl_int.sizeof, &matrix_dim);
        clSetKernelArg(internal, 2, cl_int.sizeof, &i);
        size_t[2] global_work3 = [BLOCK_SIZE * ((matrix_dim - i) / BLOCK_SIZE - 1), BLOCK_SIZE * ((matrix_dim - i) / BLOCK_SIZE - 1)];
        size_t[2] local_work3  = [BLOCK_SIZE, BLOCK_SIZE];
        status = clEnqueueNDRangeKernel(runtime.queue, internal, 2, null, global_work3.ptr, local_work3.ptr, 0, null, null);
        assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));
      }
      clSetKernelArg(diagonal, 0, cl_mem.sizeof, &d_m);
      clSetKernelArg(diagonal, 1, cl_int.sizeof, &matrix_dim);
      clSetKernelArg(diagonal, 2, cl_int.sizeof, &i);
      size_t[1] global_work1 = [BLOCK_SIZE];
      status = clEnqueueNDRangeKernel(runtime.queue, diagonal, 1, null, global_work1.ptr, null, 0, null, null);
      assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));

      status = clEnqueueReadBuffer(runtime.queue, d_m, 1, 0, m.length * cl_float.sizeof, m.ptr, 0, null, null);
      assert(status == CL_SUCCESS, "opencl_lud " ~ cl_strerror(status));
      clFinish(runtime.queue);
      clReleaseMemObject(d_m);
    }
    catch( Exception e )
    {
      writeln(e);
    }
  }

  void baseline_lud()
  {
    try
    {
      create_matrix();
      foreach (i; 1 .. matrix_dim)
      {
        m[i, 0] /= m[0, 0];
      }
      cl_int status;
      cl_mem d_m;
      d_m = clCreateBuffer(runtime.context, CL_MEM_READ_WRITE, m.length * cl_float.sizeof, null, &status);
      assert(status == CL_SUCCESS, "baseline_lud " ~ cl_strerror(status));
      status = clEnqueueWriteBuffer(runtime.queue, d_m, 1, 0, m.length * cl_float.sizeof, m.ptr, 0, null, null);
      assert(status == CL_SUCCESS, "baseline_lud " ~ cl_strerror(status));
      foreach (k; 1 .. matrix_dim)
      {
        size_t[1] global_work1 = [matrix_dim - k];
        clSetKernelArg(baseline_h, 0, cl_mem.sizeof, &d_m);
        clSetKernelArg(baseline_h, 1, cl_int.sizeof, &matrix_dim);
        clSetKernelArg(baseline_h, 2, cl_int.sizeof, &k);
        status = clEnqueueNDRangeKernel(runtime.queue, baseline_h, 1, null, global_work1.ptr, null, 0, null, null);
        assert(status == CL_SUCCESS, "baseline_lud " ~ cl_strerror(status));
        size_t[1] global_work2 = [matrix_dim - k - 1];
        if (global_work2[0] == 0) break;
        clSetKernelArg(baseline_v, 0, cl_mem.sizeof, &d_m);
        clSetKernelArg(baseline_v, 1, cl_int.sizeof, &matrix_dim);
        clSetKernelArg(baseline_v, 2, cl_int.sizeof, &k);
        status = clEnqueueNDRangeKernel(runtime.queue, baseline_v, 1, null, global_work2.ptr, null, 0, null, null);
        assert(status == CL_SUCCESS, "baseline_lud " ~ cl_strerror(status));
      }
      status = clEnqueueReadBuffer(runtime.queue, d_m, 1, 0, m.length * cl_float.sizeof, m.ptr, 0, null, null);
      assert(status == CL_SUCCESS, "baseline_lud " ~ cl_strerror(status));
      clFinish(runtime.queue);
      clReleaseMemObject(d_m);
    }
    catch( Exception e )
    {
      writeln(e);
    }
  }

  void sequential_lud()
  {
    auto ts = 0;
    create_matrix();
    for (auto k = 0; k < matrix_dim; k += BLOCK_SIZE)
    {
      foreach (i; 0 .. BLOCK_SIZE - 1)
      {
        foreach (tx; i + 1 .. BLOCK_SIZE)
        {
          foreach (j; 0 .. i)
          {
            m[k + tx, k + i] -= m[k + tx, k + j] * m[k + j, k + i];
            if (do_animate) history.add_event(ts, tx, I(k + tx, k + i), [I(k + tx, k + j), I(k + j, k + i)]);
          }
          m[k + tx, k + i] /= m[k + i, k + i];
          if (do_animate) history.add_event(ts++, tx, I(k + tx, k + i), [I(k + i, k + i), I(k + i, k + i)]);
        }
        foreach (tx; i + 1 .. BLOCK_SIZE)
        {
          foreach (j; 0 .. i + 1)
          {
            m[k + i + 1, k + tx] -= m[k + i + 1, k + j] * m[k + j, k + tx];
            if (do_animate) history.add_event(ts, tx, I(k + i + 1, k + tx), [I(k + i + 1, k + j), I(k + j, k + tx)]);
          }
          ++ts;
        }
      }
      if (k >= matrix_dim - BLOCK_SIZE) break;
      if (do_animate) history.add_event(ts++, 0, -1, []);

      foreach (bx; 0 .. ((matrix_dim - k) / BLOCK_SIZE - 1))
      {
        foreach (tx; 0 .. BLOCK_SIZE)
        {
          auto idx = tx;
          foreach (i; 1 .. BLOCK_SIZE)
          {
            foreach (j; 0 .. i)
            {
              m[k + i, k + (bx + 1) * BLOCK_SIZE + idx] -= m[k + i, k + j] * m[k + j, k + (bx + 1) * BLOCK_SIZE + idx];
              if (do_animate) history.add_event(ts, bx * 2 * BLOCK_SIZE + tx, I(k + i, k + (bx + 1) * BLOCK_SIZE + idx), [I(k + i, k + j), I(k + j, k + (bx + 1) * BLOCK_SIZE + idx)]);
            }
            ++ts;
          }
        }
        foreach (tx; BLOCK_SIZE .. 2 * BLOCK_SIZE)
        {
          auto idx = tx - BLOCK_SIZE;
          foreach (i; 0 .. BLOCK_SIZE)
          {
            foreach (j; 0 .. i)
            {
              m[k + (bx + 1) * BLOCK_SIZE + idx, k + i] -= m[k + (bx + 1) * BLOCK_SIZE + idx, k + j] * m[k + j, k + i];
              if (do_animate) history.add_event(ts, bx * 2 * BLOCK_SIZE + tx, I(k + (bx + 1) * BLOCK_SIZE + idx, k + i), [I(k + (bx + 1) * BLOCK_SIZE + idx, k + j), I(k + j, k + i)]);
            }
            m[k + (bx + 1) * BLOCK_SIZE + idx, k + i] /= m[k + i, k + i];
            if (do_animate) history.add_event(ts++, bx * 2 * BLOCK_SIZE + tx, I(k + (bx + 1) * BLOCK_SIZE + idx, k + i), [I(k + i, k + i)]);
          }
        }
      }
      if (do_animate) history.add_event(ts++, 0, -1, []);

      foreach (bx; 0 .. (matrix_dim - k) / BLOCK_SIZE - 1)
      {
        foreach (by; 0 .. (matrix_dim - k) / BLOCK_SIZE - 1)
        {
          auto global_row_id = k + (by + 1) * BLOCK_SIZE;
          auto global_col_id = k + (bx + 1) * BLOCK_SIZE;

          foreach (tx; 0 .. BLOCK_SIZE)
          {
            foreach (ty; 0 .. BLOCK_SIZE)
            {
              foreach (i; 0 .. BLOCK_SIZE)
              {
                m[global_row_id + ty, global_col_id + tx] -= m[global_row_id + ty, k + i] * m[k + i, global_col_id + tx];
                if (do_animate) history.add_event(ts, (bx * ((matrix_dim - k) / BLOCK_SIZE - 1) + by) * BLOCK_SIZE * BLOCK_SIZE + tx * BLOCK_SIZE + ty, I(global_row_id + ty, global_col_id + tx), [I(global_row_id + ty, k + i), I(k + i, global_col_id + tx)]);
              }
              ++ts;
            }
          }
        }
      }
      if (do_animate) history.add_event(ts++, 0, -1, []);
    }
  }

  /**
   */
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

  /**
   */
  void run()
  {
    StopWatch timer;
    TickDuration ticks;

    if (doolittle)
    {
      timer.start();
      doolittle_lud();
      timer.stop();
      ticks = timer.peek();
      writefln( "SERIAL %5.3f [s]", ticks.usecs / 1E6 );
      validate();
      if (do_animate) history.save_animation();
      run_once = false;
    }

    if (do_animate)
    {
      sequential_lud();
      validate();
      if (do_animate) history.save_animation();
      run_once = false;
    }

    timer.reset();
    timer.start();
    baseline_lud();
    timer.stop();
    ticks = timer.peek();
    writefln( "BASELN %5.3f [s]", ticks.usecs / 1E6 );
    validate();

    timer.reset();
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
