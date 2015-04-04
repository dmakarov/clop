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

module clop.examples.bp;

import std.conv;
import std.datetime;
import std.math : abs, exp;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;

/++
 + The program implements a simple neural network consisting of 3
 + levels: input, hidden, and output.  Every node of a level
 + connected to every node in the next level.  The first node of
 + each level is a normalization node and always set to 1.0
 + regardless of the input fed into the network. The edges,
 + connecting nodes, assigned weights.  The network finds the
 + weights that map given input to the expected output. Initially,
 + the weights are assigned randomly, then the output is computed
 + for a given input and the error is calculated comparing the
 + computed output with the expected output.  The error is used to
 + propagate adjustments to the weigths so the computed output
 + matches closer the expected output.
 +/
class Application {

  alias float CL_FP;
  static immutable int SEED = 1;
  static immutable int BLOCK_SIZE = 16;
  static immutable CL_FP ETA      = 0.3f;
  static immutable CL_FP MOMENTUM = 0.3f;
  static immutable CL_FP ONEF     = 1.0f;

  size_t input_n;               // number of input units
  size_t hidden_n;              // number of hidden units
  size_t output_n;              // number of output units
  size_t num_blocks;
  CL_FP[] input_units;          // the input units
  CL_FP[] hidden_units;         // the hidden units
  CL_FP[] hidden_deltas;        // hidden unit errors
  CL_FP[] output_units;         // the output units
  CL_FP[] output_deltas;        // output unit errors
  CL_FP[] partial_sum;
  CL_FP[] target;               // target vector
  /++
   + Weigths and adjustments are arranged in 2-dimensional arrays so
   + that the first dimension corresponds to the nodes the edges enter
   + and the second dimension corresponds to the nodes emanate from.
   + Thus i2h_weights[x, ...] are weights of edges that connect the
   + input level with the node x of the hidden level.
   +/
  NDArray!CL_FP i2h_weights;    // weights from input to hidden layer
  NDArray!CL_FP i2h_changes;
  NDArray!CL_FP h2o_weights;    // weights from hidden to output layer
  NDArray!CL_FP h2o_changes;

  // validation data
  CL_FP[] h_hidden_units;
  NDArray!CL_FP h_i2h_weights;
  NDArray!CL_FP h_i2h_changes;

  /++
   + Construct the application initializing all the data structures
   +/
  this(string[] args)
  {
    if (args.length != 2)
    {
      throw new Exception("ERROR: invalid args # " ~ to!(string)(args.length - 1));
    }
    input_n = to!(int)(args[1]);
    if (input_n % BLOCK_SIZE != 0)
    {
      auto r = to!(string)(input_n);
      auto b = to!(string)(BLOCK_SIZE);
      throw new Exception("ERROR: input number # (" ~ r ~ ") must be a multiple of " ~ b);
    }
    output_n       = 1;
    hidden_n       = BLOCK_SIZE;
    num_blocks     = input_n / BLOCK_SIZE;
    i2h_weights    = new NDArray!CL_FP(hidden_n + 1, input_n + 1);
    i2h_changes    = new NDArray!CL_FP(hidden_n + 1, input_n + 1);
    h2o_weights    = new NDArray!CL_FP(output_n + 1, hidden_n + 1);
    h2o_changes    = new NDArray!CL_FP(output_n + 1, hidden_n + 1);
    input_units    = new CL_FP[input_n + 1];
    hidden_units   = new CL_FP[hidden_n + 1];
    hidden_deltas  = new CL_FP[hidden_n + 1];
    output_units   = new CL_FP[output_n + 1];
    output_deltas  = new CL_FP[output_n + 1];
    partial_sum    = new CL_FP[input_n]; // num_blocks * BLOCK_SIZE
    target         = new CL_FP[output_n + 1];
    // validation data
    h_hidden_units = new CL_FP[hidden_n + 1];
    h_i2h_weights  = new NDArray!CL_FP(hidden_n + 1, input_n + 1);
    h_i2h_changes  = new NDArray!CL_FP(hidden_n + 1, input_n + 1);

    Mt19937 gen;
    gen.seed(SEED);
    foreach (i; 0 .. i2h_changes.length) i2h_changes[i] = 0;
    foreach (i; 0 .. h2o_changes.length) h2o_changes[i] = 0;
    foreach (i; 0 .. input_units.length) input_units[i] = uniform(0.0, 1.0, gen);
    foreach (i; 0 .. target.length     ) target[i]      = uniform(0.0, 1.0, gen);
    foreach (i; 0 .. i2h_weights.length) i2h_weights[i] = 0.00001f * uniform(0.0, 1.0, gen);
    foreach (i; 0 .. h2o_weights.length) h2o_weights[i] = 0.00001f * uniform(0.0, 1.0, gen);
  }

  /++
   + w[i, j] is the weight of edge connecting f[i] and t[j]
   +/
  void layerforward(CL_FP[] f, CL_FP[] t, NDArray!CL_FP w)
  {
    // Set up thresholding unit
    f[0] = ONEF;
    // For each unit in layer t compute weighted sum of its inputs f
    foreach (i; 1 .. t.length)
    {
      auto sum = w[i, 0];
      foreach (j; 1 .. f.length)
        sum += w[i, j] * f[j];
      t[i] = ONEF / (ONEF + exp(-sum));
    }
  }

  CL_FP output_error()
  {
    CL_FP errsum = 0.0;
    foreach (i; 1 .. output_units.length)
    {
      auto o = output_units[i];
      output_deltas[i] = -o * (ONEF - o) * (target[i] - o);
      errsum += abs(output_deltas[i]);
    }
    return errsum;
  }

  CL_FP hidden_error()
  {
    CL_FP errsum = 0.0;
    foreach (j; 1 .. hidden_units.length)
    {
      CL_FP sum = 0.0;
      foreach (i; 1 .. output_units.length)
        sum += output_deltas[i] * h2o_weights[i, j];
      hidden_deltas[j] = hidden_units[j] * (ONEF - hidden_units[j]) * sum;
      errsum += abs(hidden_deltas[j]);
    }
    return errsum;
  }

  void adjust_weights(CL_FP[] od, CL_FP[] iu, NDArray!CL_FP w, NDArray!CL_FP c)
  {
    iu[0] = ONEF;
    foreach (i; 1 .. od.length)
      foreach (j; 1 .. iu.length)
      {
        auto adjust = MOMENTUM * c[i, j] + (ONEF - MOMENTUM) * ETA * iu[j] * od[i];
        c[i, j]  = adjust;
        w[i, j] += adjust;
      }
  }

  bool clop_train_kernel(ref CL_FP eo, ref CL_FP eh)
  {
    import std.algorithm : sum;
    auto s = new CL_FP[input_n];
    foreach (j; 0 .. hidden_n)
    {
      mixin (compile(q{
            NDRange(i : 0 .. input_n)
            {
              s[i] = input_units[i] * i2h_weights[i, j];
            }
          }));
      hidden_units[j] = ONEF / (ONEF + exp(-sum(s)));
    }

    layerforward(hidden_units, output_units, h2o_weights);
    eo = output_error();
    eh = hidden_error();

    auto valid = validation_block_1(eo, eh);

    adjust_weights(output_deltas, hidden_units, h2o_weights, h2o_changes);

    mixin (compile(q{
          NDRange(i : 1 .. input_n, j : 1 .. hidden_n)
          {
            float adjust = MOMENTUM * i2h_changes[i, j] + (ONEF - MOMENTUM) * ETA * input_units[i] * hidden_deltas[j];
            i2h_weights[i, j] += adjust;
            i2h_changes[i, j]  = adjust;
          }
        }));

    valid = valid && validation_block_2();

    return valid;
  }

  bool opencl_train_kernel(ref CL_FP eo, ref CL_FP eh)
  {
    try
    {
      char[] code = q{
        #pragma OPENCL EXTENSION cl_khr_fp64 : enable
        #define BLOCK_SIZE 16
        #define ETA        0.3f
        #define MOMENTUM   0.3f
        #define ONEF       1.0f
        #define FP         float

        /**
         * The kernel computes the components that latter summed up to
         * get the value at the node j
         */
        __kernel void layerforward(__global FP* f,
                                   __global FP* w,
                                   __global FP* s,
                                            int j,
                                            int n)
        {
          int i = get_global_id(0);
          s[i] = f[i] * w[i * n + j];
        }

        /**
         * The kernel implements partial reduction
         * The global index space is hidden_n x input_n, i.e. a rectangle
         * with hidden_n height and input_n width.
         * The local index space is hidden_n x hidden_n, i.e. a square.
         * The local square are lined up in a row of input_n / hidden_n
         * squares in the global index space.
         * @param inputs holds hidden_n elements
         * @param summands holds hidden_n x hidden_n elements
         * @param n2 is equal to hidden_n
         */
        __kernel void layerforward_optimized(__global FP* f,
                                             __global FP* w,
                                             __global FP* sums,
                                             __local  FP* inputs,
                                             __local  FP* summands,
                                                      int n2)
        { // workgroups are organized so that get_group_id(0) is always 0.
          int row = get_local_id(0);
          int col = get_local_id(1);
          int gid = get_group_id(1);
          int len = get_num_groups(1) * n2 + 1;
          // hidden_n threads in the group load the hidden_n input values
          if (row == 0)
          {
            inputs[col] = f[gid * n2 + col + 1];
          }
          barrier(CLK_LOCAL_MEM_FENCE);
          // the index of an element at row, col in the first square
          int point = row * n2 + col;
          // the index of a weight element at row, col in group gid
          int index = (row + 1) * len + gid * n2 + col + 1;
          // compute products of every pair of weigths and inputs
          summands[point] = w[index] * inputs[col];
          barrier(CLK_LOCAL_MEM_FENCE);
          // compute sums of log_2(n2) products
          for (int i = 1; i < n2; i = i * 2)
          {
            if (col % (2 * i) == 0)
            {
              summands[point] += summands[row * n2 + col + i];
            }
            barrier(CLK_LOCAL_MEM_FENCE);
          }
          if (col == 0)
          {
            sums[gid * n2 + row] = summands[row * n2];
          }
        }

        __kernel void adjust_weights(__global FP* od,
                                     __global FP* iu,
                                     __global FP* w,
                                     __global FP* c)
        { // ii and jj have the same meaning as in host version of adjust_weights.
          int i = get_global_id(0) + 1;
          int j = get_global_id(1) + 1;
          int index = i * (1 + get_global_size(1)) + j;
          FP adjust = MOMENTUM * c[index] + (ONEF - MOMENTUM) * ETA * iu[j] * od[i];
          c[index]  = adjust;
          w[index] += adjust;
        }
      }.dup;

      cl_int status;
      char*[] strs = [code.ptr];
      size_t code_length = code.length;
      auto program = clCreateProgramWithSource(runtime.context, 1, strs.ptr, &code_length, &status);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clBuildProgram(program, 1, &runtime.device, "", null, null);
      if (status != CL_SUCCESS)
      {
        char[1024] log;
        clGetProgramBuildInfo(program, runtime.device, CL_PROGRAM_BUILD_LOG, 1023, log.ptr, null);
        writeln("CL_PROGRAM_BUILD_LOG:\n", log, "\nEOD");
      }
      assert(status == CL_SUCCESS, cl_strerror(status));
      cl_kernel[2] kernels;
      kernels[0] = clCreateKernel(program, "layerforward_optimized", &status);
      assert(status == CL_SUCCESS, cl_strerror(status));
      kernels[1] = clCreateKernel(program, "adjust_weights", &status);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clReleaseProgram(program);
      assert(status == CL_SUCCESS, cl_strerror(status));
      cl_mem d_input_units = clCreateBuffer(runtime.context, CL_MEM_READ_ONLY, input_units.length * CL_FP.sizeof, null, &status);
      assert(status == CL_SUCCESS, cl_strerror(status));
      cl_mem d_i2h_weights = clCreateBuffer(runtime.context, CL_MEM_READ_WRITE, i2h_weights.length * CL_FP.sizeof, null, &status);
      assert(status == CL_SUCCESS, cl_strerror(status));
      cl_mem d_partial_sum = clCreateBuffer(runtime.context, CL_MEM_WRITE_ONLY, partial_sum.length * CL_FP.sizeof, null, &status);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clEnqueueWriteBuffer(runtime.queue, d_input_units, CL_TRUE, 0, input_units.length * CL_FP.sizeof, input_units.ptr, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clEnqueueWriteBuffer(runtime.queue, d_i2h_weights, CL_TRUE, 0, i2h_weights.length * CL_FP.sizeof, i2h_weights.ptr, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));
      clSetKernelArg(kernels[0], 0, cl_mem.sizeof                     , &d_input_units);
      clSetKernelArg(kernels[0], 1, cl_mem.sizeof                     , &d_i2h_weights);
      clSetKernelArg(kernels[0], 2, cl_mem.sizeof                     , &d_partial_sum);
      clSetKernelArg(kernels[0], 3, CL_FP.sizeof * hidden_n           , null          );
      clSetKernelArg(kernels[0], 4, CL_FP.sizeof * hidden_n * hidden_n, null          );
      clSetKernelArg(kernels[0], 5, cl_int.sizeof                     , &hidden_n     );
      size_t[] global_work = [hidden_n, input_n];
      size_t[] local_work  = [hidden_n, hidden_n];
      status = clEnqueueNDRangeKernel(runtime.queue, kernels[0], 2, null, global_work.ptr, local_work.ptr, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clEnqueueReadBuffer(runtime.queue, d_partial_sum, CL_TRUE, 0, partial_sum.length * CL_FP.sizeof, partial_sum.ptr, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));
      foreach (j; 1 .. hidden_n + 1)
      {
        auto sum = i2h_weights[j, 0];
        foreach (k; 0 .. num_blocks)
        {
          sum += partial_sum[k * hidden_n + j - 1];
          debug (DEBUG) writefln("partial_sum[%2d] %.12f", k * hidden_n + j - 1 ,partial_sum[k * hidden_n + j - 1]);
        }
        hidden_units[j] = ONEF / (ONEF + exp(-sum));
      }
      clReleaseMemObject(d_partial_sum);
      layerforward(hidden_units, output_units, h2o_weights);
      eo = output_error();
      eh = hidden_error();

      auto valid = validation_block_1(eo, eh);

      adjust_weights(output_deltas, hidden_units, h2o_weights, h2o_changes);
      cl_mem d_hidden_deltas = clCreateBuffer(runtime.context, CL_MEM_READ_ONLY, hidden_deltas.length * CL_FP.sizeof, null, &status);
      assert(status == CL_SUCCESS, cl_strerror(status));
      cl_mem d_i2h_changes = clCreateBuffer(runtime.context, CL_MEM_READ_WRITE, i2h_changes.length * CL_FP.sizeof, null, &status);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clEnqueueWriteBuffer(runtime.queue, d_hidden_deltas, CL_TRUE, 0, hidden_deltas.length * CL_FP.sizeof, hidden_deltas.ptr, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clEnqueueWriteBuffer(runtime.queue, d_i2h_weights, CL_TRUE, 0, i2h_weights.length * CL_FP.sizeof, i2h_weights.ptr, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clEnqueueWriteBuffer(runtime.queue, d_i2h_changes, CL_TRUE, 0, i2h_changes.length * CL_FP.sizeof, i2h_changes.ptr, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));
      clSetKernelArg(kernels[1], 0, cl_mem.sizeof, &d_hidden_deltas);
      clSetKernelArg(kernels[1], 1, cl_mem.sizeof, &d_input_units);
      clSetKernelArg(kernels[1], 2, cl_mem.sizeof, &d_i2h_weights);
      clSetKernelArg(kernels[1], 3, cl_mem.sizeof, &d_i2h_changes);
      status = clEnqueueNDRangeKernel(runtime.queue, kernels[1], 2, null, global_work.ptr, null, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clEnqueueReadBuffer(runtime.queue, d_i2h_weights, CL_TRUE, 0, i2h_weights.length * CL_FP.sizeof, i2h_weights.ptr, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));
      status = clEnqueueReadBuffer(runtime.queue, d_i2h_changes, CL_TRUE, 0, i2h_changes.length * CL_FP.sizeof, i2h_changes.ptr, 0, null, null);
      assert(status == CL_SUCCESS, cl_strerror(status));

      valid = valid && validation_block_2();

      clReleaseMemObject(d_input_units);
      clReleaseMemObject(d_i2h_weights);
      clReleaseMemObject(d_hidden_deltas);
      clReleaseMemObject(d_i2h_changes);
      return valid;
    }
    catch (Exception e)
    {
      writeln(e);
      return false;
    }
  }

  /++
   + This version of layerforward performs additions in exactly the
   + same order as the kernel version does.  In principle the results
   + should be precisely equal.
   +/
  void layerforward_parallel(CL_FP[] f, CL_FP[] t, NDArray!CL_FP w)
  {
    f[0] = ONEF;

    auto sums = new CL_FP[input_n];
    auto inputs = new CL_FP[hidden_n];
    auto summands = new NDArray!CL_FP(hidden_n, hidden_n);

    foreach (gid; 0 .. num_blocks)
    {
      foreach (col; 0 .. hidden_n)
      {
        inputs[col] = f[gid * hidden_n + col + 1];
      }
      foreach (row; 0 .. hidden_n)
      {
        foreach (col; 0 .. hidden_n)
        {
          summands[row, col] = w[row + 1, gid * hidden_n + col + 1] * inputs[col];
          debug (DEBUG) writefln("summands[%2d, %2d] <- w[%2d, %2d] * f[%2d]", row, col, row + 1, gid * hidden_n + col + 1, gid * hidden_n + col + 1);
        }
      }
      for (auto i = 1; i < hidden_n; i = i * 2)
      {
        foreach (row; 0 .. hidden_n)
        {
          foreach (col; 0 .. hidden_n)
          {
            if (col % (2 * i) == 0)
            {
              summands[row, col] += summands[row, col + i];
              debug (DEBUG) writefln("summands[%2d, %2d] += summands[%2d, %2d]", row, col, row, col + i);
            }
          }
        }
      }
      foreach (row; 0 .. hidden_n)
      {
        sums[gid * hidden_n + row] = summands[row, 0];
        debug (DEBUG) writefln("sums[%2d] <- summands[%2d, %2d]", gid * hidden_n + row, row, 0);
      }
    }

    debug (DEBUG)
    foreach (i; 0 .. input_n)
    {
      if (sums[i] != partial_sum[i])
      {
        writefln("%2d sums %.12f != %.12f partial_sum", i, sums[i], partial_sum[i]);
      }
    }

    foreach (j; 1 .. hidden_n + 1)
    {
      auto sum = i2h_weights[j, 0];
      debug (DEBUG) writefln("sum <- i2h_weights[%2d, 0]", j);
      foreach (k; 0 .. num_blocks)
      {
        sum += sums[k * hidden_n + j - 1];
        debug (DEBUG) writefln("sum += sums[%2d]", k*hidden_n+j -1);
      }
      t[j] = ONEF / (ONEF + exp(-sum));
      debug (DEBUG) writefln("t[%2d] <- sum", j);
    }
  }

  bool validation_block_1(CL_FP eo, CL_FP eh)
  {
    bool valid = true;
    layerforward_parallel(input_units, h_hidden_units, i2h_weights);
    layerforward(h_hidden_units, output_units, h2o_weights);
    auto h_eo = output_error();
    auto h_eh = hidden_error();
    if (h_eo != eo || h_eh != eh)
    {
      writeln("validation block 1, host computed error do not match device computed erros.");
      writefln("%.12f %.12f\n%.12f %.12f", eo, eh, h_eo, h_eh);
      valid = false;
    }
    foreach (ii; 0 .. hidden_n + 1)
      if (h_hidden_units[ii] != hidden_units[ii])
      {
        writeln("validation block 1, host computed hidden units do not match device results.");
        writefln("%2s %.12f %.12f", ii, h_hidden_units[ii], hidden_units[ii]);
        valid = false;
        break;
      }
    foreach (i; 0 .. hidden_n + 1)
      foreach (j; 0 .. input_n + 1)
      {
        h_i2h_weights[i, j] = i2h_weights[i, j];
        h_i2h_changes[i, j] = 0.0f;
      }
    return valid;
  }

  bool validation_block_2()
  {
    bool valid = true;
    adjust_weights(hidden_deltas, input_units, h_i2h_weights, h_i2h_changes);
    foreach (i; 1 .. hidden_n + 1)
      foreach (j; 1 .. input_n + 1)
        if (abs(h_i2h_weights[i, j] - i2h_weights[i, j]) > 1E-10)
        {
          writeln("validation block 2, host computed weights do not match device results.");
          writefln("%6s->%2s %.12f %.12f (diff %g)",
                   j, i, h_i2h_weights[i, j], i2h_weights[i, j], h_i2h_weights[i, j] - i2h_weights[i, j]);
          valid = false;
          break;
        }
    return valid;
  }

  void run()
  {
    bool valid;
    CL_FP eo, eh;
    StopWatch timer;
    TickDuration ticks;

    timer.reset();
    timer.start();
    valid = opencl_train_kernel(eo, eh);
    timer.stop();
    ticks = timer.peek();
    writefln("OPENCL %5.3f [s]", ticks.usecs / 1E6);
    if (!valid)
      writefln("bp: out error %f, hidden error %f", eo, eh);

    valid = clop_train_kernel(eo, eh);
    if (!valid)
      writefln("bp: out error %f, hidden error %f", eo, eh);
  }
}

int
main(string[] args)
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
