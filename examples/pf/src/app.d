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
module clop.examples.nw;

import std.conv, std.datetime, std.getopt, std.random, std.stdio;
import derelict.opencl.cl;
import clop.compiler;

class Application {
  static immutable int BLOCK_SIZE = 128;
  NDArray!int data;
  NDArray!int move;
  int[] sums;
  int[] gold;
  int rows;
  int cols;
  int height;

  cl_kernel kernel_noblocks;
  cl_kernel kernel_ghost_zone;

  this(string[] args)
  {
    if (args.length != 4)
    {
      throw new Exception("ERROR: invalid arguments # " ~ to!string(args.length - 1));
    }
    rows   = clp2(to!(int)(args[1]) - 1) + 1;
    cols   = clp2(to!(int)(args[2]));
    height = to!(int)(args[3]);
    if (cols % BLOCK_SIZE != 0)
    {
      throw new Exception("ERROR: number of columns " ~ to!string(cols) ~
                          " must be multiple of " ~ to!string(BLOCK_SIZE));
    }
    if (cols % height != 0)
    {
      throw new Exception("ERROR: number of columns " ~ to!string(cols) ~
                          " must be divisible by height " ~ to!string(height));
    }
    data   = new NDArray!int(rows, cols); assert(data !is null, "Can't allocate memory for data");
    move   = new NDArray!int(rows, cols); assert(move !is null, "Can't allocate memory for move");
    sums   = new int[cols];        assert(null != sums, "Can't allocate memory for sums");
    gold   = new int[cols];        assert(null != gold, "Can't allocate memory for gold");
    Mt19937 e;
    e.seed(1);
    foreach (ref item; data)
      item = uniform(0, 10, e);

    /**
     *
     *
     */
    char[] code = q{
      #define MIN3(a, b, c) (((a) < (b)) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))
      #define MOV3(w, s, e) (((w) < (s)) ? ((w) < (e) ?  1  : -1) : ((s) < (e) ?  0  :  -1))

      __kernel void
      pf_noblocks(__global int* data, __global int* src, __global int* dst, __global int* move, int cols, int step)
      {
        const int tx = get_global_id(0);
        int s = src[tx];
        int w = (tx > 0      ) ? src[tx - 1] : INT_MAX;
        int e = (tx < cols - 1) ? src[tx + 1] : INT_MAX;
        dst[tx] = data[step * cols + tx] + MIN3(w, s, e);
        move[step * cols + tx] = MOV3(w, s, e);
      }

      /**
       * pf_ghost_zone process several rows in blocks computing the
       * cumulative weight of the shortest path going through every
       * element of the matrix.
       * @param start -- the starting row of this run
       * @param steps -- the number of rows processed in this run
       */
      __kernel void
      pf_ghost_zone( __global int* data  , //
                     __global int* src   , //
                     __global int* dst   , //
                     __global int* move  , //
                              int  rows  , //
                              int  cols  , //
                              int  height, //
                              int  start , //
                              int  steps , //
                     __local  int* prev  , //
                     __local  int* sums  ) //
      {
        const int BLOCK_SIZE = get_local_size(0) - 2 * (height - 1);
        const int tx = get_local_id(0);
        const int bx = get_group_id(0);
        int block  = bx * BLOCK_SIZE;
        /* Thread ids run from 0 to get_local_size(0).
           The first height - 1 ids are outside the left side of
           the block and the last height - 1 ids are outside the
           right side of the block.  Thus k == block when tx == height - 1. */
        int k = block + tx - (height - 1);
        int n = get_local_size(0) + 1; // the last index in prev and sums
        int m = block + BLOCK_SIZE + height - 1; // the last index in src
        int x = tx + 1; // elements in prev and sums shifted to the
                        // right by one relative to thread ids,
                        // because we account for the outtermost elements

        // we need to initialize prev from src
        if (tx == 0)
        { // first load two outter most elements, that no thread is mapped to
          if (0 < k   ) prev[0] = src[k - 1];
          if (m < cols) prev[n] = src[m];
        }
        // now each thread loads the element below it in src
        if (-1 < k && k < cols) prev[x] = src[k];
        barrier(CLK_LOCAL_MEM_FENCE);

        for (int r = start; r < start + steps; ++r)
        {
          if (-1 < k && k < cols)
          {
            int s = prev[x];
            int w = (k > 0       ) ? prev[x - 1] : INT_MAX;
            int e = (k < cols - 1) ? prev[x + 1] : INT_MAX;
            sums[x] = data[r * cols + k] + MIN3(w, s, e);
            // we must update move only within the block
            move[r * cols + k] = MOV3(w, s, e);
          }
          barrier(CLK_GLOBAL_MEM_FENCE);
          prev[x] = sums[x];
          barrier(CLK_LOCAL_MEM_FENCE);
        }
        // now save sums to dst
        if (height - 1 < x && x < BLOCK_SIZE + height)
          dst[k] = sums[x];
      }
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource(runtime.context, 1, strs.ptr, &size, &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    status = clBuildProgram(program, 1, &runtime.device, "", null, null);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel_noblocks = clCreateKernel(program, "pf_noblocks", &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel_ghost_zone = clCreateKernel(program, "pf_ghost_zone", &status);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    status = clReleaseProgram(program);
    assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
  }

  /**
   *  Round up to the next power of 2.
   */
  uint clp2(uint x)
  {
    x = x - 1;
    x = x | (x >>  1);
    x = x | (x >>  2);
    x = x | (x >>  4);
    x = x | (x >>  8);
    x = x | (x >> 16);
    return x + 1;
  }

  /**
   */
  void validate()
  {
    int diff = 0;
    foreach (ii; 0 .. cols)
      if (sums[ii] != gold[ii])
        ++diff;
    if (diff > 0)
      writeln("DIFFs ", diff);
  }

  /**
   */
  void reset()
  {
    sums[] = 0;
  }

  void dump()
  {
    auto of = File("pf.out", "w");
    foreach (r; 0 .. rows)
    {
      foreach (c; 0 .. cols)
        of.write(data[r * cols + c]);
      of.writeln();
    }
    of.write("  ", gold[0]);
    foreach (c; 1 .. cols)
    {
      of.writef(" %3d", gold[c]);
      if (c % 16 == 15 && c + 1 != cols)
        of.write("\n ");
    }
    of.writeln();
  }

  bool IN_RANGE(int x, int min, int max)
  {
    return (min <= x && x <= max);
  }

  int max(int a, int b)
  {
    return a < b ? b : a;
  }

  int min(int a, int b)
  {
    return a < b ? a : b;
  }

  int min3(int a, int b, int c)
  {
    auto k = a < b ? a : b;
    return k < c ? k : c;
  }

  int mov3(int w, int s, int e)
  {
    return (w < s) ? (w < e ? 1 : -1) : (s < e ? 0 : -1);
  }

  void baseline()
  {
    int[][2] results;
    results[0] = new int[cols];
    results[1] = new int[cols];
    foreach (c; 0 .. cols)
      results[0][c] = data[c];
    int src = 0;
    foreach (r; 1 .. rows)
    {
      auto dst = 1 - src;
      foreach (c; 0 .. cols)
      {
        auto s = results[src][c];
        auto w = (c > 0      ) ? results[src][c - 1] : int.max;
        auto e = (c < cols - 1) ? results[src][c + 1] : int.max;
        results[dst][c] = data[r * cols + c] + min3(w, s, e);
        move[r * cols + c] = mov3(w, s, e);
      }
      src = dst;
    }
    gold[] = results[src][];
  }

  void ghost_zone_blocks()
  {
    int[][2] results;
    results[0] = new int[BLOCK_SIZE + 2 * height];
    results[1] = new int[BLOCK_SIZE + 2 * height];
    int[] temp = new int[cols];
    foreach (c; 0 .. cols)
      sums[c] = data[c];
    for (int r = 1; r < rows; r += height)
    {
      for (int block = 0; block < cols; block += BLOCK_SIZE)
      {
        foreach (k; -height .. BLOCK_SIZE + height)
          if (-1 < block + k && block + k < cols)
            results[0][height + k] = sums[block + k];
        int src = 0;
        int border = height - 1;
        foreach (step; r .. min(r + height, rows))
        {
          auto dst = 1 - src;
          foreach (k; -border .. BLOCK_SIZE + border)
            if (-1 < block + k && block + k < cols)
            {
              auto c = block + k;
              auto s = results[src][height + k];
              auto w = (c > 0      ) ? results[src][height + k - 1] : int.max;
              auto e = (c < cols - 1) ? results[src][height + k + 1] : int.max;
              results[dst][height + k] = data[step * cols + c] + min3(w, s, e);
              move[step * cols + c] = mov3(w, s, e);
            }
          border -= 1;
          src = dst;
        }
        foreach (k; 0 .. BLOCK_SIZE)
          temp[block + k] = results[src][height + k];
      }
      sums[] = temp[];
    }
  }

  double opencl_noblocks(bool cleanup)
  {
    double result = 0.0;
    cl_event event;

    foreach (c; 0 .. cols)
    {
      sums[c] = data[c];
    }
    cl_int status;
    cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
    cl_mem ddata = clCreateBuffer(runtime.context, flags, cl_int.sizeof * data.length, data.ptr, &status);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    flags = CL_MEM_WRITE_ONLY;
    cl_mem dmove = clCreateBuffer(runtime.context, flags, cl_int.sizeof * move.length, null, &status);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    cl_mem[2] dsums;
    flags = CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR;
    dsums[0] = clCreateBuffer(runtime.context, flags, cl_int.sizeof * cols, sums.ptr, &status);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    flags = CL_MEM_READ_WRITE;
    dsums[1] = clCreateBuffer(runtime.context, flags, cl_int.sizeof * cols, null, &status);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));

    status = clSetKernelArg(kernel_noblocks,  0, cl_mem.sizeof, &ddata);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    status = clSetKernelArg(kernel_noblocks,  3, cl_mem.sizeof, &dmove);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    status = clSetKernelArg(kernel_noblocks,  4, cl_int.sizeof, &cols);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));

    int src = 0;
    size_t gwsize = cols;

    StopWatch timer;
    TickDuration ticks;
    double ittime = 0.0;
    int itcount = 0;

    foreach (step; 1 .. rows)
    {
    timer.reset();
    timer.start();
      int dst = 1 - src;

      status = clSetKernelArg(kernel_noblocks,  1, cl_mem.sizeof, &dsums[src]);
      assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks,  2, cl_mem.sizeof, &dsums[dst]);
      assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks,  5, cl_int.sizeof, &step);
      assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));

      status = clEnqueueNDRangeKernel(runtime.queue               ,
                                      kernel_noblocks             ,
                                      1                           ,
                                      null                        ,
                                      &gwsize                     ,
                                      null                        ,
                                      0                           ,
                                      null                        ,
                                      &event                     );
      assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
      status = clWaitForEvents(1, &event);
      assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));

      cl_ulong start_time;
      status = clGetEventProfilingInfo (event                     , // cl_event          event
                                        CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                        cl_ulong.sizeof           , // size_t            param_value_size
                                        &start_time               , // void*             param_value
                                        null                     ); // size_t*           param_value_size_ret
      assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
      cl_ulong end_time;
      status = clGetEventProfilingInfo (event                     , // cl_event          event
                                        CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                        cl_ulong.sizeof           , // size_t            param_value_size
                                        &end_time                 , // void*             param_value
                                        null                     ); // size_t*           param_value_size_ret
      assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
      result += (end_time - start_time) / 1E9;

      src = dst;
    timer.stop();
    ticks = timer.peek();
    ittime += ticks.usecs / 1E6;
    itcount += 1;
    }
    // writefln("%d iterations in %f s, average iteration time %f", itcount, ittime, ittime / itcount);

    status = clEnqueueReadBuffer(runtime.queue              ,
                                 dsums[src]                 ,
                                 CL_TRUE                    ,
                                 0                          ,
                                 cl_int.sizeof * cols       ,
                                 sums.ptr                   ,
                                 0                          ,
                                 null                       ,
                                 null                      );
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    status = clEnqueueReadBuffer(runtime.queue              ,
                                 dmove                      ,
                                 CL_TRUE                    ,
                                 0                          ,
                                 cl_int.sizeof * rows * cols,
                                 move.ptr                   ,
                                 0                          ,
                                 null                       ,
                                 null                      );
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));

    status = clReleaseMemObject(dsums[1]);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    status = clReleaseMemObject(dsums[0]);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    status = clReleaseMemObject(dmove);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    status = clReleaseMemObject(ddata);
    assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    if (cleanup)
    {
      status = clReleaseKernel(kernel_noblocks);
      assert(status == CL_SUCCESS, "opencl_noblocks " ~ cl_strerror(status));
    }
    return result;
  }

  double opencl_ghost_zone(bool cleanup)
  {
    double result = 0.0;
    cl_event event;

    foreach (c; 0 .. cols)
      sums[c] = data[c];
    cl_int status;
    cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
    cl_mem ddata = clCreateBuffer(runtime.context, flags, cl_int.sizeof * data.length, data.ptr, &status);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
    flags = CL_MEM_WRITE_ONLY;
    cl_mem dmove = clCreateBuffer(runtime.context, flags, cl_int.sizeof * move.length, null, &status);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
    cl_mem[2] dsums;
    flags = CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR;
    dsums[0]  = clCreateBuffer(runtime.context, flags, cl_int.sizeof * cols, sums.ptr, &status);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
    flags = CL_MEM_READ_WRITE;
    dsums[1]  = clCreateBuffer(runtime.context, flags, cl_int.sizeof * cols, null, &status);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));

    size_t blocks = cols / BLOCK_SIZE;
    size_t lwsize = BLOCK_SIZE + 2 * (height - 1);
    size_t gwsize = blocks * lwsize;
    int src = 0;

    for (int r = 1; r < rows; r += height)
    {
      int steps = min(height, rows - r);
      int dst = 1 - src;

      status = clSetKernelArg(kernel_ghost_zone,  0, cl_mem.sizeof, &ddata);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone,  1, cl_mem.sizeof, &dsums[src]);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone,  2, cl_mem.sizeof, &dsums[dst]);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone,  3, cl_mem.sizeof, &dmove);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone,  4, cl_int.sizeof, &rows);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone,  5, cl_int.sizeof, &cols);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone,  6, cl_int.sizeof, &height);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone,  7, cl_int.sizeof, &r);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone,  8, cl_int.sizeof, &steps);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone,  9, cl_int.sizeof * (lwsize + 2), null);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clSetKernelArg(kernel_ghost_zone, 10, cl_int.sizeof * (lwsize + 2), null);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));

      status = clEnqueueNDRangeKernel(runtime.queue,
                                       kernel_ghost_zone,
                                       1,
                                       null,
                                       &gwsize,
                                       &lwsize,
                                       0,
                                       null,
                                       &event);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
      status = clWaitForEvents(1, &event);
      assert(status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror(status));

      cl_ulong start_time;
      status = clGetEventProfilingInfo (event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &start_time               , // void*             param_value
                                         null                    ); // size_t*           param_value_size_ret
      assert(status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror(status));
      cl_ulong end_time;
      status = clGetEventProfilingInfo (event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &end_time                 , // void*             param_value
                                         null                    ); // size_t*           param_value_size_ret
      assert(status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror(status));
      result += (end_time - start_time) / 1E9;

      src = dst;
    }

    status = clEnqueueReadBuffer(runtime.queue,
                                 dsums[src],
                                 CL_TRUE,
                                 0,
                                 cl_int.sizeof * cols,
                                 sums.ptr,
                                 0,
                                 null,
                                 null);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
    status = clEnqueueReadBuffer(runtime.queue,
                                 dmove,
                                 CL_TRUE,
                                 0,
                                 cl_int.sizeof * rows * cols,
                                 move.ptr,
                                 0,
                                 null,
                                 null);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));

    status = clReleaseMemObject(dsums[1]);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
    status = clReleaseMemObject(dsums[0]);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
    status = clReleaseMemObject(dmove);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
    status = clReleaseMemObject(ddata);
    assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
    if (cleanup)
    {
      status = clReleaseKernel(kernel_ghost_zone);
      assert(status == CL_SUCCESS, "opencl_ghost_zone " ~ cl_strerror(status));
    }
    return result;
  }

  void clop_mock()
  {
    NDArray!int dsums = new NDArray!int(rows, cols);
    foreach (c; 0 .. cols)
    {
      dsums[c] = data[c];
    }

    try
    {
    import std.datetime;
    StopWatch timer;
    timer.start();

    static clop.rt.instance.Instance instance_app_579_2;
    if (instance_app_579_2 is null)
    {
      instance_app_579_2 = new clop.rt.instance.Instance("mock_579_2");
      runtime.register_instance(instance_app_579_2);
    }
    if (!instance_app_579_2.ready)
    {
      // make list of kernel parameters
      auto kernel_params = "", macros = "";
      kernel_params ~= "" ~ "int " ~ "rows";
      kernel_params ~= ", " ~ "int " ~ "cols";
      kernel_params ~= ", __global " ~ typeid(*dsums.ptr).toString() ~ "* " ~ "dsums";
      kernel_params ~= ", __global " ~ typeid(*data.ptr).toString() ~ "* " ~ "data";
      kernel_params ~= ", __global " ~ typeid(*move.ptr).toString() ~ "* " ~ "move";
      kernel_params ~= ", __constant " ~ "ulong* " ~ "clop_ndarray_dimensions_dsums";
      kernel_params ~= ", __constant " ~ "ulong* " ~ "clop_ndarray_dimensions_data";
      kernel_params ~= ", __constant " ~ "ulong* " ~ "clop_ndarray_dimensions_move";
      kernel_params ~= ", " ~ "int " ~ "height";
      kernel_params ~= ", " ~ "int " ~ "r";
      kernel_params ~= ", __local " ~ "int* dsums_local";
      instance_app_579_2.prepare_resources("clop_kernel_main_app_579_2_Horizontal_rectangular_tiling_height_BLOCK_SIZE", macros ~ q{ int min3 ( int a , int b , int c ) { return a < b ? ( c < a ? c : a ) : ( c < b ? c : b ) ; } int mov3 ( int w , int s , int e ) { return w < s ? ( e < w ? -1 : 1 ) : ( e < s ? -1 : 0 ) ; }} ~ "__kernel void clop_kernel_main_app_579_2_Horizontal_rectangular_tiling_height_BLOCK_SIZE(" ~ kernel_params ~ ")" ~ q{
            {
              int tx = get_local_id(0);
              int start = r;
              int stride = get_local_size(0) - 2 * height + 2;
              int stride_local = get_local_size(0) + 2;
              int c = get_group_id(0) * (stride_local - 2 * height) - height + tx + 1;
              if (-1 < c && c < (cols))
              {
                dsums_local[tx + 1] = dsums[(r - 1) * clop_ndarray_dimensions_dsums[1] + c];
              }
              else
              {
                dsums_local[tx + 1] = INT_MAX;
              }
              if (tx == 0)
              {
                dsums_local[0] = (c > 0) ? dsums[(r - 1) * clop_ndarray_dimensions_dsums[1] + c - 1] : INT_MAX;
              }
              if (tx == get_local_size(0) - 1)
              {
                dsums_local[tx + 2] = (c < cols - 1) ? dsums[(r - 1) * clop_ndarray_dimensions_dsums[1] + c + 1] : INT_MAX;
              }
              int x1;
              for (int r = 0; r < height; ++r)
              {
                int x = r * stride_local + tx + 1;
                x1 = x + stride_local;
                if (-1 < c && c < (cols))
                {
                  int w = dsums_local[x - 1];
                  int s = dsums_local[x];
                  int e = dsums_local[x + 1];
                  int clop_temp_2 = data[(r + start) * clop_ndarray_dimensions_data[1] + c];
                  float clop_temp_3 = min3(w, s, e);
                  dsums_local[x1] = clop_temp_2 + clop_temp_3;
                  move[(r + start) * clop_ndarray_dimensions_move[1] + c] = mov3(w, s, e);
                }
                else dsums_local[x1] = INT_MAX;
                barrier(CLK_LOCAL_MEM_FENCE);
              }
              if (height - 2 < tx && tx < stride_local - height - 1)
              {
                dsums[(start + height - 1) * clop_ndarray_dimensions_dsums[1] + c] = dsums_local[x1];
              }
            }
          });
    }
    // setting up buffers and kernel arguments
    runtime.status = clSetKernelArg(instance_app_579_2.kernel, 0, cl_int.sizeof, &rows);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: 0"));
    runtime.status = clSetKernelArg(instance_app_579_2.kernel, 1, cl_int.sizeof, &cols);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: 1"));
    dsums.create_buffer(runtime.context);
    dsums.push_buffer(runtime.queue);
    runtime.status = clSetKernelArg(instance_app_579_2.kernel, 2, cl_mem.sizeof, dsums.get_buffer);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: 2"));
    data.create_buffer(runtime.context);
    data.push_buffer(runtime.queue);
    runtime.status = clSetKernelArg(instance_app_579_2.kernel, 3, cl_mem.sizeof, data.get_buffer);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: 3"));
    move.create_buffer(runtime.context);
    move.push_buffer(runtime.queue);
    runtime.status = clSetKernelArg(instance_app_579_2.kernel, 4, cl_mem.sizeof, move.get_buffer);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: 4"));
    cl_ulong[] clop_ndarray_dimensions_dsums = dsums.get_dimensions();
    cl_mem clop_ndarray_dimensions_dsums_buffer = clCreateBuffer(runtime.context, CL_MEM_READ_ONLY, cl_ulong.sizeof * clop_ndarray_dimensions_dsums.length, null, &runtime.status);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clCreateBuffer"));
    runtime.status = clEnqueueWriteBuffer(runtime.queue, clop_ndarray_dimensions_dsums_buffer, CL_TRUE, 0, cl_ulong.sizeof * clop_ndarray_dimensions_dsums.length, clop_ndarray_dimensions_dsums.ptr, 0, null, null);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueWriteBuffer"));
    runtime.status = clSetKernelArg(instance_app_579_2.kernel, 5, cl_mem.sizeof, &clop_ndarray_dimensions_dsums_buffer);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: 5"));
    cl_ulong[] clop_ndarray_dimensions_data = data.get_dimensions();
    cl_mem clop_ndarray_dimensions_data_buffer = clCreateBuffer(runtime.context, CL_MEM_READ_ONLY, cl_ulong.sizeof * clop_ndarray_dimensions_data.length, null, &runtime.status);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clCreateBuffer"));
    runtime.status = clEnqueueWriteBuffer(runtime.queue, clop_ndarray_dimensions_data_buffer, CL_TRUE, 0, cl_ulong.sizeof * clop_ndarray_dimensions_data.length, clop_ndarray_dimensions_data.ptr, 0, null, null);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueWriteBuffer"));
    runtime.status = clSetKernelArg(instance_app_579_2.kernel, 6, cl_mem.sizeof, &clop_ndarray_dimensions_data_buffer);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: 6"));
    cl_ulong[] clop_ndarray_dimensions_move = move.get_dimensions();
    cl_mem clop_ndarray_dimensions_move_buffer = clCreateBuffer(runtime.context, CL_MEM_READ_ONLY, cl_ulong.sizeof * clop_ndarray_dimensions_move.length, null, &runtime.status);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clCreateBuffer"));
    runtime.status = clEnqueueWriteBuffer(runtime.queue, clop_ndarray_dimensions_move_buffer, CL_TRUE, 0, cl_ulong.sizeof * clop_ndarray_dimensions_move.length, clop_ndarray_dimensions_move.ptr, 0, null, null);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueWriteBuffer"));
    runtime.status = clSetKernelArg(instance_app_579_2.kernel, 7, cl_mem.sizeof, &clop_ndarray_dimensions_move_buffer);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: 7"));
    runtime.status = clSetKernelArg(instance_app_579_2.kernel, 10, cl_int.sizeof * ((BLOCK_SIZE + 2 * height) * (height + 1)), null);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg: 10"));
    for (uint i = 1; i < rows; i += height)
    {
      size_t global_work_size = cols + (cols / BLOCK_SIZE) * (2 * height - 2);
      size_t local_work_size  = BLOCK_SIZE + 2 * height - 2;
      // set additional kernel arguments if needed.
      clSetKernelArg(instance_app_579_2.kernel, 8, cl_int.sizeof, &height);
      clSetKernelArg(instance_app_579_2.kernel, 9, cl_int.sizeof, &i);
      runtime.status = clEnqueueNDRangeKernel(runtime.queue, instance_app_579_2.kernel, 1, null, &global_work_size, &local_work_size, 0, null, null);
      assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueNDRangeKernel"));
    }
    dsums.pull_buffer(runtime.queue);
    move.pull_buffer(runtime.queue);
    runtime.status = clReleaseMemObject(clop_ndarray_dimensions_dsums_buffer);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clReleaseMemObject"));
    runtime.status = clReleaseMemObject(clop_ndarray_dimensions_data_buffer);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clReleaseMemObject"));
    runtime.status = clReleaseMemObject(clop_ndarray_dimensions_move_buffer);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clReleaseMemObject"));
    timer.stop();
    TickDuration ticks = timer.peek();
    writefln("CLOP app_733_2_Horizontal_rectangular_tiling_height_BLOCK_SIZE %5.3f [s]", ticks.usecs / 1E6);
    }
    catch (Exception e)
    {
      writeln(e);
    }
    foreach (c; 0 .. cols)
    {
      sums[c] = dsums[rows - 1, c];
    }
  }

  /**
   *  CLOP implementation of Path Finder.
   *  Instead of switching between two arrays for current and previous
   *  rows, we use one large array of the same size as the data array.
   *  We fill in the array row after row, using the CLOP compiler to
   *  generate the host side loop with synchronization between each
   *  row computation.
   */
  void clop_dsl()
  {
    NDArray!int dsums = new NDArray!int(rows, cols);
    foreach (c; 0 .. cols)
    {
      dsums[c] = data[c];
    }
    mixin(compile(q{
      int min3(int a, int b, int c)
      {
        return a < b ? (c < a ? c : a) : (c < b ? c : b);
      }
      int mov3(int w, int s, int e)
      {
        return w < s ? (e < w ? -1 : 1) : (e < s ? -1 : 0);
      }
      Horizontal NDRange(r : 1 .. rows, c : 0 .. cols) {
        int s = dsums[r - 1, c];
        int w = (c > 0       ) ? dsums[r - 1, c - 1] : INT_MAX;
        int e = (c < cols - 1) ? dsums[r - 1, c + 1] : INT_MAX;
        dsums[r, c] = data[r, c] + min3(w, s, e);
        move[r, c] = mov3(w, s, e);
      } apply(rectangular_tiling(height, BLOCK_SIZE))
    }));
    foreach (c; 0 .. cols)
    {
      sums[c] = dsums[rows - 1, c];
    }
  }

  void run()
  {
    size_t size;
    StopWatch timer;
    TickDuration ticks;
    double benchmark = runtime.benchmark(rows * cols);
    double rate;

    timer.start();
    baseline();
    timer.stop();
    ticks = timer.peek();
    writefln("%2d MI SEQUENTIAL %5.3f [s],         %7.2f MI/s",
             rows * cols / (1024 * 1024),
             ticks.usecs / 1E6,
             rows * cols * 1E6 / (1024 * 1024 * ticks.usecs));

    timer.reset();
    timer.start();
    ghost_zone_blocks();
    timer.stop();
    ticks = timer.peek();
    writefln("%2d MI GHOST ZONE %5.3f [s],         %7.2f MI/s",
             rows * cols / (1024 * 1024),
             ticks.usecs / 1E6,
             rows * cols * 1E6 / (1024 * 1024 * ticks.usecs));
    validate();

    reset();
    rate = opencl_noblocks(false);
    reset();
    timer.reset();
    timer.start();
    rate = opencl_noblocks(true);
    timer.stop();
    ticks = timer.peek();
    writefln("%2d MI CL NOBLOCK %5.3f (%5.3f) [s], %7.2f MI/s, estimated %7.2f MI/s",
             rows * cols / (1024 * 1024),
             ticks.usecs / 1E6, rate,
             (rows - 1) * cols / (1024 * 1024 * rate),
             2 * benchmark / 6);
    validate();

    clGetKernelWorkGroupInfo(kernel_ghost_zone,
                             runtime.device,
                             CL_KERNEL_WORK_GROUP_SIZE,
                             size.sizeof,
                             &size,
                             null);
    if (size >= BLOCK_SIZE + 2 * (height - 1))
    {
      reset();
      rate = opencl_ghost_zone(false);
      reset();
      timer.reset();
      timer.start();
      rate = opencl_ghost_zone(true);
      timer.stop();
      ticks = timer.peek();
      writefln("%2d MI CL GHOST Z %5.3f (%5.3f) [s], %7.2f MI/s",
               rows * cols / (1024 * 1024),
               ticks.usecs / 1E6, rate,
               (rows - 1) * cols / (1024 * 1024 * rate));
      validate();
    }

    reset();
    timer.reset();
    timer.start();
    clop_dsl();
    timer.stop();
    ticks = timer.peek();
    writefln("%2d MI CLOP %5.3f [s], %7.2f MI/s",
             rows * cols / (1024 * 1024),
             ticks.usecs / 1E6,
             (rows - 1) * cols / to!double(ticks.usecs));
    validate();

    reset();
    clop_mock();
    validate();
  }
}

int main(string[] args)
{
  uint platform = uint.max, device = uint.max;
  auto gor = getopt(args, std.getopt.config.passThrough, "device|d", &device, "platform|p", &platform);
  if (gor.helpWanted)
  {
    writefln("Usage: %s [-a -b <block size> -d device -m -p platform -v] <rows> <columns> <height>", args[0]);
    return 0;
  }
  if (platform == uint.max && device != uint.max)
  {
    platform = 0;
  }
  bool selected_device = platform != uint.max && device != uint.max;
  uint[] platforms = runtime.get_platforms();
  for (uint p = 0; p < platforms.length; ++p)
    foreach (d; 0 .. platforms[p])
    {
      if (selected_device && (p != platform || d != device))
      {
        continue;
      }
      try
      {
        runtime.init(p, d);
        writeln("--------------------------------------------------");
        auto app = new Application(args);
        app.run();
      }
      catch (Exception msg)
      {
        writeln("PF: ", msg);
        return -1;
      }
      finally
      {
        runtime.shutdown();
      }
      writeln("==================================================");
    }
  return 0;
}

// Local Variables:
// compile-command: "dub run --build=verbose :pf -- 64 128 4"
// End:
