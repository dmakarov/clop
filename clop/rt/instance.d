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

import std.exception;
import std.stdio;
import derelict.opencl.cl;
import clop.rt.ctx;

/++
 +
 +/
final class Instance
{
  bool ready;
  string name;
  string kname;
  cl_int status;
  cl_mem[] buffers;
  cl_kernel kernel;
  cl_program program;

  this(string n)
  {
    name = n;
  }

  @property override
  string toString()
  {
    return name;
  }

  bool is_ready(string kernel_name)
  {
    return false;
  }

  void prepare_resources(string kernel_name, string program_source)
  {
    kname = kernel_name ~ "\0"; // clCreateKernel expects 0 terminated strings
    debug (VERBOSE) writeln("OpenCL program:\n", program_source, "EOF");
    char[] program_source_array = program_source.dup;
    size_t program_source_size = program_source_array.length;
    char* program_source_ptr = program_source_array.ptr;
    /++
     + runtime.context      - a valid OpenCL context
     + 1                    - number of elements in &program_source_ptr array
     + &program_source_ptr  - array of pointers to strings that make up the program source code
     + &program_source_size - array with the number of chars in each string (the string length)
     + &status              - returns an appropriate error code
     +/
    program = clCreateProgramWithSource(runtime.context, 1, &program_source_ptr, &program_source_size, &status);
    assert(status == CL_SUCCESS, cl_strerror(status, "clCreateProgramWithSource"));
    status = clBuildProgram(program, 1, &runtime.device, "", null, null);
    if (status != CL_SUCCESS)
    {
      immutable size_t size = 3072;
      size_t actual_size;
      char[size] buffer;
      auto status = clGetProgramBuildInfo(program, runtime.device, CL_PROGRAM_BUILD_LOG, size, buffer.ptr, &actual_size);
      if (status == CL_SUCCESS)
      {
        char[] log = buffer[0 .. actual_size];
        writeln("CL_PROGRAM_BUILD_LOG:\n", log, "\nEOF");
      }
      else
      {
        writeln("Program did not build and clGetProgramBuildInfo returned", cl_strerror(status));
      }
    }
    enforce(status == CL_SUCCESS, cl_strerror(status, "clBuildProgram"));
    kernel = clCreateKernel(program, cast(char*)kname, &status);
    assert(status == CL_SUCCESS, cl_strerror(status, "clCreateKernel"));
    ready = true;
  }

  void release_resources()
  {
    if (ready)
    {
      ready = false;
      foreach (buffer; buffers)
      {
        status = clReleaseMemObject(buffer);
        assert(status == CL_SUCCESS, cl_strerror(status, "clReleaseMemObject"));
      }
      status = clReleaseKernel(kernel);
      assert(status == CL_SUCCESS, cl_strerror(status, "clReleaseKernel"));
      status = clReleaseProgram(program);
      assert(status == CL_SUCCESS, cl_strerror(status, "clReleaseProgram"));
      debug (VERBOSE) writeln("CLOP Instance " ~ name ~ " released all resources.");
    }
  }

  size_t get_work_group_size(size_t global_work_size)
  {
    size_t param_value = 1;
    auto status = clGetKernelWorkGroupInfo(kernel, null, CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE, size_t.sizeof, &param_value, null);
    assert(status == CL_SUCCESS, cl_strerror(status, "clGetKernelWorkGroupInfo"));
    if (param_value >= global_work_size)
      return global_work_size;
    param_value = global_work_size / param_value;
    while (param_value < global_work_size && global_work_size % param_value != 0)
      ++param_value;
    return global_work_size / param_value;
  }
}

unittest
{
  auto instance = new Instance("the_instance");
  assert(instance !is null);
  assert(instance.toString() == "the_instance");
  assert(!instance.is_ready("the_kernel"));
}

// Local Variables:
// coding: utf-8
// End:
