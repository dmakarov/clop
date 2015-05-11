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
module clop.rt.ctx;

public import std.algorithm;
import std.exception;
import std.stdio;
import std.string;

import derelict.opencl.cl;

public import clop.rt.ndarray;
import clop.rt.instance;

struct Runtime
{
  static bool has_platform_ids = false;
  cl_platform_id[] platform_ids;
  cl_int status;
  cl_device_id device;
  cl_context context;
  cl_command_queue queue;
  cl_uint num_platforms;
  cl_uint num_devices;

  cl_kernel kernel_copy;
  cl_kernel kernel_madd;
  size_t[3] work_item_sizes;

  Instance[] clops;

  /++
   +/
  void init(size_t platform_id = 0, size_t device_id = 0, bool verbose = true)
  {
    if (!has_platform_ids)
    {
      DerelictCL.load();
      status = clGetPlatformIDs(0, null, &num_platforms);
      assert(status == CL_SUCCESS && num_platforms > 0 && platform_id < num_platforms, "No OpenCL platform found: " ~ cl_strerror(status));
      platform_ids = new cl_platform_id[num_platforms];
      assert(platform_ids != null, "Can't allocate array of OpenCL platform IDs.");
      status = clGetPlatformIDs(num_platforms, platform_ids.ptr, null);
      assert(status == CL_SUCCESS, "Can't get OpenCL platform IDs: " ~ cl_strerror(status));
    }
    auto platform = platform_ids[platform_id];
    status = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 0, null, &num_devices);
    assert(status == CL_SUCCESS && num_devices > 0 && device_id < num_devices, "No OpenCL device found:" ~ cl_strerror(status));
    auto devices = new cl_device_id[num_devices];
    assert(devices != null, "Can't allocate array of OpenCL device IDs.");
    status = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, num_devices, devices.ptr, null);
    assert(status == CL_SUCCESS, "Can't get OpenCL device IDs: " ~ cl_strerror(status));
    device = devices[device_id];
    context = clCreateContext(null, 1, &device, null, null, &status);
    assert(status == CL_SUCCESS, "Can't create OpenCL context: " ~ cl_strerror(status));
    queue = clCreateCommandQueue(context, device, CL_QUEUE_PROFILING_ENABLE, &status);
    assert(status == CL_SUCCESS, "Can't create OpenCL command queue:" ~ cl_strerror(status));
    if (verbose)
    {
      size_t value_size;
      status = clGetDeviceInfo(device, CL_DEVICE_NAME, 0, null, &value_size);
      assert(value_size > 0, "Can't get the device name: " ~ cl_strerror(status));
      char[] buffer = new char[value_size];
      assert(buffer != null, "Can't allocate buffer to hold the device name.");
      status = clGetDeviceInfo(device, CL_DEVICE_NAME, value_size, buffer.ptr, null);
      assert(status == CL_SUCCESS, "Can't get the device name: " ~ cl_strerror(status));
      if (buffer[$ - 1] == '\0') buffer.length -= 1;
      writefln("OpenCL device: \"%s\"", buffer);
    }

    char[] code = q{
      /**
       *  benchmark the device global memory bandwidth
       */
      __kernel void
      copy(__global float* out, __global const float* in)
      {
        int x = get_global_id(0);
        out[x] = in[x];
      }

      /**
       *  benchmark compute bound performance
       */
      __kernel void
      madd(__global float* out, __global const float* in)
      {
        int x = get_global_id(0);
        float a = in[x];
        float b = 3.9f * a * (1.0f - a)
                - 0.9f * a * (2.9f + a)
                + 2.3f * a * (1.3f - a)
                - 1.7f * a * (3.1f + a)
                + 0.7f * a * (0.5f - a)
                - 1.9f * a * (2.3f + a)
                + 1.1f * a * (5.3f - a)
                + 6.1f * a * (0.7f - a);
        out[x] = b;
      }
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource(context, 1, strs.ptr, &size, &status);
    assert(status == CL_SUCCESS, "Can't create program with source " ~ cl_strerror(status));
    status = clBuildProgram(program, 1, &device, "", null, null);
    assert(status == CL_SUCCESS, "Can't build program " ~ cl_strerror(status));
    kernel_copy = clCreateKernel(program, "copy", &status);
    assert(status == CL_SUCCESS, "Can't create kernel copy " ~ cl_strerror(status));
    kernel_madd = clCreateKernel(program, "madd", &status);
    assert(status == CL_SUCCESS, "Can't create kernel madd " ~ cl_strerror(status));
    status = clReleaseProgram(program);
    assert(status == CL_SUCCESS, "Can't release program " ~ cl_strerror(status));
    status = clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_SIZES, work_item_sizes.sizeof, work_item_sizes.ptr, null);
    assert(status == CL_SUCCESS, "Can't get max work item sizes " ~ cl_strerror(status));
  }

  bool is_valid_work_group(size_t[] sizes)
  {
    foreach (i, s; sizes)
      if (i > 2 || s > work_item_sizes[i])
        return false;
    return true;
  }

  uint[] get_platforms()
  {
    DerelictCL.load();
    status = clGetPlatformIDs(0, null, &num_platforms);
    assert(status == CL_SUCCESS, "No OpenCL platform found: " ~ cl_strerror(status));
    platform_ids = new cl_platform_id[num_platforms];
    assert(platform_ids != null, "Can't allocate array of OpenCL platform IDs.");
    status = clGetPlatformIDs(num_platforms, platform_ids.ptr, null);
    assert(status == CL_SUCCESS, "Can't get OpenCL platform IDs: " ~ cl_strerror(status));
    auto platforms = new uint[num_platforms];
    assert(platforms != null, "Can't allocate array of platforms.");
    foreach (p; 0 .. platforms.length)
    {
      status = clGetDeviceIDs(platform_ids[p], CL_DEVICE_TYPE_ALL, 0, null, &platforms[p]);
      assert(status == CL_SUCCESS, "No OpenCL device found:" ~ cl_strerror(status));
    }
    has_platform_ids = true;
    return platforms;
  }

  void register_instance(Instance instance)
  {
    debug (VERBOSE) writeln("The CLOP runtime registered a new instance " ~ instance.toString);
    clops ~= instance;
  }

  /++
   +
   +/
  double benchmark(cl_int size)
  {
    double result;
    cl_int status;
    cl_event event;

    cl_float[] image = new cl_float[size];
    cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
    cl_mem din  = clCreateBuffer(context, flags, cl_float.sizeof * size, image.ptr, &status);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));
    flags = CL_MEM_WRITE_ONLY;
    cl_mem dout = clCreateBuffer(context, flags, cl_float.sizeof * size, null, &status);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));

    status = clSetKernelArg(kernel_copy, 0, cl_mem.sizeof, &dout);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));
    status = clSetKernelArg(kernel_copy, 1, cl_mem.sizeof, &din);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));

    size_t gwsize = size;
    // warm up, do not measure time.
    status = clEnqueueNDRangeKernel(queue, kernel_copy, 1, null, &gwsize, null, 0, null, &event);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));
    status = clWaitForEvents(1, &event);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));
    // now a real run with time measurement.

    status = clEnqueueNDRangeKernel(queue, kernel_copy, 1, null, &gwsize, null, 0, null, &event);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));
    status = clWaitForEvents(1, &event);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));

    cl_ulong start_time;
    status = clGetEventProfilingInfo (event                     , // cl_event          event
                                      CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                      cl_ulong.sizeof           , // size_t            param_value_size
                                      &start_time               , // void*             param_value
                                      null                     ); // size_t*           param_value_size_ret
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));
    cl_ulong end_time;
    status = clGetEventProfilingInfo (event                     , // cl_event          event
                                      CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                      cl_ulong.sizeof           , // size_t            param_value_size
                                      &end_time                 , // void*             param_value
                                      null                     ); // size_t*           param_value_size_ret
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));

    result = size * 1E9 / (1024 * 1024 * (end_time - start_time));
    writef("%2.0f MI: 2 I/O %7.2f MI/s", size / (1024.0 * 1024.0), result);

    status = clSetKernelArg(kernel_madd, 0, cl_mem.sizeof, &dout);
    assert(status == CL_SUCCESS, "runtime madd benchmark " ~ cl_strerror(status));
    status = clSetKernelArg(kernel_madd, 1, cl_mem.sizeof, &din);
    assert(status == CL_SUCCESS, "runtime madd benchmark " ~ cl_strerror(status));

    status = clEnqueueNDRangeKernel(queue, kernel_madd, 1, null, &gwsize, null, 0, null, &event);
    assert(status == CL_SUCCESS, "runtime madd benchmark " ~ cl_strerror(status));
    status = clWaitForEvents(1, &event);
    assert(status == CL_SUCCESS, "runtime madd benchmark " ~ cl_strerror(status));

    status = clEnqueueNDRangeKernel(queue, kernel_madd, 1, null, &gwsize, null, 0, null, &event);
    assert(status == CL_SUCCESS, "runtime madd benchmark " ~ cl_strerror(status));
    status = clWaitForEvents(1, &event);
    assert(status == CL_SUCCESS, "runtime madd benchmark " ~ cl_strerror(status));

    status = clGetEventProfilingInfo (event                     , // cl_event          event
                                      CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                      cl_ulong.sizeof           , // size_t            param_value_size
                                      &start_time               , // void*             param_value
                                      null                     ); // size_t*           param_value_size_ret
    assert(status == CL_SUCCESS, "runtime madd benchmark " ~ cl_strerror(status));
    status = clGetEventProfilingInfo (event                     , // cl_event          event
                                      CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                      cl_ulong.sizeof           , // size_t            param_value_size
                                      &end_time                 , // void*             param_value
                                      null                     ); // size_t*           param_value_size_ret
    assert(status == CL_SUCCESS, "runtime madd benchmark " ~ cl_strerror(status));

    writefln(", 2 I/O + 24 FP %7.2f MI/s", size * 1E9 / (1024 * 1024 * (end_time - start_time)));

    status = clReleaseMemObject(din);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));
    status = clReleaseMemObject(dout);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));

    status = clReleaseKernel(kernel_copy);
    assert(status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror(status));
    status = clReleaseKernel(kernel_madd);
    assert(status == CL_SUCCESS, "runtime madd benchmark " ~ cl_strerror(status));

    return result;
  }

  /++
   +/
  void shutdown()
  {
    foreach (instance; clops)
      instance.release_resources();
    status = clReleaseCommandQueue(queue);
    assert(status == CL_SUCCESS, "clReleaseCommandQueue failed.");
    status = clReleaseContext(context);
    assert(status == CL_SUCCESS, "clReleaseContext failed.");
    writeln("CLOP runtime shut down.");
  }
}

static Runtime runtime = Runtime();

/++
 +/
string cl_strerror(cl_int err, string msg = "")
{
  switch (err)
  {
  case CL_BUILD_PROGRAM_FAILURE:                     return msg ~ " CL_BUILD_PROGRAM_FAILURE";
  case CL_COMPILER_NOT_AVAILABLE:                    return msg ~ " CL_COMPILER_NOT_AVAILABLE";
  case CL_DEVICE_NOT_AVAILABLE:                      return msg ~ " CL_DEVICE_NOT_AVAILABLE";
  case CL_DEVICE_NOT_FOUND:                          return msg ~ " CL_DEVICE_NOT_FOUND";
  case CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST: return msg ~ " CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST";
  case CL_IMAGE_FORMAT_MISMATCH:                     return msg ~ " CL_IMAGE_FORMAT_MISMATCH";
  case CL_IMAGE_FORMAT_NOT_SUPPORTED:                return msg ~ " CL_IMAGE_FORMAT_NOT_SUPPORTED";
  case CL_INVALID_ARG_INDEX:                         return msg ~ " CL_INVALID_ARG_INDEX";
  case CL_INVALID_ARG_SIZE:                          return msg ~ " CL_INVALID_ARG_SIZE";
  case CL_INVALID_ARG_VALUE:                         return msg ~ " CL_INVALID_ARG_VALUE";
  case CL_INVALID_BINARY:                            return msg ~ " CL_INVALID_BINARY";
  case CL_INVALID_BUFFER_SIZE:                       return msg ~ " CL_INVALID_BUFFER_SIZE";
  case CL_INVALID_BUILD_OPTIONS:                     return msg ~ " CL_INVALID_BUILD_OPTIONS";
  case CL_INVALID_COMMAND_QUEUE:                     return msg ~ " CL_INVALID_COMMAND_QUEUE";
  case CL_INVALID_CONTEXT:                           return msg ~ " CL_INVALID_CONTEXT";
  case CL_INVALID_DEVICE:                            return msg ~ " CL_INVALID_DEVICE";
  case CL_INVALID_DEVICE_TYPE:                       return msg ~ " CL_INVALID_DEVICE_TYPE";
  case CL_INVALID_EVENT:                             return msg ~ " CL_INVALID_EVENT";
  case CL_INVALID_EVENT_WAIT_LIST:                   return msg ~ " CL_INVALID_EVENT_WAIT_LIST";
  case CL_INVALID_GL_OBJECT:                         return msg ~ " CL_INVALID_GL_OBJECT";
  case CL_INVALID_GLOBAL_OFFSET:                     return msg ~ " CL_INVALID_GLOBAL_OFFSET";
  case CL_INVALID_GLOBAL_WORK_SIZE:                  return msg ~ " CL_INVALID_GLOBAL_WORK_SIZE";
  case CL_INVALID_HOST_PTR:                          return msg ~ " CL_INVALID_HOST_PTR";
  case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:           return msg ~ " CL_INVALID_IMAGE_FORMAT_DESCRIPTOR";
  case CL_INVALID_IMAGE_SIZE:                        return msg ~ " CL_INVALID_IMAGE_SIZE";
  case CL_INVALID_KERNEL:                            return msg ~ " CL_INVALID_KERNEL";
  case CL_INVALID_KERNEL_ARGS:                       return msg ~ " CL_INVALID_KERNEL_ARGS";
  case CL_INVALID_KERNEL_DEFINITION:                 return msg ~ " CL_INVALID_KERNEL_DEFINITION";
  case CL_INVALID_KERNEL_NAME:                       return msg ~ " CL_INVALID_KERNEL_NAME";
  case CL_INVALID_MEM_OBJECT:                        return msg ~ " CL_INVALID_MEM_OBJECT";
  case CL_INVALID_MIP_LEVEL:                         return msg ~ " CL_INVALID_MIP_LEVEL";
  case CL_INVALID_OPERATION:                         return msg ~ " CL_INVALID_OPERATION";
  case CL_INVALID_PLATFORM:                          return msg ~ " CL_INVALID_PLATFORM";
  case CL_INVALID_PROGRAM:                           return msg ~ " CL_INVALID_PROGRAM";
  case CL_INVALID_PROGRAM_EXECUTABLE:                return msg ~ " CL_INVALID_PROGRAM_EXECUTABLE";
  case CL_INVALID_QUEUE_PROPERTIES:                  return msg ~ " CL_INVALID_QUEUE_PROPERTIES";
  case CL_INVALID_SAMPLER:                           return msg ~ " CL_INVALID_SAMPLER";
  case CL_INVALID_VALUE:                             return msg ~ " CL_INVALID_VALUE";
  case CL_INVALID_WORK_DIMENSION:                    return msg ~ " CL_INVALID_WORK_DIMENSION";
  case CL_INVALID_WORK_GROUP_SIZE:                   return msg ~ " CL_INVALID_WORK_GROUP_SIZE";
  case CL_INVALID_WORK_ITEM_SIZE:                    return msg ~ " CL_INVALID_WORK_ITEM_SIZE";
  case CL_MAP_FAILURE:                               return msg ~ " CL_MAP_FAILURE";
  case CL_MEM_COPY_OVERLAP:                          return msg ~ " CL_MEM_COPY_OVERLAP";
  case CL_MEM_OBJECT_ALLOCATION_FAILURE:             return msg ~ " CL_MEM_OBJECT_ALLOCATION_FAILURE";
  case CL_MISALIGNED_SUB_BUFFER_OFFSET:              return msg ~ " CL_MISALIGNED_SUB_BUFFER_OFFSET";
  case CL_OUT_OF_HOST_MEMORY:                        return msg ~ " CL_OUT_OF_HOST_MEMORY";
  case CL_OUT_OF_RESOURCES:                          return msg ~ " CL_OUT_OF_RESOURCES";
  case CL_PROFILING_INFO_NOT_AVAILABLE:              return msg ~ " CL_PROFILING_INFO_NOT_AVAILABLE";
  default:                                           return msg ~ " UNKNOWN CL ERROR";
  }
}
