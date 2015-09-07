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

#ifndef CLOP_EXAMPLES_COMMON_HPP
#define CLOP_EXAMPLES_COMMON_HPP

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif

using namespace std;

class clop_examples_common
{
#ifdef __MACH__
  host_name_port_t self;
  clock_serv_t cclock;
#endif

  bool release_opencl_resources;
  cl_platform_id* platforms;
  cl_platform_id platform;
  cl_device_id* devices;
  cl_device_id device;
  cl_program program;
  cl_context context;
  cl_command_queue queue;
  string kernel_file;
  vector<string> kernel_names;
  vector<cl_kernel> kernels;

public:

  clop_examples_common(const string& file, vector<const char*> names) : kernel_file(file), kernels(names.size())
  {
#ifdef __MACH__
    self = mach_host_self();
    host_get_clock_service(self, REALTIME_CLOCK, &cclock);
#endif
    for (auto n : names)
      kernel_names.emplace_back(n);
    release_opencl_resources = false;
  }

  void reset(cl_uint platform_index, cl_uint device_index)
  {
    cl_uint num_platforms, num_devices;
    cl_int status = clGetPlatformIDs(0, nullptr, &num_platforms);
    check_status(status, "init_cl (clGetPlatformIDs 1) ");
    if (0 == num_platforms) throw string("init_cl no OpenCL platform found");
    if (platform_index >= num_platforms) throw string("init_cl invalid platform index");
    if (nullptr == (platforms = new cl_platform_id[num_platforms])) throw string("init_cl can't allocate memory for platforms");
    status = clGetPlatformIDs(num_platforms, platforms, nullptr);
    check_status(status, "init_cl (clGetPlatformIDs 2) ");
    platform = platforms[platform_index];

    // 2: detect OpenCL devices
    status = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 0, nullptr, &num_devices);
    check_status(status, "init_cl (clGetDeviceIDs 1) ");
    if (0 == num_devices) throw string("init_cl no OpenCL device found");
    if (device_index >= num_devices) throw string("init_cl invalid device index");
    if (nullptr == (devices = new cl_device_id[num_devices])) throw string("init_cl can't allocate memory for devices");
    status = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, num_devices, devices, nullptr);
    check_status(status, "init_cl (clGetDeviceIDs 2) ");
    device = devices[device_index];

    // 3: Create an OpenCL context
    context = clCreateContext(nullptr, 1, &device, nullptr, nullptr, &status);
    check_status(status, "init_cl (clCreateContext) ");

    // 4: Create an OpenCL command queue
    queue = clCreateCommandQueue(context, device, 0, &status);
    check_status(status, "init_cl (clCreateCommandQueue) ");

    // 5: Load CL file, build CL program object, create CL kernel object
    load_kernels(kernel_file, device_index);
    for (int kn = 0; kn < kernels.size(); ++kn)
    {
      // get a kernel object handle for a kernel with the given name
      cl_kernel kernel = clCreateKernel(program, (kernel_names[kn]).c_str(), &status);
      check_status(status, "init_cl(clCreateKernel) \"" + string(kernel_names[kn]) + "\" ");
      kernels[kn] = kernel;
    }
    release_opencl_resources = true;
  }

  ~clop_examples_common()
  {
    if (release_opencl_resources)
    {
      for (auto k : kernels)
        clReleaseKernel(k);
      clReleaseCommandQueue(queue);
      clReleaseContext(context);
    }
#ifdef __MACH__
    mach_port_deallocate(self, cclock);
#endif
  }

  cl_context get_context()
  {
    return context;
  }

  cl_command_queue get_queue()
  {
    return queue;
  }

  vector<cl_kernel> get_kernels()
  {
    return kernels;
  }

  double gettime()
  {
#ifdef __MACH__
    mach_timespec_t ts;
    clock_get_time(cclock, &ts);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
#endif
    return ts.tv_sec + ts.tv_nsec * 1e-9;
  }

  /**
   */
  void check_status(cl_int status, string msg)
  {
    if (CL_SUCCESS != status) throw msg + error_code_to_string(status);
  }

  void load_kernels(string src_file, cl_uint device_index)
  {
    cl_int status;
    size_t size;
    const char* source = (char*) binary_to_array(src_file, &size);
    program = clCreateProgramWithSource(context, 1, &source, &size, &status);
    check_status(status, "CLHelper::CLHelper: (clCreateProgramWithSource) ");
    delete [] source;
    status = clBuildProgram(program, 1, &device, nullptr, nullptr, nullptr);
    if (CL_SUCCESS != status)
    {
      string msg = "CLHelper::CLHelper: (clBuildProgram) " + error_code_to_string(status);
      size_t size;
      status = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, nullptr, &size);
      check_status(status, "CLHelper::CLHelper: (clGetProgramBuildInfo 1) ");
      char* buf = new char[size];
      status = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, size, buf, nullptr);
      check_status(status, "CLHelper::CLHelper: (clGetProgramBuildInfo 2) ");
      array_to_binary("errinfo.txt", buf, size - 1);
      delete [] buf;
      throw msg;
    }
  }

  /**
   * @function read the contents of a file into a byte array.
   */
  void* binary_to_array(const string bin_file, size_t *size) throw (string)
  {
    ifstream f(bin_file.c_str(), ifstream::in | ifstream::binary);
    try
    {
      if (f.is_open())
      {
        f.seekg(0, ifstream::end);
        *size = f.tellg();
        f.seekg(0, ifstream::beg);
        char *buf = new char[*size + 1];
        if (nullptr == buf) throw string("CLHelper::binary_to_array: can't allocate memory");
        f.read(buf, *size);
        buf[*size] = '\0';
        f.close();
        return buf;
      }
    }
    catch(string msg) { fprintf(stderr, "CLHelper::binary_to_array: exception caught %s\n", msg.c_str()); }
    catch(...) {}
    if (f.is_open()) f.close();
    throw string("CLHelper::binary_to_array: can't read file ") + bin_file;
  }

  /**
   * @function write a byte array into a binary file.
   */
  void array_to_binary(const string bin_file, const char *buf, size_t size) throw (string)
  {
    ofstream f(bin_file.c_str(), ofstream::out | ofstream::binary);
    try
    {
      if (f.is_open())
      {
        f.write(buf, size);
        f.close();
        return;
      }
    }
    catch(string msg) { fprintf(stderr, "CLHelper::array_to_binary: exception caught %s\n", msg.c_str()); }
    catch(...) {}
    if (f.is_open()) f.close();
    throw string("CLHelper::array_to_binary: can't write to file ") + bin_file;
  }

  string error_code_to_string(cl_int code)
  {
    switch (code)
    {
    case CL_BUILD_PROGRAM_FAILURE:                     return "CL_BUILD_PROGRAM_FAILURE";
    case CL_COMPILER_NOT_AVAILABLE:                    return "CL_COMPILER_NOT_AVAILABLE";
    case CL_DEVICE_NOT_AVAILABLE:                      return "CL_DEVICE_NOT_AVAILABLE";
    case CL_DEVICE_NOT_FOUND:                          return "CL_DEVICE_NOT_FOUND";
#ifdef CL_VERSION_1_2
    case CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST: return "CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST";
#endif
    case CL_IMAGE_FORMAT_MISMATCH:                     return "CL_IMAGE_FORMAT_MISMATCH";
    case CL_IMAGE_FORMAT_NOT_SUPPORTED:                return "CL_IMAGE_FORMAT_NOT_SUPPORTED";
    case CL_INVALID_ARG_INDEX:                         return "CL_INVALID_ARG_INDEX";
    case CL_INVALID_ARG_SIZE:                          return "CL_INVALID_ARG_SIZE";
    case CL_INVALID_ARG_VALUE:                         return "CL_INVALID_ARG_VALUE";
    case CL_INVALID_BINARY:                            return "CL_INVALID_BINARY";
    case CL_INVALID_BUFFER_SIZE:                       return "CL_INVALID_BUFFER_SIZE";
    case CL_INVALID_BUILD_OPTIONS:                     return "CL_INVALID_BUILD_OPTIONS";
    case CL_INVALID_COMMAND_QUEUE:                     return "CL_INVALID_COMMAND_QUEUE";
    case CL_INVALID_CONTEXT:                           return "CL_INVALID_CONTEXT";
    case CL_INVALID_DEVICE:                            return "CL_INVALID_DEVICE";
    case CL_INVALID_DEVICE_TYPE:                       return "CL_INVALID_DEVICE_TYPE";
    case CL_INVALID_EVENT:                             return "CL_INVALID_EVENT";
    case CL_INVALID_EVENT_WAIT_LIST:                   return "CL_INVALID_EVENT_WAIT_LIST";
    case CL_INVALID_GL_OBJECT:                         return "CL_INVALID_GL_OBJECT";
    case CL_INVALID_GLOBAL_OFFSET:                     return "CL_INVALID_GLOBAL_OFFSET";
    case CL_INVALID_GLOBAL_WORK_SIZE:                  return "CL_INVALID_GLOBAL_WORK_SIZE";
    case CL_INVALID_HOST_PTR:                          return "CL_INVALID_HOST_PTR";
    case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:           return "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR";
    case CL_INVALID_IMAGE_SIZE:                        return "CL_INVALID_IMAGE_SIZE";
    case CL_INVALID_KERNEL:                            return "CL_INVALID_KERNEL";
    case CL_INVALID_KERNEL_ARGS:                       return "CL_INVALID_KERNEL_ARGS";
    case CL_INVALID_KERNEL_DEFINITION:                 return "CL_INVALID_KERNEL_DEFINITION";
    case CL_INVALID_KERNEL_NAME:                       return "CL_INVALID_KERNEL_NAME";
    case CL_INVALID_MEM_OBJECT:                        return "CL_INVALID_MEM_OBJECT";
    case CL_INVALID_MIP_LEVEL:                         return "CL_INVALID_MIP_LEVEL";
    case CL_INVALID_OPERATION:                         return "CL_INVALID_OPERATION";
    case CL_INVALID_PLATFORM:                          return "CL_INVALID_PLATFORM";
    case CL_INVALID_PROGRAM:                           return "CL_INVALID_PROGRAM";
    case CL_INVALID_PROGRAM_EXECUTABLE:                return "CL_INVALID_PROGRAM_EXECUTABLE";
    case CL_INVALID_QUEUE_PROPERTIES:                  return "CL_INVALID_QUEUE_PROPERTIES";
    case CL_INVALID_SAMPLER:                           return "CL_INVALID_SAMPLER";
    case CL_INVALID_VALUE:                             return "CL_INVALID_VALUE";
    case CL_INVALID_WORK_DIMENSION:                    return "CL_INVALID_WORK_DIMENSION";
    case CL_INVALID_WORK_GROUP_SIZE:                   return "CL_INVALID_WORK_GROUP_SIZE";
    case CL_INVALID_WORK_ITEM_SIZE:                    return "CL_INVALID_WORK_ITEM_SIZE";
    case CL_MAP_FAILURE:                               return "CL_MAP_FAILURE";
    case CL_MEM_COPY_OVERLAP:                          return "CL_MEM_COPY_OVERLAP";
    case CL_MEM_OBJECT_ALLOCATION_FAILURE:             return "CL_MEM_OBJECT_ALLOCATION_FAILURE";
#ifdef CL_VERSION_1_2
    case CL_MISALIGNED_SUB_BUFFER_OFFSET:              return "CL_MISALIGNED_SUB_BUFFER_OFFSET";
#endif
    case CL_OUT_OF_HOST_MEMORY:                        return "CL_OUT_OF_HOST_MEMORY";
    case CL_OUT_OF_RESOURCES:                          return "CL_OUT_OF_RESOURCES";
    case CL_PROFILING_INFO_NOT_AVAILABLE:              return "CL_PROFILING_INFO_NOT_AVAILABLE";
    }
    return "UNKNOWN CL ERROR";
  }

}; // clop_examples_common class

#endif // CLOP_EXAMPLES_COMMON_HPP
