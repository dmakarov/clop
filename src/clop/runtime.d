module clop.runtime;

import std.datetime;
import std.stdio;

import derelict.opencl.cl;

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

  /**
   */
  void init( uint platform_id = 0, uint device_id = 0, bool verbose = true )
  {
    if ( !has_platform_ids )
    {
      DerelictCL.load();
      status = clGetPlatformIDs( 0, null, &num_platforms );                                  assert( status == CL_SUCCESS && num_platforms > 0 && platform_id < num_platforms, "No OpenCL platform found: " ~ cl_strerror( status ) );
      platform_ids = new cl_platform_id[num_platforms];                                      assert( platform_ids != null, "Can't allocate array of OpenCL platform IDs." );
      status = clGetPlatformIDs( num_platforms, platform_ids.ptr, null );                    assert( status == CL_SUCCESS, "Can't get OpenCL platform IDs: " ~ cl_strerror( status ) );
    }
    auto platform = platform_ids[platform_id];
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, 0, null, &num_devices );          assert( status == CL_SUCCESS && num_devices > 0 && device_id < num_devices, "No OpenCL device found:" ~ cl_strerror( status ) );
    auto devices = new cl_device_id[num_devices];                                            assert( devices != null, "Can't allocate array of OpenCL device IDs." );
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, num_devices, devices.ptr, null ); assert( status == CL_SUCCESS, "Can't get OpenCL device IDs: " ~ cl_strerror( status ) );
    device = devices[device_id];
    context = clCreateContext( null, 1, &device, null, null, &status );                      assert( status == CL_SUCCESS, "Can't create OpenCL context: " ~ cl_strerror( status ) );
    queue = clCreateCommandQueue( context, device, 0, &status );                             assert( status == CL_SUCCESS, "Can't create OpenCL command queue:" ~ cl_strerror( status ) );
    if ( verbose )
    {
      size_t value_size;
      status = clGetDeviceInfo( device, CL_DEVICE_NAME, 0, null, &value_size );              assert( value_size > 0, "Can't get the device name: " ~ cl_strerror( status ) );
      char[] buffer = new char[value_size];                                                  assert( buffer != null, "Can't allocate buffer to hold the device name." );
      status = clGetDeviceInfo( device, CL_DEVICE_NAME, value_size, buffer.ptr, null );      assert( status == CL_SUCCESS, "Can't get the device name: " ~ cl_strerror( status ) );
      if ( buffer[$ - 1] == '\0' ) buffer.length -= 1;
      writefln( "OpenCL device: \"%s\"", buffer );
    }

    char[] code = q{
      /**
       */
      __kernel void
      copy( __global float* out, __global const float* in, int w )
      {
        int x = get_global_id( 0 );
        int y = get_global_id( 1 );
        out[y * w + x] = in[y * w + x];
      }
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( context, 1, strs.ptr, &size, &status );        assert( status == CL_SUCCESS, "Can't create program with source " ~ cl_strerror( status ) );
    status = clBuildProgram( program, 1, &device, "", null, null );                          assert( status == CL_SUCCESS, "Can't build program " ~ cl_strerror( status ) );
    kernel_copy = clCreateKernel( program, "copy", &status );                                assert( status == CL_SUCCESS, "Can't create kernel " ~ cl_strerror( status ) );
    status = clReleaseProgram( program );                                                    assert( status == CL_SUCCESS, "Can't release program " ~ cl_strerror( status ) );
  }

  uint[] get_platforms()
  {
    DerelictCL.load();
    status = clGetPlatformIDs( 0, null, &num_platforms );                                    assert( status == CL_SUCCESS, "No OpenCL platform found: " ~ cl_strerror( status ) );
    platform_ids = new cl_platform_id[num_platforms];                                        assert( platform_ids != null, "Can't allocate array of OpenCL platform IDs." );
    status = clGetPlatformIDs( num_platforms, platform_ids.ptr, null );                      assert( status == CL_SUCCESS, "Can't get OpenCL platform IDs: " ~ cl_strerror( status ) );
    auto platforms = new uint[num_platforms];                                                assert( platforms != null, "Can't allocate array of platforms." );
    foreach ( p; 0 .. platforms.length )
    {
      status = clGetDeviceIDs( platform_ids[p], CL_DEVICE_TYPE_ALL, 0, null, &platforms[p] );
      assert( status == CL_SUCCESS, "No OpenCL device found:" ~ cl_strerror( status ) );
    }
    has_platform_ids = true;
    return platforms;
  }

  void benchmark()
  {
    cl_int status;
    cl_event event;
    StopWatch timer;
    TickDuration ticks;

    for ( cl_int width = 512; width < 8193; width *= 2 )
    {
      cl_float[] image = new cl_float[width * width];
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem din  = clCreateBuffer( context, flags, cl_float.sizeof * width * width, image.ptr, &status );
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      flags = CL_MEM_WRITE_ONLY;
      cl_mem dout = clCreateBuffer( context, flags, cl_float.sizeof * width * width, null, &status );
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );

      status = clSetKernelArg( kernel_copy, 0, cl_mem.sizeof, &dout );
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_copy, 1, cl_mem.sizeof, &din );
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_copy, 2, cl_int.sizeof, &width );
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );

      size_t[2] gwsize = [width, width];
      timer.start();
      status = clEnqueueNDRangeKernel( queue, kernel_copy, 2, null, gwsize.ptr, null, 0, null, &event );
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      status = clWaitForEvents( 1, &event );
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      timer.stop();
      ticks = timer.peek();
      writefln( "Size %8d, time %8.6f [s], bandwidth %4.2f GB/s",
                width * width,
                ticks.usecs / 1E6,
                width * width * cl_float.sizeof * 1E6 / ( 1024 * 1024 * 1024 * ticks.usecs ) );
      timer.reset();

      status = clReleaseMemObject( din );
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dout );
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
    }
    status = clReleaseKernel( kernel_copy );
    assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
  }

  /**
   */
  void shutdown()
  {
    status = clReleaseCommandQueue( queue ); assert( status == CL_SUCCESS, "clReleaseCommandQueue failed." );
    status = clReleaseContext( context );    assert( status == CL_SUCCESS, "clReleaseContext failed." );
    writeln( "CLOP runtime shut down." );
  }
}

static Runtime runtime = Runtime();

/**
 */
string
cl_strerror( cl_int err )
{
  switch ( err )
  {
  case CL_BUILD_PROGRAM_FAILURE:                     return "CL_BUILD_PROGRAM_FAILURE";
  case CL_COMPILER_NOT_AVAILABLE:                    return "CL_COMPILER_NOT_AVAILABLE";
  case CL_DEVICE_NOT_AVAILABLE:                      return "CL_DEVICE_NOT_AVAILABLE";
  case CL_DEVICE_NOT_FOUND:                          return "CL_DEVICE_NOT_FOUND";
  case CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST: return "CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST";
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
  case CL_MISALIGNED_SUB_BUFFER_OFFSET:              return "CL_MISALIGNED_SUB_BUFFER_OFFSET";
  case CL_OUT_OF_HOST_MEMORY:                        return "CL_OUT_OF_HOST_MEMORY";
  case CL_OUT_OF_RESOURCES:                          return "CL_OUT_OF_RESOURCES";
  case CL_PROFILING_INFO_NOT_AVAILABLE:              return "CL_PROFILING_INFO_NOT_AVAILABLE";
  default:                                           return "UNKNOWN CL ERROR";
  }
}
