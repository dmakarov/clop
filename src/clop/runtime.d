module clop.runtime;

import std.stdio;

import derelict.opencl.cl;

struct Runtime
{
  cl_int status;
  cl_device_id device;
  cl_context context;
  cl_command_queue queue;

  /**
   */
  void init( uint platform_id = 0, uint device_id = 0 )
  {
    DerelictCL.load();
    cl_uint num_platforms, num_devices;
    status = clGetPlatformIDs( 0, null, &num_platforms );
    assert( status == CL_SUCCESS && num_platforms > 0 && platform_id < num_platforms, "No OpenCL platform found." );
    auto platforms = new cl_platform_id[num_platforms];
    assert( platforms != null, "Can't allocate array of OpenCL platform IDs." );
    status = clGetPlatformIDs( num_platforms, platforms.ptr, null );
    assert( status == CL_SUCCESS, "Can't get OpenCL platform IDs." );
    auto platform = platforms[platform_id];
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, 0, null, &num_devices );
    assert( status == CL_SUCCESS && num_devices > 0 && device_id < num_devices, "No OpenCL device found." );
    auto devices = new cl_device_id[num_devices];
    assert( devices != null, "Can't allocate array of OpenCL device IDs." );
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, num_devices, devices.ptr, null );
    assert( status == CL_SUCCESS, "Can't get OpenCL device IDs." );
    device = devices[device_id];
    context = clCreateContext( null, 1, &device, null, null, &status );
    assert( status == CL_SUCCESS, "Can't create OpenCL context." );
    queue = clCreateCommandQueue( context, device, 0, &status );
    assert( status == CL_SUCCESS, "Can't create OpenCL command queue." );
  }

  /**
   */
  void shutdown()
  {
    status = clReleaseCommandQueue( queue );
    assert( status == CL_SUCCESS, "clReleaseCommandQueue failed." );
    status = clReleaseContext( context );
    assert( status == CL_SUCCESS, "clReleaseContext failed." );
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
