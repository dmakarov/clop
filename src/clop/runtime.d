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
