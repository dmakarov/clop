module clop.runtime;

import derelict.opencl.cl;

struct Runtime
{
  cl_int status;
  cl_device_id device;
  cl_context context;
  cl_command_queue queue;

  ~this()
  {
    status = clReleaseCommandQueue( queue );
    assert( status == CL_SUCCESS, "clReleaseCommandQueue failed." );
    status = clReleaseContext( context );
    assert( status == CL_SUCCESS, "clReleaseContext failed." );
  }

  void init( uint pid = 0, uint did = 1 )
  {
    DerelictCL.load();
    cl_uint num_platforms, num_devices;
    status = clGetPlatformIDs( 0, null, &num_platforms );
    assert( status == CL_SUCCESS && num_platforms > 0 && pid < num_platforms, "No OpenCL platform found." );
    auto platforms = new cl_platform_id[num_platforms];
    assert( platforms != null, "Can't allocate array of OpenCL platform IDs." );
    status = clGetPlatformIDs( num_platforms, platforms.ptr, null );
    assert( status == CL_SUCCESS, "Can't get OpenCL platform IDs." );
    auto platform = platforms[pid];
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, 0, null, &num_devices );
    assert( status == CL_SUCCESS && num_devices > 0 && did < num_devices, "No OpenCL device found." );
    auto devices = new cl_device_id[num_devices];
    assert( devices != null, "Can't allocate array of OpenCL device IDs." );
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, num_devices, devices.ptr, null );
    assert( status == CL_SUCCESS, "Can't get OpenCL device IDs." );
    device = devices[did];
    context = clCreateContext( null, 1, &device, null, null, &status );
    assert( status == CL_SUCCESS, "Can't create OpenCL context." );
    queue = clCreateCommandQueue( context, device, 0, &status );
    assert( status == CL_SUCCESS, "Can't create OpenCL command queue." );
  }
}

static Runtime runtime = Runtime();
