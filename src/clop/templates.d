module clop.templates;

enum

template_clop_unit = q{
  try
  {
    char[] clop_opencl_program_source = (%s).dup;
    %s
  }
  catch( Exception e )
  {
    writeln( e );
  }
},

template_create_opencl_kernel = q{
  debug ( DEBUG ) writeln( "OpenCL program:\n", clop_opencl_program_source, "EOF" );
  size_t clop_opencl_program_source_size = clop_opencl_program_source.length;
  char* clop_opencl_program_source_pointer = clop_opencl_program_source.ptr;
  auto program = clCreateProgramWithSource( runtime.context, 1, &clop_opencl_program_source_pointer, &clop_opencl_program_source_size, &runtime.status );
  assert( runtime.status == CL_SUCCESS, "clCreateProgramWithSource failed " ~ cl_strerror( runtime.status ) );
  runtime.status = clBuildProgram( program, 1, &runtime.device, "", null, null );
  if ( runtime.status != CL_SUCCESS )
  {
    char[3072] log;
    clGetProgramBuildInfo( program, runtime.device, CL_PROGRAM_BUILD_LOG, 3071, log.ptr, null );
    writeln( "CL_PROGRAM_BUILD_LOG:\n", log, "\nEOD" );
  }
  assert( runtime.status == CL_SUCCESS, "clBuildProgram failed " ~ cl_strerror( runtime.status ) );
  auto clop_opencl_kernel = clCreateKernel( program, "%s", &runtime.status );
  assert( runtime.status == CL_SUCCESS, "clCreateKernel failed " ~ cl_strerror( runtime.status ) );
},

template_antidiagonal_invoke_kernel = q{
  uint clop_c0 = %s;
  runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg + 1, cl_int.sizeof, &clop_c0 );
  assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
  uint clop_r0;
  for ( clop_r0 = %s; clop_r0 < %s; ++clop_r0 )
  {
    size_t global = clop_r0;
    runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg, cl_int.sizeof, &clop_r0 );
    assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
    runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
    assert( runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel failed." );
  }
  for ( clop_c0 = %s + 1, clop_r0 -= 1; clop_c0 < %s; ++clop_c0 )
  {
    size_t global = clop_r0 - clop_c0 + 1;
    runtime.status = clSetKernelArg( clop_opencl_kernel, 5, cl_int.sizeof, &clop_c0 );
    assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
    runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
    assert( runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel failed." );
  }
},

template_antidiagonal_rectangular_blocks_invoke_kernel = q{
  auto max_blocks = ((%s) - 1) / (%s);
  for ( int i = 0; i < 2 * max_blocks - 1; ++i )
  {
    cl_int br = ( i < max_blocks ) ? i :     max_blocks - 1;
    cl_int bc = ( i < max_blocks ) ? 0 : i - max_blocks + 1;
    size_t wgroup = %s;
    size_t global = (%s) * min( br + 1, max_blocks - bc );
    runtime.status = clSetKernelArg( %s, 5, cl_int.sizeof, &bc );
    assert( runtime.status == CL_SUCCESS, "clSetKernelArg " ~ cl_strerror( runtime.status ) );
    runtime.status = clSetKernelArg( %s, 6, cl_int.sizeof, &br );
    assert( runtime.status == CL_SUCCESS, "clSetKernelArg " ~ cl_strerror( runtime.status ) );
    runtime.status = clEnqueueNDRangeKernel( runtime.queue, %s, 1, null, &global, &wgroup, 0, null, null );
    assert( runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel " ~ cl_strerror( runtime.status ) );
  }
},

template_shared_memory_data_init = q{
  for ( int %s = 0; %s < (%s); ++%s )
    for ( int %s = 0; %s < (%s); %s += (%s) )
      if ( (%s) + (%s) < (%s) )
        %s[(%s) * (%s) + (%s) + (%s)] =
        %s[(%s) * (%s) + (%s) + (%s)];
  barrier( CLK_LOCAL_MEM_FENCE );
},

template_shared_memory_fini = q{
  for ( int %s = 0; %s < %s; ++%s )
    for ( int %s = 0; %s < (%s); %s += (%s) )
      if ( (%s) + (%s) < (%s) )
        %s[(%s) * (%s) + (%s) + (%s)] =
        %s[(%s) * (%s) + (%s) + (%s)];
},

template_antidiagonal_loop_prefix = "
  for ( int %s = 0; %s < (%s) + (%s) - 1; ++%s )
  {
",

template_shared_index_recalculation = "
    int %s = (%s) + (%s) + ((%s) < (%s) ? 0 : (%s) - (%s) + 1);
    int %s = (%s) - (%s) + ((%s) < (%s) ? (%s) : (%s) - 1);
",

template_antidiagonal_loop_suffix = "
    barrier( CLK_LOCAL_MEM_FENCE );
  }
",

template_2d_index = "(%s) * (%s) + (%s)";
