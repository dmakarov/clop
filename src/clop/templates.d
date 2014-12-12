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
  assert( runtime.status == CL_SUCCESS, "clCreateProgramWithSource failed." );
  runtime.status = clBuildProgram( program, 1, &runtime.device, "", null, null );
  if ( runtime.status != CL_SUCCESS )
  {
    char[1024] log;
    clGetProgramBuildInfo( program, runtime.device, CL_PROGRAM_BUILD_LOG, 1024, log.ptr, null );
    writeln( log );
  }
  assert( runtime.status == CL_SUCCESS, "clBuildProgram failed." );
  auto clop_opencl_kernel = clCreateKernel( program, "%s", &runtime.status );
  assert( runtime.status == CL_SUCCESS, "clCreateKernel failed." );
},

template_antidiagonal_invoke_kernel = q{
  uint clop_c0 = %s;
  runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg + 1, cl_int.sizeof, &clop_c0 );
  assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
  uint clop_r0;
  for ( clop_r0 = %s; clop_r0 < %s; ++clop_r0 )
  {
    size_t global = clop_r0;
    //runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg, cl_int.sizeof, &clop_r0 );
    assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
    //runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
    assert( runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel failed." );
  }
  for ( clop_c0 = %s + 1, clop_r0 -= 1; clop_c0 < %s; ++clop_c0 )
  {
    size_t global = clop_r0 - clop_c0 + 1;
    //runtime.status = clSetKernelArg( clop_opencl_kernel, clop_opencl_kernel_arg + 1, cl_int.sizeof, &clop_c0 );
    assert( runtime.status == CL_SUCCESS, "clSetKernelArg failed." );
    //runtime.status = clEnqueueNDRangeKernel( runtime.queue, clop_opencl_kernel, 1, null, &global, null, 0, null, null );
    assert( runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel failed." );
  }
},

template_shared_memory_init = q{
  if ( tx == 0 )
    for ( int i = 0; i < %s; ++i ) // h
      for ( int j = 0; j < %s; ++j ) // w
        %s[i * (%s + %s)] = %s[%s]; // u, bs, w, v, nw
  for ( int i = 0; i < %s; ++i ) // w
        %s[(tx + %s) * (%s + %s)] = %s[%s]; // u, h, bs, w, v, wi
  for ( int i = 0; i < %s; ++i ) // h
        %s[(tx + %s)] = %s[%s]; // u, w, v, ni
  barrier( CLK_LOCAL_MEM_FENCE );
},

template_shared_memory_fini = q{
  for ( int ty = 0; ty < %s; ++ty )
    %s[ty * %s + tx] = %s[(ty + 1) * (%s + 1) + tx + 1];
},

template_antidiagonal_loop_prefix = "
  for ( int %s = 0; %s < 2 * %s - 1; ++%s )
  {
    int %s = (%s < %s ? %s : %s - 1) + tx;
    int %s = (%s < %s ? 0 : %s - %s + 1) - tx;
    if ( %s < %s && %s >= 0 )
",

template_antidiagonal_loop_suffix = "
    barrier( CLK_LOCAL_MEM_FENCE );
  }
";
