module clop.ct.templates;

enum

template_clop_unit = q{
  try
  {
    import std.datetime;
    StopWatch timer;
    timer.start();
    %s
    timer.stop();
    TickDuration ticks = timer.peek();
    writefln("CLOP %s %%5.3f [s]", ticks.usecs / 1E6);
  }
  catch (Exception e)
  {
    writeln(e);
  }
},

template_create_opencl_kernel = q{
    static cl_kernel clop_opencl_kernel_%s;
    static bool kernel_has_been_created_%s;
    static Instance instance_%s;
    if (!kernel_has_been_created_%s)
    {
      instance_%s = new Instance;
      runtime.register_instance(instance_%s);
      kernel_has_been_created_%s = true;
      // params
      %s
      // kernel
      char[] clop_opencl_program_source = (%s).dup;
      debug (VERBOSE) writeln("OpenCL program:\n", clop_opencl_program_source, "EOF");
      size_t clop_opencl_program_source_size = clop_opencl_program_source.length;
      char* clop_opencl_program_source_pointer = clop_opencl_program_source.ptr;
      auto program = clCreateProgramWithSource(runtime.context, 1, &clop_opencl_program_source_pointer, &clop_opencl_program_source_size, &runtime.status);
      assert(runtime.status == CL_SUCCESS, "clCreateProgramWithSource failed " ~ cl_strerror(runtime.status));
      runtime.status = clBuildProgram(program, 1, &runtime.device, "", null, null);
      if (runtime.status != CL_SUCCESS)
      {
        char[3072] log;
        clGetProgramBuildInfo(program, runtime.device, CL_PROGRAM_BUILD_LOG, 3071, log.ptr, null);
        writeln("CL_PROGRAM_BUILD_LOG:\n", log, "\nEOD");
      }
      assert(runtime.status == CL_SUCCESS, "clBuildProgram failed " ~ cl_strerror(runtime.status));
      clop_opencl_kernel_%s = clCreateKernel(program, "%s", &runtime.status);
      assert(runtime.status == CL_SUCCESS, "clCreateKernel failed " ~ cl_strerror(runtime.status));
    }
},

template_plain_invoke_kernel = q{
    size_t[] offset = %s, global = %s;
    runtime.status = clEnqueueNDRangeKernel(runtime.queue, %s, %s, offset.ptr, global.ptr, null, 0, null, null);
    assert(runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel " ~ cl_strerror(runtime.status));
},

template_antidiagonal_invoke_kernel = q{
  foreach (i; 2 .. 2 * %s - 1)
  {
    size_t global = (i < %s) ? i - 1 : 2 * %s - i - 1;
    runtime.status = clSetKernelArg(%s, %s, cl_int.sizeof, &i);
    assert(runtime.status == CL_SUCCESS, "clSetKernelArg " ~ cl_strerror(runtime.status));
    runtime.status = clEnqueueNDRangeKernel(runtime.queue, %s, 1, null, &global, null, 0, null, null);
    assert(runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel " ~ cl_strerror(runtime.status));
  }
},

template_antidiagonal_rectangular_blocks_invoke_kernel = q{
  auto max_blocks = ((%s) - 1) / (%s);
  for (int i = 0; i < 2 * max_blocks - 1; ++i)
  {
    cl_int br = (i < max_blocks) ? i :     max_blocks - 1;
    cl_int bc = (i < max_blocks) ? 0 : i - max_blocks + 1;
    size_t wgroup = %s;
    size_t global = (%s) * min(br + 1, max_blocks - bc);
    runtime.status = clSetKernelArg(%s, %d, cl_int.sizeof, &bc);
    assert(runtime.status == CL_SUCCESS, "clSetKernelArg " ~ cl_strerror(runtime.status));
    runtime.status = clSetKernelArg(%s, %d, cl_int.sizeof, &br);
    assert(runtime.status == CL_SUCCESS, "clSetKernelArg " ~ cl_strerror(runtime.status));
    runtime.status = clEnqueueNDRangeKernel(runtime.queue, %s, 1, null, &global, &wgroup, 0, null, null);
    assert(runtime.status == CL_SUCCESS, "clEnqueueNDRangeKernel " ~ cl_strerror(runtime.status));
  }
},

template_shared_memory_data_init = q{
  for (int %s = 0; %s < (%s); ++%s)
    for (int %s = 0; %s < (%s); %s += (%s))
      if ((%s) + (%s) < (%s))
        %s[(%s) * (%s) + (%s) + (%s)] =
        %s[(%s) * (%s) + (%s) + (%s)];
  barrier(CLK_LOCAL_MEM_FENCE);
},

template_shared_memory_fini = q{
  for (int %s = 0; %s < %s; ++%s)
    for (int %s = 0; %s < (%s); %s += (%s))
      if ((%s) + (%s) < (%s))
        %s[(%s) * (%s) + (%s) + (%s)] =
        %s[(%s) * (%s) + (%s) + (%s)];
},

template_antidiagonal_loop_prefix = "
  for (int %s = 0; %s < (%s) + (%s) - 1; ++%s)
  {
",

template_shared_index_recalculation = "
    int %s = (%s) + (%s) + ((%s) < (%s) ? 0 : (%s) - (%s) + 1);
    int %s = (%s) - (%s) + ((%s) < (%s) ? (%s) : (%s) - 1);
",

template_antidiagonal_loop_suffix = "
    barrier(CLK_LOCAL_MEM_FENCE);
  }
",

template_2d_index = "(%s) * (%s) + (%s)";
