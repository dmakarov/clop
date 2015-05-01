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
module clop.ct.templates;

version (UNITTEST_DEBUG) import std.stdio;
import std.format, std.string, std.variant;
import pegged.grammar;

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
    static clop.rt.instance.Instance instance_%s;
    if (instance_%s is null)
    {
      instance_%s = new clop.rt.instance.Instance("%s");
      runtime.register_instance(instance_%s);
    }
    if (!instance_%s.ready)
    {
          // make list of kernel parameters
          %s
          instance_%s.prepare_resources("%s", %s);
    }
},

template_plain_invoke_kernel = q{
          // kernel invocation

          size_t[] offset = %s, global = %s;
          runtime.status = clEnqueueNDRangeKernel(runtime.queue, %s, %s, offset.ptr, global.ptr, null, 0, null, null);
          assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueNDRangeKernel"));
},

template_antidiagonal_invoke_kernel = q{
  foreach (i; 2 .. 2 * %s - 1)
  {
    size_t global = (i < %s) ? i - 1 : 2 * %s - i - 1;
    runtime.status = clSetKernelArg(%s, %s, cl_int.sizeof, &i);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg in antidiagonal loop"));
    runtime.status = clEnqueueNDRangeKernel(runtime.queue, %s, 1, null, &global, null, 0, null, null);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueNDRangeKernel"));
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
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg"));
    runtime.status = clSetKernelArg(%s, %d, cl_int.sizeof, &br);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clSetKernelArg"));
    runtime.status = clEnqueueNDRangeKernel(runtime.queue, %s, 1, null, &global, &wgroup, 0, null, null);
    assert(runtime.status == CL_SUCCESS, cl_strerror(runtime.status, "clEnqueueNDRangeKernel"));
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

struct ReduceSnippet
{
  private static immutable reduce_snippet = q{
    barrier(CLK_LOCAL_MEM_FENCE);
    int clop_local_thread_id = get_local_id(0);
    for (int i = 1; i < ${a.length}; i = i * 2)
    {
      if (clop_local_thread_id % (2 * i) == 0)
        ${a}[clop_local_thread_id]
              = ${func}(${b}[clop_local_thread_id    ],
                        ${b}[clop_local_thread_id + i]);
      barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (clop_local_thread_id == 0)
      result = func(${a}, ${b}[0]);
  };

  string instantiate_template(string function_literal, ParseTree argument_list)
  {
    version (UNITTEST_DEBUG)
    {
      writefln("UT: ReduceSnippet.instantiate_template: %s", argument_list.matches);
    }
    return reduce_snippet;
  }
}

struct MapSnippet
{
  private static immutable map_snippet = q{
  };

  string instantiate_template(string function_literal, ParseTree argument_list)
  {
    version (UNITTEST_DEBUG)
    {
      writefln("UT: MapSnippet.instantiate_template: %s", argument_list.matches);
    }
    return map_snippet;
  }
}

// snippet container is wrapper around other snippet types, so that
// specific snippets can be used without knowing their respective
// types.
struct SnippetContainer(T)
{
  private T SnippetContainer_payload;

  this(T bind_to)
  {
    SnippetContainer_payload = bind_to;
  }

  @property T SnippetContainer_get()
  {
    return SnippetContainer_payload;
  }

  alias SnippetContainer_get this;

  string binary_function(string function_literal, string type = "float", string a = "a", string b = "b")
  {
    // FIXME: generate unique function name
    auto name = "clop_binary_function";
    auto func = chomp(chompPrefix(function_literal, `"`), `"`);
    auto definition = format(q{
        %s %s(%s %s, %s %s)
        {return (%s);}
      }, type, name, type, a, type, b, func);
    return definition;
  }
}

// I need this type to be able to check that there is a member with a
// specific name.  In typed enum all members must be of the same type
enum ExpansionPattern : Algebraic!(SnippetContainer!ReduceSnippet, SnippetContainer!MapSnippet)
{
  reduce = Algebraic!(SnippetContainer!ReduceSnippet, SnippetContainer!MapSnippet)(SnippetContainer!ReduceSnippet()),
  map    = Algebraic!(SnippetContainer!ReduceSnippet, SnippetContainer!MapSnippet)(SnippetContainer!MapSnippet())
}
