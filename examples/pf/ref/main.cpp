#include <cassert>
#include <algorithm>
#include <exception>
#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <unistd.h>

#include "common.hpp"

using namespace std;

/**
 *  possible command line options
 *   100 100000 20
 *   200 100000 20
 *   400 100000 20
 *   800 100000 20
 *  1600 100000 20
 */

class Application {

  static const unsigned HALO = 3; // halo width along one direction when advancing to the next iteration

public:

  Application(int argc, char** argv) : rows(100), cols(100000), height(20), setup{"Kernels.cl", {"pathfinder"}}
  {
    cl_uint platform = 0;
    cl_uint device = 0;
    int ch;
    while ((ch = getopt(argc, argv, "d:p:")) != -1)
    {
      switch (ch)
      {
      case 'd': sscanf(optarg, "%d", &device);   break;
      case 'p': sscanf(optarg, "%d", &platform); break;
      default:  usage(argv[0]);
      }
    }
    if (argc - optind < 3)
      usage(argv[0]);
    argv  += optind;
    rows   = atoi(argv[0]);
    cols   = atoi(argv[1]);
    height = atoi(argv[2]);

    data.reset(new int[rows * cols]);
    result.reset(new int[cols]);
    uniform_int_distribution<unsigned> u(0, 10);
    default_random_engine e;
    for (auto ii = 0; ii < rows; ++ii)
    {
      auto p = data.get() + cols * ii;
      for (auto jj = 0; jj < cols; ++jj)
        p[jj] = u(e);
    }
    setup.reset(platform, device, "");
    kernels = setup.get_kernels();
    context = setup.get_context();
    queue = setup.get_queue();
  }

  void run()
  {
    auto t1 = setup.gettime();
    size_t gwsize = rows * cols;
    size_t lwsize;
    clGetKernelWorkGroupInfo(kernels[0], setup.get_device(), CL_KERNEL_WORK_GROUP_SIZE,
                             sizeof(size_t), &lwsize, nullptr);
    if (lwsize == 0)
    {
      cerr << "ERROR: max work group size is zero!\n";
      abort();
    }
    if (lwsize > gwsize)
    {
      lwsize = gwsize;
    }
    else
    {
      auto nthreads = lwsize;
      for (nthreads = lwsize; gwsize % nthreads != 0; --nthreads);
      lwsize = nthreads;
    }
    cl_mem d_result[2];
    auto d_wall = clCreateBuffer(context, CL_MEM_READ_ONLY  | CL_MEM_COPY_HOST_PTR,
                                 sizeof(int) * (gwsize - cols), data.get() + cols, nullptr);
    d_result[0] = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
                                 sizeof(int) * cols, data.get(), nullptr);
    d_result[1] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int) * cols, nullptr, nullptr);

    auto border = height * HALO;
    auto halo = HALO;
    auto src = 0;
    for (auto h = 0; h < rows - 1; h += height)
    {
      auto iteration = min(height, rows - h - 1);
      auto dst = 1 - src;

      clSetKernelArg(kernels[0],  0, sizeof(int)   , &iteration);
      clSetKernelArg(kernels[0],  1, sizeof(cl_mem), &d_wall);
      clSetKernelArg(kernels[0],  2, sizeof(cl_mem), &d_result[src]);
      clSetKernelArg(kernels[0],  3, sizeof(cl_mem), &d_result[dst]);
      clSetKernelArg(kernels[0],  4, sizeof(int)   , &cols);
      clSetKernelArg(kernels[0],  5, sizeof(int)   , &rows);
      clSetKernelArg(kernels[0],  6, sizeof(int)   , &h);
      clSetKernelArg(kernels[0],  7, sizeof(int)   , &border);
      clSetKernelArg(kernels[0],  8, sizeof(int)   , &halo);
      clSetKernelArg(kernels[0],  9, sizeof(int) * lwsize, nullptr);
      clSetKernelArg(kernels[0], 10, sizeof(int) * lwsize, nullptr);

      clEnqueueNDRangeKernel(queue, kernels[0], 1, nullptr, &gwsize, &lwsize, 0, nullptr, nullptr);

      src = dst;
    }

    // Copy results back to host.
    // setup.queue           - command queue.
    // config.result[src]    - result on the device.
    // CL_TRUE               - blocking
    // 0                     - offset
    // sizeof(cl_int) * cols - size to copy
    // result                - pointer to the memory on the host
    // 0                     - number of events in wait list
    // NULL                  - event wait list
    // &event                - event object for profiling data access
    clEnqueueReadBuffer(queue, d_result[src], CL_TRUE, 0, sizeof(int) * cols, result.get(), 0, nullptr, nullptr);

    clReleaseMemObject(d_wall);
    clReleaseMemObject(d_result[0]);
    clReleaseMemObject(d_result[1]);
    auto t2 = setup.gettime();
    cout << "TIME " << t2 - t1 << endl;
  }

private:

  unique_ptr<int[]> data;
  unique_ptr<int[]> result;
  int rows, cols, height;

  cl_context context;
  cl_command_queue queue;
  vector<cl_kernel> kernels;
  clop_examples_common setup;

  void usage(const char* name)
  {
    cout << "Usage: " << name << " [-p <platform>] [-d <device>] rows columns height\n"
         << "  rows:     maximum number of centers allowed\n"
         << "  columns:  minimum number of centers allowed\n"
         << "  height:   dimension of each data point\n";
    exit(EXIT_FAILURE);
  }

};

int main(int argc, char** argv)
{
  try
  {
    Application app(argc, argv);
    app.run();
  }
  catch (exception& e)
  {
    cerr << "pf: " << e.what() << endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

// Local Variables:
// company-c-headers-path-user: ("../../include")
// End:
