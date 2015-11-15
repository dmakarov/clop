#include <cassert>
#include <algorithm>
#include <exception>
#include <iostream>
#include <fstream>
#include <memory>
#include <random>
#include <string>
#include <unistd.h>

#include "common.hpp"

using namespace std;

/**
 *  possible command line options
 *   128 131072 20
 *   256 131072 20
 *   512 131072 20
 *  1024 131072 20
 *  2048 131072 20
 */

class Application {

  static const unsigned HALO = 2; // halo width along one direction when advancing to the next iteration

public:

  Application(int argc, char** argv) : rows(128), cols(131072), height(20),
                                       setup{"Kernels.cl", {"pathfinder"}}
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
    rows   = clp2(atoi(argv[0]));
    cols   = clp2(atoi(argv[1]));
    height = atoi(argv[2]);

    data.reset(new int[rows * cols]);
    results.reset(new int[cols]);
    default_random_engine e;
    uniform_int_distribution<unsigned> u(0, 9);
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
    auto lwsize = setup.get_kernel_work_group_size(0);
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
    cl_mem d_results[2];
    auto d_wall = clCreateBuffer(context, CL_MEM_READ_ONLY  | CL_MEM_COPY_HOST_PTR,
                                 sizeof(int) * (gwsize - cols), data.get() + cols, nullptr);
    d_results[0] = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
                                  sizeof(int) * cols, data.get(), nullptr);
    d_results[1] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int) * cols, nullptr, nullptr);

    auto border = height * HALO;
    auto halo = HALO;
    auto src = 0;
    for (auto h = 0; h < rows - 1; h += height)
    {
      auto iteration = min(height, rows - h - 1);
      auto dst = 1 - src;

      clSetKernelArg(kernels[0],  0, sizeof(int)   , &iteration);
      clSetKernelArg(kernels[0],  1, sizeof(cl_mem), &d_wall);
      clSetKernelArg(kernels[0],  2, sizeof(cl_mem), &d_results[src]);
      clSetKernelArg(kernels[0],  3, sizeof(cl_mem), &d_results[dst]);
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

    clEnqueueReadBuffer(queue, d_results[src], CL_TRUE, 0, sizeof(int) * cols,
                        results.get(), 0, nullptr, nullptr);

    clReleaseMemObject(d_wall);
    clReleaseMemObject(d_results[0]);
    clReleaseMemObject(d_results[1]);
    auto t2 = setup.gettime();
    cout << "TIME " << t2 - t1 << endl;
    dump();
  }

private:

  unique_ptr<int[]> data;
  unique_ptr<int[]> results;
  unsigned rows, cols, height;

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

  void dump()
  {
    ofstream out("pf.out");
    if (out)
    {
      for (auto r = 0; r < rows; ++r)
      {
        const auto* p = data.get() + r * cols;
        for (auto c = 0; c < cols; ++c)
          out << p[c];
        out << endl;
      }
      out << "  " << results[0];
      for (auto c = 1; c < cols; ++c)
      {
        out << " " << results[c];
        if (c % 32 == 31 && c + 1 != cols)
          out << endl << " ";
      }
      out << endl;
    }
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
