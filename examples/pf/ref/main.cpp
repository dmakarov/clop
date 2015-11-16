#include <cassert>
#include <algorithm>
#include <exception>
#include <iomanip>
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
    mt19937 e;
    e.seed(1);
    uniform_int_distribution<unsigned> u(0, 9);
    for (auto ii = 0; ii < rows; ++ii)
    {
      auto p = data.get() + cols * ii;
      for (auto jj = 0; jj < cols; ++jj)
        p[jj] = u(e);
    }
    copy_n(data.get(), cols, results.get());
    setup.reset(platform, device, "");
    kernels = setup.get_kernels();
    context = setup.get_context();
    queue = setup.get_queue();
  }

  void run()
  {
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
    auto t1 = setup.gettime();
    cl_mem d_results[2];
    auto flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
    auto d_data = clCreateBuffer(context, flags, sizeof(int) * rows * cols, data.get(), nullptr);
    flags = CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR;
    d_results[0] = clCreateBuffer(context, flags, sizeof(int) * cols, results.get(), nullptr);
    d_results[1] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int) * cols, nullptr, nullptr);

    auto src = 0;
    for (auto r = 1; r < rows; r += height)
    {
      auto steps = min(height, rows - r);
      auto dst = 1 - src;

      clSetKernelArg(kernels[0], 0, sizeof(cl_mem), &d_data);
      clSetKernelArg(kernels[0], 1, sizeof(cl_mem), &d_results[src]);
      clSetKernelArg(kernels[0], 2, sizeof(cl_mem), &d_results[dst]);
      clSetKernelArg(kernels[0], 3, sizeof(int)   , &rows);
      clSetKernelArg(kernels[0], 4, sizeof(int)   , &cols);
      clSetKernelArg(kernels[0], 5, sizeof(int)   , &height);
      clSetKernelArg(kernels[0], 6, sizeof(int)   , &r);
      clSetKernelArg(kernels[0], 7, sizeof(int)   , &steps);
      clSetKernelArg(kernels[0], 8, sizeof(int) * (lwsize + 2), nullptr);
      clSetKernelArg(kernels[0], 9, sizeof(int) * (lwsize + 2), nullptr);

      clEnqueueNDRangeKernel(queue, kernels[0], 1, nullptr, &gwsize, &lwsize, 0, nullptr, nullptr);

      src = dst;
    }

    clEnqueueReadBuffer(queue, d_results[src], CL_TRUE, 0, sizeof(int) * cols,
                        results.get(), 0, nullptr, nullptr);

    clReleaseMemObject(d_results[1]);
    clReleaseMemObject(d_results[0]);
    clReleaseMemObject(d_data);
    auto t2 = setup.gettime();
    cout << "TIME " << t2 - t1 << endl;
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
      out << "  " << setw(3) << results[0];
      for (auto c = 1; c < cols; ++c)
      {
        out << " " << setw(3) << results[c];
        if (c % 16 == 15 && c + 1 != cols)
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
