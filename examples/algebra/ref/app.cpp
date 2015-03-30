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

#include <unistd.h>
#include "common.hpp"

class Application {

  static const int SEED = 1;

public:

  Application(size_t size, cl_uint device, cl_uint platform, vector<const char*> kernel_names)
    : size_(size), cec_(platform, device, "Kernels.cl", kernel_names)
  {
    kernels_ = cec_.get_kernels();
    context_ = cec_.get_context();
    queue_ = cec_.get_queue();
    if (nullptr == (A = (cl_float*) calloc(size_, sizeof(cl_float))))
    {
      throw string("ERROR can't allocate alignment A");
    }
    if (nullptr == (B = (cl_float*) calloc(size_, sizeof(cl_float))))
    {
      throw string("ERROR can't allocate alignment B");
    }
    if (nullptr == (C = (cl_float*) calloc(size_, sizeof(cl_float))))
    {
      throw string("ERROR can't allocate alignment C");
    }
    srandom(SEED);
    const long normal = 0x7fffffff;
    for (int ii = 0; ii < size_; ++ii)
    {
      A[ii] = (cl_float) random() / normal;
      B[ii] = (cl_float) random() / normal;
    }
  }

  ~Application()
  {
    free(A);
    free(B);
    free(C);
  }

  void compute()
  {
    cl_int status;
    cl_mem A_d = clCreateBuffer(context_, CL_MEM_READ_ONLY, size_ * sizeof(cl_float), nullptr, &status);
    cec_.check_status(status, "compute clCreateBuffer 1 ");
    cl_mem B_d = clCreateBuffer(context_, CL_MEM_READ_ONLY, size_ * sizeof(cl_float), nullptr, &status);
    cec_.check_status(status, "compute clCreateBuffer 2 ");
    cl_mem C_d = clCreateBuffer(context_, CL_MEM_WRITE_ONLY, size_ * sizeof(cl_float), nullptr, &status);
    cec_.check_status(status, "compute clCreateBuffer 3 ");
    status = clEnqueueWriteBuffer(queue_, A_d, CL_TRUE, 0, size_ * sizeof(cl_float), A, 0, nullptr, nullptr);
    cec_.check_status(status, "compute clEnqueueWriteBuffer 1 ");
    status = clEnqueueWriteBuffer(queue_, B_d, CL_TRUE, 0, size_ * sizeof(cl_float), B, 0, nullptr, nullptr);
    cec_.check_status(status, "compute clEnqueueWriteBuffer 2 ");

    clSetKernelArg(kernels_[0], 0, sizeof(cl_mem), &A_d);
    clSetKernelArg(kernels_[0], 1, sizeof(cl_mem), &B_d);
    clSetKernelArg(kernels_[0], 2, sizeof(cl_mem), &C_d);

    status = clEnqueueNDRangeKernel(queue_, kernels_[0], 1, nullptr, &size_, nullptr, 0, nullptr, nullptr);
    cec_.check_status(status, "compute clEnqueueNDRangeKernel ");

    status = clEnqueueReadBuffer(queue_, C_d, CL_TRUE, 0, size_ * sizeof(cl_float), C, 0, nullptr, nullptr);
    cec_.check_status(status, "compute clEnqueueReadBuffer ");
    clReleaseMemObject(A_d);
    clReleaseMemObject(B_d);
    clReleaseMemObject(C_d);
  }

  void validate()
  {
    int diffs = 0;
    for (int ii = 0; ii < size_; ++ii)
      if (C[ii] != A[ii] + B[ii])
        ++diffs;
    if (0 != diffs)
      fprintf(stdout, "DIFFS %d\n", diffs);
  }

  void run()
  {
    double t1 = cec_.gettime();
    compute();
    double t2 = cec_.gettime();
    validate();
    fprintf(stdout, "OPENCL %f s\n", t2 - t1);
  }

private:

  cl_float* A;
  cl_float* B;
  cl_float* C;
  size_t size_;
  ClopExamplesCommon cec_;
  cl_context context_;
  cl_command_queue queue_;
  vector<cl_kernel> kernels_;
};

void usage(char** argv)
{
  const char* help = "Usage: %s [-p <platform>] [-d <device>] <size>\n"
                     "\t<size>    - sequence length\n";
  fprintf(stderr, help, argv[0]);
  exit(EXIT_FAILURE);
}

int main(int argc, char** argv)
{
  int ch;
  size_t size;
  cl_uint device_index = 0;
  cl_uint platform_index = 0;
  while ((ch = getopt(argc, argv, "d:p:")) != -1)
  {
    switch (ch)
    {
    case 'd': sscanf(optarg, "%d", &device_index);   break;
    case 'p': sscanf(optarg, "%d", &platform_index); break;
    default:  usage(argv);
    }
  }
  argc -= optind;
  argv += optind;
  if (argc < 1)
  {
    usage(argv);
  }
  size = atoi(argv[0]);

  try
  {
    vector<const char*> kernel_names = {"vector_sum"};
    Application app(size, device_index, platform_index, kernel_names);
    app.run();
  }
  catch (string msg)
  {
    std::cerr << "algebra: " << msg << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
