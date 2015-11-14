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

#include <cmath>
#include <unistd.h> // for getopt
#include "common.hpp"

#define BLOCK_SIZE 16
#define ETA        0.3f
#define MOMENTUM   0.3f
#define ONEF       1.0f

#define CL_FP cl_float

class Application
{
  static const int SEED = 1;
  static constexpr CL_FP ERROR = 0.0001f;

public:

  Application(size_t size, cl_uint device, cl_uint platform, vector<const char*> kernel_names)
    : input_n(size), hidden_n(BLOCK_SIZE), output_n(1)
    , cec_(platform, device, "Kernels.cl", kernel_names)
    , context_(cec_.get_context())
    , queue_(cec_.get_queue())
    , kernels_(cec_.get_kernels())
  {
    srandom(SEED);
    input_units    = alloc_1d(input_n + 1);
    input_weights  = alloc_2d(input_n + 1, hidden_n + 1);
    input_changes  = alloc_2d(input_n + 1, hidden_n + 1);
    hidden_units   = alloc_1d(hidden_n + 1);
    hidden_deltas  = alloc_1d(hidden_n + 1);
    hidden_weights = alloc_2d(hidden_n + 1, output_n + 1);
    hidden_changes = alloc_2d(hidden_n + 1, output_n + 1);
    output_units   = alloc_1d(output_n + 1);
    output_deltas  = alloc_1d(output_n + 1);
    target         = alloc_1d(output_n + 1);
    randomize_1d(input_units, input_n);
    randomize_2d(input_weights, input_n, hidden_n);
    randomize_2d(hidden_weights, hidden_n, output_n);
    randomize_1d(target, output_n);
    zero_2d(input_changes, input_n, hidden_n);
    zero_2d(hidden_changes, hidden_n, output_n);
    num_blocks     = input_n / BLOCK_SIZE;
    partial_sum    = new CL_FP[num_blocks * BLOCK_SIZE];
  }

  ~Application()
  {
    delete [] partial_sum;
    free(input_units);
    free(input_weights[0]);
    free(input_weights);
    free(input_changes[0]);
    free(input_changes);
    free(hidden_units);
    free(hidden_deltas);
    free(hidden_weights[0]);
    free(hidden_weights);
    free(hidden_changes[0]);
    free(hidden_changes);
    free(output_units);
    free(output_deltas);
    free(target);
  }

  CL_FP* alloc_1d(int n) throw (string)
  {
    CL_FP *new_t = (CL_FP*) malloc(n * sizeof(CL_FP));
    if (nullptr == new_t) throw string("BPNN::alloc_1d: couldn't allocate FPs");
    return new_t;
  }

  CL_FP** alloc_2d(int m, int n) throw (string)
  {
    CL_FP **new_t = (CL_FP**) malloc(m * sizeof(CL_FP*));
    if (nullptr == new_t) throw string("BPNN::alloc_2d: couldn't allocate ptrs");
    new_t[0] = (CL_FP*) malloc(m * n * sizeof(CL_FP));
    if (nullptr == new_t[0]) throw string("BPNN::alloc_2d: couldn't allocate FPs");
    for (int ii = 1; ii < m; ++ii) new_t[ii] = new_t[0] + ii * n;
    return new_t;
  }

  void randomize_1d(CL_FP *w, int m)
  {
    for (int ii = 0; ii <= m; ++ii)
    {
      CL_FP rr = random();
      w[ii] = rr / RAND_MAX;
    }
  }

  void randomize_2d(CL_FP **w, int m, int n)
  {
    for (int ii = 0; ii <= m; ++ii)
      for (int jj = 0; jj <= n; ++jj)
      {
        CL_FP rr = 0.00001f * random();
        w[ii][jj] = rr / RAND_MAX;
      }
  }

  void zero_1d(CL_FP *w, int m)
  {
    for (int ii = 0; ii <= m; ++ii) w[ii] = 0.0;
  }

  void zero_2d(CL_FP **w, int m, int n)
  {
    for (int ii = 0; ii <= m; ++ii)
      for (int jj = 0; jj <= n; ++jj)
        w[ii][jj] = 0.0;
  }

  void layerforward(CL_FP *l1, CL_FP *l2, CL_FP **conn, int n1, int n2)
  {
    l1[0] = ONEF;                   // Set up thresholding unit
    for (int j = 1; j <= n2; ++j) // For each unit in second layer
    {                               // compute weighted sum of its inputs
      CL_FP sum = conn[0][j];
      for (int i = 1; i <= n1; ++i) sum += conn[i][j] * l1[i];
      l2[j] = ONEF / (ONEF + exp(-sum));
    }
  }

  // this version of layerforward performs additions in exactly the
  // same order as the kernel version does.
  void layerforward_parallel(CL_FP *l1, CL_FP *l2, CL_FP **conn, int n1, int n2)
  {
    float *work = new float[(n1 + 1) * (n2 + 1)];
    l1[0] = ONEF;
    for (int r = 1; r <= n1; ++r)
      for (int c = 1; c <= n2; ++c)
        work[r * (n2 + 1) + c] = l1[r] * conn[r][c];
    for (int k = 1; k < n2; k *= 2)
      for (int i = 1; i <= n1; ++i)
        if ((i - 1) % (2 * k) == 0)
          for (int j = 1; j <= n2; ++j)
            work[i * (n2 + 1) + j] += work[(i + k) * (n2 + 1) + j];
    for (int j = 1; j <= n2; ++j)
    {
      CL_FP sum = conn[0][j];
      for (int i = 1; i <= n1; i += n2)
        sum += work[i * (n2 + 1) + j];
      l2[j] = ONEF / (ONEF + exp(-sum));
    }
    delete [] work;
  }

  void output_error(CL_FP *err)
  {
    CL_FP errsum = 0.0;
    for (size_t ii = 1; ii <= output_n; ++ii)
    {
      CL_FP o = output_units[ii];
      output_deltas[ii] = -o * (ONEF - o) * (target[ii] - o);
      errsum += fabs(output_deltas[ii]);
    }
    *err = errsum;
  }

  void hidden_error(CL_FP *err)
  {
    CL_FP errsum = 0.0;
    for (size_t ii = 1; ii <= hidden_n; ++ii)
    {
      CL_FP sum = 0.0;
      for (size_t jj = 1; jj <= output_n; ++jj) sum += output_deltas[jj] * hidden_weights[ii][jj];
      hidden_deltas[ii] = hidden_units[ii] * (ONEF - hidden_units[ii]) * sum;
      errsum += fabs(hidden_deltas[ii]);
    }
    *err = errsum;
  }

  void adjust_weights(CL_FP *deltas, int ndelta, CL_FP *o, int no, CL_FP **weights, CL_FP **changes)
  {
    o[0] = ONEF;
    for (int ii = 1; ii <= ndelta; ++ii)
      for (int jj = 0; jj <= no; ++jj)
      {
        changes[jj][ii] = MOMENTUM * changes[jj][ii] + (ONEF - MOMENTUM) * ETA * o[jj] * deltas[ii];
        weights[jj][ii] += changes[jj][ii];
      }
  }

  bool train_kernel(CL_FP *eo, CL_FP *eh)
  {
    bool valid = true;
    cl_int status;
    cl_mem d_input_units   = clCreateBuffer(context_, CL_MEM_READ_WRITE, (input_n + 1)                    * sizeof(CL_FP), nullptr, &status);
    cl_mem d_input_weights = clCreateBuffer(context_, CL_MEM_READ_WRITE, (input_n + 1) * (hidden_n + 1) * sizeof(CL_FP), nullptr, &status);
    cl_mem d_partial_sum   = clCreateBuffer(context_, CL_MEM_READ_WRITE, input_n                            * sizeof(CL_FP), nullptr, &status);
    status = clEnqueueWriteBuffer(queue_, d_input_units  , CL_TRUE, 0, (input_n + 1)                    * sizeof(CL_FP), input_units     , 0, nullptr, nullptr);
    status = clEnqueueWriteBuffer(queue_, d_input_weights, CL_TRUE, 0, (input_n + 1) * (hidden_n + 1) * sizeof(CL_FP), input_weights[0], 0, nullptr, nullptr);
    clSetKernelArg(kernels_[0], 0, sizeof(cl_mem)                     , &d_input_units  );
    clSetKernelArg(kernels_[0], 1, sizeof(cl_mem)                     , &d_input_weights);
    clSetKernelArg(kernels_[0], 2, sizeof(cl_mem)                     , &d_partial_sum  );
    clSetKernelArg(kernels_[0], 3, sizeof(CL_FP) * hidden_n           , nullptr         );
    clSetKernelArg(kernels_[0], 4, sizeof(CL_FP) * hidden_n * hidden_n, nullptr         );
    clSetKernelArg(kernels_[0], 5, sizeof(int)                        , &hidden_n       );
    size_t global_work[] = { hidden_n, input_n };
    size_t local_work[]  = { hidden_n, hidden_n };
    status = clEnqueueNDRangeKernel(queue_, kernels_[0], 2, nullptr, global_work, local_work, 0, nullptr, nullptr);
    status = clEnqueueReadBuffer(queue_, d_partial_sum, CL_TRUE, 0, input_n * sizeof(CL_FP), partial_sum, 0, nullptr, nullptr);
    for (size_t j = 1; j <= hidden_n; ++j)
    {
      CL_FP sum = input_weights[0][j];
      for (size_t k = 0; k < num_blocks; ++k) sum += partial_sum[k * hidden_n + j - 1];
      hidden_units[j] = ONEF / (ONEF + exp(-sum));
    }
    clReleaseMemObject(d_partial_sum);
    layerforward(hidden_units, output_units, hidden_weights, hidden_n, output_n);
    output_error(eo);
    hidden_error(eh);
    // VALIDATION BLOCK START
    CL_FP h_eo, h_eh;
    CL_FP* backup_hidden_units = new CL_FP[hidden_n + 1];
    memcpy(backup_hidden_units, hidden_units, sizeof(CL_FP) * (hidden_n + 1));
    layerforward_parallel(input_units, hidden_units, input_weights, input_n, hidden_n);
    layerforward(hidden_units, output_units, hidden_weights, hidden_n, output_n);
    output_error(&h_eo);
    hidden_error(&h_eh);
    //if (fabs(h_eo - *eo) > ERROR || fabs(h_eh - *eh) > ERROR)
    if (h_eo != *eo || h_eh != *eh)
    {
      printf("%.12f %.12f\n%.12f %.12f\n", *eo, *eh, h_eo, h_eh);
      valid = false;
    }
    for (size_t ii = 0; ii <= hidden_n; ++ii)
      if (fabs(backup_hidden_units[ii] - hidden_units[ii]) > ERROR)
        //if (backup_hidden_units[ii] != hidden_units[ii])
      {
        printf("%2lu %.12f %.12f\n", ii, backup_hidden_units[ii], hidden_units[ii]);
        valid = false;
        break;
      }
    delete [] backup_hidden_units;
    float **backup_input_weights = alloc_2d(input_n + 1, hidden_n + 1);
    float **backup_input_changes = alloc_2d(input_n + 1, hidden_n + 1);
    zero_2d(input_changes, input_n, hidden_n);
    for (size_t i = 0; i <= input_n; ++i)
      for (size_t j = 0; j <= hidden_n; ++j)
      {
        backup_input_weights[i][j] = input_weights[i][j];
        backup_input_changes[i][j] = 0.0f;
      }
    // VALIDATION BLOCK END
    adjust_weights(output_deltas, output_n, hidden_units, hidden_n, hidden_weights, hidden_changes);
    cl_mem d_hidden_deltas = clCreateBuffer(context_, CL_MEM_READ_WRITE, (hidden_n + 1)                   * sizeof(CL_FP), nullptr, &status);
    cl_mem d_input_changes = clCreateBuffer(context_, CL_MEM_READ_WRITE, (input_n + 1) * (hidden_n + 1) * sizeof(CL_FP), nullptr, &status);
    status = clEnqueueWriteBuffer(queue_, d_hidden_deltas, CL_TRUE, 0, (hidden_n + 1)                   * sizeof(CL_FP), hidden_deltas   , 0, nullptr, nullptr);
    status = clEnqueueWriteBuffer(queue_, d_input_weights, CL_TRUE, 0, (input_n + 1) * (hidden_n + 1) * sizeof(CL_FP), input_weights[0], 0, nullptr, nullptr);
    status = clEnqueueWriteBuffer(queue_, d_input_changes, CL_TRUE, 0, (input_n + 1) * (hidden_n + 1) * sizeof(CL_FP), input_changes[0], 0, nullptr, nullptr);
    clSetKernelArg(kernels_[1], 0, sizeof(cl_mem), &d_hidden_deltas);
    clSetKernelArg(kernels_[1], 1, sizeof(cl_mem), &d_input_units);
    clSetKernelArg(kernels_[1], 2, sizeof(cl_mem), &d_input_weights);
    clSetKernelArg(kernels_[1], 3, sizeof(cl_mem), &d_input_changes);
    status = clEnqueueNDRangeKernel(queue_, kernels_[1], 2, nullptr, global_work, nullptr, 0, nullptr, nullptr);
    status = clEnqueueReadBuffer(queue_, d_input_weights, CL_TRUE, 0, (input_n + 1) * (hidden_n + 1) * sizeof(CL_FP), input_weights[0], 0, nullptr, nullptr);
    status = clEnqueueReadBuffer(queue_, d_input_changes, CL_TRUE, 0, (input_n + 1) * (hidden_n + 1) * sizeof(CL_FP), input_changes[0], 0, nullptr, nullptr);
    // VALIDATION BLOCK START
    adjust_weights(hidden_deltas, hidden_n, input_units, input_n, backup_input_weights, backup_input_changes);
    for (size_t i = 1; i <= input_n; ++i)
      for (size_t j = 1; j <= hidden_n; ++j)
        //        if (fabs(backup_input_weights[i][j] - input_weights[i][j]) > Config::ERROR)
        if (backup_input_weights[i][j] != input_weights[i][j])
        {
          printf("%6lu:%2lu %.12f %.12f\n", i, j, backup_input_weights[i][j], input_weights[i][j]);
          valid = false;
          break;
        }
    free(backup_input_weights[0]);
    free(backup_input_changes[0]);
    free(backup_input_weights);
    free(backup_input_changes);
    // VALIDATION BLOCK END
    clReleaseMemObject(d_input_units);
    clReleaseMemObject(d_input_weights);
    clReleaseMemObject(d_hidden_deltas);
    clReleaseMemObject(d_input_changes);
    return valid;
  }

  void run()
  {
    CL_FP out_err, hid_err;
    double t1 = cec_.gettime();
    if (!train_kernel(&out_err, &hid_err))
    {
      fprintf(stdout, "bp: out error %f, hidden error %f\n", out_err, hid_err);
    }
    double t2 = cec_.gettime();
    fprintf(stdout, "OPENCL %f [s]\n", t2 - t1);
  }

  size_t input_n;              // number of input units
  size_t hidden_n;             // number of hidden units
  size_t output_n;             // number of output units
  CL_FP *input_units;          // the input units
  CL_FP **input_weights;       // weights from input to hidden layer
  CL_FP **input_changes;
  CL_FP *hidden_units;         // the hidden units
  CL_FP *hidden_deltas;        // storage for hidden unit error
  CL_FP **hidden_weights;      // weights from hidden to output layer
  CL_FP **hidden_changes;
  CL_FP *output_units;         // the output units
  CL_FP *output_deltas;        // storage for output unit error
  CL_FP *target;               // storage for target vector
  CL_FP *partial_sum;
  size_t num_blocks;

  clop_examples_common cec_;
  cl_context context_;
  cl_command_queue queue_;
  vector<cl_kernel> kernels_;
}; // Application class

static void usage(char **argv)
{
  const char *help = "Usage: %s [-p <platform>] [-d <device>] <size>\n"
                     "\t<size>    - number of input elements, must be divisible by %d\n";
  fprintf(stderr, help, argv[0], BLOCK_SIZE);
  exit(EXIT_FAILURE);
}

int
main(int argc, char** argv)
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
  if ((size = atoi(argv[0])) % BLOCK_SIZE != 0)
  {
    fprintf(stderr, "bp: the size (%zu) must be divisible by %d\n", size, BLOCK_SIZE);
    exit(EXIT_FAILURE);
  }

  try
  {
    vector<const char*> kernel_names = {"bpnn_layerforward", "bpnn_adjust_weights"};
    Application app(size, device_index, platform_index, kernel_names);
    app.run();
  }
  catch (string msg)
  {
    std::cerr << "bp: " << msg << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

// Local Variables:
// company-c-headers-path-user: ("../../include")
// End:
