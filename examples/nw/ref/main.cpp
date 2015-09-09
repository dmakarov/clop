#include <exception>
#include <iostream>
#include <memory>
#include <random>
#include <unistd.h>

#include "common.hpp"

using namespace std;

/**
   The Needleman-Wunsch algorithm for sequence alignment.

   Compute the F matrix:

   for i=0 to length(A) F_{i,0} ← d*i
   for j=0 to length(B) F_{0,j} ← d*j
   for i=1 to length(A)
     for j=1 to length(B)
     {
       Match  ← F_{i-1,j-1} + S_{A_i,B_j}
       Delete ← F_{i-1,j} + d
       Insert ← F_{i,j-1} + d
       F_{i,j} ← max(Match, Insert, Delete)
     }

   Compute alignments:

   AlignmentA ← ""
   AlignmentB ← ""
   i ← length(A)
   j ← length(B)
   while (i > 0 or j > 0)
   {
     if (i > 0 and j > 0 and F_{i,j} == F_{i-1,j-1} + S_{A_i, B_j})
     {
       AlignmentA ← A_i + AlignmentA
       AlignmentB ← B_j + AlignmentB
       i ← i - 1
       j ← j - 1
     }
     else if (i > 0 and F_{i,j} == F_{i-1,j} + d)
     {
       AlignmentA ← A_i + AlignmentA
       AlignmentB ← "-" + AlignmentB
       i ← i - 1
     }
     else (j > 0 and F_{i,j} == F_{i,j-1} + d)
     {
       AlignmentA ← "-" + AlignmentA
       AlignmentB ← B_j + AlignmentB
       j ← j - 1
     }
   }
*/

static int BLOSUM62[] = {
   4, -1, -2, -2,  0, -1, -1,  0, -2, -1, -1, -1, -1, -2, -1,  1,  0, -3, -2,  0, -2, -1,  0, -4,
  -1,  5,  0, -2, -3,  1,  0, -2,  0, -3, -2,  2, -1, -3, -2, -1, -1, -3, -2, -3, -1,  0, -1, -4,
  -2,  0,  6,  1, -3,  0,  0,  0,  1, -3, -3,  0, -2, -3, -2,  1,  0, -4, -2, -3,  3,  0, -1, -4,
  -2, -2,  1,  6, -3,  0,  2, -1, -1, -3, -4, -1, -3, -3, -1,  0, -1, -4, -3, -3,  4,  1, -1, -4,
   0, -3, -3, -3,  9, -3, -4, -3, -3, -1, -1, -3, -1, -2, -3, -1, -1, -2, -2, -1, -3, -3, -2, -4,
  -1,  1,  0,  0, -3,  5,  2, -2,  0, -3, -2,  1,  0, -3, -1,  0, -1, -2, -1, -2,  0,  3, -1, -4,
  -1,  0,  0,  2, -4,  2,  5, -2,  0, -3, -3,  1, -2, -3, -1,  0, -1, -3, -2, -2,  1,  4, -1, -4,
   0, -2,  0, -1, -3, -2, -2,  6, -2, -4, -4, -2, -3, -3, -2,  0, -2, -2, -3, -3, -1, -2, -1, -4,
  -2,  0,  1, -1, -3,  0,  0, -2,  8, -3, -3, -1, -2, -1, -2, -1, -2, -2,  2, -3,  0,  0, -1, -4,
  -1, -3, -3, -3, -1, -3, -3, -4, -3,  4,  2, -3,  1,  0, -3, -2, -1, -3, -1,  3, -3, -3, -1, -4,
  -1, -2, -3, -4, -1, -2, -3, -4, -3,  2,  4, -2,  2,  0, -3, -2, -1, -2, -1,  1, -4, -3, -1, -4,
  -1,  2,  0, -1, -3,  1,  1, -2, -1, -3, -2,  5, -1, -3, -1,  0, -1, -3, -2, -2,  0,  1, -1, -4,
  -1, -1, -2, -3, -1,  0, -2, -3, -2,  1,  2, -1,  5,  0, -2, -1, -1, -1, -1,  1, -3, -1, -1, -4,
  -2, -3, -3, -3, -2, -3, -3, -3, -1,  0,  0, -3,  0,  6, -4, -2, -2,  1,  3, -1, -3, -3, -1, -4,
  -1, -2, -2, -1, -3, -1, -1, -2, -2, -3, -3, -1, -2, -4,  7, -1, -1, -4, -3, -2, -2, -1, -2, -4,
   1, -1,  1,  0, -1,  0,  0,  0, -1, -2, -2,  0, -1, -2, -1,  4,  1, -3, -2, -2,  0,  0,  0, -4,
   0, -1,  0, -1, -1, -1, -1, -2, -2, -1, -1, -1, -1, -2, -1,  1,  5, -2, -2,  0, -1, -1,  0, -4,
  -3, -3, -4, -4, -2, -2, -3, -2, -2, -3, -2, -3, -1,  1, -4, -3, -2, 11,  2, -3, -4, -3, -2, -4,
  -2, -2, -2, -3, -2, -1, -2, -3,  2, -1, -1, -2, -1,  3, -3, -2, -2,  2,  7, -1, -3, -2, -1, -4,
   0, -3, -3, -3, -1, -2, -2, -3, -3,  3,  1, -2,  1, -1, -2, -2,  0, -3, -1,  4, -3, -2, -1, -4,
  -2, -1,  3,  4, -3,  0,  1, -1,  0, -3, -4,  0, -3, -3, -2,  0, -1, -4, -3, -3,  4,  1, -1, -4,
  -1,  0,  0,  1, -3,  3,  4, -2,  0, -3, -3,  1, -1, -3, -1,  0, -1, -3, -2, -2,  1,  4, -1, -4,
   0, -1, -1, -1, -2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -2,  0,  0, -2, -1, -1, -1, -1, -1, -4,
  -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4,  1
};

class Application {

  static const int BLOCK = 32;
  static const int CHARS = 24;

public:

  Application(int argc, char** argv) : setup{"Kernels.cl", {"nw1", "nw2", "rhombus"}}
  {
    cl_uint platform = 0;
    cl_uint device = 0;
    int ch;
    while ((ch = getopt(argc, argv, "d:p:")) != -1)
    {
      switch (ch)
      {
      case 'p': sscanf(optarg, "%d", &platform); break;
      case 'd': sscanf(optarg, "%d", &device);   break;
      default:  usage(argv);
      }
    }
    argc -= optind;
    argv += optind;
    if (argc < 2) usage(argv);
    rows    = atoi(argv[0]) + 1;
    cols    = atoi(argv[0]) + 1;
    penalty = atoi(argv[1]);
    if ((rows - 1) % 16 != 0)
      throw invalid_argument(string("sequence length must be a multiple of ") + to_string(BLOCK));
    F.reset(new int[rows * cols]);
    G.reset(new int[rows * cols]);
    S.reset(new int[rows * cols]);
    M.reset(new int[rows + cols]);
    N.reset(new int[cols + cols]);

    uniform_int_distribution<unsigned> u(0, CHARS - 1);
    default_random_engine e;
    for (int jj = 0; jj < cols; ++jj)
    {
      F[jj] = -penalty * jj;
      G[jj] = -penalty * jj;
      N[jj] = u(e);
    }
    for (int ii = 0; ii < rows; ++ii)
    {
      F[ii * cols] = -penalty * ii;
      G[ii * cols] = -penalty * ii;
      M[ii] = u(e);
    }
    for (int ii = 1; ii < rows; ++ii)
      for (int jj = 1; jj < cols; ++jj)
        S[ii * cols + jj] = BLOSUM62[M[ii] * CHARS + N[jj]];

    setup.reset(platform, device, string("-DBLOCK=") + to_string(BLOCK) + string(" -DCHARS=") + to_string(CHARS));
    kernels = setup.get_kernels();
    context = setup.get_context();
    queue = setup.get_queue();
  }

  void run()
  {
    compute_scores_serial();
    compute_scores_device();
    validate_results();
    compute_scores_device_rhombus();
    validate_results();
  }

private:

  unique_ptr<int[]> M, N, S, F, G;
  int rows, cols, penalty;
  cl_context context;
  cl_command_queue queue;
  vector<cl_kernel> kernels;
  clop_examples_common setup;

  void usage(char** argv)
  {
    const char *help = "Usage: %s [-p <platform>] [-d <device>] <size> <penalty>\n"
      "\t<size>    - sequence length\n"
      "\t<penalty> - gap penalty (positive integer)\n";
    fprintf(stderr, help, argv[0]);
    exit(EXIT_FAILURE);
  }

  int max3(int a, int b, int c)
  {
    int k = a > b ? a : b;
    return k > c ? k : c;
  }

  inline int I(int i, int j)
  {
    return i * cols + j;
  }

  void compute_scores_serial()
  {
    auto t1 = setup.gettime();
    for (int ii = 1; ii < rows; ++ii)
      for (int jj = 1; jj < rows; ++jj)
        G[I(ii, jj)] = max3(G[I(ii - 1, jj - 1)] + S[I(ii, jj)],
                            G[I(ii - 1, jj)] - penalty,
                            G[I(ii, jj - 1)] - penalty);
    auto t2 = setup.gettime();
    cout << "HOST CPU " << t2 - t1 << " s" << endl;
  }

  void validate_results()
  {
    size_t diffs = 0;
    for (int ii = 0; ii < rows; ++ii)
      for (int jj = 0; jj < cols; ++jj)
        if (F[I(ii, jj)] != G[I(ii, jj)])
          ++diffs;
    if (diffs)
      cerr << "DIFFS " << diffs << endl;
  }

  void compute_scores_device()
  {
    auto t1 = setup.gettime();
    auto F_d = clCreateBuffer(context, CL_MEM_READ_WRITE, cols * rows * sizeof(int), nullptr, nullptr);
    auto S_d = clCreateBuffer(context, CL_MEM_READ_WRITE, cols * rows * sizeof(int), nullptr, nullptr);
    clEnqueueWriteBuffer(queue, F_d, CL_TRUE, 0, cols * rows * sizeof(int), F.get(), 0, nullptr, nullptr);
    clEnqueueWriteBuffer(queue, S_d, CL_TRUE, 0, cols * rows * sizeof(int), S.get(), 0, nullptr, nullptr);
    int worksize = cols - 1;
    int offset_r = 0, offset_c = 0; // these two parameters are for extension use, don't worry about it.
    int block_width = worksize / BLOCK ;

    clSetKernelArg(kernels[0],  0, sizeof(cl_mem)                            , &S_d        );
    clSetKernelArg(kernels[0],  1, sizeof(cl_mem)                            , &F_d        );
    clSetKernelArg(kernels[0],  2, sizeof(cl_int)                            , &cols       );
    clSetKernelArg(kernels[0],  3, sizeof(cl_int)                            , &penalty    );
    clSetKernelArg(kernels[0],  5, sizeof(cl_int)                            , &block_width);
    clSetKernelArg(kernels[0],  6, sizeof(cl_int)                            , &worksize   );
    clSetKernelArg(kernels[0],  7, sizeof(cl_int)                            , &offset_r   );
    clSetKernelArg(kernels[0],  8, sizeof(cl_int)                            , &offset_c   );
    clSetKernelArg(kernels[0],  9, sizeof(cl_int) * (BLOCK + 1) * (BLOCK + 1), nullptr);
    clSetKernelArg(kernels[0], 10, sizeof(cl_int) * (BLOCK    ) * (BLOCK    ), nullptr);

    clSetKernelArg(kernels[1],  0, sizeof(cl_mem)                            , &S_d        );
    clSetKernelArg(kernels[1],  1, sizeof(cl_mem)                            , &F_d        );
    clSetKernelArg(kernels[1],  2, sizeof(cl_int)                            , &cols       );
    clSetKernelArg(kernels[1],  3, sizeof(cl_int)                            , &penalty    );
    clSetKernelArg(kernels[1],  5, sizeof(cl_int)                            , &block_width);
    clSetKernelArg(kernels[1],  6, sizeof(cl_int)                            , &worksize   );
    clSetKernelArg(kernels[1],  7, sizeof(cl_int)                            , &offset_r   );
    clSetKernelArg(kernels[1],  8, sizeof(cl_int)                            , &offset_c   );
    clSetKernelArg(kernels[1],  9, sizeof(cl_int) * (BLOCK + 1) * (BLOCK + 1), nullptr);
    clSetKernelArg(kernels[1], 10, sizeof(cl_int) * (BLOCK    ) * (BLOCK    ), nullptr);

    size_t local_work  = BLOCK;
    for(int blk = 1 ; blk <= worksize / BLOCK; ++blk)
    {
      size_t global_work = BLOCK * blk;
      clSetKernelArg(kernels[0], 4, sizeof(int), &blk);
      clEnqueueNDRangeKernel(queue, kernels[0], 1, nullptr, &global_work, &local_work, 0, nullptr, nullptr);
    }
    for(int blk = worksize / BLOCK - 1; blk > 0; --blk)
    {
      size_t global_work = BLOCK * blk;
      clSetKernelArg(kernels[1], 4, sizeof(int), &blk);
      clEnqueueNDRangeKernel(queue, kernels[1], 1, nullptr, &global_work, &local_work, 0, nullptr, nullptr);
    }
    clEnqueueReadBuffer(queue, F_d, CL_TRUE, 0, cols * rows * sizeof(int), F.get(), 0, nullptr, nullptr);
    clReleaseMemObject(F_d);
    clReleaseMemObject(S_d);
    auto t2 = setup.gettime();
    cout << "SQUARES  " << t2 - t1 << " s" << endl;
  }

  void compute_scores_device_rhombus()
  {
    auto t1 = setup.gettime();
    auto F_d = clCreateBuffer(context, CL_MEM_READ_WRITE, cols * rows * sizeof(int), nullptr, nullptr);
    auto S_d = clCreateBuffer(context, CL_MEM_READ_ONLY, CHARS * CHARS * sizeof(int), nullptr, nullptr);
    auto M_d = clCreateBuffer(context, CL_MEM_READ_ONLY, rows * sizeof(int), nullptr, nullptr);
    auto N_d = clCreateBuffer(context, CL_MEM_READ_ONLY, cols * sizeof(int), nullptr, nullptr);
    clEnqueueWriteBuffer(queue, F_d, CL_TRUE, 0, cols * rows * sizeof(int), F.get(), 0, nullptr, nullptr);
    clEnqueueWriteBuffer(queue, S_d, CL_TRUE, 0, CHARS * CHARS * sizeof(int), BLOSUM62, 0, nullptr, nullptr);
    clEnqueueWriteBuffer(queue, M_d, CL_TRUE, 0, rows * sizeof(int), M.get(), 0, nullptr, nullptr);
    clEnqueueWriteBuffer(queue, N_d, CL_TRUE, 0, cols * sizeof(int), N.get(), 0, nullptr, nullptr);
    clSetKernelArg(kernels[2], 0, sizeof(cl_mem), &S_d);
    clSetKernelArg(kernels[2], 1, sizeof(cl_mem), &M_d);
    clSetKernelArg(kernels[2], 2, sizeof(cl_mem), &N_d);
    clSetKernelArg(kernels[2], 3, sizeof(cl_mem), &F_d);
    clSetKernelArg(kernels[2], 4, sizeof(cl_int), &cols);
    clSetKernelArg(kernels[2], 5, sizeof(cl_int), &penalty);
    size_t local_work = BLOCK;
    for (int i = 0; i < rows / BLOCK - 1; ++i)
    {
      cl_int bc = 1;
      size_t global_work = ((2 * i + bc) * BLOCK < cols - 1) ? BLOCK * (i + 1) : (cols - 1) / 2;
      clSetKernelArg(kernels[2], 6, sizeof(int), &i);
      clSetKernelArg(kernels[2], 7, sizeof(int), &bc);
      clEnqueueNDRangeKernel(queue, kernels[2], 1, nullptr, &global_work, &local_work, 0, nullptr, nullptr);
      bc = 2;
      global_work = ((2 * i + bc) * BLOCK < cols - 1) ? BLOCK * (i + 1) : (cols - 1) / 2 - BLOCK;
      if (global_work == 0)
        continue;
      clSetKernelArg(kernels[2], 7, sizeof(int), &bc);
      clEnqueueNDRangeKernel(queue, kernels[2], 1, nullptr, &global_work, &local_work, 0, nullptr, nullptr);
    }
    for (int i = 1; i < cols / BLOCK; ++i)
    {
      cl_int br = rows / BLOCK - 1;
      size_t global_work = BLOCK * (((cols - 1) / BLOCK - i + 1) / 2);
      clSetKernelArg(kernels[2], 6, sizeof(int), &br);
      clSetKernelArg(kernels[2], 7, sizeof(int), &i);
      clEnqueueNDRangeKernel(queue, kernels[2], 1, nullptr, &global_work, &local_work, 0, nullptr, nullptr);
    }
    clEnqueueReadBuffer(queue, F_d, CL_TRUE, 0, cols * rows * sizeof(int), F.get(), 0, nullptr, nullptr);
    clReleaseMemObject(F_d);
    clReleaseMemObject(S_d);
    clReleaseMemObject(M_d);
    clReleaseMemObject(N_d);
    auto t2 = setup.gettime();
    cout << "DIAMONDS " << t2 - t1 << " s" << endl;
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
    std::cerr << "nw: " << e.what() << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
