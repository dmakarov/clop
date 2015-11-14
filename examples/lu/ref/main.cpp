/*
 * =====================================================================================
 *
 *       Filename:  lud.cu
 *
 *    Description:  The main wrapper for the suite
 *
 *        Version:  1.0
 *        Created:  10/22/2009 08:40:34 PM
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Liang Wang (lw2aw), lw2aw@virginia.edu
 *        Company:  CS@UVa
 *
 * =====================================================================================
 */

#include <getopt.h>
#include <sys/time.h>

#include <cmath>
#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <stdexcept>
#include <sstream>
#include <string>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif

class LUD {
private:
  struct stopwatch { struct timeval begin, end; };
  static const auto BLOCK_SIZE = 16;
  static std::map<cl_int, std::string> errstr;
  template<typename T> static T MIN(T i, T j) { return i < j ? i : j; }

public:

  LUD(int argc, char* argv[]) : context(0), cmd_queue(0)
  {
    int opt, option_index = 0;
    struct option long_options[] = {
      /* name, has_arg, flag, val */
      {  "size", 1, NULL, 's'},
      {"verify", 0, NULL, 'v'},
      {       0, 0,    0,   0}
    };
    while ((opt = getopt_long(argc, argv, "::vs:", long_options, &option_index)) != -1)
    {
      switch (opt)
      {
      case 'v': do_verify = 1;                     break;
      case 's': matrix_dim = atoi(optarg);         break;
      case '?': std::cerr << "invalid option\n";   break;
      case ':': std::cerr << "missing argument\n"; break;
      default:
        std::cerr << "Usage: " << argv[0] << " [-v] -s matrix_size\n";
        throw std::invalid_argument(std::string(1, opt));
      }
    }
    if (optind < argc || 0 == matrix_dim)
    {
      std::cerr << "Usage: " << argv[0] << " [-v] -s matrix_size\n";
      throw std::invalid_argument("invalid command line arguments");
    }
    m.reset(new float[matrix_dim * matrix_dim]);
    if (do_verify)
    {
      mm.reset(new float[matrix_dim * matrix_dim]);
    }
    std::ifstream input("Kernels.cl");
    std::ostringstream stream;
    std::string line;
    while (std::getline(input, line))
    {
      stream << line << std::endl;
    }
    input.close();
    opencl_source = stream.str();
    std::cout << "Read opencl source file, size " << opencl_source.size() << std::endl;
    query_opencl_configuration();
  }

  void run()
  {
    std::cout << "WG size of kernel = " << BLOCK_SIZE << " X " << BLOCK_SIZE << std::endl;
    for (auto p = 0; p != num_platforms; ++p)
    {
      for (auto d = 1; d != num_devices[p]; ++d)
      {
        create_matrix();
        setup_opencl_device(p, d);
        stopwatch sw;
        cl_int status;
        cl_mem d_m;
        d_m = clCreateBuffer(context, CL_MEM_READ_WRITE, matrix_dim * matrix_dim * sizeof(float), NULL, &status);
        if (status != CL_SUCCESS)
        {
          std::cerr << "ERROR: clCreateBuffer d_m (size:" << matrix_dim * matrix_dim << ") => " << errstr[status] << std::endl;
          throw std::runtime_error(errstr[status]);
        }
        /* beginning of timing point */
        stopwatch_start(sw);
        status = clEnqueueWriteBuffer(cmd_queue, d_m, 1, 0, matrix_dim * matrix_dim * sizeof(float), m.get(), 0, 0, 0);
        if (status != CL_SUCCESS)
        {
          std::cerr << "ERROR: clEnqueueWriteBuffer d_m (size:" << matrix_dim * matrix_dim << ") => " << errstr[status] << std::endl;
          throw std::runtime_error(errstr[status]);
        }
        int i;
        for (i = 0; i < matrix_dim - BLOCK_SIZE; i += BLOCK_SIZE)
        {
          clSetKernelArg(diagonal, 0, sizeof(void*), (void*)&d_m);
          clSetKernelArg(diagonal, 1, sizeof(float) * BLOCK_SIZE * BLOCK_SIZE, 0);
          clSetKernelArg(diagonal, 2, sizeof(cl_int), (void*)&matrix_dim);
          clSetKernelArg(diagonal, 3, sizeof(cl_int), (void*)&i);

          size_t global_work1[3] = {BLOCK_SIZE, 1, 1};
          size_t local_work1[3]  = {BLOCK_SIZE, 1, 1};

          status = clEnqueueNDRangeKernel(cmd_queue, diagonal, 2, NULL, global_work1, local_work1, 0, 0, 0);
          if (status != CL_SUCCESS)
          {
            std::cerr << "ERROR:  diagonal clEnqueueNDRangeKernel() failed => " << errstr[status] << std::endl;
            throw std::runtime_error(errstr[status]);
          }

          clSetKernelArg(perimeter, 0, sizeof(void*), (void*) &d_m);
          clSetKernelArg(perimeter, 1, sizeof(float) * BLOCK_SIZE * BLOCK_SIZE, 0);
          clSetKernelArg(perimeter, 2, sizeof(float) * BLOCK_SIZE * BLOCK_SIZE, 0);
          clSetKernelArg(perimeter, 3, sizeof(float) * BLOCK_SIZE * BLOCK_SIZE, 0);
          clSetKernelArg(perimeter, 4, sizeof(cl_int), (void*) &matrix_dim);
          clSetKernelArg(perimeter, 5, sizeof(cl_int), (void*) &i);

          size_t global_work2[3] = {BLOCK_SIZE * 2 * ((matrix_dim - i) / BLOCK_SIZE - 1), 1, 1};
          size_t local_work2[3]  = {BLOCK_SIZE * 2, 1, 1};

          status = clEnqueueNDRangeKernel(cmd_queue, perimeter, 2, NULL, global_work2, local_work2, 0, 0, 0);
          if (status != CL_SUCCESS)
          {
            std::cerr << "ERROR:  perimeter clEnqueueNDRangeKernel() failed => " << errstr[status] << std::endl;
            throw std::runtime_error(errstr[status]);
          }

          clSetKernelArg(internal, 0, sizeof(void*), (void*) &d_m);
          clSetKernelArg(internal, 1, sizeof(float) * BLOCK_SIZE * BLOCK_SIZE, 0);
          clSetKernelArg(internal, 2, sizeof(float) * BLOCK_SIZE * BLOCK_SIZE, 0);
          clSetKernelArg(internal, 3, sizeof(cl_int), (void*) &matrix_dim);
          clSetKernelArg(internal, 4, sizeof(cl_int), (void*) &i);

          size_t global_work3[3] = {BLOCK_SIZE * ((matrix_dim - i) / BLOCK_SIZE - 1), BLOCK_SIZE * ((matrix_dim - i) / BLOCK_SIZE - 1), 1};
          size_t local_work3[3]  = {BLOCK_SIZE, BLOCK_SIZE, 1};

          status = clEnqueueNDRangeKernel(cmd_queue, internal, 2, NULL, global_work3, local_work3, 0, 0, 0);
          if (status != CL_SUCCESS)
          {
            std::cerr << "ERROR:  internal clEnqueueNDRangeKernel() failed => " << errstr[status] << std::endl;
            throw std::runtime_error(errstr[status]);
          }
        }
        clSetKernelArg(diagonal, 0, sizeof(void*), (void*) &d_m);
        clSetKernelArg(diagonal, 1, sizeof(float) * BLOCK_SIZE * BLOCK_SIZE, 0);
        clSetKernelArg(diagonal, 2, sizeof(cl_int), (void*) &matrix_dim);
        clSetKernelArg(diagonal, 3, sizeof(cl_int), (void*) &i);

        size_t global_work1[3] = {BLOCK_SIZE, 1, 1};
        size_t local_work1[3]  = {BLOCK_SIZE, 1, 1};
        status = clEnqueueNDRangeKernel(cmd_queue, diagonal, 2, NULL, global_work1, local_work1, 0, 0, 0);
        if (status != CL_SUCCESS)
        {
          std::cerr << "ERROR:  diagonal clEnqueueNDRangeKernel() failed => " << errstr[status] << std::endl;
          throw std::runtime_error(errstr[status]);
        }

        status = clEnqueueReadBuffer(cmd_queue, d_m, 1, 0, matrix_dim * matrix_dim * sizeof(float), m.get(), 0, 0, 0);
        if (status != CL_SUCCESS)
        {
          std::cerr << "ERROR: clEnqueueReadBuffer  d_m (size:" << matrix_dim * matrix_dim << ") => " << errstr[status] << std::endl;
          throw std::runtime_error(errstr[status]);
        }
        clFinish(cmd_queue);
        /* end of timing point */
        stopwatch_stop(sw);
        std::cout << "Time consumed(ms): " << 1000 * get_interval_by_sec(sw) << std::endl;

        clReleaseMemObject(d_m);
        clReleaseCommandQueue(cmd_queue);
        clReleaseContext(context);

        if (do_verify)
        {
          std::cout << ">>>Verify<<<<\n";
          std::unique_ptr<float[]> tmp(new float[matrix_dim * matrix_dim]);
          for (int i = 0; i < matrix_dim; ++i)
          {
            for (int j = 0; j < matrix_dim; ++j)
            {
              float sum = 0;
              for (int k = 0; k <= MIN(i, j); ++k)
              {
                float l = (i == k) ? 1 : m[i * matrix_dim + k];
                float u = m[k * matrix_dim + j];
                sum += l * u;
              }
              tmp[i * matrix_dim + j] = sum;
            }
          }
          for (int i = 0; i < matrix_dim; ++i)
          {
            for (int j = 0; j < matrix_dim; ++j)
            {
              if (fabs(mm[i * matrix_dim + j] - tmp[i * matrix_dim + j]) > 0.0001)
              {
                std::cerr << "dismatch at (" << i << ", " << j << "): (o)"
                          << m[i * matrix_dim + j] << " (n)"
                          << tmp[i * matrix_dim + j] << std::endl;
              }
            }
          }
        }
      }
    }
  }

private:

  std::unique_ptr<cl_platform_id[]>                  platform_list;
  cl_uint                                            num_platforms;
  std::unique_ptr<std::unique_ptr<cl_device_id[]>[]> device_list;
  std::unique_ptr<cl_uint[]>                         num_devices;
  std::string                                        opencl_source;
  cl_context                                         context;
  cl_command_queue                                   cmd_queue;
  cl_kernel                                          diagonal;
  cl_kernel                                          perimeter;
  cl_kernel                                          internal;
  int                                                do_verify = 0;
  size_t                                             matrix_dim = 32;
  std::unique_ptr<float[]>                           m;
  std::unique_ptr<float[]>                           mm;

  void query_opencl_configuration()
  {
    cl_int status;
    if ((status = clGetPlatformIDs(0, nullptr, &num_platforms)) != CL_SUCCESS)
    {
      std::cerr << "ERROR: clGetPlatformIDs(0, 0, *) failed " << errstr[status] << std::endl;
      throw std::runtime_error(errstr[status]);
    }
    if (0 == num_platforms)
    {
      std::cerr << "ERROR: No OpenCL platforms found.\n";
      throw std::runtime_error("OpenCL unavailable");
    }
    platform_list.reset(new cl_platform_id[num_platforms]);
    if ((status = clGetPlatformIDs(num_platforms, platform_list.get(), nullptr)) != CL_SUCCESS)
    {
      std::cerr << "ERROR: clGetPlatformIDs(*, *, 0) failed " << errstr[status] << std::endl;
      throw std::runtime_error(errstr[status]);
    }
    std::cout << "Found " << num_platforms << " OpenCL platforms.\n";
    device_list.reset(new std::unique_ptr<cl_device_id[]>[num_platforms]);
    num_devices.reset(new cl_uint[num_platforms]);
    for (cl_uint p = 0; p != num_platforms; ++p)
    {
      cl_uint num;
      if ((status = clGetDeviceIDs(platform_list[p], CL_DEVICE_TYPE_ALL, 0, nullptr, &num)) != CL_SUCCESS)
      {
        std::cerr << "ERROR: clGetDeviceIDs(..., 0, 0, *) failed " << errstr[status] << std::endl;
        throw std::runtime_error(errstr[status]);
      }
      if (0 == num)
      {
        std::cerr << "ERROR: No OpenCL devices found.\n";
        throw std::runtime_error("OpenCL unavailable");
      }
      std::cout << "Found " << num << " OpenCL devices.\n";
      num_devices[p] = num;
      device_list[p].reset(new cl_device_id[num]);
      if ((status = clGetDeviceIDs(platform_list[p], CL_DEVICE_TYPE_ALL, num, device_list[p].get(), nullptr)) != CL_SUCCESS)
      {
        std::cerr << "ERROR: clGetDeviceIDs(..., *, *, 0) failed " << errstr[status] << std::endl;
        throw std::runtime_error(errstr[status]);
      }
    }
  }

  void setup_opencl_device(cl_uint platform_index, cl_uint device_index)
  {
    cl_int status;
    context = clCreateContext(nullptr, 1, device_list[platform_index].get() + device_index, NULL, NULL, &status);
    if (!context)
    {
      std::cerr << "ERROR: clCreateContext() failed " << errstr[status] << std::endl;
      throw std::runtime_error("clCreateContext");
    }
    cmd_queue = clCreateCommandQueue(context, device_list[platform_index][device_index], 0, &status);
    if (!cmd_queue)
    {
      std::cerr << "ERROR: clCreateCommandQueue() failed " << errstr[status] << std::endl;
      throw std::runtime_error("clCreateCommandQueue");
    }
    const char* slist[2] = { opencl_source.c_str(), 0 };
    cl_program prog = clCreateProgramWithSource(context, 1, slist, NULL, &status);
    if (status != CL_SUCCESS)
    {
      std::cerr << "ERROR: clCreateProgramWithSource() failed => " << errstr[status] << std::endl;
      throw std::runtime_error("clCreateProgramWithSource");
    }
    char clOptions[110] = {'\0'};
    sprintf(clOptions + strlen(clOptions), "-DBLOCK_SIZE=%d", BLOCK_SIZE);
    status = clBuildProgram(prog, 0, NULL, clOptions, NULL, NULL);
    { // show warnings/errors
      static char log[65536];
      memset(log, 0, sizeof(log));
      cl_device_id device_id = 0;
      clGetContextInfo(context, CL_CONTEXT_DEVICES, sizeof(device_id), &device_id, NULL);
      clGetProgramBuildInfo(prog, device_id, CL_PROGRAM_BUILD_LOG, sizeof(log) - 1, log, NULL);
      if (status || strstr(log, "warning:") || strstr(log, "error:"))
      {
        std::cerr << "<<<<\n" << log << "\n>>>>\n";
      }
    }
    if (status != CL_SUCCESS)
    {
      std::cerr<< "ERROR: clBuildProgram() => " << errstr[status] << std::endl;
      throw std::runtime_error("clBuildProgram");
    }
    diagonal  = clCreateKernel(prog, "lud_diagonal" , &status);
    perimeter = clCreateKernel(prog, "lud_perimeter", &status);
    internal  = clCreateKernel(prog, "lud_internal" , &status);
    if (status != CL_SUCCESS)
    {
      std::cerr << "ERROR: clCreateKernel() => " << errstr[status] << std::endl;
      throw std::runtime_error("clCreateKernel");
    }
    clReleaseProgram(prog);
  }

  // Generate well-conditioned matrix internally by Ke Wang 2013/08/07 22:20:06
  void create_matrix()
  {
    std::cout << "Creating matrix internally size = " << matrix_dim << std::endl;
    const float lambda = -0.001;
    float coe[2 * matrix_dim - 1];
    float coe_i = 0.0;
    for (int i = 0; i < matrix_dim; ++i)
    {
      coe_i = 10 * std::exp(lambda * i);
      coe[matrix_dim - 1 + i] = coe_i;
      coe[matrix_dim - 1 - i] = coe_i;
    }
    for (int i = 0; i < matrix_dim; ++i)
    {
      for (int j = 0; j < matrix_dim; ++j)
      {
        m[i * matrix_dim + j] = coe[matrix_dim - 1 - i + j];
      }
    }
    if (do_verify)
    {
      std::copy(m.get(), m.get() + matrix_dim * matrix_dim, mm.get());
    }
  }

  double gettime()
  {
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec * 1e-6;
  }

  void stopwatch_start(stopwatch& sw)
  {
    bzero(&sw.begin, sizeof(struct timeval));
    bzero(&sw.end  , sizeof(struct timeval));
    gettimeofday(&sw.begin, NULL);
  }

  void stopwatch_stop(stopwatch& sw)
  {
    gettimeofday(&sw.end, NULL);
  }

  double get_interval_by_sec(const stopwatch& sw)
  {
    return ((double)(sw.end.tv_sec  - sw.begin.tv_sec) +
            (double)(sw.end.tv_usec - sw.begin.tv_usec) / 1000000);
  }

};

std::map<cl_int, std::string> LUD::errstr{
  {CL_BUILD_PROGRAM_FAILURE , "CL_BUILD_PROGRAM_FAILURE" },
  {CL_COMPILER_NOT_AVAILABLE, "CL_COMPILER_NOT_AVAILABLE"},
  {CL_INVALID_BINARY        , "CL_INVALID_BINARY"        },
  {CL_INVALID_BUILD_OPTIONS , "CL_INVALID_BUILD_OPTIONS" },
  {CL_INVALID_DEVICE        , "CL_INVALID_DEVICE"        },
  {CL_INVALID_OPERATION     , "CL_INVALID_OPERATION"     },
  {CL_INVALID_OPERATION     , "CL_INVALID_OPERATION"     },
  {CL_INVALID_PROGRAM       , "CL_INVALID_PROGRAM"       },
  {CL_INVALID_VALUE         , "CL_INVALID_VALUE"         },
  {CL_OUT_OF_HOST_MEMORY    , "CL_OUT_OF_HOST_MEMORY"    },
  {CL_OUT_OF_RESOURCES      , "CL_OUT_OF_RESOURCES"      }
};

int main(int argc, char* argv[])
{
  try
  {
    LUD test(argc, argv);
    test.run();
  }
  catch (const std::exception& err)
  {
    std::cerr << "Fatal error: " << err.what() << std::endl;
    exit(EXIT_FAILURE);
  }
  std::cout << "Done.\n";
  return 0;
}

// Local Variables:
// company-c-headers-path-user: ("../../include")
// End:
