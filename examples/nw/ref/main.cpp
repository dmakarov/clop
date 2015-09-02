#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

#include <unistd.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif

using std::ifstream;
using std::ofstream;
using std::string;
using std::stringstream;

struct Application {

  static const int BLOCK =   16;
  static const int CHARS =   24;
  static const int LIMIT = -999;
  static const int SEED  =    1;
  static const int BLOSUM62[CHARS][CHARS];

  Application( int argc, char **argv )
  {
#ifdef __MACH__
    self = mach_host_self();
    host_get_clock_service( self, REALTIME_CLOCK, &cclock );
#endif
    t1 = gettime();
    kernel_file = "Kernels.cl";
    kernel_names[0] = "nw1";
    kernel_names[1] = "nw2";
    kernel_names[2] = "rhombus";
    outfile = NULL;
    platform_index = 0;
    device_index = 0;
    int ch;
    while ( ( ch = getopt( argc, argv, "d:o:p:" ) ) != -1 )
    {
      switch ( ch )
      {
      case 'p': sscanf( optarg, "%d", &platform_index ); break;
      case 'd': sscanf( optarg, "%d", &device_index );   break;
      case 'o': outfile = optarg;                        break;
      default:  usage( argv );
      }
    }
    argc -= optind;
    argv += optind;
    if ( argc < 2 ) usage( argv );
    rows    = atoi( argv[0] ) + 1;
    cols    = atoi( argv[0] ) + 1;
    penalty = atoi( argv[1] );
    if ( ( rows - 1 ) % 16 != 0 )
    {
      ss << "ERROR the dimension value (" << rows << ") must be a multiple of " << BLOCK;
      throw ss.str();
    }
    if ( NULL == ( F = (int*) calloc( rows * cols, sizeof(int) ) ) )
    {
      throw string( "ERROR can't allocate scores matrix" );
    }
    if ( NULL == ( S = (int*) calloc( rows * cols, sizeof(int) ) ) )
    {
      throw string( "ERROR can't allocate similarity matrix" );
    }
    if ( NULL == ( I = (int*) calloc( rows * cols, sizeof(int) ) ) )
    {
      throw string( "ERROR can't allocate input itemsets" );
    }
    if ( NULL == ( O = (int*) calloc( rows * cols, sizeof(int) ) ) )
    {
      throw string( "ERROR can't allocate output itemsets" );
    }
    if ( NULL == ( A = (int*) calloc( rows, sizeof(int ) ) ) )
    {
      throw string( "ERROR can't allocate alignment A" );
    }
    if ( NULL == ( B = (int*) calloc( cols, sizeof(int ) ) ) )
    {
      throw string( "ERROR can't allocate alignment B" );
    }
    if ( NULL == ( M = (int*) calloc( rows, sizeof(int ) ) ) )
    {
      throw string( "ERROR can't allocate alignment M" );
    }
    if ( NULL == ( N = (int*) calloc( cols, sizeof(int ) ) ) )
    {
      throw string( "ERROR can't allocate alignment N" );
    }
  }

  ~Application()
  {
    free( A );
    free( B );
    free( M );
    free( N );
    free( F );
    free( S );
    free( I );
    free( O );
    t2 = gettime();
    fprintf( stdout, "TOTAL   %f\n", t2 - t1 );
#ifdef __MACH__
    mach_port_deallocate( self, cclock );
#endif
  }

  void init()
  {
    srandom( SEED );
    for ( int ii = 1; ii < rows; ++ii ) M[ii] = random() % ( CHARS - 1 ) + 1;
    for ( int jj = 1; jj < cols; ++jj ) N[jj] = random() % ( CHARS - 1 ) + 1;
    for ( int ii = 1; ii < rows; ++ii )
      for ( int jj = 1; jj < cols; ++jj )
      {
        S[ii * cols + jj] = BLOSUM62[M[ii]][N[jj]];
      }
  }

  double gettime()
  {
#ifdef __MACH__
    mach_timespec_t ts;
    clock_get_time( cclock, &ts );
#else
    struct timespec ts;
    clock_gettime( CLOCK_MONOTONIC, &ts );
#endif
    return ts.tv_sec + ts.tv_nsec * 1e-9;
  }

  string error_code_to_string( cl_int code )
  {
    switch ( code )
    {
    case CL_BUILD_PROGRAM_FAILURE:                     return "CL_BUILD_PROGRAM_FAILURE";
    case CL_COMPILER_NOT_AVAILABLE:                    return "CL_COMPILER_NOT_AVAILABLE";
    case CL_DEVICE_NOT_AVAILABLE:                      return "CL_DEVICE_NOT_AVAILABLE";
    case CL_DEVICE_NOT_FOUND:                          return "CL_DEVICE_NOT_FOUND";
#ifdef CL_VERSION_1_2
    case CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST: return "CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST";
#endif
    case CL_IMAGE_FORMAT_MISMATCH:                     return "CL_IMAGE_FORMAT_MISMATCH";
    case CL_IMAGE_FORMAT_NOT_SUPPORTED:                return "CL_IMAGE_FORMAT_NOT_SUPPORTED";
    case CL_INVALID_ARG_INDEX:                         return "CL_INVALID_ARG_INDEX";
    case CL_INVALID_ARG_SIZE:                          return "CL_INVALID_ARG_SIZE";
    case CL_INVALID_ARG_VALUE:                         return "CL_INVALID_ARG_VALUE";
    case CL_INVALID_BINARY:                            return "CL_INVALID_BINARY";
    case CL_INVALID_BUFFER_SIZE:                       return "CL_INVALID_BUFFER_SIZE";
    case CL_INVALID_BUILD_OPTIONS:                     return "CL_INVALID_BUILD_OPTIONS";
    case CL_INVALID_COMMAND_QUEUE:                     return "CL_INVALID_COMMAND_QUEUE";
    case CL_INVALID_CONTEXT:                           return "CL_INVALID_CONTEXT";
    case CL_INVALID_DEVICE:                            return "CL_INVALID_DEVICE";
    case CL_INVALID_DEVICE_TYPE:                       return "CL_INVALID_DEVICE_TYPE";
    case CL_INVALID_EVENT:                             return "CL_INVALID_EVENT";
    case CL_INVALID_EVENT_WAIT_LIST:                   return "CL_INVALID_EVENT_WAIT_LIST";
    case CL_INVALID_GL_OBJECT:                         return "CL_INVALID_GL_OBJECT";
    case CL_INVALID_GLOBAL_OFFSET:                     return "CL_INVALID_GLOBAL_OFFSET";
    case CL_INVALID_GLOBAL_WORK_SIZE:                  return "CL_INVALID_GLOBAL_WORK_SIZE";
    case CL_INVALID_HOST_PTR:                          return "CL_INVALID_HOST_PTR";
    case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:           return "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR";
    case CL_INVALID_IMAGE_SIZE:                        return "CL_INVALID_IMAGE_SIZE";
    case CL_INVALID_KERNEL:                            return "CL_INVALID_KERNEL";
    case CL_INVALID_KERNEL_ARGS:                       return "CL_INVALID_KERNEL_ARGS";
    case CL_INVALID_KERNEL_DEFINITION:                 return "CL_INVALID_KERNEL_DEFINITION";
    case CL_INVALID_KERNEL_NAME:                       return "CL_INVALID_KERNEL_NAME";
    case CL_INVALID_MEM_OBJECT:                        return "CL_INVALID_MEM_OBJECT";
    case CL_INVALID_MIP_LEVEL:                         return "CL_INVALID_MIP_LEVEL";
    case CL_INVALID_OPERATION:                         return "CL_INVALID_OPERATION";
    case CL_INVALID_PLATFORM:                          return "CL_INVALID_PLATFORM";
    case CL_INVALID_PROGRAM:                           return "CL_INVALID_PROGRAM";
    case CL_INVALID_PROGRAM_EXECUTABLE:                return "CL_INVALID_PROGRAM_EXECUTABLE";
    case CL_INVALID_QUEUE_PROPERTIES:                  return "CL_INVALID_QUEUE_PROPERTIES";
    case CL_INVALID_SAMPLER:                           return "CL_INVALID_SAMPLER";
    case CL_INVALID_VALUE:                             return "CL_INVALID_VALUE";
    case CL_INVALID_WORK_DIMENSION:                    return "CL_INVALID_WORK_DIMENSION";
    case CL_INVALID_WORK_GROUP_SIZE:                   return "CL_INVALID_WORK_GROUP_SIZE";
    case CL_INVALID_WORK_ITEM_SIZE:                    return "CL_INVALID_WORK_ITEM_SIZE";
    case CL_MAP_FAILURE:                               return "CL_MAP_FAILURE";
    case CL_MEM_COPY_OVERLAP:                          return "CL_MEM_COPY_OVERLAP";
    case CL_MEM_OBJECT_ALLOCATION_FAILURE:             return "CL_MEM_OBJECT_ALLOCATION_FAILURE";
#ifdef CL_VERSION_1_2
    case CL_MISALIGNED_SUB_BUFFER_OFFSET:              return "CL_MISALIGNED_SUB_BUFFER_OFFSET";
#endif
    case CL_OUT_OF_HOST_MEMORY:                        return "CL_OUT_OF_HOST_MEMORY";
    case CL_OUT_OF_RESOURCES:                          return "CL_OUT_OF_RESOURCES";
    case CL_PROFILING_INFO_NOT_AVAILABLE:              return "CL_PROFILING_INFO_NOT_AVAILABLE";
    }
    return "UNKNOWN CL ERROR";
  }

  /**
   */
  void check_status( cl_int status, string msg )
  {
    if ( CL_SUCCESS != status ) throw msg + error_code_to_string( status );
  }

  void init_cl()
  {
    cl_uint num_platforms, num_devices;
    cl_int status = clGetPlatformIDs( 0, NULL, &num_platforms );
    check_status( status, "init_cl ( clGetPlatformIDs 1 ) " );
    if ( 0 == num_platforms ) throw string( "init_cl no OpenCL platform found" );
    if ( platform_index >= num_platforms ) throw string( "init_cl invalid platform index" );
    if ( NULL == ( platforms = new cl_platform_id[num_platforms] ) ) throw string( "init_cl can't allocate memory for platforms" );
    status = clGetPlatformIDs( num_platforms, platforms, NULL );
    check_status( status, "init_cl ( clGetPlatformIDs 2 ) " );
    platform = platforms[platform_index];

    // 2: detect OpenCL devices
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, 0, NULL, &num_devices );
    check_status( status, "init_cl ( clGetDeviceIDs 1 ) " );
    if ( 0 == num_devices ) throw string( "init_cl no OpenCL device found" );
    if ( device_index >= num_devices ) throw string( "init_cl invalid device index" );
    if ( NULL == ( devices = new cl_device_id[num_devices] ) ) throw string( "init_cl can't allocate memory for devices" );
    status = clGetDeviceIDs( platform, CL_DEVICE_TYPE_ALL, num_devices, devices, NULL );
    check_status( status, "init_cl ( clGetDeviceIDs 2 ) " );
    device = devices[device_index];

    // 3: Create an OpenCL context
    context = clCreateContext( NULL, 1, &device, NULL, NULL, &status );
    check_status( status, "init_cl ( clCreateContext ) " );

    // 4: Create an OpenCL command queue
    queue = clCreateCommandQueue( context, device, 0, &status );
    check_status( status, "init_cl ( clCreateCommandQueue ) " );

    // 5: Load CL file, build CL program object, create CL kernel object
    load_kernels( kernel_file, device_index );
    for ( int kn = 0; kn < 3; ++kn )
    {
      // get a kernel object handle for a kernel with the given name
      cl_kernel kernel = clCreateKernel( program, ( kernel_names[kn] ).c_str(), &status );
      check_status( status, "init_cl ( clCreateKernel ) \"" + kernel_names[kn] + "\" " );
      kernels[kn] = kernel;
    }
  }

  void load_kernels( string src_file, cl_uint device_index )
  {
    cl_int status;
    size_t size;
    const char* source = (char*) binary_to_array( src_file, &size );
    program = clCreateProgramWithSource( context, 1, &source, &size, &status );
    check_status( status, "CLHelper::CLHelper: ( clCreateProgramWithSource ) " );
    delete [] source;
    status = clBuildProgram( program, 1, &device, NULL, NULL, NULL );
    if ( CL_SUCCESS != status )
    {
      string msg = "CLHelper::CLHelper: ( clBuildProgram ) " + error_code_to_string( status );
      size_t size;
      status = clGetProgramBuildInfo( program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &size );
      check_status( status, "CLHelper::CLHelper: ( clGetProgramBuildInfo 1 ) " );
      char* buf = new char[size];
      status = clGetProgramBuildInfo( program, device, CL_PROGRAM_BUILD_LOG, size, buf, NULL );
      check_status( status, "CLHelper::CLHelper: ( clGetProgramBuildInfo 2 ) " );
      array_to_binary( "errinfo.txt", buf, size - 1 );
      delete [] buf;
      throw msg;
    }
  }

  /**
   * @function read the contents of a file into a byte array.
   */
  void* binary_to_array( const string bin_file, size_t *size ) throw ( string )
  {
    ifstream f( bin_file.c_str(), ifstream::in | ifstream::binary );
    try
    {
      if ( f.is_open() )
      {
        f.seekg( 0, ifstream::end );
        *size = f.tellg();
        f.seekg( 0, ifstream::beg );
        char *buf = new char[*size + 1];
        if ( NULL == buf ) throw string( "CLHelper::binary_to_array: can't allocate memory" );
        f.read( buf, *size );
        buf[*size] = '\0';
        f.close();
        return buf;
      }
    }
    catch( string msg ) { fprintf( stderr, "CLHelper::binary_to_array: exception caught %s\n", msg.c_str() ); }
    catch( ... ) {}
    if ( f.is_open() ) f.close();
    throw string( "CLHelper::binary_to_array: can't read file " ) + bin_file;
  }

  /**
   * @function write a byte array into a binary file.
   */
  void array_to_binary( const string bin_file, const char *buf, size_t size ) throw ( string )
  {
    ofstream f( bin_file.c_str(), ofstream::out | ofstream::binary );
    try
    {
      if ( f.is_open() )
      {
        f.write( buf, size );
        f.close();
        return;
      }
    }
    catch( string msg ) { fprintf( stderr, "CLHelper::array_to_binary: exception caught %s\n", msg.c_str() ); }
    catch( ... ) {}
    if ( f.is_open() ) f.close();
    throw string( "CLHelper::array_to_binary: can't write to file " ) + bin_file;
  }

  void fini()
  {
    if ( NULL != outfile )
    {
      FILE *fp = fopen( outfile, "w" );
      if ( NULL == fp )
      {
        throw string( "ERROR: can't create file " ) + outfile;
      }
      for ( int ii = 1; ii < rows - 1; ++ii )
      {
        fprintf( fp, "%c", M[ii] + 'A' );
      }
      fprintf( fp, "\n" );
      for ( int ii = 1; ii < cols - 1; ++ii )
      {
        fprintf( fp, "%c", N[ii] + 'A' );
      }
      fprintf( fp, "\n\n" );
      for ( int ii = 1; ii < rows - 1; ++ii )
      {
        fprintf( fp, "%c", A[ii] + 'A' );
      }
      fprintf( fp, "\n" );
      for ( int ii = 1; ii < cols - 1; ++ii )
      {
        fprintf( fp, "%c", B[ii] + 'A' );
      }
      fprintf( fp, "\n\n" );
#if 0
      for ( int ii = 0; ii < rows; ++ii )
      {
        fprintf( fp, "%5d", F[ii * cols] );
        for ( int jj = 1; jj < cols; ++jj )
        {
          fprintf( fp, " %5d", F[ii * cols + jj] );
        }
        fprintf( fp, "\n" );
      }
      fprintf( fp, "\n" );
      for ( int ii = 0; ii < rows; ++ii )
      {
        fprintf( fp, "%5d", O[ii * cols] );
        for ( int jj = 1; jj < cols; ++jj )
        {
          fprintf( fp, " %5d", O[ii * cols + jj] );
        }
        fprintf( fp, "\n" );
      }
#endif
      fclose( fp );
    }
  }

  string summary()
  {
    ss << "\"SEED='"    << SEED
       << "',rows='"    << rows
       << "',cols='"    << cols
       << "',penalty='" << penalty
       << "'\"";
    return ss.str();
  }

  int maximum( int a, int b, int c )
  {
    int k = a > b ? a : b;
    return k > c ? k : c;
  }

  /**
     The algorithm of filling in F matrix

     for i=0 to length(A) F(i,0) ← d*i
     for j=0 to length(B) F(0,j) ← d*j
     for i=1 to length(A)
     for j=1 to length(B)
     {
       Match  ← F( i-1, j-1) + S(Ai, Bj)
       Delete ← F( i-1, j  ) + d
       Insert ← F( i  , j-1) + d
       F(i,j) ← max(Match, Insert, Delete)
     }
  */
  void compute_scores_serial()
  {
    for ( int ii = 0; ii < rows; ++ii ) F[ii * cols] = -penalty * ii;
    for ( int jj = 0; jj < cols; ++jj ) F[jj       ] = -penalty * jj;
    for ( int ii = 1; ii < rows; ++ii )
      for ( int jj = 1; jj < rows; ++jj )
      {
        int m = F[(ii - 1) * cols + jj - 1] + S[ii * cols + jj];
        int d = F[(ii - 1) * cols + jj    ] - penalty;
        int i = F[ ii      * cols + jj - 1] - penalty;
        F[ii * cols + jj] = maximum( m, d, i );
      }
  }

  /**
     AlignmentA ← ""
     AlignmentB ← ""
     i ← length(A)
     j ← length(B)
     while (i > 0 or j > 0)
     {
       if (i > 0 and j > 0 and F(i,j) == F(i-1,j-1) + S(Ai, Bj))
       {
         AlignmentA ← Ai + AlignmentA
         AlignmentB ← Bj + AlignmentB
         i ← i - 1
         j ← j - 1
       }
       else if (i > 0 and F(i,j) == F(i-1,j) + d)
       {
         AlignmentA ← Ai + AlignmentA
         AlignmentB ← "-" + AlignmentB
         i ← i - 1
       }
       else (j > 0 and F(i,j) == F(i,j-1) + d)
       {
         AlignmentA ← "-" + AlignmentA
         AlignmentB ← Bj + AlignmentB
         j ← j - 1
       }
     }
   */
  void compute_alignments()
  {
#if 0
    for ( int ii = 0; ii < rows; ++ii )
      for ( int jj = 0; jj < cols; ++jj )
        if ( F[ii * cols + jj] != O[ii * cols + jj] )
          fprintf( stderr, "mismatch %d,%d %d != %d\n", ii, jj, F[ii * cols + jj], O[ii * cols + jj] );
#endif
    int i = rows - 2;
    int j = cols - 2;
    int m = i;
    int n = j;
    for ( ; i != 0 && j != 0; --m, --n )
    {
      if ( i > 0 && j > 0 && F[i * cols + j] == F[(i - 1) * cols + j - 1] + S[i * cols + j] )
      {
        A[m] = M[i];
        B[n] = N[j];
        --i;
        --j;
      }
      else if ( i > 0 && F[i * cols + j] == F[(i - 1) * cols + j] - penalty )
      {
        A[m] = M[i];
        B[n] = 30;
        --i;
      }
      else if ( j > 0 && F[i * cols + j] == F[i * cols + j - 1] - penalty )
      {
        A[m] = 30;
        B[n] = N[j];
        --j;
      }
      else
      {
        fprintf( stderr, "i %d, j %d\n", i, j );
        throw string("ERROR invalid F");
      }
    }
  }

  void compute_scores_device()
  {
    for ( int ii = 1; ii < rows; ++ii ) I[ii * cols] = -ii * penalty;
    for ( int jj = 1; jj < cols; ++jj ) I[jj       ] = -jj * penalty;
    int nworkitems = 16, workgroupsize = 0;
    // set global and local workitems
    size_t local_work = workgroupsize > 0 ? workgroupsize : 1;
    size_t global_work = nworkitems; //nworkitems = no. of GPU threads
    cl_int status;
    cl_mem I_d = clCreateBuffer( context, CL_MEM_READ_WRITE, cols * rows * sizeof(int), NULL, &status );
    check_status( status, "compute_scores_device clCreateBuffer 1 " );
    cl_mem S_d = clCreateBuffer( context, CL_MEM_READ_WRITE, cols * rows * sizeof(int), NULL, &status );
    check_status( status, "compute_scores_device clCreateBuffer 2 " );
    status = clEnqueueWriteBuffer( queue, I_d, CL_TRUE, 0, cols * rows * sizeof(int), I, 0, NULL, NULL );
    check_status( status, "compute_scores_device clEnqueueWriteBuffer 1 " );
    status = clEnqueueWriteBuffer( queue, S_d, CL_TRUE, 0, cols * rows * sizeof(int), S, 0, NULL, NULL );
    check_status( status, "compute_scores_device clEnqueueWriteBuffer 2 " );
    int worksize = cols - 1;
    int offset_r = 0, offset_c = 0; // these two parameters are for extension use, don't worry about it.
    int block_width = worksize / BLOCK ;

    clSetKernelArg( kernels[0],  0, sizeof(cl_mem)                             , &S_d         );
    clSetKernelArg( kernels[0],  1, sizeof(cl_mem)                             , &I_d         );
    clSetKernelArg( kernels[0],  2, sizeof(int)                                , &cols        );
    clSetKernelArg( kernels[0],  3, sizeof(int)                                , &penalty     );
    clSetKernelArg( kernels[0],  5, sizeof(int)                                , &block_width );
    clSetKernelArg( kernels[0],  6, sizeof(int)                                , &worksize    );
    clSetKernelArg( kernels[0],  7, sizeof(int)                                , &offset_r    );
    clSetKernelArg( kernels[0],  8, sizeof(int)                                , &offset_c    );
    clSetKernelArg( kernels[0],  9, sizeof(int) * ( BLOCK + 1 ) * ( BLOCK + 1 ), NULL         );
    clSetKernelArg( kernels[0], 10, sizeof(int) * ( BLOCK     ) * ( BLOCK     ), NULL         );

    clSetKernelArg( kernels[1],  0, sizeof(cl_mem)                             , &S_d         );
    clSetKernelArg( kernels[1],  1, sizeof(cl_mem)                             , &I_d         );
    clSetKernelArg( kernels[1],  2, sizeof(int)                                , &cols        );
    clSetKernelArg( kernels[1],  3, sizeof(int)                                , &penalty     );
    clSetKernelArg( kernels[1],  5, sizeof(int)                                , &block_width );
    clSetKernelArg( kernels[1],  6, sizeof(int)                                , &worksize    );
    clSetKernelArg( kernels[1],  7, sizeof(int)                                , &offset_r    );
    clSetKernelArg( kernels[1],  8, sizeof(int)                                , &offset_c    );
    clSetKernelArg( kernels[1],  9, sizeof(int) * ( BLOCK + 1 ) * ( BLOCK + 1 ), NULL         );
    clSetKernelArg( kernels[1], 10, sizeof(int) *   BLOCK       *   BLOCK      , NULL         );

    for( int blk = 1 ; blk <= worksize / BLOCK; ++blk )
    {
      global_work = BLOCK * blk;
      local_work  = BLOCK;
      clSetKernelArg( kernels[0], 4, sizeof(int), &blk );
      status = clEnqueueNDRangeKernel( queue, kernels[0], 1, NULL, &global_work, &local_work, 0, NULL, NULL );
      check_status( status, "compute_scores_device clEnqueueNDRangeKernel 1 " );
    }
    for( int blk = worksize / BLOCK - 1; blk > 0; --blk )
    {
      global_work = BLOCK * blk;
      local_work  = BLOCK;
      clSetKernelArg( kernels[1], 4, sizeof(int), &blk );
      status = clEnqueueNDRangeKernel( queue, kernels[1], 1, NULL, &global_work, &local_work, 0, NULL, NULL );
      check_status( status, "compute_scores_device clEnqueueNDRangeKernel 2 " );
    }
    status = clEnqueueReadBuffer( queue, I_d, CL_TRUE, 0, cols * rows * sizeof(int), O, 0, NULL, NULL );
    check_status( status, "compute_scores_device clEnqueueReadBuffer " );
    clReleaseMemObject( I_d );
    clReleaseMemObject( S_d );
  }

  void compute_scores_device_rhombus()
  {
    for ( int ii = 1; ii < rows; ++ii ) I[ii * cols] = -ii * penalty;
    for ( int jj = 1; jj < cols; ++jj ) I[jj       ] = -jj * penalty;
    int nworkitems = 16, workgroupsize = 0;
    // set global and local workitems
    size_t local_work = workgroupsize > 0 ? workgroupsize : 1;
    size_t global_work = nworkitems; //nworkitems = no. of GPU threads
    cl_int status;
    cl_mem I_d = clCreateBuffer( context, CL_MEM_READ_WRITE, cols * rows * sizeof(int), NULL, &status );
    check_status( status, "compute_scores_device clCreateBuffer 1 " );
    cl_mem S_d = clCreateBuffer( context, CL_MEM_READ_WRITE, cols * rows * sizeof(int), NULL, &status );
    check_status( status, "compute_scores_device clCreateBuffer 2 " );
    status = clEnqueueWriteBuffer( queue, I_d, CL_TRUE, 0, cols * rows * sizeof(int), I, 0, NULL, NULL );
    check_status( status, "compute_scores_device clEnqueueWriteBuffer 1 " );
    status = clEnqueueWriteBuffer( queue, S_d, CL_TRUE, 0, cols * rows * sizeof(int), S, 0, NULL, NULL );
    check_status( status, "compute_scores_device clEnqueueWriteBuffer 2 " );

    clSetKernelArg( kernels[2], 0, sizeof(cl_mem)                             , &S_d         );
    clSetKernelArg( kernels[2], 1, sizeof(cl_mem)                             , &I_d         );
    clSetKernelArg( kernels[2], 4, sizeof(int)                                , &cols        );
    clSetKernelArg( kernels[2], 5, sizeof(int)                                , &penalty     );
    clSetKernelArg( kernels[2], 6, sizeof(int) * ( BLOCK + 1 ) * ( BLOCK + 2 ), NULL         );
    clSetKernelArg( kernels[2], 7, sizeof(int) * ( BLOCK     ) * ( BLOCK     ), NULL         );

    for( int blk = 0; blk < rows / BLOCK; ++blk )
    {
      global_work = BLOCK * (blk+1);
      local_work  = BLOCK;
      int Y = 0;
      clSetKernelArg( kernels[2], 2, sizeof(int), &blk );
      clSetKernelArg( kernels[2], 3, sizeof(int), &Y );
      status = clEnqueueNDRangeKernel( queue, kernels[2], 1, NULL, &global_work, &local_work, 0, NULL, NULL );
      check_status( status, "compute_scores_device_rhombus clEnqueueNDRangeKernel 1 " );
      Y = 1;
      clSetKernelArg( kernels[2], 3, sizeof(int), &Y );
      status = clEnqueueNDRangeKernel( queue, kernels[2], 1, NULL, &global_work, &local_work, 0, NULL, NULL );
      check_status( status, "compute_scores_device_rhombus clEnqueueNDRangeKernel 2 " );
    }
    for( int blk = 2; blk <= cols / BLOCK; ++blk )
    {
      global_work = rows - 1;
      local_work  = BLOCK;
      int X = rows / BLOCK - 1;
      clSetKernelArg( kernels[2], 2, sizeof(int), &X );
      clSetKernelArg( kernels[2], 3, sizeof(int), &blk );
      status = clEnqueueNDRangeKernel( queue, kernels[2], 1, NULL, &global_work, &local_work, 0, NULL, NULL );
      check_status( status, "compute_scores_device_rhombus clEnqueueNDRangeKernel 2 " );
    }
    status = clEnqueueReadBuffer( queue, I_d, CL_TRUE, 0, cols * rows * sizeof(int), O, 0, NULL, NULL );
    check_status( status, "compute_scores_device clEnqueueReadBuffer " );
    clReleaseMemObject( I_d );
    clReleaseMemObject( S_d );
  }

  void usage( char **argv )
  {
    const char *help = "Usage: %s [-p <platform>] [-d <device>] [-o <outfile>]  <size> <penalty>\n"
      "\t<size>    - sequence length\n"
      "\t<penalty> - gap penalty (positive integer)\n"
      "\t<outfile> - output file\n";
    fprintf( stderr, help, argv[0] );
    exit( EXIT_FAILURE );
  }

  void run()
  {
    init();
    init_cl();
    double t1 = gettime();
    compute_scores_serial();
    double t2 = gettime();
    compute_scores_device();
    double t3 = gettime();
    compute_scores_device_rhombus();
    double t4 = gettime();
    compute_alignments();
    fini();
    fprintf( stdout, "BLOCKED %f\nSERIAL  %f\nRHOMBUS %f\n", t3 - t2, t2 - t1, t4 - t3 );
  }

  int *A;
  int *B;
  int *M;
  int *N;
  int *F;
  int *S;
  int *I;
  int *O;
  int rows, cols, penalty;
  const char *outfile;
  stringstream ss;

  cl_uint platform_index;
  cl_platform_id *platforms;
  cl_platform_id platform;
  cl_uint device_index;
  cl_device_id *devices;
  cl_device_id device;
  string kernel_file;
  string kernel_names[3];
  cl_kernel kernels[3];
  cl_context context;
  cl_command_queue queue;
  cl_program program;
#ifdef __MACH__
  host_name_port_t self;
  clock_serv_t cclock;
#endif
  double t1, t2;
};

const int Application::BLOSUM62[CHARS][CHARS] = {
  { 4, -1, -2, -2,  0, -1, -1,  0, -2, -1, -1, -1, -1, -2, -1,  1,  0, -3, -2,  0, -2, -1,  0, -4},
  {-1,  5,  0, -2, -3,  1,  0, -2,  0, -3, -2,  2, -1, -3, -2, -1, -1, -3, -2, -3, -1,  0, -1, -4},
  {-2,  0,  6,  1, -3,  0,  0,  0,  1, -3, -3,  0, -2, -3, -2,  1,  0, -4, -2, -3,  3,  0, -1, -4},
  {-2, -2,  1,  6, -3,  0,  2, -1, -1, -3, -4, -1, -3, -3, -1,  0, -1, -4, -3, -3,  4,  1, -1, -4},
  { 0, -3, -3, -3,  9, -3, -4, -3, -3, -1, -1, -3, -1, -2, -3, -1, -1, -2, -2, -1, -3, -3, -2, -4},
  {-1,  1,  0,  0, -3,  5,  2, -2,  0, -3, -2,  1,  0, -3, -1,  0, -1, -2, -1, -2,  0,  3, -1, -4},
  {-1,  0,  0,  2, -4,  2,  5, -2,  0, -3, -3,  1, -2, -3, -1,  0, -1, -3, -2, -2,  1,  4, -1, -4},
  { 0, -2,  0, -1, -3, -2, -2,  6, -2, -4, -4, -2, -3, -3, -2,  0, -2, -2, -3, -3, -1, -2, -1, -4},
  {-2,  0,  1, -1, -3,  0,  0, -2,  8, -3, -3, -1, -2, -1, -2, -1, -2, -2,  2, -3,  0,  0, -1, -4},
  {-1, -3, -3, -3, -1, -3, -3, -4, -3,  4,  2, -3,  1,  0, -3, -2, -1, -3, -1,  3, -3, -3, -1, -4},
  {-1, -2, -3, -4, -1, -2, -3, -4, -3,  2,  4, -2,  2,  0, -3, -2, -1, -2, -1,  1, -4, -3, -1, -4},
  {-1,  2,  0, -1, -3,  1,  1, -2, -1, -3, -2,  5, -1, -3, -1,  0, -1, -3, -2, -2,  0,  1, -1, -4},
  {-1, -1, -2, -3, -1,  0, -2, -3, -2,  1,  2, -1,  5,  0, -2, -1, -1, -1, -1,  1, -3, -1, -1, -4},
  {-2, -3, -3, -3, -2, -3, -3, -3, -1,  0,  0, -3,  0,  6, -4, -2, -2,  1,  3, -1, -3, -3, -1, -4},
  {-1, -2, -2, -1, -3, -1, -1, -2, -2, -3, -3, -1, -2, -4,  7, -1, -1, -4, -3, -2, -2, -1, -2, -4},
  { 1, -1,  1,  0, -1,  0,  0,  0, -1, -2, -2,  0, -1, -2, -1,  4,  1, -3, -2, -2,  0,  0,  0, -4},
  { 0, -1,  0, -1, -1, -1, -1, -2, -2, -1, -1, -1, -1, -2, -1,  1,  5, -2, -2,  0, -1, -1,  0, -4},
  {-3, -3, -4, -4, -2, -2, -3, -2, -2, -3, -2, -3, -1,  1, -4, -3, -2, 11,  2, -3, -4, -3, -2, -4},
  {-2, -2, -2, -3, -2, -1, -2, -3,  2, -1, -1, -2, -1,  3, -3, -2, -2,  2,  7, -1, -3, -2, -1, -4},
  { 0, -3, -3, -3, -1, -2, -2, -3, -3,  3,  1, -2,  1, -1, -2, -2,  0, -3, -1,  4, -3, -2, -1, -4},
  {-2, -1,  3,  4, -3,  0,  1, -1,  0, -3, -4,  0, -3, -3, -2,  0, -1, -4, -3, -3,  4,  1, -1, -4},
  {-1,  0,  0,  1, -3,  3,  4, -2,  0, -3, -3,  1, -1, -3, -1,  0, -1, -3, -2, -2,  1,  4, -1, -4},
  { 0, -1, -1, -1, -2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -2,  0,  0, -2, -1, -1, -1, -1, -1, -4},
  {-4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4,  1}
};

int main(int argc, char** argv)
{
  try
  {
    Application app(argc, argv);
    app.run();
  }
  catch (string msg)
  {
    std::cerr << "nw: " << msg << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
