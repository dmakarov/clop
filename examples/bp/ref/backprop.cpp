#include <cmath>
#include <sstream>
#include <unistd.h> // for getopt

#include "CLHelper.h"
#include "backprop.h"

//#define VALIDATION 1

using std::stringstream;

struct Config {
  static const int SEED = 1;
  static const CL_FP ERROR;
  Config() : size( -1 ), outfile( NULL ) { srandom( SEED ); }
  string summary()
  {
    ss << "\"SEED='" << SEED << "',size='" << size << "',outfile='" << ( outfile ? outfile : "(null)" ) << "'\"";
    return ss.str();
  }
  void output()
  {
    if ( NULL != outfile )
    {
      FILE *fp = fopen( outfile, "w" );
      if ( NULL == fp )
      {
        fprintf( stderr, "ERROR: can't create file %s\n", outfile );
        exit( EXIT_FAILURE );
      }
      fprintf( fp, "output error %.12f\nhidden error %.12f\n", out_err, hid_err );
      fclose( fp );
    }
  }
  int size;
  CL_FP out_err, hid_err;
  const char *outfile;
  stringstream ss;
};

const CL_FP Config::ERROR = 0.0001f;

struct BPNN
{
  BPNN( int n_in, int n_hidden, int n_out )
  {
    input_n        = n_in;
    hidden_n       = n_hidden;
    output_n       = n_out;
    input_units    = alloc_1d( input_n + 1 );
    input_weights  = alloc_2d( input_n + 1, hidden_n + 1 );
    input_changes  = alloc_2d( input_n + 1, hidden_n + 1 );
    hidden_units   = alloc_1d( hidden_n + 1 );
    hidden_deltas  = alloc_1d( hidden_n + 1 );
    hidden_weights = alloc_2d( hidden_n + 1, output_n + 1 );
    hidden_changes = alloc_2d( hidden_n + 1, output_n + 1 );
    output_units   = alloc_1d( output_n + 1 );
    output_deltas  = alloc_1d( output_n + 1 );
    target         = alloc_1d( output_n + 1 );
    randomize_1d( input_units, input_n );
    randomize_2d( input_weights, input_n, hidden_n );
    randomize_2d( hidden_weights, hidden_n, output_n );
    randomize_1d( target, output_n );
    zero_2d( input_changes, input_n, hidden_n );
    zero_2d( hidden_changes, hidden_n, output_n );
    num_blocks     = input_n / BLOCK_SIZE;
    partial_sum    = new CL_FP[num_blocks * BLOCK_SIZE];
  }
  ~BPNN()
  {
    delete [] partial_sum;
    free( input_units );
    free( input_weights[0] );
    free( input_weights );
    free( input_changes[0] );
    free( input_changes );
    free( hidden_units );
    free( hidden_deltas );
    free( hidden_weights[0] );
    free( hidden_weights );
    free( hidden_changes[0] );
    free( hidden_changes );
    free( output_units );
    free( output_deltas );
    free( target );
  }
  CL_FP* alloc_1d( int n ) throw ( string )
  {
    CL_FP *new_t = (CL_FP*) malloc( n * sizeof(CL_FP) );
    if ( NULL == new_t ) throw string( "BPNN::alloc_1d: couldn't allocate FPs" );
    return new_t;
  }
  CL_FP** alloc_2d( int m, int n ) throw ( string )
  {
    CL_FP **new_t = (CL_FP**) malloc( m * sizeof(CL_FP*) );
    if ( NULL == new_t ) throw string( "BPNN::alloc_2d: couldn't allocate ptrs" );
    new_t[0] = (CL_FP*) malloc( m * n * sizeof(CL_FP) );
    if ( NULL == new_t[0] ) throw string( "BPNN::alloc_2d: couldn't allocate FPs" );
    for ( int ii = 1; ii < m; ++ii ) new_t[ii] = new_t[0] + ii * n;
    return new_t;
  }
  void randomize_1d( CL_FP *w, int m )
  {
    for ( int ii = 0; ii <= m; ++ii )
    {
      CL_FP rr = random();
      w[ii] = rr / RAND_MAX;
    }
  }
  void randomize_2d( CL_FP **w, int m, int n )
  {
    for ( int ii = 0; ii <= m; ++ii )
      for ( int jj = 0; jj <= n; ++jj )
      {
        CL_FP rr = 0.00001f * random();
        w[ii][jj] = rr / RAND_MAX;
      }
  }
  void zero_1d( CL_FP *w, int m )
  {
    for ( int ii = 0; ii <= m; ++ii ) w[ii] = 0.0;
  }
  void zero_2d( CL_FP **w, int m, int n )
  {
    for ( int ii = 0; ii <= m; ++ii )
      for ( int jj = 0; jj <= n; ++jj )
        w[ii][jj] = 0.0;
  }
  void layerforward( CL_FP *l1, CL_FP *l2, CL_FP **conn, int n1, int n2 )
  {
    l1[0] = ONEF;                   // Set up thresholding unit
    for ( int j = 1; j <= n2; ++j ) // For each unit in second layer
    {                               // compute weighted sum of its inputs
      CL_FP sum = conn[0][j];
      for ( int i = 1; i <= n1; ++i ) sum += conn[i][j] * l1[i];
      l2[j] = ONEF / ( ONEF + exp( -sum ) );
    }
  }
  // this version of layerforward performs additions in exactly the
  // same order as the kernel version does.
  void layerforward_parallel( CL_FP *l1, CL_FP *l2, CL_FP **conn, int n1, int n2 )
  {
    float *work = new float[(n1 + 1) * (n2 + 1)];
    l1[0] = ONEF;
    for ( int r = 1; r <= n1; ++r )
      for ( int c = 1; c <= n2; ++c )
        work[r * (n2 + 1) + c] = l1[r] * conn[r][c];
    for ( int k = 1; k < n2; k *= 2 )
      for ( int i = 1; i <= n1; ++i )
        if ( ( i - 1 ) % ( 2 * k ) == 0 )
          for ( int j = 1; j <= n2; ++j )
            work[i * (n2 + 1) + j] += work[(i + k) * (n2 + 1) + j];
    for ( int j = 1; j <= n2; ++j )
    {
      CL_FP sum = conn[0][j];
      for ( int i = 1; i <= n1; i += n2 )
        sum += work[i * (n2 + 1) + j];
      l2[j] = ONEF / ( ONEF + exp( -sum ) );
    }
    delete [] work;
  }
  void output_error( CL_FP *err )
  {
    CL_FP errsum = 0.0;
    for ( size_t ii = 1; ii <= output_n; ++ii )
    {
      CL_FP o = output_units[ii];
      output_deltas[ii] = -o * ( ONEF - o ) * ( target[ii] - o );
      errsum += fabs( output_deltas[ii] );
    }
    *err = errsum;
  }
  void hidden_error( CL_FP *err )
  {
    CL_FP errsum = 0.0;
    for ( size_t ii = 1; ii <= hidden_n; ++ii )
    {
      CL_FP sum = 0.0;
      for ( size_t jj = 1; jj <= output_n; ++jj ) sum += output_deltas[jj] * hidden_weights[ii][jj];
      hidden_deltas[ii] = hidden_units[ii] * ( ONEF - hidden_units[ii] ) * sum;
      errsum += fabs( hidden_deltas[ii] );
    }
    *err = errsum;
  }
  void adjust_weights( CL_FP *deltas, int ndelta, CL_FP *o, int no, CL_FP **weights, CL_FP **changes )
  {
    o[0] = ONEF;
    for ( int ii = 1; ii <= ndelta; ++ii )
      for ( int jj = 0; jj <= no; ++jj )
      {
        changes[jj][ii] = MOMENTUM * changes[jj][ii] + ( ONEF - MOMENTUM ) * ETA * o[jj] * deltas[ii];
        weights[jj][ii] += changes[jj][ii];
      }
  }
  bool train_kernel( CLHelper *cl, CL_FP *eo, CL_FP *eh )
  {
    bool valid = true;
    cl_mem d_input_units   = cl->createBuffer( CL_MEM_READ_WRITE, ( input_n + 1 )                    * sizeof(CL_FP), NULL );
    cl_mem d_input_weights = cl->createBuffer( CL_MEM_READ_WRITE, ( input_n + 1 ) * ( hidden_n + 1 ) * sizeof(CL_FP), NULL );
    cl_mem d_partial_sum   = cl->createBuffer( CL_MEM_READ_WRITE, input_n                            * sizeof(CL_FP), NULL );
    cl->enqueueWriteBuffer( d_input_units  , CL_TRUE, 0, ( input_n + 1 )                    * sizeof(CL_FP), input_units     , 0, NULL, NULL );
    cl->enqueueWriteBuffer( d_input_weights, CL_TRUE, 0, ( input_n + 1 ) * ( hidden_n + 1 ) * sizeof(CL_FP), input_weights[0], 0, NULL, NULL );
    cl->setKernelArg( 0, 0, sizeof(cl_mem)                     , &d_input_units   );
    cl->setKernelArg( 0, 1, sizeof(cl_mem)                     , &d_input_weights );
    cl->setKernelArg( 0, 2, sizeof(cl_mem)                     , &d_partial_sum   );
    cl->setKernelArg( 0, 3, sizeof(CL_FP) * hidden_n           , NULL             );
    cl->setKernelArg( 0, 4, sizeof(CL_FP) * hidden_n * hidden_n, NULL             );
    cl->setKernelArg( 0, 5, sizeof(int)                        , &hidden_n        );
    size_t global_work[] = { hidden_n, input_n };
    size_t local_work[]  = { hidden_n, hidden_n };
    cl->enqueueNDRangeKernel( 0, 2, NULL, global_work, local_work, 0, NULL, NULL );
    cl->enqueueReadBuffer( d_partial_sum, CL_TRUE, 0, input_n * sizeof(CL_FP), partial_sum, 0, NULL, NULL );
    for ( size_t j = 1; j <= hidden_n; ++j )
    {
      CL_FP sum = input_weights[0][j];
      for ( size_t k = 0; k < num_blocks; ++k ) sum += partial_sum[k * hidden_n + j - 1];
      hidden_units[j] = ONEF / ( ONEF + exp( -sum ) );
    }
    cl->releaseMemObject( d_partial_sum );
    layerforward( hidden_units, output_units, hidden_weights, hidden_n, output_n );
    output_error( eo );
    hidden_error( eh );
#if VALIDATION
    CL_FP h_eo, h_eh;
    CL_FP* backup_hidden_units = new CL_FP[hidden_n + 1];
    memcpy( backup_hidden_units, hidden_units, sizeof(CL_FP) * ( hidden_n + 1 ) );
    layerforward_parallel( input_units, hidden_units, input_weights, input_n, hidden_n );
    layerforward( hidden_units, output_units, hidden_weights, hidden_n, output_n );
    output_error( &h_eo );
    hidden_error( &h_eh );
    //    if ( fabs( h_eo - *eo ) > Config::ERROR || fabs( h_eh - *eh ) > Config::ERROR )
    if ( h_eo != *eo || h_eh != *eh )
    {
      printf( "%.12f %.12f\n%.12f %.12f\n", *eo, *eh, h_eo, h_eh );
      valid = false;
    }
    for ( size_t ii = 0; ii <= hidden_n; ++ii )
      //      if ( fabs( backup_hidden_units[ii] - hidden_units[ii] ) > Config::ERROR )
      if ( backup_hidden_units[ii] != hidden_units[ii] )
      {
        printf( "%2lu %.12f %.12f\n", ii, backup_hidden_units[ii], hidden_units[ii] );
        valid = false;
        break;
      }
    delete [] backup_hidden_units;
    float **backup_input_weights = alloc_2d( input_n + 1, hidden_n + 1 );
    float **backup_input_changes = alloc_2d( input_n + 1, hidden_n + 1 );
    zero_2d( input_changes, input_n, hidden_n );
    for ( size_t i = 0; i <= input_n; ++i )
      for ( size_t j = 0; j <= hidden_n; ++j)
        backup_input_weights[i][j] = input_weights[i][j];
#endif
    adjust_weights( output_deltas, output_n, hidden_units, hidden_n, hidden_weights, hidden_changes );
    cl_mem d_hidden_deltas = cl->createBuffer( CL_MEM_READ_WRITE, ( hidden_n + 1 )                   * sizeof(CL_FP), NULL );
    cl_mem d_input_changes = cl->createBuffer( CL_MEM_READ_WRITE, ( input_n + 1 ) * ( hidden_n + 1 ) * sizeof(CL_FP), NULL );
    cl->enqueueWriteBuffer( d_hidden_deltas, CL_TRUE, 0, ( hidden_n + 1 )                   * sizeof(CL_FP), hidden_deltas   , 0, NULL, NULL );
    cl->enqueueWriteBuffer( d_input_weights, CL_TRUE, 0, ( input_n + 1 ) * ( hidden_n + 1 ) * sizeof(CL_FP), input_weights[0], 0, NULL, NULL );
    cl->enqueueWriteBuffer( d_input_changes, CL_TRUE, 0, ( input_n + 1 ) * ( hidden_n + 1 ) * sizeof(CL_FP), input_changes[0], 0, NULL, NULL );
    cl->setKernelArg( 1, 0, sizeof(cl_mem), &d_hidden_deltas );
    cl->setKernelArg( 1, 1, sizeof(cl_mem), &d_input_units );
    cl->setKernelArg( 1, 2, sizeof(cl_mem), &d_input_weights );
    cl->setKernelArg( 1, 3, sizeof(cl_mem), &d_input_changes );
    cl->enqueueNDRangeKernel( 1, 2, NULL, global_work, NULL, 0, NULL, NULL );
    cl->enqueueReadBuffer( d_input_weights, CL_TRUE, 0, ( input_n + 1 ) * ( hidden_n + 1 ) * sizeof(CL_FP), input_weights[0], 0, NULL, NULL );
    cl->enqueueReadBuffer( d_input_changes, CL_TRUE, 0, ( input_n + 1 ) * ( hidden_n + 1 ) * sizeof(CL_FP), input_changes[0], 0, NULL, NULL );
#if VALIDATION
    adjust_weights( hidden_deltas, hidden_n, input_units, input_n, backup_input_weights, backup_input_changes );
    for ( size_t i = 1; i <= input_n; ++i )
      for ( size_t j = 1; j <= hidden_n; ++j )
        //        if ( fabs( backup_input_weights[i][j] - input_weights[i][j] ) > Config::ERROR )
        if ( backup_input_weights[i][j] != input_weights[i][j] )
        {
          printf( "%6lu:%2lu %.12f %.12f\n", i, j, backup_input_weights[i][j], input_weights[i][j] );
          valid = false;
          break;
        }
    free( backup_input_weights[0] );
    free( backup_input_changes[0] );
    free( backup_input_weights );
    free( backup_input_changes );
#endif
    cl->releaseMemObject( d_input_units );
    cl->releaseMemObject( d_input_weights );
    cl->releaseMemObject( d_hidden_deltas );
    cl->releaseMemObject( d_input_changes );
    return valid;
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
};

static bool
backprop( CLHelper *cl, Config *cfg, double *prep ) throw ( string )
{
  double t1 = gettime();
  BPNN *net = new BPNN( cfg->size, BLOCK_SIZE, 1 ); // ( 16, 1 can not be changed )
  if ( NULL == net ) throw string( "backprop: couldn't allocate neural network" );
  double t2 = gettime();
  *prep += t2 - t1;
  bool result = net->train_kernel( cl, &cfg->out_err, &cfg->hid_err );
  delete net;
  return result;
}

static void
usage( char **argv )
{
  const char *help = "Usage: %s [-p <platform>] [-d <device>] [-o <outfile>] <size>\n"
                     "\t<size>    - number of input elements, must be divisible by %d\n"
                     "\t<outfile> - name of the output file\n";
  fprintf( stderr, help, argv[0], BLOCK_SIZE );
  exit( EXIT_FAILURE );
}

static void
parse_command_line( int argc, char **argv, cl_uint *platform, cl_uint *device, Config *cfg )
{
  int ch;
  while ( ( ch = getopt( argc, argv, "d:o:p:" ) ) != -1 )
  {
    switch ( ch )
    {
    case 'd': sscanf( optarg, "%d", device );     break;
    case 'o': cfg->outfile = optarg;
    case 'p': sscanf( optarg, "%d", platform );   break;
    default:  usage( argv );
    }
  }
  if ( argc - optind < 1 ) usage( argv );
  if ( ( cfg->size = atoi( argv[optind] ) ) % BLOCK_SIZE != 0 )
  {
    fprintf( stderr, "ERROR: the number of input points (%d) must be divisible by %d\n", cfg->size, BLOCK_SIZE );
    exit( EXIT_FAILURE );
  }
}

int
main( int argc, char **argv )
{
  string kernel_file    = "Kernels.cl";
  string kernel_names[] = { "bpnn_layerforward", "bpnn_adjust_weights" };
  cl_uint platform      = 0;
  cl_uint device        = 0;
  bool passed           = false;
  CLHelper *cl          = NULL;
  Config cfg;

  parse_command_line( argc, argv, &platform, &device, &cfg );
  try
  {
    double prep = 0.0;
    double t1 = gettime();
    cl = new CLHelper( "backprop", platform, device, kernel_file, sizeof(kernel_names) / sizeof(string), kernel_names );
    passed = backprop( cl, &cfg, &prep );
    cl->release();
    double t2 = gettime();
    cl->statistics( t2 - t1, cfg.summary().c_str(), prep );
    cfg.output();
    delete cl;
  }
  catch ( std::string msg )
  {
    fprintf( stderr, "ERROR: exception caught in main function-> %s\n", msg.c_str() );
    if ( NULL != cl ) delete cl;
    return EXIT_FAILURE;
  }
  return passed ? EXIT_SUCCESS : EXIT_FAILURE;
}
