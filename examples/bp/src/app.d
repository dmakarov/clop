/*
  The MIT License (MIT)
  =====================

  Copyright (c) 2015 Dmitri Makarov <dmakarov@alumni.stanford.edu>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*/
module clop.examples.bp;

import std.conv;
import std.datetime;
import std.getopt;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;


class Application {

  alias float CL_FP;
  static immutable int SEED = 1;
  static immutable CL_FP ERROR = 0.0001f;
  static immutable int BLOCK_SIZE = 16;

  size_t input_n;               // number of input units
  size_t hidden_n;              // number of hidden units
  size_t output_n;              // number of output units
  size_t num_blocks;
  CL_FP[] input_units;          // the input units
  CL_FP[] hidden_units;         // the hidden units
  CL_FP[] hidden_deltas;        // hidden unit errors
  CL_FP[] output_units;         // the output units
  CL_FP[] output_deltas;        // output unit errors
  CL_FP[] partial_sum;
  CL_FP[] target;               // target vector
  NDArray!CL_FP input_weights;  // weights from input to hidden layer
  NDArray!CL_FP input_changes;
  NDArray!CL_FP hidden_weights; // weights from hidden to output layer
  NDArray!CL_FP hidden_changes;

  /++
   + Construct the application initializing all the data structures
   +/
  this(string[] args)
  {
    input_n        = n_in;
    hidden_n       = BLOCK_SIZE;
    output_n       = 1;
    num_blocks     = input_n / BLOCK_SIZE;
    input_weights  = new NDArray!CL_FP(input_n + 1, hidden_n + 1);
    input_changes  = new NDArray!CL_FP(input_n + 1, hidden_n + 1);
    hidden_weights = new NDArray!CL_FP(hidden_n + 1, output_n + 1);
    hidden_changes = new NDArray!CL_FP(hidden_n + 1, output_n + 1);
    input_units    = new CL_FP[input_n + 1];
    hidden_units   = new CL_FP[hidden_n + 1];
    hidden_deltas  = new CL_FP[hidden_n + 1];
    output_units   = new CL_FP[output_n + 1];
    output_deltas  = new CL_FP[output_n + 1];
    partial_sum    = new CL_FP[num_blocks * BLOCK_SIZE];
    target         = new CL_FP[output_n + 1];
    foreach (ref a; input_changes)
      a = 0;
    foreach (ref a; hidden_changes)
      a = 0;
    Mt19937 gen;
    gen.seed( SEED );
    foreach (ref a; input_units)
      a = uniform(0.0, 1.0, gen);
    foreach (ref a; target)
      a = uniform(0.0, 1.0, gen);
    foreach (ref a; input_weights)
      a = 0.00001f * uniform(0.0, 1.0, gen);
    foreach (ref a; hidden_weights)
      a = 0.00001f * uniform(0.0, 1.0, gen);

    char[] opencl_code = q{
      #pragma OPENCL EXTENSION cl_khr_fp64 : enable

      #define FP float

      #define BLOCK_SIZE 16
      #define ETA        0.3f
      #define MOMENTUM   0.3f
      #define ONEF       1.0f

      __kernel void bpnn_layerforward(__global FP *x, __global FP *w, __global FP *sums, __local FP *inputs, __local FP *summands, int n2)
      { // workgroups are organized so that get_group_id( 0 ) is always 1.
        int gcl = get_group_id( 1 );
        int row = get_local_id( 0 );
        int col = get_local_id( 1 );
        if ( row == 0 ) inputs[col] = x[n2 * gcl + col + 1];
        barrier( CLK_LOCAL_MEM_FENCE );
        int kk = row * n2 + col;
        int index = ( n2 + 1 ) * n2 * gcl + ( n2 + 1 ) * row + col + n2 + 2;
        summands[kk] = w[index] * inputs[row];
        barrier( CLK_LOCAL_MEM_FENCE );
        for ( int i = 1; i < n2; i = i * 2 )
        {
          if ( row % ( 2 * i ) == 0 ) summands[kk] += summands[( row + i ) * n2 + col];
          barrier( CLK_LOCAL_MEM_FENCE );
        }
        if ( row == 0 ) sums[gcl * n2 + col] = summands[kk];
      }

      __kernel void bpnn_adjust_weights(__global FP* deltas, __global FP* o, __global FP* weights, __global FP* changes)
      { // ii and jj have the same meaning as in host version of adjust_weights.
        int ii = get_global_id( 1 ) + 1;
        int jj = get_global_id( 0 ) + 1;
        int index = ii * ( 1 + get_global_size( 0 ) ) + jj;
        FP adjust = MOMENTUM * changes[index] + ( ONEF - MOMENTUM ) * ETA * o[ii] * deltas[jj];
        weights[index] += adjust;
        changes[index]  = adjust;
      }
    }.dup;
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

  bool opencl_train_kernel( CLHelper *cl, CL_FP *eo, CL_FP *eh )
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
    debug (VALIDATION)
    {
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
    }
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
    debug (VALIDATION)
    {
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
    }
    cl->releaseMemObject( d_input_units );
    cl->releaseMemObject( d_input_weights );
    cl->releaseMemObject( d_hidden_deltas );
    cl->releaseMemObject( d_input_changes );
    return valid;
  }

  bool clop_train_kernel(CLHelper *cl, CL_FP *eo, CL_FP *eh)
  {
    bool valid = true;

    mixin (compile(q{
          NDRange(col : 0 .. hidden_n, row : 0 .. input_n) {
            int gcl = get_group_id(1);
            int row = get_local_id(0);
            int col = get_local_id(1);
            if (row == 0)
            {
              inputs[col] = x[n2 * gcl + col + 1];
            }
            barrier(CLK_LOCAL_MEM_FENCE);
            int kk = row * n2 + col;
            int index = (n2 + 1) * n2 * gcl + (n2 + 1) * row + col + n2 + 2;
            summands[kk] = w[index] * inputs[row];
            barrier(CLK_LOCAL_MEM_FENCE);
            for (int i = 1; i < n2; i = i * 2)
            {
              if (row % (2 * i) == 0)
              {
                summands[kk] += summands[(row + i) * n2 + col];
              }
              barrier(CLK_LOCAL_MEM_FENCE);
            }
            if (row == 0)
            {
              sums[gcl * n2 + col] = summands[kk];
            }
          }
        }));

    for ( size_t j = 1; j <= hidden_n; ++j )
    {
      CL_FP sum = input_weights[0][j];
      for ( size_t k = 0; k < num_blocks; ++k ) sum += partial_sum[k * hidden_n + j - 1];
      hidden_units[j] = ONEF / ( ONEF + exp( -sum ) );
    }

    layerforward( hidden_units, output_units, hidden_weights, hidden_n, output_n );
    output_error( eo );
    hidden_error( eh );
    debug (VALIDATION)
    {
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
          writefln( "%2lu %.12f %.12f\n", ii, backup_hidden_units[ii], hidden_units[ii] );
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
    }
    adjust_weights( output_deltas, output_n, hidden_units, hidden_n, hidden_weights, hidden_changes );

    mixin (compile(q{
          NDRange(jj : 1 .. hidden_n, ii : 1 .. input_n) {
            FP adjust = MOMENTUM * changes[jj, ii] + (ONEF - MOMENTUM) * ETA * o[ii] * deltas[jj];
            weights[jj, ii] += adjust;
            changes[jj, ii]  = adjust;
          }
        }));

    debug (VALIDATION)
    {
      adjust_weights( hidden_deltas, hidden_n, input_units, input_n, backup_input_weights, backup_input_changes );
      for ( size_t i = 1; i <= input_n; ++i )
        for ( size_t j = 1; j <= hidden_n; ++j )
          if ( backup_input_weights[i][j] != input_weights[i][j] )
          {
            writefln( "%6lu:%2lu %.12f %.12f\n", i, j, backup_input_weights[i][j], input_weights[i][j] );
            valid = false;
            break;
          }
    }
    return valid;
  }

  void run()
  {
    opencl_train_kernel();
    clop_train_kernel();
  }
}

int
main(string[] args)
{
  auto platforms = runtime.get_platforms();
  foreach (p; 0 .. platforms.length)
    foreach (d; 0 .. platforms[p])
    {
      try
      {
        runtime.init(p, d);
        writeln("==================================================");
        auto app = new Application(args);
        app.run();
        writeln("==================================================");
      }
      catch (Exception msg)
      {
        writeln("BP: ", msg);
        return -1;
      }
      finally
      {
        runtime.shutdown();
      }
    }
  return 0;
}
