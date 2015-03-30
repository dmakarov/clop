#pragma OPENCL EXTENSION cl_khr_fp64 : enable

#define ETA        0.3f
#define MOMENTUM   0.3f
#define ONEF       1.0f

#define FP float

__kernel void
bpnn_layerforward(__global FP *x,
                  __global FP *w,
                  __global FP *sums,
                  __local  FP *inputs,
                  __local  FP *summands,
                           int n2)
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

__kernel void
bpnn_adjust_weights(__global FP* deltas,
                    __global FP* o,
                    __global FP* weights,
                    __global FP* changes)
{ // ii and jj have the same meaning as in host version of adjust_weights.
  int ii = get_global_id( 1 ) + 1;
  int jj = get_global_id( 0 ) + 1;
  int index = ii * ( 1 + get_global_size( 0 ) ) + jj;
  FP adjust = MOMENTUM * changes[index] + ( ONEF - MOMENTUM ) * ETA * o[ii] * deltas[jj];
  weights[index] += adjust;
  changes[index]  = adjust;
}
