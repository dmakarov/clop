#define I(r,c) ((r) * cols + (c))

int max3(int a, int b, int c)
{
  int k = a > b ? a : b;
  return k > c ? k : c;
}

__kernel void
nw1( __global int *similarity_d,
     __global int *input_itemsets_d,
              int cols,
              int penalty,
              int blk,
              int block_width,
              int offset_r, int offset_c,
     __local  int *input_itemsets_l,
     __local  int *similarity_l )
{
  // Block index
  int bx = get_group_id(0);
  // Thread index
  int tx = get_local_id(0);
  // Base elements
  int base = offset_r * cols + offset_c;
  int b_index_x = bx;
  int b_index_y = blk - 1 - bx;
  int index_nw  = base + cols * BLOCK * b_index_y + BLOCK * b_index_x;
  int index_w   = index_nw + cols;
  int index_n   = index_nw + tx + 1;
  int index     = index_n  + cols;
  if (tx == 0) input_itemsets_l[tx * (BLOCK + 1)] = input_itemsets_d[index_nw];
  barrier( CLK_LOCAL_MEM_FENCE );
  for ( int ty = 0; ty < BLOCK; ++ty )
    similarity_l[(ty) * BLOCK + (tx)] =  similarity_d[index + cols * ty];
  barrier( CLK_LOCAL_MEM_FENCE );
  input_itemsets_l[(tx + 1) * (BLOCK + 1)] = input_itemsets_d[index_w + cols * tx];
  barrier( CLK_LOCAL_MEM_FENCE );
  input_itemsets_l[(tx + 1)] = input_itemsets_d[index_n];
  barrier( CLK_LOCAL_MEM_FENCE );
  for ( int m = 0; m < BLOCK; ++m )
  {
    if ( tx <= m )
    {
      int t_index_x =  tx + 1;
      int t_index_y =  m - tx + 1;
      input_itemsets_l[(t_index_x) + (t_index_y) * (BLOCK + 1)] =
        max3(input_itemsets_l[(t_index_x - 1) + (t_index_y - 1) * (BLOCK + 1)] + similarity_l[(t_index_y - 1) * BLOCK + (t_index_x - 1)],
             input_itemsets_l[(t_index_x - 1) + (t_index_y) * (BLOCK + 1)] - penalty,
             input_itemsets_l[(t_index_x) + (t_index_y - 1) * (BLOCK + 1)] - penalty);
    }
    barrier( CLK_LOCAL_MEM_FENCE );
  }
  barrier( CLK_LOCAL_MEM_FENCE );
  for ( int m = BLOCK - 2; m >=0; --m )
  {
    if ( tx <= m )
    {
      int t_index_x =  tx + BLOCK - m ;
      int t_index_y =  BLOCK - tx;
      input_itemsets_l[(t_index_x) + (t_index_y) * (BLOCK + 1)] =
        max3(input_itemsets_l[(t_index_x - 1) + (t_index_y - 1) * (BLOCK + 1)] + similarity_l[(t_index_y - 1) * BLOCK + (t_index_x - 1)],
             input_itemsets_l[(t_index_x - 1) + (t_index_y) * (BLOCK + 1)] - penalty,
             input_itemsets_l[(t_index_x) + (t_index_y - 1) * (BLOCK + 1)] - penalty);
    }
    barrier( CLK_LOCAL_MEM_FENCE );
  }
  for ( int ty = 0 ; ty < BLOCK ; ++ty )
    input_itemsets_d[index + ty * cols] = input_itemsets_l[( tx + 1 ) + ( ty + 1) * (BLOCK + 1)];
}

__kernel void
nw2( __global int *similarity_d,
     __global int *input_itemsets_d,
     int cols, int penalty, int blk,
     int block_width,
     int offset_r, int offset_c,
     __local int *input_itemsets_l,
     __local int *similarity_l )
{
  int bx = get_group_id(0);
  // Thread index
  int tx = get_local_id(0);
  // Base elements
  int base = offset_r * cols + offset_c;
  int b_index_x = bx + block_width - blk;
  int b_index_y = block_width - bx - 1;
  int index_nw  = base + cols * BLOCK * b_index_y + BLOCK * b_index_x;
  int index_w   = index_nw + cols;
  int index_n   = index_nw + tx + 1;
  int index     = index_n + cols;
  if (tx == 0) input_itemsets_l[tx * (BLOCK + 1)] = input_itemsets_d[index_nw];
  for ( int ty = 0; ty < BLOCK; ++ty )
    similarity_l[(ty) * BLOCK + (tx)] =  similarity_d[index + cols * ty];
  barrier( CLK_LOCAL_MEM_FENCE );
  input_itemsets_l[(tx + 1) * (BLOCK + 1)] = input_itemsets_d[index_w + cols * tx];
  barrier( CLK_LOCAL_MEM_FENCE );
  input_itemsets_l[(tx + 1)] = input_itemsets_d[index_n];
  barrier( CLK_LOCAL_MEM_FENCE );
  for ( int m = 0 ; m < BLOCK ; ++m )
  {
    if ( tx <= m )
    {
      int t_index_x =  tx + 1;
      int t_index_y =  m - tx + 1;
      int match = input_itemsets_l[( t_index_x - 1) + (t_index_y - 1) * (BLOCK + 1)] + similarity_l[(t_index_y - 1) * BLOCK + (t_index_x - 1)];
      int remove = input_itemsets_l[( t_index_x - 1) + (t_index_y) * (BLOCK + 1)] - penalty;
      int insert = input_itemsets_l[( t_index_x) + (t_index_y - 1) * (BLOCK + 1)] - penalty;
      input_itemsets_l[( t_index_x) + (t_index_y) * (BLOCK + 1)] = max3( match, remove, insert );
    }
    barrier( CLK_LOCAL_MEM_FENCE );
  }
  for ( int m = BLOCK - 2; m >= 0; --m )
  {
    if ( tx <= m )
    {
      int t_index_x =  tx + BLOCK - m ;
      int t_index_y =  BLOCK - tx;
      int match = input_itemsets_l[( t_index_x - 1) + (t_index_y - 1) * (BLOCK + 1)] + similarity_l[(t_index_y - 1) * BLOCK + (t_index_x - 1)];
      int remove = input_itemsets_l[( t_index_x - 1) + (t_index_y) * (BLOCK + 1)] - penalty;
      int insert = input_itemsets_l[( t_index_x) + (t_index_y - 1) * (BLOCK + 1)] - penalty;
      input_itemsets_l[( t_index_x) + (t_index_y) * (BLOCK + 1)] = max3( match, remove, insert );
    }
    barrier( CLK_LOCAL_MEM_FENCE );
  }
  for ( int ty = 0 ; ty < BLOCK ; ++ty )
    input_itemsets_d[index + ty * cols] = input_itemsets_l[( tx + 1 ) + ( ty + 1) * (BLOCK + 1)];
}

__kernel void rhombus(__global const int* A,
                      __global const int* M,
                      __global const int* N,
                      __global       int* B,
                                     int  cols,
                                     int  penalty,
                                     int  br,
                                     int  bc)
{
  __local int s[CHARS * CHARS];
  __local int t[(BLOCK + 1) * (BLOCK + 2)];
  int bx = get_group_id(0);
  int tx = get_local_id(0);
  int rr = (br -     bx) * BLOCK + 1;
  int cc = (bc + 2 * bx) * BLOCK + 1;

  int index   = I(rr + tx, cc - tx);
  int index_n = I(rr -  1, cc + tx);
  int index_w = index - 1;
  int ii = 0;

  while (ii + tx < CHARS)
  {
    for (int ty = 0; ty < CHARS; ++ty)
      s[ty * CHARS + ii + tx] = A[ty * CHARS + ii + tx];
    ii += BLOCK;
  }
  barrier(CLK_LOCAL_MEM_FENCE);

  if (bc == 1 && bx == 0)
    for (int m = 0; m < BLOCK; ++m)
    {
      int x = m - tx + 1;
      if (x > 0)
        B[I(rr + tx, x)] = max3(B[I(rr + tx - 1, x - 1)] + s[M[rr + tx] * CHARS + N[x]],
                                B[I(rr + tx - 1, x    )] - penalty,
                                B[I(rr + tx    , x - 1)] - penalty);
      barrier(CLK_GLOBAL_MEM_FENCE);
    }

  int y0 = (tx + 1) + (BLOCK + 1);
  int y1 = (tx + 1) * (BLOCK + 1);
  t[tx] = B[I(rr + tx - 1, cc - tx - 1)];
  t[y0] = B[index_w];
  t[y1] = B[index_n];
  barrier(CLK_LOCAL_MEM_FENCE);

  int y = M[rr + tx];
  int x = N[cc - tx];
  for (int k = 0; k < BLOCK; ++k)
  {
    int nextX = N[cc - tx + k + 1];
    t[(k + 2) * (BLOCK + 1) + tx + 1] = max3(t[k * (BLOCK + 1) + tx] + s[y * CHARS + x],
                                                  t[(k + 1) * (BLOCK + 1) + tx] - penalty,
                                                  t[(k + 1) * (BLOCK + 1) + tx + 1] - penalty);
    x = nextX;
    barrier(CLK_LOCAL_MEM_FENCE);
  }

  for (int k = 0; k < BLOCK; ++k)
    B[I(rr + k, cc - k + tx)] = t[(tx + 2) * (BLOCK + 1) + k + 1];
  barrier(CLK_GLOBAL_MEM_FENCE);

  if (cc + BLOCK == cols)
    for (int m = 0; m < BLOCK; ++m)
    {
      int x = cc + BLOCK + m - tx;
      if (x < cols)
        B[I(rr + tx, x)] = max3(B[I(rr + tx - 1, x - 1)] + s[M[rr + tx] * CHARS + N[x]],
                                B[I(rr + tx - 1, x    )] - penalty,
                                B[I(rr + tx    , x - 1)] - penalty);
      barrier(CLK_GLOBAL_MEM_FENCE);
    }
}

__kernel void rhomboid(__global const int* S,
                       __global       int* F,
                                      int  cols,
                                      int  penalty,
                                      int  diagonal)
{
  int bx = get_group_id(0);
  int tx = get_local_id(0);
  int height = (cols - 1) / BLOCK;
  int margin = 1;
  int r0 = diagonal < 2 * height ? margin - 1 + BLOCK * (1 - bx + diagonal / 2)
                                 : cols - 1 - BLOCK * bx;
  int c0 = diagonal < 2 * height ? margin - ((diagonal + 1) & 1) * BLOCK + 2 * bx * BLOCK
                                 : margin + (diagonal + 1 - 2 * (cols - 1) / BLOCK) * BLOCK + 2 * bx * BLOCK;
  int r = r0 - tx;
  c0 = c0 + tx;
  for (int k = 0; k < BLOCK; ++k)
  {
    int c = c0 + k;
    if (0 < c && c < cols)
      F[I(r, c)] = max3(F[I(r - 1, c - 1)] + S[I(r, c)], F[I(r, c - 1)] - penalty, F[I(r - 1, c)] - penalty);
    barrier(CLK_GLOBAL_MEM_FENCE);
  }
}
