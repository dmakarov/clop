#define BLOCK_SIZE 16

#define F(i, j)  input_itemsets_l[ (j)      +  (i)      * (BLOCK_SIZE + 1)]
#define N(i, j)  input_itemsets_l[((j) - 1) +  (i)      * (BLOCK_SIZE + 1)]
#define W(i, j)  input_itemsets_l[ (j)      + ((i) - 1) * (BLOCK_SIZE + 1)]
#define NW(i, j) input_itemsets_l[((j) - 1) + ((i) - 1) * (BLOCK_SIZE + 1)]
#define S(i, j)  similarity_l[(j) + (i) * BLOCK_SIZE]

int
maximum( int a, int b, int c )
{
  int k = a > b ? a : b;
  return k > c ? k : c;
}

__kernel void
nw1(__global int *similarity_d, __global int *input_itemsets_d, int cols, int penalty, int blk, int block_width, int worksize, int offset_r, int offset_c, __local int *input_itemsets_l, __local int *similarity_l )
{
  // Block index
  int bx = get_group_id(0);
  // Thread index
  int tx = get_local_id(0);
  // Base elements
  int base = offset_r * cols + offset_c;
  int b_index_x = bx;
  int b_index_y = blk - 1 - bx;
  int index_nw  = base + cols * BLOCK_SIZE * b_index_y + BLOCK_SIZE * b_index_x;
  int index_w   = index_nw + cols;
  int index_n   = index_nw + tx + 1;
  int index     = index_n  + cols;
  if (tx == 0) F(tx, 0) = input_itemsets_d[index_nw];
  barrier( CLK_LOCAL_MEM_FENCE );
  for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
    S(ty, tx) =  similarity_d[index + cols * ty];
  barrier( CLK_LOCAL_MEM_FENCE );
  F((tx + 1), 0) = input_itemsets_d[index_w + cols * tx];
  barrier( CLK_LOCAL_MEM_FENCE );
  F(0, (tx + 1)) = input_itemsets_d[index_n];
  barrier( CLK_LOCAL_MEM_FENCE );
  for ( int m = 0; m < BLOCK_SIZE; ++m )
  {
    if ( tx <= m )
    {
      int t_index_x =  tx + 1;
      int t_index_y =  m - tx + 1;
      int match = NW( t_index_y, t_index_x ) + S( t_index_y - 1, t_index_x - 1 );
      int delete = W( t_index_y, t_index_x ) - penalty;
      int insert = N( t_index_y, t_index_x ) - penalty;
      F(t_index_y, t_index_x) = maximum( match, delete, insert );
    }
    barrier( CLK_LOCAL_MEM_FENCE );
  }
  barrier( CLK_LOCAL_MEM_FENCE );
  for ( int m = BLOCK_SIZE - 2; m >=0; --m )
  {
    if ( tx <= m )
    {
      int t_index_x =  tx + BLOCK_SIZE - m ;
      int t_index_y =  BLOCK_SIZE - tx;
      int match = NW( t_index_y, t_index_x ) + S( t_index_y - 1, t_index_x - 1 );
      int delete = W( t_index_y, t_index_x ) - penalty;
      int insert = N( t_index_y, t_index_x ) - penalty;
      F(t_index_y, t_index_x) = maximum( match, delete, insert );
    }
    barrier( CLK_LOCAL_MEM_FENCE );
  }
  for ( int ty = 0 ; ty < BLOCK_SIZE ; ++ty )
    input_itemsets_d[index + ty * cols] = F( ty + 1, tx + 1 );
}

__kernel void
nw2( __global int *similarity_d, __global int *input_itemsets_d, int cols, int penalty, int blk, int block_width, int worksize, int offset_r, int offset_c, __local int *input_itemsets_l, __local int *similarity_l )
{
  int bx = get_group_id(0);
  // Thread index
  int tx = get_local_id(0);
  // Base elements
  int base = offset_r * cols + offset_c;
  int b_index_x = bx + block_width - blk;
  int b_index_y = block_width - bx - 1;
  int index_nw  = base + cols * BLOCK_SIZE * b_index_y + BLOCK_SIZE * b_index_x;
  int index_w   = index_nw + cols;
  int index_n   = index_nw + tx + 1;
  int index     = index_n + cols;
  if (tx == 0) F(tx, 0) = input_itemsets_d[index_nw];
  for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
    S(ty, tx) =  similarity_d[index + cols * ty];
  barrier( CLK_LOCAL_MEM_FENCE );
  F((tx + 1), 0) = input_itemsets_d[index_w + cols * tx];
  barrier( CLK_LOCAL_MEM_FENCE );
  F(0, (tx + 1)) = input_itemsets_d[index_n];
  barrier( CLK_LOCAL_MEM_FENCE );
  for ( int m = 0 ; m < BLOCK_SIZE ; ++m )
  {
    if ( tx <= m )
    {
      int t_index_x =  tx + 1;
      int t_index_y =  m - tx + 1;
      int match = NW( t_index_y, t_index_x ) + S( t_index_y - 1, t_index_x - 1 );
      int delete = W( t_index_y, t_index_x ) - penalty;
      int insert = N( t_index_y, t_index_x ) - penalty;
      F(t_index_y, t_index_x) = maximum( match, delete, insert );
    }
    barrier( CLK_LOCAL_MEM_FENCE );
  }
  for ( int m = BLOCK_SIZE - 2; m >= 0; --m )
  {
    if ( tx <= m )
    {
      int t_index_x =  tx + BLOCK_SIZE - m ;
      int t_index_y =  BLOCK_SIZE - tx;
      int match = NW( t_index_y, t_index_x ) + S( t_index_y - 1, t_index_x - 1 );
      int delete = W( t_index_y, t_index_x ) - penalty;
      int insert = N( t_index_y, t_index_x ) - penalty;
      F(t_index_y, t_index_x) = maximum( match, delete, insert );
    }
    barrier( CLK_LOCAL_MEM_FENCE );
  }
  for ( int ty = 0 ; ty < BLOCK_SIZE ; ++ty )
    input_itemsets_d[index + ty * cols] = F( ty + 1, tx + 1 );
}

__kernel void
rhombus( __global const int* S, __global int* F, int Y, int X, int cols, int penalty, __local int* s, __local int* t )
{
  int bx = get_group_id( 0 );
  int tx = get_local_id( 0 );
  int xx = X + 2 * bx;
  int yy = Y -     bx;

  if ( xx == 0 )
  {
    // 1.
    int index    = cols * BLOCK_SIZE * yy + tx * cols + cols + 1;
    int index_w  = cols * BLOCK_SIZE * yy + tx * cols + cols;
    int index_nw = cols * BLOCK_SIZE * yy + tx * cols;
    int index_n0 = cols * BLOCK_SIZE * yy + 1;

    if ( tx == 0 ) t[0] = F[index_nw];
    for ( int ty = 0; ty < BLOCK_SIZE; ++ty ) s[tx * BLOCK_SIZE + ty] = S[index + ty];

    t[tx + 1] = F[index_n0 + tx];
    t[(tx + 1) * (BLOCK_SIZE + 2)] = F[index_w];
    barrier( CLK_LOCAL_MEM_FENCE );
    // 2.
    for ( int m = 0; m < BLOCK_SIZE; ++m )
    {
      if ( tx <= m )
      {
        int x = tx + 1;
        int y = m - tx + 1;

        t[y * (BLOCK_SIZE + 2) + x] = maximum( t[(y-1) * (BLOCK_SIZE + 2) + x-1] + s[(y-1) * BLOCK_SIZE + x-1],
                                               t[(y-1) * (BLOCK_SIZE + 2) + x] - penalty,
                                               t[y * (BLOCK_SIZE + 2) + x-1] - penalty );
      }
      barrier( CLK_LOCAL_MEM_FENCE );
    }
    // 3.
    for ( int ty = 0; ty < BLOCK_SIZE - tx; ++ty )
      F[index + ty] = t[(tx+1) * (BLOCK_SIZE + 2) + ty+1];
  }
  else if ( xx < cols / BLOCK_SIZE )
  {
    // 1.
    int index    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols + 1;
    int index_w  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols;
    int index_nw = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1);
    int index_n0 = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + 1;

    if ( tx == 0 ) t[0] = F[index_nw];

    for ( int ty = 0; ty < BLOCK_SIZE; ++ty ) s[tx * BLOCK_SIZE + ty] = S[index + ty];
    t[tx + 1] = F[index_n0 + tx];
    t[(tx + 1) * (BLOCK_SIZE + 2)] = F[index_w - 1];
    t[(tx + 1) * (BLOCK_SIZE + 2) + 1] = F[index_w];
    barrier( CLK_LOCAL_MEM_FENCE );
    // 2.
    for ( int m = 0; m < BLOCK_SIZE; ++m )
    {
      int x =  m + 2;
      int y =  tx + 1;

      t[y * (BLOCK_SIZE + 2) + x] = maximum( t[(y-1) * (BLOCK_SIZE + 2) + x-2] + s[(y-1) * BLOCK_SIZE + x-2], t[(y-1) * (BLOCK_SIZE + 2) + x-1] - penalty, t[y * (BLOCK_SIZE + 2) + x-1] - penalty );
      barrier( CLK_LOCAL_MEM_FENCE );
    }
    // 3.
    for ( int ty = 0; ty < BLOCK_SIZE; ++ty )
      F[index + ty] = t[(tx+1) * (BLOCK_SIZE + 2) + ty+2];
  }
  else if ( xx == cols / BLOCK_SIZE )
  {
    // 1.
    int index    = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols + 1;
    int index_w  = cols * BLOCK_SIZE * yy + BLOCK_SIZE * xx + tx * (cols - 1) + cols;

    for ( int ty = 0; ty < tx; ++ty ) s[tx * BLOCK_SIZE + ty] = S[index + ty];
    t[(tx + 1) * (BLOCK_SIZE + 2)] = F[index_w - 1];
    t[(tx + 1) * (BLOCK_SIZE + 2) + 1] = F[index_w];
    barrier( CLK_LOCAL_MEM_FENCE );
    // 2.
    for ( int m = 0; m < BLOCK_SIZE; ++m )
    {
      if ( m < tx )
      {
        int x =  m + 2;
        int y =  tx + 1;

        t[y * (BLOCK_SIZE + 2) + x] = maximum( t[(y-1) * (BLOCK_SIZE + 2) + x-2] + s[(y-1) * BLOCK_SIZE + x-2], t[(y-1) * (BLOCK_SIZE + 2) + x-1] - penalty, t[y * (BLOCK_SIZE + 2) + x-1] - penalty );
      }
      barrier( CLK_LOCAL_MEM_FENCE );
    }
    // 3.
    for ( int ty = 0; ty < tx; ++ty )
      F[index + ty] = t[(tx+1) * (BLOCK_SIZE + 2) + ty+2];
  }
}
