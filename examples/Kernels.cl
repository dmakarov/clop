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
