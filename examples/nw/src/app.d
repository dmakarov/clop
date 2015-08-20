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
module clop.examples.nw;

import std.conv;
import std.datetime;
import std.format;
import std.getopt;
import std.random;
import std.stdio;

import derelict.opencl.cl;
import clop.compiler;

static auto run_once = true;

/**
 * Needleman-Wunsch algorithm implementation.
 */
class Application {
  static immutable int SEED  =  1;
  static immutable int CHARS = 24;
  static int[CHARS * CHARS] BLOSUM62 = [
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
  -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4,  1];

  const uint BLOCK_SIZE;

  NDArray!int F; // matrix of computed scores
  NDArray!int S; // matrix of matches
  int[] M; // characters of sequence A
  int[] N; // characters of sequence B
  int[] G; // copy of F for validation
  int rows;
  int cols;
  int penalty;

  cl_kernel kernel_noblocks;
  cl_kernel kernel_noblocks_indirectS;
  cl_kernel kernel_rectangles;
  cl_kernel kernel_rectangles_indirectS;
  cl_kernel kernel_diamonds;
  cl_kernel kernel_diamonds_indirectS;
  cl_kernel kernel_diamonds_indirectS_prefetch;

  bool do_animate = false;
  bool do_verify = false;
  History history;

  /**
   */
  this(string[] args)
  {
    uint block_size = 16;
    getopt(args, "animate|a", &do_animate, "block_size|b", &block_size, "verify|v", &do_verify);
    if (args.length != 3)
    {
      writefln("Usage: %s [-a -b <block size> -v] <sequence length> <penalty>", args[0]);
      throw new Exception("invalid command line arguments");
    }
    do_animate = do_animate && run_once;
    BLOCK_SIZE = block_size;
    rows       = to!(int)(args[1]) + 1;
    cols       = to!(int)(args[1]) + 1;
    penalty    = to!(int)(args[2]);
    if ((rows - 1) % BLOCK_SIZE != 0)
    {
      auto r = to!(string)(rows - 1);
      auto b = to!(string)(BLOCK_SIZE);
      throw new Exception("ERROR: rows # (" ~ r ~ ") must be a multiple of " ~ b);
    }
    F = new NDArray!int(rows, cols); assert(F !is null, "Can't allocate array F");
    S = new NDArray!int(rows, cols); assert(S !is null, "Can't allocate array S");
    G = new int[rows * cols];        assert(G !is null, "Can't allocate array G");
    M = new int[rows];               assert(M !is null, "Can't allocate array M");
    N = new int[cols];               assert(N !is null, "Can't allocate array N");

    Mt19937 gen;
    gen.seed(SEED);
    foreach (r; 1 .. rows)
    {
      F[r, 0] = -penalty * r;
      M[r] = uniform(1, CHARS, gen);
    }
    foreach (c; 1 .. cols)
    {
      F[0, c] = -penalty * c;
      N[c] = uniform(1, CHARS, gen);
    }
    foreach (r; 1 .. rows)
      foreach (c; 1 .. cols)
        S[r, c] = BLOSUM62[M[r] * CHARS + N[c]];

    /**
     */
    char[] code = q{
      #define I(r,c) ((r) * cols + (c))
      /* A forward declaration.
         Some OpenCL compilers issue a warning if it's not present. */
      int max3(int a, int b, int c);

      int max3(int a, int b, int c)
      {
        int k = a > b ? a : b;
        return k > c ? k : c;
      }

      /**
       */
      __kernel void
      nw_noblocks(__global const int* S,
                  __global       int* F,
                                 int  cols,
                                 int  penalty,
                                 int  diagonal)
      {
        int tx = get_global_id(0);
        int c = diagonal < cols ?            tx + 1 : diagonal - cols + tx + 1;
        int r = diagonal < cols ? diagonal - tx - 1 :            cols - tx - 1;
        F[I(r, c)] = max3(F[I(r - 1, c - 1)] + S[I(r, c)],
                          F[I(r - 1, c    )] - penalty,
                          F[I(r    , c - 1)] - penalty);
      } /* nw_noblocks */

      /**
       */
      __kernel void
      nw_noblocks_indirectS(__global const int* S,
                            __global const int* M,
                            __global const int* N,
                            __global       int* F,
                                           int  cols,
                                           int  penalty,
                                           int  diagonal)
      {
        int tx = get_global_id(0);
        int c = diagonal < cols ?            tx + 1 : diagonal - cols + tx + 1;
        int r = diagonal < cols ? diagonal - tx - 1 :            cols - tx - 1;
        F[I(r, c)] = max3(F[I(r - 1, c - 1)] + S[M[r] * CHARS + N[c]],
                          F[I(r - 1, c    )] - penalty,
                          F[I(r    , c - 1)] - penalty);
      } /* nw_noblocks_indirectS */

      /**
       */
      __kernel void
      nw_rectangles(__global const int* S,
                    __global       int* F,
                                   int  cols,
                                   int  penalty,
                                   int  br,
                                   int  bc)
      {
        __local int s[ BLOCK_SIZE      *  BLOCK_SIZE];
        __local int t[(BLOCK_SIZE + 1) * (BLOCK_SIZE + 1)];
        int bx = get_group_id(0);
        int tx = get_local_id(0);
        int rr = (br - bx) * BLOCK_SIZE + 1; // these are the indexes of
        int cc = (bc + bx) * BLOCK_SIZE + 1; // the block's top-left element
        // 1.
        int index   = I(rr, cc + tx);
        int index_n = index - cols;
        int index_w = I(rr + tx, cc - 1);

        for (int k = 0; k < BLOCK_SIZE; ++k)
          s[k * BLOCK_SIZE + tx] = S[I(k, index)];

        if (tx == 0) t[0] = F[index_n - 1];          // copy the NW element into the TL corner of t
        t[(tx + 1) * (BLOCK_SIZE + 1)] = F[index_w]; // copy the column of W elements into the left column of t
        t[(tx + 1)                   ] = F[index_n]; // copy the row of N elements into the top row of t
        barrier(CLK_LOCAL_MEM_FENCE);
        // 2.
        for (int k = 0; k < BLOCK_SIZE; ++k)
        {
          if (tx <= k)
          {
            int x =     tx + 1;
            int y = k - tx + 1;
            t[y * (BLOCK_SIZE + 1) + x] = max3(t[(y - 1) * (BLOCK_SIZE + 1) + x - 1] + s[(y - 1) * BLOCK_SIZE + x - 1],
                                               t[(y - 1) * (BLOCK_SIZE + 1) + x    ] - penalty,
                                               t[(y    ) * (BLOCK_SIZE + 1) + x - 1] - penalty);
          }
          barrier(CLK_LOCAL_MEM_FENCE);
        }
        // 3.
        for (int k = BLOCK_SIZE - 2; k >= 0; --k)
        {
          if (tx <= k)
          {
            int x = BLOCK_SIZE + tx - k;
            int y = BLOCK_SIZE - tx;
            t[y * (BLOCK_SIZE + 1) + x] = max3(t[(y - 1) * (BLOCK_SIZE + 1) + x - 1] + s[(y - 1) * BLOCK_SIZE + x - 1],
                                               t[(y - 1) * (BLOCK_SIZE + 1) + x    ] - penalty,
                                               t[(y    ) * (BLOCK_SIZE + 1) + x - 1] - penalty);
          }
          barrier(CLK_LOCAL_MEM_FENCE);
        }
        // 4.
        for (int ty = 0; ty < BLOCK_SIZE; ++ty)
          F[I(ty, index)] = t[(ty + 1) * (BLOCK_SIZE + 1) + tx + 1];
      } /* nw_rectangles */

      /**
       */
      __kernel void
      nw_rectangles_indirectS(__global const int* S,
                              __global const int* M,
                              __global const int* N,
                              __global       int* F,
                                             int  cols,
                                             int  penalty,
                                             int  br,
                                             int  bc)
      {
        __local int s[CHARS * CHARS];
        __local int t[(BLOCK_SIZE + 1) * (BLOCK_SIZE + 1)];
        int bx = get_group_id(0);
        int tx = get_local_id(0);
        int rr = (br - bx) * BLOCK_SIZE + 1; // these are the indexes of
        int cc = (bc + bx) * BLOCK_SIZE + 1; // the block's top-left element
        // 1.
        int index   = I(rr, cc + tx);
        int index_n = index - cols;
        int index_w = I(rr + tx, cc - 1);

        // Copy BLOSUM array into shared memory.  Each thread copies
        // one or more columns.  The next columns are copied when the
        // number of threads in the group is smaller than the number
        // of columns in BLOSUM.
        int ii = 0;
        while (ii + tx < CHARS)
        {
          for (int ty = 0; ty < CHARS; ++ty)
            s[ty * CHARS + ii + tx] = S[ty * CHARS + ii + tx];
          ii += BLOCK_SIZE;
        }
        if (tx == 0) t[0] = F[index_n - 1];        // copy the NW element into the TL corner of t
        t[(tx + 1) * (BLOCK_SIZE + 1)] = F[index_w]; // copy the column of W elements into the left column of t
        t[(tx + 1)                   ] = F[index_n]; // copy the row of N elements into the top row of t
        barrier(CLK_LOCAL_MEM_FENCE);
        // 2.
        for (int k = 0; k < BLOCK_SIZE; ++k)
        {
          if (tx <= k)
          {
            int x =     tx + 1;
            int y = k - tx + 1;
            t[y * (BLOCK_SIZE + 1) + x] = max3(t[(y - 1) * (BLOCK_SIZE + 1) + x - 1] + s[M[rr + y - 1] * CHARS + N[cc + x - 1]],
                                               t[(y - 1) * (BLOCK_SIZE + 1) + x    ] - penalty,
                                               t[(y    ) * (BLOCK_SIZE + 1) + x - 1] - penalty);
          }
          barrier(CLK_LOCAL_MEM_FENCE);
        }
        // 3.
        for (int k = BLOCK_SIZE - 2; k >= 0; --k)
        {
          if (tx <= k)
          {
            int x = BLOCK_SIZE + tx - k;
            int y = BLOCK_SIZE - tx;
            t[y * (BLOCK_SIZE + 1) + x] = max3(t[(y - 1) * (BLOCK_SIZE + 1) + x - 1] + s[M[rr + y - 1] * CHARS + N[cc + x - 1]],
                                               t[(y - 1) * (BLOCK_SIZE + 1) + x    ] - penalty,
                                               t[(y    ) * (BLOCK_SIZE + 1) + x - 1] - penalty);
          }
          barrier(CLK_LOCAL_MEM_FENCE);
        }
        // 4.
        for (int ty = 0; ty < BLOCK_SIZE; ++ty)
          F[I(ty, index)] = t[(ty + 1) * (BLOCK_SIZE + 1) + tx + 1];
      } /* nw_rectangles_indirectS */

      /**
       */
      __kernel void
      nw_diamonds(__global const int* S,
                  __global       int* F,
                                 int  cols,
                                 int  penalty,
                                 int  br,
                                 int  bc)
      {
        __local int s[(BLOCK_SIZE    ) * (BLOCK_SIZE    )];
        __local int t[(BLOCK_SIZE + 1) * (BLOCK_SIZE + 2)];
        int bx = get_group_id(0);
        int tx = get_local_id(0);
        int rr = (br -     bx) * BLOCK_SIZE + 1;
        int cc = (bc + 2 * bx) * BLOCK_SIZE + 1;

        int index   = I(rr + tx, cc - tx);
        int index_n = I(rr -  1, cc + tx);
        int index_w = index - 1;

        if (bc == 1 && bx == 0)
          for (int m = 0; m < BLOCK_SIZE; ++m)
          {
            int x = m - tx + 1;
            if (x > 0)
              F[I(rr + tx, x)] = max3(F[I(rr + tx - 1, x - 1)] + S[I(rr + tx, x)],
                                      F[I(rr + tx - 1, x    )] - penalty,
                                      F[I(rr + tx    , x - 1)] - penalty);
            barrier(CLK_GLOBAL_MEM_FENCE);
          }

        // 1.
        for (int k = 0; k < BLOCK_SIZE; ++k)
          s[k * BLOCK_SIZE + tx] = S[I(rr + k, cc - k + tx)];
        if (tx == 0) t[0] = F[index_n - 1];
        int y0 = (tx + 1) * (BLOCK_SIZE + 2);
        int y1 = (tx    ) * (BLOCK_SIZE + 2);
        t[tx + 1] = F[index_n];
        t[y0    ] = F[index_w - 1];
        t[y0 + 1] = F[index_w];
        barrier(CLK_LOCAL_MEM_FENCE);
        // 2.

        for (int k = 0; k < BLOCK_SIZE; ++k)
        {
          t[y0 + k + 2] = max3(t[y1 + k] + s[tx * BLOCK_SIZE + k],
                               t[y1 + k + 1] - penalty, t[y0 + k + 1] - penalty);
          barrier(CLK_LOCAL_MEM_FENCE);
        }

        // 3.
        for (int k = 0; k < BLOCK_SIZE; ++k)
          F[I(rr + k, cc - k + tx)] = t[(k + 1) * (BLOCK_SIZE + 2) + tx + 2];
        barrier(CLK_GLOBAL_MEM_FENCE);

        if (cc + BLOCK_SIZE == cols)
          for (int m = 0; m < BLOCK_SIZE; ++m)
          {
            int x = cc + BLOCK_SIZE + m - tx;
            if (x < cols)
              F[I(rr + tx, x)] = max3(F[I(rr + tx - 1, x - 1)] + S[I(rr + tx, x)],
                                      F[I(rr + tx - 1, x    )] - penalty,
                                      F[I(rr + tx    , x - 1)] - penalty);
            barrier(CLK_GLOBAL_MEM_FENCE);
            }

      } /* nw_diamonds */

      /**
       */
      __kernel void
      nw_diamonds_indirectS(__global const int* S      , //
                            __global const int* M      , //
                            __global const int* N      , //
                            __global       int* F      , //
                                           int  cols   , //
                                           int  penalty, //
                                           int  br     , //
                                           int  bc     ) //
      {
        __local int s[CHARS * CHARS];
        __local int t[(BLOCK_SIZE + 1) * (BLOCK_SIZE + 2)];
        int bx = get_group_id(0);
        int tx = get_local_id(0);
        int rr = (br -     bx) * BLOCK_SIZE + 1;
        int cc = (bc + 2 * bx) * BLOCK_SIZE + 1;

        int index   = I(rr + tx, cc - tx);
        int index_n = I(rr -  1, cc + tx);
        int index_w = index - 1;
        int ii = 0;

        while (ii + tx < CHARS)
        {
          for (int ty = 0; ty < CHARS; ++ty)
            s[ty * CHARS + ii + tx] = S[ty * CHARS + ii + tx];
          ii += BLOCK_SIZE;
        }
        barrier(CLK_LOCAL_MEM_FENCE);

        if (bc == 1 && bx == 0)
          for (int m = 0; m < BLOCK_SIZE; ++m)
          {
            int x = m - tx + 1;
            if (x > 0)
              F[I(rr + tx, x)] = max3(F[I(rr + tx - 1, x - 1)] + s[M[rr + tx] * CHARS + N[x]],
                                      F[I(rr + tx - 1, x    )] - penalty,
                                      F[I(rr + tx    , x - 1)] - penalty);
            barrier(CLK_GLOBAL_MEM_FENCE);
          }

        if (tx == 0) t[0] = F[index_n - 1];
        int y0 = (tx + 1) * (BLOCK_SIZE + 2);
        int y1 = (tx    ) * (BLOCK_SIZE + 2);
        t[tx + 1] = F[index_n];
        t[y0    ] = F[index_w - 1];
        t[y0 + 1] = F[index_w];
        barrier(CLK_LOCAL_MEM_FENCE);

        for (int k = 0; k < BLOCK_SIZE; ++k)
        {
          t[y0 + k + 2] = max3(t[y1 + k] + s[M[rr + tx] * CHARS + N[cc - tx + k]],
                               t[y1 + k + 1] - penalty, t[y0 + k + 1] - penalty);
          barrier(CLK_LOCAL_MEM_FENCE);
        }

        for (int k = 0; k < BLOCK_SIZE; ++k)
          F[I(rr + k, cc - k + tx)] = t[(k + 1) * (BLOCK_SIZE + 2) + tx + 2];
        barrier(CLK_GLOBAL_MEM_FENCE);

        if (cc + BLOCK_SIZE == cols)
          for (int m = 0; m < BLOCK_SIZE; ++m)
          {
            int x = cc + BLOCK_SIZE + m - tx;
            if (x < cols)
              F[I(rr + tx, x)] = max3(F[I(rr + tx - 1, x - 1)] + s[M[rr + tx] * CHARS + N[x]],
                                      F[I(rr + tx - 1, x    )] - penalty,
                                      F[I(rr + tx    , x - 1)] - penalty);
            barrier(CLK_GLOBAL_MEM_FENCE);
          }
      } /* nw_diamonds_indirectS */

      /**
       */
      __kernel void
      nw_diamonds_indirectS_prefetch(__global const int* S      , //
                                     __global const int* M      , //
                                     __global const int* N      , //
                                     __global       int* F      , //
                                                    int  cols   , //
                                                    int  penalty, //
                                                    int  br     , //
                                                    int  bc     ) //
      {
        __local int s[CHARS * CHARS];
        __local int t[(BLOCK_SIZE + 1) * (BLOCK_SIZE + 2)];
        int bx = get_group_id(0);
        int tx = get_local_id(0);
        int rr = (br -     bx) * BLOCK_SIZE + 1;
        int cc = (bc + 2 * bx) * BLOCK_SIZE + 1;

        int index   = I(rr + tx, cc - tx);
        int index_n = I(rr -  1, cc + tx);
        int index_w = index - 1;
        int ii = 0;

        while (ii + tx < CHARS)
        {
          for (int ty = 0; ty < CHARS; ++ty)
            s[ty * CHARS + ii + tx] = S[ty * CHARS + ii + tx];
          ii += BLOCK_SIZE;
        }
        barrier(CLK_LOCAL_MEM_FENCE);

        if (bc == 1 && bx == 0)
          for (int m = 0; m < BLOCK_SIZE; ++m)
          {
            int x = m - tx + 1;
            if (x > 0)
              F[I(rr + tx, x)] = max3(F[I(rr + tx - 1, x - 1)] + s[M[rr + tx] * CHARS + N[x]],
                                      F[I(rr + tx - 1, x    )] - penalty,
                                      F[I(rr + tx    , x - 1)] - penalty);
            barrier(CLK_GLOBAL_MEM_FENCE);
          }

        if (tx == 0) t[0] = F[index_n - 1];
        int y0 = (tx + 1) * (BLOCK_SIZE + 2);
        int y1 = (tx    ) * (BLOCK_SIZE + 2);
        t[tx + 1] = F[index_n];
        t[y0    ] = F[index_w - 1];
        t[y0 + 1] = F[index_w];
        barrier(CLK_LOCAL_MEM_FENCE);

        int y = M[rr + tx];
        int x = N[cc - tx];
        for (int k = 0; k < BLOCK_SIZE; ++k)
        {
          int nextX = N[cc - tx + k + 1];
          t[y0 + k + 2] = max3(t[y1 + k] + s[y * CHARS + x],
                               t[y1 + k + 1] - penalty, t[y0 + k + 1] - penalty);
          x = nextX;
          barrier(CLK_LOCAL_MEM_FENCE);
        }

        for (int k = 0; k < BLOCK_SIZE; ++k)
          F[I(rr + k, cc - k + tx)] = t[(k + 1) * (BLOCK_SIZE + 2) + tx + 2];
        barrier(CLK_GLOBAL_MEM_FENCE);

        if (cc + BLOCK_SIZE == cols)
          for (int m = 0; m < BLOCK_SIZE; ++m)
          {
            int x = cc + BLOCK_SIZE + m - tx;
            if (x < cols)
              F[I(rr + tx, x)] = max3(F[I(rr + tx - 1, x - 1)] + s[M[rr + tx] * CHARS + N[x]],
                                      F[I(rr + tx - 1, x    )] - penalty,
                                      F[I(rr + tx    , x - 1)] - penalty);
            barrier(CLK_GLOBAL_MEM_FENCE);
          }
      } /* nw_diamonds_indirectS_prefetch */
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource(runtime.context, 1, strs.ptr, &size, &status);                                    assert(status == CL_SUCCESS, "this" ~ cl_strerror(status));
    auto clopts = format("-DBLOCK_SIZE=%s -DCHARS=%s", BLOCK_SIZE, CHARS);
    status = clBuildProgram(program, 1, &runtime.device, clopts.ptr, null, null);
    if (status != CL_SUCCESS)
    {
      char[4096] log;
      size_t log_size;
      clGetProgramBuildInfo(program, runtime.device, CL_PROGRAM_BUILD_LOG, log.length, log.ptr, &log_size);
      writeln("CL_PROGRAM_BUILD_LOG:\n", log[0 .. log_size - 1], "\nEOL");
    }                                                                                                                          assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel_noblocks             = clCreateKernel(program, "nw_noblocks"             , &status);                                assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel_rectangles           = clCreateKernel(program, "nw_rectangles"           , &status);                                assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel_diamonds             = clCreateKernel(program, "nw_diamonds"             , &status);                                assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel_noblocks_indirectS   = clCreateKernel(program, "nw_noblocks_indirectS"   , &status);                                assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel_rectangles_indirectS = clCreateKernel(program, "nw_rectangles_indirectS" , &status);                                assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel_diamonds_indirectS   = clCreateKernel(program, "nw_diamonds_indirectS"   , &status);                                assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    kernel_diamonds_indirectS_prefetch = clCreateKernel(program, "nw_diamonds_indirectS_prefetch", &status);                   assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    status = clReleaseProgram(program);                                                                                        assert(status == CL_SUCCESS, "this " ~ cl_strerror(status));
    if (do_animate)
    {
      history = new History("a.tex", rows);
    }
  } // this()

  /**
   */
  long I(long i, long j)
  {
    return i * cols + j;
  }

  /**
   */
  void verify()
  {
    if (do_verify)
    {
      auto diff = 0;
      foreach (ii; 0 .. F.length)
        if (F[ii] != G[ii]) ++diff;
      if (diff > 0)
        writeln("DIFFs ", diff);
    }
  }

  /**
   */
  void reset()
  {
    F[] = 0;
    foreach (c; 0 .. cols) F[0, c] = -penalty * c;
    foreach (r; 1 .. rows) F[r, 0] = -penalty * r;
  }

  /**
   * Maximum of three numbers.
   */
  int max3(immutable int a, immutable int b, immutable int c) const
  {
    auto k = a > b ? a : b;
    return k > c ? k : c;
  }

  /**
   * baseline_nw:
   * implements sequential computation of the alignment scores matrix.
   *
   * The algorithm of filling in the matrix F
   * <p>
   * <code>
   *    for i=0 to length(A) F(i,0) ← d*i
   *    for j=0 to length(B) F(0,j) ← d*j
   *    for i=1 to length(A)
   *    for j=1 to length(B)
   *    {
   *      Match  ← F(i-1, j-1) + S(Ai, Bj)
   *      Delete ← F(i-1, j ) + d
   *      Insert ← F(i  , j-1) + d
   *      F(i,j) ← max(Match, Insert, Delete)
   *    }
   * </code>
   */
  void baseline_nw()
  {
    foreach (r; 1 .. rows)
      foreach (c; 1 .. cols)
        F[r, c] = max3(F[r - 1, c - 1] + BLOSUM62[M[r] * CHARS + N[c]],
                       F[r - 1, c    ] - penalty,
                       F[r    , c - 1] - penalty);
  }

  /**
   * rectangles: implements sequential computation of the alignment
   * scores matrix for a single rectangular block of fixed size
   * #BLOCK_SIZE.
   */
  void rectangles()
  {
    auto ts = 0;
    auto max_blocks = (cols - 1) / BLOCK_SIZE;
    auto cur_blocks = 1;
    auto inc_blocks = 1;
    foreach (i; 0 .. 2 * max_blocks - 1)
    {
      auto br = i < max_blocks ? i :     max_blocks - 1;
      auto bc = i < max_blocks ? 0 : i - max_blocks + 1;
      foreach (bx; 0 .. cur_blocks)
      {
        foreach (k; 0 .. BLOCK_SIZE)
          foreach (tx; 0 .. BLOCK_SIZE)
            if (tx <= k)
            {
              auto r = br * BLOCK_SIZE + k - tx + 1;
              auto c = bc * BLOCK_SIZE     + tx + 1;
              F[r, c] = max3(F[r - 1, c - 1] + BLOSUM62[M[r] * CHARS + N[c]],
                             F[r - 1, c    ] - penalty,
                             F[r    , c - 1] - penalty);
              history.add_event(ts++, bx * BLOCK_SIZE + tx, I(r, c),
                                [I(r - 1, c - 1), I(r - 1, c), I(r, c - 1)]);
            }
        for (int k = BLOCK_SIZE - 2; k != -1; --k)
          foreach (tx; 0 .. BLOCK_SIZE)
            if (tx <= k)
            {
              auto r = br * BLOCK_SIZE + BLOCK_SIZE     - tx;
              auto c = bc * BLOCK_SIZE + BLOCK_SIZE - k + tx;
              F[r, c] = max3(F[r - 1, c - 1] + BLOSUM62[M[r] * CHARS + N[c]],
                             F[r - 1, c    ] - penalty,
                             F[r    , c - 1] - penalty);
              history.add_event(ts++, bx * BLOCK_SIZE + tx, I(r, c),
                                [I(r - 1, c - 1), I(r - 1, c), I(r, c - 1)]);
            }
        --br;
        ++bc;
      }
      history.add_event(ts++, -1, -1, []);
      if (i == max_blocks - 1) inc_blocks = -1;
      cur_blocks += inc_blocks;
    }
  }

  /**
   */
  int diamond_blocks(int groups, int br, int bc, int ts)
  {
    foreach (bx ; 0 .. groups)
    {
      int rr = br * BLOCK_SIZE + 1;
      int cc = bc * BLOCK_SIZE + 1;
      if (bc == 1 && bx == 0)
      {
        foreach (m; 0 .. BLOCK_SIZE)
        {
          foreach (tx; 0 .. BLOCK_SIZE)
          {
            int c = m - tx + 1;
            if (c > 0)
            {
              F[rr + tx, c] = max3(F[rr + tx - 1, c - 1] + BLOSUM62[M[rr + tx] * CHARS + N[c]],
                                   F[rr + tx - 1, c    ] - penalty,
                                   F[rr + tx    , c - 1] - penalty);
              history.add_event(ts++, bx * BLOCK_SIZE + tx, I(rr + tx, c), [I(rr + tx - 1, c - 1), I(rr + tx - 1, c), I(rr + tx, c - 1)]);
            }
          }
        }
      }
      foreach (c; 0 .. BLOCK_SIZE)
      {
        int k = cc;
        foreach (tx; 0 .. BLOCK_SIZE)
        {
          F[rr + tx, k + c] = max3(F[rr + tx - 1, k + c - 1] + BLOSUM62[M[rr + tx] * CHARS + N[k + c]],
                                   F[rr + tx - 1, k + c    ] - penalty,
                                   F[rr + tx    , k + c - 1] - penalty);
          history.add_event(ts++, bx * BLOCK_SIZE + tx, I(rr + tx, k + c), [I(rr + tx - 1, k + c - 1), I(rr + tx - 1, k + c), I(rr + tx, k + c - 1)]);
          --k;
        }
      }
      if (cc + BLOCK_SIZE == cols)
      {
        foreach (m; 0 .. BLOCK_SIZE)
        {
          foreach (tx; 0 .. BLOCK_SIZE)
          {
            int c = cc + BLOCK_SIZE + m - tx;
            if (c < cols)
            {
              F[rr + tx, c] = max3(F[rr + tx - 1, c - 1] + BLOSUM62[M[rr + tx] * CHARS + N[c]],
                                   F[rr + tx - 1, c    ] - penalty,
                                   F[rr + tx    , c - 1] - penalty);
              history.add_event(ts++, bx * BLOCK_SIZE + tx, I(rr + tx, c), [I(rr + tx - 1, c - 1), I(rr + tx - 1, c), I(rr + tx, c - 1)]);
            }
          }
        }
      }
      --br;
      bc += 2;
    }
    return ts;
  }

  /**
   */
  void diamonds()
  {
    auto ts = 0;
    auto max_groups = (cols - 1) / BLOCK_SIZE;
    foreach (i; 0 .. rows / BLOCK_SIZE - 1)
    {
      auto groups = 2 * i + 1 < max_groups ? i + 1 : max_groups / 2;
      ts = diamond_blocks(groups, i, 1, ts);
      history.add_event(ts++, -1, -1, []);
      groups = 2 * i + 2 < max_groups ? i + 1 : max_groups / 2 - 1;
      ts = diamond_blocks(groups, i, 2, ts);
      history.add_event(ts++, -1, -1, []);
    }
    foreach (i; 1 .. cols / BLOCK_SIZE)
    {
      auto groups = (max_groups - i + 1) / 2;
      ts = diamond_blocks(groups, rows / BLOCK_SIZE - 1, i, ts);
      history.add_event(ts++, -1, -1, []);
    }
  }

  /**
   */
  double opencl_noblocks()
  {
    double result = 0.0;
    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer(runtime.context, flags, cl_int.sizeof * S.length, S.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer(runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks, 0, cl_mem.sizeof, &dS     );                                                    assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks, 1, cl_mem.sizeof, &dF     );                                                    assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks, 2, cl_int.sizeof, &cols   );                                                    assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks, 3, cl_int.sizeof, &penalty);                                                    assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
      foreach (i; 2 .. 2 * cols - 1)
      {
        size_t global = (i < cols) ? i - 1 : 2 * cols - i - 1;
        status = clSetKernelArg(kernel_noblocks, 4, cl_int.sizeof, &i);                                                        assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_noblocks, 1, null, &global, null, 0, null, &event);              assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
      }
      status = clEnqueueReadBuffer(runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null);             assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
      status = clReleaseMemObject(dS);                                                                                         assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
      status = clReleaseMemObject(dF);                                                                                         assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
      status = clReleaseKernel(kernel_noblocks);                                                                               assert(status == CL_SUCCESS, "opencl_noblocks" ~ cl_strerror(status));
    }
    catch (Exception e)
    {
      write(e);
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_noblocks_indirectS()
  {
    double result = 0.0;
    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer(runtime.context, flags, cl_int.sizeof * BLOSUM62.length, BLOSUM62.ptr, &status);              assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      cl_mem dM = clCreateBuffer(runtime.context, flags, cl_int.sizeof * M.length, M.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      cl_mem dN = clCreateBuffer(runtime.context, flags, cl_int.sizeof * N.length, N.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer(runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks_indirectS, 0, cl_mem.sizeof, &dS     );                                          assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks_indirectS, 1, cl_mem.sizeof, &dM     );                                          assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks_indirectS, 2, cl_mem.sizeof, &dN     );                                          assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks_indirectS, 3, cl_mem.sizeof, &dF     );                                          assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks_indirectS, 4, cl_int.sizeof, &cols   );                                          assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_noblocks_indirectS, 5, cl_int.sizeof, &penalty);                                          assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      foreach (i; 2 .. 2 * cols - 1)
      {
        size_t global = (i < cols) ? i - 1 : 2 * cols - i - 1;
        status = clSetKernelArg(kernel_noblocks_indirectS, 6, cl_int.sizeof, &i);                                              assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_noblocks_indirectS, 1, null, &global, null, 0, null, &event);    assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
      }
      status = clEnqueueReadBuffer(runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null);             assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dF);                                                                                         assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dN);                                                                                         assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dM);                                                                                         assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dS);                                                                                         assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
      status = clReleaseKernel(kernel_noblocks_indirectS);                                                                     assert(status == CL_SUCCESS, "opencl_noblocks_indirectS" ~ cl_strerror(status));
    }
    catch(Exception e)
    {
      write(e);
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_rectangles()
  {
    double result = 0.0;
    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer(runtime.context, flags, cl_int.sizeof * S.length, S.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer(runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles, 0, cl_mem.sizeof, &dS     );                                                  assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles, 1, cl_mem.sizeof, &dF     );                                                  assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles, 2, cl_int.sizeof, &cols   );                                                  assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles, 3, cl_int.sizeof, &penalty);                                                  assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
      auto max_blocks = (cols - 1) / BLOCK_SIZE;
      auto cur_blocks = 1;
      auto inc_blocks = 1;
      foreach (i; 0 .. 2 * max_blocks - 1)
      {
        size_t wgroup = BLOCK_SIZE;
        size_t global = BLOCK_SIZE * cur_blocks;
        cl_int br = (i < max_blocks) ? i :     max_blocks - 1;
        cl_int bc = (i < max_blocks) ? 0 : i - max_blocks + 1;
        status = clSetKernelArg(kernel_rectangles, 4, cl_int.sizeof, &br);                                                     assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
        status = clSetKernelArg(kernel_rectangles, 5, cl_int.sizeof, &bc);                                                     assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_rectangles, 1, null, &global, &wgroup, 0, null, &event);         assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
        if (i == max_blocks - 1) inc_blocks = -1;
        cur_blocks += inc_blocks;
      }
      status = clEnqueueReadBuffer(runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null);             assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
      status = clReleaseMemObject(dS);                                                                                         assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
      status = clReleaseMemObject(dF);                                                                                         assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
      status = clReleaseKernel(kernel_rectangles);                                                                             assert(status == CL_SUCCESS, "opencl_rectangles" ~ cl_strerror(status));
    }
    catch(Exception e)
    {
      write(e);
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_rectangles_indirectS()
  {
    double result = 0.0;
    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer(runtime.context, flags, cl_int.sizeof * BLOSUM62.length, BLOSUM62.ptr, &status);              assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      cl_mem dM = clCreateBuffer(runtime.context, flags, cl_int.sizeof * M.length, M.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      cl_mem dN = clCreateBuffer(runtime.context, flags, cl_int.sizeof * N.length, N.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer(runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles_indirectS, 0, cl_mem.sizeof, &dS     );                                        assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles_indirectS, 1, cl_mem.sizeof, &dM     );                                        assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles_indirectS, 2, cl_mem.sizeof, &dN     );                                        assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles_indirectS, 3, cl_mem.sizeof, &dF     );                                        assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles_indirectS, 4, cl_int.sizeof, &cols   );                                        assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_rectangles_indirectS, 5, cl_int.sizeof, &penalty);                                        assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      auto max_blocks = (cols - 1) / BLOCK_SIZE;
      auto cur_blocks = 1;
      auto inc_blocks = 1;
      foreach (i; 0 .. 2 * max_blocks - 1)
      {
        size_t wgroup = BLOCK_SIZE;
        size_t global = BLOCK_SIZE * cur_blocks;
        cl_int br = (i < max_blocks) ? i :     max_blocks - 1;
        cl_int bc = (i < max_blocks) ? 0 : i - max_blocks + 1;
        status = clSetKernelArg(kernel_rectangles_indirectS, 6, cl_int.sizeof, &br);                                           assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
        status = clSetKernelArg(kernel_rectangles_indirectS, 7, cl_int.sizeof, &bc);                                           assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue,
                                        kernel_rectangles_indirectS,
                                        1,
                                        null,
                                        &global,
                                        &wgroup,
                                        0,
                                        null,
                                        &event);                                                                               assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
        if (i == max_blocks - 1) inc_blocks = -1;
        cur_blocks += inc_blocks;
      }
      status = clEnqueueReadBuffer(runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null);             assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dF);                                                                                         assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dN);                                                                                         assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dM);                                                                                         assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dS);                                                                                         assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
      status = clReleaseKernel(kernel_rectangles_indirectS);                                                                   assert(status == CL_SUCCESS, "opencl_rectangles_indirectS" ~ cl_strerror(status));
    }
    catch(Exception e)
    {
      write(e);
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_diamonds()
  {
    double result = 0.0;
    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer(runtime.context, flags, cl_int.sizeof * S.length, S.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer(runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds, 0, cl_mem.sizeof, &dS     );                                                    assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds, 1, cl_mem.sizeof, &dF     );                                                    assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds, 2, cl_int.sizeof, &cols   );                                                    assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds, 3, cl_int.sizeof, &penalty);                                                    assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
      size_t wgroup = BLOCK_SIZE;
      foreach (i; 0 .. rows / BLOCK_SIZE - 1)
      {
        cl_int br = i;
        cl_int bc = 1;
        size_t global = ((2 * i + bc) * BLOCK_SIZE < cols - 1) ? BLOCK_SIZE * (i + 1) : (cols - 1) / 2;
        status = clSetKernelArg(kernel_diamonds, 4, cl_int.sizeof, &br);                                                       assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        status = clSetKernelArg(kernel_diamonds, 5, cl_int.sizeof, &bc);                                                       assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_diamonds, 1, null, &global, &wgroup, 0, null, &event);           assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
        bc = 2;
        global = ((2 * i + bc) * BLOCK_SIZE < cols - 1) ? BLOCK_SIZE * (i + 1) : (cols - 1) / 2 - BLOCK_SIZE;
        if (global == 0) continue;
        status = clSetKernelArg(kernel_diamonds, 5, cl_int.sizeof, &bc);                                                       assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_diamonds, 1, null, &global, &wgroup, 0, null, &event);           assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));

        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
      }
      foreach (i; 1 .. cols / BLOCK_SIZE)
      {
        cl_int br = rows / BLOCK_SIZE - 1;
        cl_int bc = i;
        size_t global = BLOCK_SIZE * (((cols - 1) / BLOCK_SIZE - bc + 1) / 2);
        status = clSetKernelArg(kernel_diamonds, 4, cl_int.sizeof, &br);                                                       assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        status = clSetKernelArg(kernel_diamonds, 5, cl_int.sizeof, &bc);                                                       assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_diamonds, 1, null, &global, &wgroup, 0, null, &event);           assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_diamonds" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
      }
      status = clEnqueueReadBuffer(runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null);             assert(status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror(status));
      status = clReleaseMemObject(dS);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror(status));
      status = clReleaseMemObject(dF);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror(status));
      status = clReleaseKernel(kernel_diamonds);                                                                               assert(status == CL_SUCCESS, "opencl_diamonds " ~ cl_strerror(status));
    }
    catch(Exception e)
    {
      write(e);
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_diamonds_indirectS()
  {
    double result = 0.0;
    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer(runtime.context, flags, cl_int.sizeof * BLOSUM62.length, BLOSUM62.ptr, &status);              assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      cl_mem dM = clCreateBuffer(runtime.context, flags, cl_int.sizeof * M.length, M.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      cl_mem dN = clCreateBuffer(runtime.context, flags, cl_int.sizeof * N.length, N.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer(runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS, 0, cl_mem.sizeof, &dS     );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS, 1, cl_mem.sizeof, &dM     );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS, 2, cl_mem.sizeof, &dN     );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS, 3, cl_mem.sizeof, &dF     );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS, 4, cl_int.sizeof, &cols   );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS, 5, cl_int.sizeof, &penalty);                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      size_t wgroup = BLOCK_SIZE;
      foreach (i; 0 .. rows / BLOCK_SIZE - 1)
      {
        cl_int br = i;
        cl_int bc = 1;
        size_t global = ((2 * i + bc) * BLOCK_SIZE < cols - 1) ? BLOCK_SIZE * (i + 1) : (cols - 1) / 2;
        status = clSetKernelArg(kernel_diamonds_indirectS, 6, cl_int.sizeof, &br);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        status = clSetKernelArg(kernel_diamonds_indirectS, 7, cl_int.sizeof, &bc);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_diamonds_indirectS, 1, null, &global, &wgroup, 0, null, &event); assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
        bc = 2;
        global = ((2 * i + bc) * BLOCK_SIZE < cols - 1) ? BLOCK_SIZE * (i + 1) : (cols - 1) / 2 - BLOCK_SIZE;
        if (global == 0) continue;
        status = clSetKernelArg(kernel_diamonds_indirectS, 7, cl_int.sizeof, &bc);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_diamonds_indirectS, 1, null, &global, &wgroup, 0, null, &event); assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));

        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
      }
      foreach (i; 1 .. cols / BLOCK_SIZE)
      {
        cl_int br = rows / BLOCK_SIZE - 1;
        cl_int bc = i;
        size_t global = BLOCK_SIZE * (((cols - 1) / BLOCK_SIZE - bc + 1) / 2);
        status = clSetKernelArg(kernel_diamonds_indirectS, 6, cl_int.sizeof, &br);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        status = clSetKernelArg(kernel_diamonds_indirectS, 7, cl_int.sizeof, &bc);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_diamonds_indirectS, 1, null, &global, &wgroup, 0, null, &event); assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
      }
      status = clEnqueueReadBuffer(runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null);             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dF);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dN);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dM);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clReleaseMemObject(dS);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
      status = clReleaseKernel(kernel_diamonds_indirectS);                                                                     assert(status == CL_SUCCESS, "opencl_diamonds_indirectS" ~ cl_strerror(status));
    }
    catch(Exception e)
    {
      write(e);
      writeln();
    }
    return result;
  }

  /**
   */
  double opencl_diamonds_indirectS_prefetch()
  {
    double result = 0.0;
    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dS = clCreateBuffer(runtime.context, flags, cl_int.sizeof * BLOSUM62.length, BLOSUM62.ptr, &status);              assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      cl_mem dM = clCreateBuffer(runtime.context, flags, cl_int.sizeof * M.length, M.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      cl_mem dN = clCreateBuffer(runtime.context, flags, cl_int.sizeof * N.length, N.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
      cl_mem dF = clCreateBuffer(runtime.context, flags, cl_int.sizeof * F.length, F.ptr, &status);                            assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 0, cl_mem.sizeof, &dS     );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 1, cl_mem.sizeof, &dM     );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 2, cl_mem.sizeof, &dN     );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 3, cl_mem.sizeof, &dF     );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 4, cl_int.sizeof, &cols   );                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 5, cl_int.sizeof, &penalty);                                          assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      size_t wgroup = BLOCK_SIZE;
      foreach (i; 0 .. rows / BLOCK_SIZE - 1)
      {
        cl_int br = i;
        cl_int bc = 1;
        size_t global = ((2 * i + bc) * BLOCK_SIZE < cols - 1) ? BLOCK_SIZE * (i + 1) : (cols - 1) / 2;
        status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 6, cl_int.sizeof, &br);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 7, cl_int.sizeof, &bc);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_diamonds_indirectS_prefetch, 1, null, &global, &wgroup, 0, null, &event); assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
        bc = 2;
        global = ((2 * i + bc) * BLOCK_SIZE < cols - 1) ? BLOCK_SIZE * (i + 1) : (cols - 1) / 2 - BLOCK_SIZE;
        if (global == 0) continue;
        status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 7, cl_int.sizeof, &bc);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_diamonds_indirectS_prefetch, 1, null, &global, &wgroup, 0, null, &event); assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));

        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
      }
      foreach (i; 1 .. cols / BLOCK_SIZE)
      {
        cl_int br = rows / BLOCK_SIZE - 1;
        cl_int bc = i;
        size_t global = BLOCK_SIZE * (((cols - 1) / BLOCK_SIZE - bc + 1) / 2);
        status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 6, cl_int.sizeof, &br);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        status = clSetKernelArg(kernel_diamonds_indirectS_prefetch, 7, cl_int.sizeof, &bc);                                             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        status = clEnqueueNDRangeKernel(runtime.queue, kernel_diamonds_indirectS_prefetch, 1, null, &global, &wgroup, 0, null, &event); assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        status = clWaitForEvents(1, &event);                                                                                   assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));

        cl_ulong start_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &start_time               , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        cl_ulong end_time;
        status = clGetEventProfilingInfo (event                     , // cl_event          event
                                          CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                          cl_ulong.sizeof           , // size_t            param_value_size
                                          &end_time                 , // void*             param_value
                                          null                     ); /* size_t*           param_value_size_ret */             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
        result += (end_time - start_time) / 1E9;
      }
      status = clEnqueueReadBuffer(runtime.queue, dF, CL_TRUE, 0, cl_int.sizeof * F.length, F.ptr, 0, null, null);             assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clReleaseMemObject(dF);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clReleaseMemObject(dN);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clReleaseMemObject(dM);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clReleaseMemObject(dS);                                                                                         assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
      status = clReleaseKernel(kernel_diamonds_indirectS_prefetch);                                                                     assert(status == CL_SUCCESS, "opencl_diamonds_indirectS_prefetch" ~ cl_strerror(status));
    }
    catch(Exception e)
    {
      write(e);
      writeln();
    }
    return result;
  }

  /**
   *  Needleman-Wunsch algorithm implementation that uses CLOP to
   *  generate the appropriate OpenCL kernel and API calls.
   */
  void clop_nw()
  {
    mixin(compile(q{
      int max3(int a, int b, int c)
      {
        int k = a > b ? a : b;
        return k > c ? k : c;
      }
      Antidiagonal NDRange(r : 1 .. rows, c : 1 .. cols) {
        F[r, c] = max3(F[r - 1, c - 1] + S[r, c],
                       F[r, c - 1] - penalty,
                       F[r - 1, c] - penalty);
      } apply(rectangular_blocking(BLOCK_SIZE, BLOCK_SIZE))
    }));
  }

  /**
   */
  void clop_nw_indirectS()
  {
    mixin(compile(q{
      int max3(int a, int b, int c)
      {
        int k = a > b ? a : b;
        return k > c ? k : c;
      }
      Antidiagonal NDRange(r : 1 .. rows, c : 1 .. cols) {
        F[r, c] = max3(F[r - 1, c - 1] + BLOSUM62[M[r] * CHARS + N[c]],
                       F[r, c - 1] - penalty,
                       F[r - 1, c] - penalty);
      } apply(rhomboid_blocking(BLOCK_SIZE, BLOCK_SIZE), prefetching())
    }));
  }

  /**
   */
  void run()
  {
    size_t size;
    StopWatch timer;
    TickDuration ticks;
    double benchmark = runtime.benchmark((rows - 1) * (cols - 1));
    double time;

    if (do_verify)
    {
      timer.start();
      baseline_nw();
      timer.stop();
      ticks = timer.peek();
      writefln("%2.0f MI BASELINE    %5.3f [s]",
               (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
               ticks.usecs / 1E6);
      G[] = F[];
    }

    if (do_animate)
    {
      reset();
      rectangles();
      writefln("%2.0f MI RECTANGLES", (rows - 1) * (cols - 1) / (1024.0 * 1024.0));
      history.save_animation();
      verify();

      reset();
      diamonds();
      writefln("%2.0f MI DIAMONDS", (rows - 1) * (cols - 1) / (1024.0 * 1024.0));
      history.save_animation();
      verify();
      run_once = false;
    }

    reset();
    timer.reset();
    timer.start();
    time = opencl_noblocks();
    timer.stop();
    ticks = timer.peek();
    writefln("%2.0f MI CL BASELINE %5.3f (%5.3f) [s], %7.2f MI/s, estimated %7.2f MI/s",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6, time,
              (rows - 1) * (cols - 1) / (1024 * 1024 * time),
              2 * benchmark / 5);
    verify();

    reset();
    timer.reset();
    timer.start();
    time = opencl_noblocks_indirectS();
    timer.stop();
    ticks = timer.peek();
    writefln("%2.0f MI CL INDIRECT %5.3f (%5.3f) [s], %7.2f MI/s",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6, time,
              (rows - 1) * (cols - 1) / (1024 * 1024 * time));
    verify();

    reset();
    timer.reset();
    timer.start();
    time = opencl_rectangles();
    timer.stop();
    ticks = timer.peek();
    writefln("%2.0f MI CL SQUARES  %5.3f (%5.3f) [s], %7.2f MI/s",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6, time,
              (rows - 1) * (cols - 1) / (1024 * 1024 * time));
    verify();

    reset();
    timer.reset();
    timer.start();
    time = opencl_rectangles_indirectS();
    timer.stop();
    ticks = timer.peek();
    writefln("%2.0f MI CL SQ INDI  %5.3f (%5.3f) [s], %7.2f MI/s",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6, time,
              (rows - 1) * (cols - 1) / (1024 * 1024 * time));
    verify();

    reset();
    timer.reset();
    timer.start();
    time = opencl_diamonds();
    timer.stop();
    ticks = timer.peek();
    writefln("%2.0f MI CL DIAMONDS %5.3f (%5.3f) [s], %7.2f MI/s",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6, time,
              (rows - 1) * (cols - 1) / (1024 * 1024 * time));
    verify();

    reset();
    timer.reset();
    timer.start();
    time = opencl_diamonds_indirectS();
    timer.stop();
    ticks = timer.peek();
    writefln("%2.0f MI CL DS INDI  %5.3f (%5.3f) [s], %7.2f MI/s",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6, time,
              (rows - 1) * (cols - 1) / (1024 * 1024 * time));
    verify();

    reset();
    timer.reset();
    timer.start();
    time = opencl_diamonds_indirectS_prefetch();
    timer.stop();
    ticks = timer.peek();
    writefln("%2.0f MI CL DS IN P  %5.3f (%5.3f) [s], %7.2f MI/s",
              (rows - 1) * (cols - 1) / (1024.0 * 1024.0),
              ticks.usecs / 1E6, time,
              (rows - 1) * (cols - 1) / (1024 * 1024 * time));
    verify();

    reset();
    clop_nw();
    verify();

    reset();
    clop_nw_indirectS();
    verify();
  }
}

int main(string[] args)
{
  auto platforms = runtime.get_platforms();
  foreach (p; 0 .. platforms.length)
    foreach (d; 1 .. platforms[p])
    {
      try
      {
        runtime.init(p, d);
        writeln("--------------------------------------------------");
        auto app = new Application(args);
        app.run();
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
      writeln("==================================================");
    }
  return 0;
}
