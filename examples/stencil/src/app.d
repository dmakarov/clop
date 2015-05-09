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
module clop.examples.stencil;

import std.conv;
import std.datetime;
import std.math;
import std.random;
import std.stdio;

import derelict.opencl.cl;

import clop.compiler;

/**
 */
class Application {
  alias float FLOAT_PRECISION;
  static immutable int SEED = 1;
  static immutable uint BLOCK_SIZE = 8;
  static immutable uint NUM_DOMAINS = 13;

  struct stencil_CTX {
    int bs_y;
  };

  FLOAT_PRECISION[] coef;
  FLOAT_PRECISION[] v;
  FLOAT_PRECISION[] u;
  FLOAT_PRECISION[] U;
  stencil_CTX stencil_ctx;

  uint xdim, ydim, zdim;

  cl_kernel kernel_stencil_5pt_noblocks;
  cl_kernel kernel_stencil_7pt_noblocks;
  cl_kernel kernel_stencil_7pt_blocks;
  cl_kernel kernel_stencil_7pt_shared;
  cl_kernel kernel_stencil_25pt_noblocks;
  cl_kernel kernel_stencil_25pt_blocks;
  cl_kernel kernel_stencil_25pt_shared;

  /**
   */
  this( string[] args )
  {
    if ( args.length != 4 )
    {
      throw new Exception( "ERROR: invalid args # " ~ to!(string)(args.length - 1) );
    }
    xdim = to!(uint)( args[1] );
    ydim = to!(uint)( args[2] );
    zdim = to!(uint)( args[3] );
    coef = new FLOAT_PRECISION[xdim * ydim * zdim * NUM_DOMAINS];
    assert( null != coef, "Can't allocate array coef" );
    v    = new FLOAT_PRECISION[xdim * ydim * zdim]; assert( null != v, "Can't allocate array v" );
    u    = new FLOAT_PRECISION[xdim * ydim * zdim]; assert( null != u, "Can't allocate array u" );
    U    = new FLOAT_PRECISION[xdim * ydim * zdim]; assert( null != U, "Can't allocate array U" );

    debug ( DEBUG )
    {
      writefln( "Stencil dimensions x:%d, y:%d, z:%d, total memory size allocated %d bytes",
                xdim, ydim, zdim, (NUM_DOMAINS + 3) * xdim * ydim * zdim * FLOAT_PRECISION.sizeof );
    }

    Mt19937 gen;
    gen.seed( SEED );
    foreach ( i; 0 .. xdim * ydim * zdim * NUM_DOMAINS )
    {
      coef[i] = uniform( 0.0, 1.0, gen );
    }
    foreach ( i; 0 .. xdim * ydim * zdim )
    {
      v[i]    = uniform( 0.0, 1.0, gen );
    }

    /**
     *  7pt stencil example implemented as kernel computing a single
     *  element of the entire space.
     **/
    char[] code = q{
      #define FLOAT_PRECISION float
      #define C( d, k, j, i ) ((d) * zdim * ydim * xdim + (k) * ydim * xdim + (j) * xdim + (i))
      #define V( k, j, i    ) (                           (k) * ydim * xdim + (j) * xdim + (i))

      #define LC( d, k, j, i ) ((d) * gz * gy * gx + (k) * gy * gx + (j) * gx + (i))
      #define LV( k, j, i    ) (                     (k) * gy * gx + (j) * gx + (i))

      __kernel void stencil_5pt_noblocks( __global       FLOAT_PRECISION * u,
                                          __global const FLOAT_PRECISION * v,
                                          __global const FLOAT_PRECISION * coef )
      {
        size_t tx = get_global_id( 0 ) + 1;
        size_t ty = get_global_id( 1 ) + 1;
        size_t xdim = get_global_size( 0 ) + 2;
        size_t ydim = get_global_size( 1 ) + 2;

        u[ty * xdim + tx] = coef[                  ty * xdim + tx] * v[(ty    ) * xdim + tx    ]
                          + coef[    xdim * ydim + ty * xdim + tx] * v[(ty    ) * xdim + tx - 1]
                          + coef[2 * xdim * ydim + ty * xdim + tx] * v[(ty    ) * xdim + tx + 1]
                          + coef[3 * xdim * ydim + ty * xdim + tx] * v[(ty - 1) * xdim + tx    ]
                          + coef[4 * xdim * ydim + ty * xdim + tx] * v[(ty + 1) * xdim + tx    ];
      } /* stencil_5pt_noblocks */

      __kernel void stencil_7pt_noblocks( __global       FLOAT_PRECISION * u,
                                          __global const FLOAT_PRECISION * v,
                                          __global const FLOAT_PRECISION * coef )
      {
        size_t tx = get_global_id( 0 ) + 1;
        size_t ty = get_global_id( 1 ) + 1;
        size_t tz = get_global_id( 2 ) + 1;
        size_t xdim = get_global_size( 0 ) + 2;
        size_t ydim = get_global_size( 1 ) + 2;
        size_t zdim = get_global_size( 2 ) + 2;

        u[V(tz,ty,tx)] = coef[C(0,tz,ty,tx)] * v[V(tz    ,ty    ,tx    )]
                       + coef[C(1,tz,ty,tx)] * v[V(tz    ,ty    ,tx - 1)]
                       + coef[C(2,tz,ty,tx)] * v[V(tz    ,ty    ,tx + 1)]
                       + coef[C(3,tz,ty,tx)] * v[V(tz    ,ty - 1,tx    )]
                       + coef[C(4,tz,ty,tx)] * v[V(tz    ,ty + 1,tx    )]
                       + coef[C(5,tz,ty,tx)] * v[V(tz - 1,ty    ,tx    )]
                       + coef[C(6,tz,ty,tx)] * v[V(tz + 1,ty    ,tx    )];
      } /* stencil_7pt_noblocks */

      __kernel void stencil_7pt_blocks( __global       FLOAT_PRECISION * u,
                                        __global const FLOAT_PRECISION * v,
                                        __global const FLOAT_PRECISION * coef )
      {
        size_t tx = get_global_id( 0 );
        size_t ty = get_global_id( 1 );
        size_t tz = get_global_id( 2 );
        size_t bs = get_local_size( 0 );
        size_t xdim = get_global_size( 0 );
        size_t ydim = bs * get_global_size( 1 );
        size_t zdim = get_global_size( 2 );

        for ( int y = 0; y < bs; ++y )
          if ( 0 < tz && tz < zdim - 1 && 0 < bs * ty + y && bs * ty + y < ydim - 1 && 0 < tx && tx < xdim - 1 )
            u[V(tz,bs * ty + y,tx)]
                           = coef[C(0,tz,bs * ty + y,tx)] * v[V(tz    ,bs * ty + y    ,tx    )]
                           + coef[C(1,tz,bs * ty + y,tx)] * v[V(tz    ,bs * ty + y    ,tx - 1)]
                           + coef[C(2,tz,bs * ty + y,tx)] * v[V(tz    ,bs * ty + y    ,tx + 1)]
                           + coef[C(3,tz,bs * ty + y,tx)] * v[V(tz    ,bs * ty + y - 1,tx    )]
                           + coef[C(4,tz,bs * ty + y,tx)] * v[V(tz    ,bs * ty + y + 1,tx    )]
                           + coef[C(5,tz,bs * ty + y,tx)] * v[V(tz - 1,bs * ty + y    ,tx    )]
                           + coef[C(6,tz,bs * ty + y,tx)] * v[V(tz + 1,bs * ty + y    ,tx    )];
      } /* stencil_7pt_blocked */

      __kernel void stencil_7pt_shared( __global       FLOAT_PRECISION * u,
                                        __global const FLOAT_PRECISION * v,
                                        __global const FLOAT_PRECISION * coef,
                                        __local        FLOAT_PRECISION * lv/*, __local        FLOAT_PRECISION * lc*/ )
      {
        size_t ix = get_local_id( 0 ) + 1;
        size_t iy = get_local_id( 1 ) + 1;
        size_t iz = get_local_id( 2 ) + 1;
        size_t gx = get_local_size( 0 ) + 2;
        size_t gy = get_local_size( 1 ) + 2;
        size_t gz = get_local_size( 2 ) + 2;
        size_t tx = get_global_id( 0 );
        size_t ty = get_global_id( 1 );
        size_t tz = get_global_id( 2 );
        size_t xdim = get_global_size( 0 );
        size_t ydim = get_global_size( 1 );
        size_t zdim = get_global_size( 2 );

        lv[LV(iz,iy,ix)] = v[V(tz,ty,tx)];
        /*for ( int ii = 0; ii < 7; ++ii )
          lc[LC(iz,iy,ix,ii)] = coef[C(tz,ty,tx,ii)];*/
        if ( 1 == iz && 0 < tz )
        {
          lv[LV(iz - 1,iy,ix)] = v[V(tz - 1,ty,tx)];
          /*for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz - 1,iy,ix,ii)] = coef[C(tz - 1,ty,tx,ii)];*/
        }
        if ( iz == gz - 2 && tz + 1 < zdim )
        {
          lv[LV(iz + 1,iy,ix)] = v[V(tz + 1,ty,tx)];
          /*for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz + 1,iy,ix,ii)] = coef[C(tz + 1,ty,tx,ii)];*/
        }
        if ( 1 == iy && 0 < ty )
        {
          lv[LV(iz,iy - 1,ix)] = v[V(tz,ty - 1,tx)];
          /*for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz,iy - 1,ix,ii)] = coef[C(tz,ty - 1,tx,ii)];*/
        }
        if ( iy == gy - 2 && ty + 1 < ydim )
        {
          lv[LV(iz,iy + 1,ix)] = v[V(tz,ty + 1,tx)];
          /*for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz,iy + 1,ix,ii)] = coef[C(tz,ty + 1,tx,ii)];*/
        }
        if ( 1 == ix && 0 < tx )
        {
          lv[LV(iz,iy,ix - 1)] = v[V(tz,ty,tx - 1)];
          /*for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz,iy,ix - 1,ii)] = coef[C(tz,ty,tx - 1,ii)];*/
        }
        if ( ix == gx - 2 && tx + 1 < xdim )
        {
          lv[LV(iz,iy,ix + 1)] = v[V(tz,ty,tx + 1)];
          /*for ( int ii = 0; ii < 7; ++ii )
            lc[LC(iz,iy,ix + 1,ii)] = coef[C(tz,ty,tx + 1,ii)];*/
        }
        barrier( CLK_LOCAL_MEM_FENCE );

        if ( 0 < tz && tz < zdim - 1 && 0 < ty && ty < ydim - 1 && 0 < tx && tx < xdim - 1 )
          u[V(tz,ty,tx)] = coef[C(0,tz,ty,tx)] * lv[LV(iz    ,iy    ,ix    )]
                         + coef[C(1,tz,ty,tx)] * lv[LV(iz    ,iy    ,ix - 1)]
                         + coef[C(2,tz,ty,tx)] * lv[LV(iz    ,iy    ,ix + 1)]
                         + coef[C(3,tz,ty,tx)] * lv[LV(iz    ,iy - 1,ix    )]
                         + coef[C(4,tz,ty,tx)] * lv[LV(iz    ,iy + 1,ix    )]
                         + coef[C(5,tz,ty,tx)] * lv[LV(iz - 1,iy    ,ix    )]
                         + coef[C(6,tz,ty,tx)] * lv[LV(iz + 1,iy    ,ix    )];
/*
          u[V(tz,ty,tx)] = lc[LC(0,iz,iy,ix)] * lv[LV(iz    ,iy    ,ix    )]
                         + lc[LC(1,iz,iy,ix)] * lv[LV(iz    ,iy    ,ix - 1)]
                         + lc[LC(2,iz,iy,ix)] * lv[LV(iz    ,iy    ,ix + 1)]
                         + lc[LC(3,iz,iy,ix)] * lv[LV(iz    ,iy - 1,ix    )]
                         + lc[LC(4,iz,iy,ix)] * lv[LV(iz    ,iy + 1,ix    )]
                         + lc[LC(5,iz,iy,ix)] * lv[LV(iz - 1,iy    ,ix    )]
                         + lc[LC(6,iz,iy,ix)] * lv[LV(iz + 1,iy    ,ix    )];
*/
      } /* stencil_7pt_shared */

      __kernel void stencil_25pt_noblocks( __global       FLOAT_PRECISION * u,
                                           __global const FLOAT_PRECISION * v,
                                           __global const FLOAT_PRECISION * coef )
      {
        size_t tx = get_global_id( 0 ) + 4;
        size_t ty = get_global_id( 1 ) + 4;
        size_t tz = get_global_id( 2 ) + 4;
        size_t xdim = get_global_size( 0 ) + 8;
        size_t ydim = get_global_size( 1 ) + 8;
        size_t zdim = get_global_size( 2 ) + 8;

        u[V(tz,ty,tx)] = coef[C( 0,tz,ty,tx)] *   v[V(tz    ,ty    ,tx    )]
                       + coef[C( 1,tz,ty,tx)] * ( v[V(tz    ,ty    ,tx - 1)] + v[V(tz    ,ty    ,tx + 1)] )
                       + coef[C( 2,tz,ty,tx)] * ( v[V(tz    ,ty - 1,tx    )] + v[V(tz    ,ty + 1,tx    )] )
                       + coef[C( 3,tz,ty,tx)] * ( v[V(tz - 1,ty    ,tx    )] + v[V(tz + 1,ty    ,tx    )] )
                       + coef[C( 4,tz,ty,tx)] * ( v[V(tz    ,ty    ,tx - 2)] + v[V(tz    ,ty    ,tx + 2)] )
                       + coef[C( 5,tz,ty,tx)] * ( v[V(tz    ,ty - 2,tx    )] + v[V(tz    ,ty + 2,tx    )] )
                       + coef[C( 6,tz,ty,tx)] * ( v[V(tz - 2,ty    ,tx    )] + v[V(tz + 2,ty    ,tx    )] )
                       + coef[C( 7,tz,ty,tx)] * ( v[V(tz    ,ty    ,tx - 3)] + v[V(tz    ,ty    ,tx + 3)] )
                       + coef[C( 8,tz,ty,tx)] * ( v[V(tz    ,ty - 3,tx    )] + v[V(tz    ,ty + 3,tx    )] )
                       + coef[C( 9,tz,ty,tx)] * ( v[V(tz - 3,ty    ,tx    )] + v[V(tz + 3,ty    ,tx    )] )
                       + coef[C(10,tz,ty,tx)] * ( v[V(tz    ,ty    ,tx - 4)] + v[V(tz    ,ty    ,tx + 4)] )
                       + coef[C(11,tz,ty,tx)] * ( v[V(tz    ,ty - 4,tx    )] + v[V(tz    ,ty + 4,tx    )] )
                       + coef[C(12,tz,ty,tx)] * ( v[V(tz - 4,ty    ,tx    )] + v[V(tz + 4,ty    ,tx    )] );
      } /* stencil_25pt_noblocks */

      __kernel void stencil_25pt_blocks( __global       FLOAT_PRECISION * u,
                                         __global const FLOAT_PRECISION * v,
                                         __global const FLOAT_PRECISION * coef )
      {
        size_t tx = get_global_id( 0 );
        size_t ty = get_global_id( 1 );
        size_t tz = get_global_id( 2 );
        size_t bs = get_local_size( 2 );
        size_t xdim = get_global_size( 0 );
        size_t ydim = get_global_size( 1 ) * bs;
        size_t zdim = get_global_size( 2 );

        for ( int y = 0; y < bs; ++y )
          if ( 0 < tz && tz < zdim - 1 && 0 < bs * ty + y && bs * ty + y < ydim - 1 && 0 < tx && tx < xdim - 1 )
            u[V(tz,bs * ty + y,tx)]
                       = coef[C( 0,tz,bs * ty + y,tx)] *   v[V(tz    ,bs * ty + y    ,tx    )]
                       + coef[C( 1,tz,bs * ty + y,tx)] * ( v[V(tz    ,bs * ty + y    ,tx - 1)] + v[V(tz    ,bs * ty + y    ,tx + 1)] )
                       + coef[C( 2,tz,bs * ty + y,tx)] * ( v[V(tz    ,bs * ty + y - 1,tx    )] + v[V(tz    ,bs * ty + y + 1,tx    )] )
                       + coef[C( 3,tz,bs * ty + y,tx)] * ( v[V(tz - 1,bs * ty + y    ,tx    )] + v[V(tz + 1,bs * ty + y    ,tx    )] )
                       + coef[C( 4,tz,bs * ty + y,tx)] * ( v[V(tz    ,bs * ty + y    ,tx - 2)] + v[V(tz    ,bs * ty + y    ,tx + 2)] )
                       + coef[C( 5,tz,bs * ty + y,tx)] * ( v[V(tz    ,bs * ty + y - 2,tx    )] + v[V(tz    ,bs * ty + y + 2,tx    )] )
                       + coef[C( 6,tz,bs * ty + y,tx)] * ( v[V(tz - 2,bs * ty + y    ,tx    )] + v[V(tz + 2,bs * ty + y    ,tx    )] )
                       + coef[C( 7,tz,bs * ty + y,tx)] * ( v[V(tz    ,bs * ty + y    ,tx - 3)] + v[V(tz    ,bs * ty + y    ,tx + 3)] )
                       + coef[C( 8,tz,bs * ty + y,tx)] * ( v[V(tz    ,bs * ty + y - 3,tx    )] + v[V(tz    ,bs * ty + y + 3,tx    )] )
                       + coef[C( 9,tz,bs * ty + y,tx)] * ( v[V(tz - 3,bs * ty + y    ,tx    )] + v[V(tz + 3,bs * ty + y    ,tx    )] )
                       + coef[C(10,tz,bs * ty + y,tx)] * ( v[V(tz    ,bs * ty + y    ,tx - 4)] + v[V(tz    ,bs * ty + y    ,tx + 4)] )
                       + coef[C(11,tz,bs * ty + y,tx)] * ( v[V(tz    ,bs * ty + y - 4,tx    )] + v[V(tz    ,bs * ty + y + 4,tx    )] )
                       + coef[C(12,tz,bs * ty + y,tx)] * ( v[V(tz - 4,bs * ty + y    ,tx    )] + v[V(tz + 4,bs * ty + y    ,tx    )] );
      } /* stencil_25pt_blocks */

      __kernel void stencil_25pt_shared( __global       FLOAT_PRECISION * u,
                                         __global const FLOAT_PRECISION * v,
                                         __global const FLOAT_PRECISION * coef,
                                         __local        FLOAT_PRECISION * lv )
      {
        size_t ix = get_local_id( 0 ) + 4;
        size_t iy = get_local_id( 1 ) + 4;
        size_t iz = get_local_id( 2 ) + 4;
        size_t gx = get_local_size( 0 ) + 8;
        size_t gy = get_local_size( 1 ) + 8;
        size_t gz = get_local_size( 2 ) + 8;
        size_t tx = get_global_id( 0 );
        size_t ty = get_global_id( 1 );
        size_t tz = get_global_id( 2 );
        size_t xdim = get_global_size( 0 );
        size_t ydim = get_global_size( 1 );
        size_t zdim = get_global_size( 2 );

        lv[LV(iz,iy,ix)] = v[V(tz,ty,tx)];
        if ( 4 == iz && 3 < tz )
        {
          lv[LV(iz - 1,iy,ix)] = v[V(tz - 1,ty,tx)];
          lv[LV(iz - 2,iy,ix)] = v[V(tz - 2,ty,tx)];
          lv[LV(iz - 3,iy,ix)] = v[V(tz - 3,ty,tx)];
          lv[LV(iz - 4,iy,ix)] = v[V(tz - 4,ty,tx)];
        }
        if ( iz == gz - 5 && tz + 4 < zdim )
        {
          lv[LV(iz + 1,iy,ix)] = v[V(tz + 1,ty,tx)];
          lv[LV(iz + 2,iy,ix)] = v[V(tz + 2,ty,tx)];
          lv[LV(iz + 3,iy,ix)] = v[V(tz + 3,ty,tx)];
          lv[LV(iz + 4,iy,ix)] = v[V(tz + 4,ty,tx)];
        }
        if ( 4 == iy && 3 < ty )
        {
          lv[LV(iz,iy - 1,ix)] = v[V(tz,ty - 1,tx)];
          lv[LV(iz,iy - 2,ix)] = v[V(tz,ty - 2,tx)];
          lv[LV(iz,iy - 3,ix)] = v[V(tz,ty - 3,tx)];
          lv[LV(iz,iy - 4,ix)] = v[V(tz,ty - 4,tx)];
        }
        if ( iy == gy - 5 && ty + 4 < ydim )
        {
          lv[LV(iz,iy + 1,ix)] = v[V(tz,ty + 1,tx)];
          lv[LV(iz,iy + 2,ix)] = v[V(tz,ty + 2,tx)];
          lv[LV(iz,iy + 3,ix)] = v[V(tz,ty + 3,tx)];
          lv[LV(iz,iy + 4,ix)] = v[V(tz,ty + 4,tx)];
        }
        if ( 4 == ix && 3 < tx )
        {
          lv[LV(iz,iy,ix - 1)] = v[V(tz,ty,tx - 1)];
          lv[LV(iz,iy,ix - 2)] = v[V(tz,ty,tx - 2)];
          lv[LV(iz,iy,ix - 3)] = v[V(tz,ty,tx - 3)];
          lv[LV(iz,iy,ix - 4)] = v[V(tz,ty,tx - 4)];
        }
        if ( ix == gx - 5 && tx + 4 < xdim )
        {
          lv[LV(iz,iy,ix + 1)] = v[V(tz,ty,tx + 1)];
          lv[LV(iz,iy,ix + 2)] = v[V(tz,ty,tx + 2)];
          lv[LV(iz,iy,ix + 3)] = v[V(tz,ty,tx + 3)];
          lv[LV(iz,iy,ix + 4)] = v[V(tz,ty,tx + 4)];
        }
        barrier( CLK_LOCAL_MEM_FENCE );

        if ( 3 < tz && tz < zdim - 4 && 3 < ty && ty < ydim - 4 && 3 < tx && tx < xdim - 4 )
          u[V(tz,ty,tx)] = coef[C( 0,tz,ty,tx)] *   lv[LV(iz    ,iy    ,ix    )]
                         + coef[C( 1,tz,ty,tx)] * ( lv[LV(iz    ,iy    ,ix - 1)] + lv[LV(iz    ,iy    ,ix + 1)] )
                         + coef[C( 2,tz,ty,tx)] * ( lv[LV(iz    ,iy - 1,ix    )] + lv[LV(iz    ,iy + 1,ix    )] )
                         + coef[C( 3,tz,ty,tx)] * ( lv[LV(iz - 1,iy    ,ix    )] + lv[LV(iz + 1,iy    ,ix    )] )
                         + coef[C( 4,tz,ty,tx)] * ( lv[LV(iz    ,iy    ,ix - 2)] + lv[LV(iz    ,iy    ,ix + 2)] )
                         + coef[C( 5,tz,ty,tx)] * ( lv[LV(iz    ,iy - 2,ix    )] + lv[LV(iz    ,iy + 2,ix    )] )
                         + coef[C( 6,tz,ty,tx)] * ( lv[LV(iz - 2,iy    ,ix    )] + lv[LV(iz + 2,iy    ,ix    )] )
                         + coef[C( 7,tz,ty,tx)] * ( lv[LV(iz    ,iy    ,ix - 3)] + lv[LV(iz    ,iy    ,ix + 3)] )
                         + coef[C( 8,tz,ty,tx)] * ( lv[LV(iz    ,iy - 3,ix    )] + lv[LV(iz    ,iy + 3,ix    )] )
                         + coef[C( 9,tz,ty,tx)] * ( lv[LV(iz - 3,iy    ,ix    )] + lv[LV(iz + 3,iy    ,ix    )] )
                         + coef[C(10,tz,ty,tx)] * ( lv[LV(iz    ,iy    ,ix - 4)] + lv[LV(iz    ,iy    ,ix + 4)] )
                         + coef[C(11,tz,ty,tx)] * ( lv[LV(iz    ,iy - 4,ix    )] + lv[LV(iz    ,iy + 4,ix    )] )
                         + coef[C(12,tz,ty,tx)] * ( lv[LV(iz - 4,iy    ,ix    )] + lv[LV(iz + 4,iy    ,ix    )] );
      } /* stencil_25pt_shared */
    }.dup;
    cl_int status;
    size_t size = code.length;
    char*[] strs = [code.ptr];
    auto program = clCreateProgramWithSource( runtime.context, 1, strs.ptr, &size, &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clBuildProgram( program, 1, &runtime.device, "", null, null );
    if ( CL_SUCCESS != status )
    {
      size_t param_value_size;
      status = clGetProgramBuildInfo ( program,
                                       runtime.device,
                                       CL_PROGRAM_BUILD_LOG,
                                       0,
                                       null,
                                       &param_value_size );
      assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
      char[] buffer = new char[param_value_size];
      assert( null != buffer, "Can't allocate buffer" );
      status = clGetProgramBuildInfo ( program,
                                       runtime.device,
                                       CL_PROGRAM_BUILD_LOG,
                                       param_value_size,
                                       buffer.ptr,
                                       null );
      assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
      writefln( "Kernel build log:\n%s", buffer );
    }
    kernel_stencil_5pt_noblocks  = clCreateKernel( program, "stencil_5pt_noblocks" , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_stencil_7pt_noblocks  = clCreateKernel( program, "stencil_7pt_noblocks" , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_stencil_7pt_blocks    = clCreateKernel( program, "stencil_7pt_blocks"   , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_stencil_7pt_shared    = clCreateKernel( program, "stencil_7pt_shared"   , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_stencil_25pt_noblocks = clCreateKernel( program, "stencil_25pt_noblocks", &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_stencil_25pt_blocks   = clCreateKernel( program, "stencil_25pt_blocks"  , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    kernel_stencil_25pt_shared   = clCreateKernel( program, "stencil_25pt_shared"  , &status );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
    status = clReleaseProgram( program );
    assert( status == CL_SUCCESS, "this " ~ cl_strerror( status ) );
  } // this()

  /**
   */
  void validate()
  {
    int diff = 0;
    foreach ( ii; 0 .. U.length )
      if ( U[ii] != float.nan && u[ii] != float.nan && fabs( U[ii] - u[ii] ) > 1E-4 )
      {
        ++diff;
        //writefln( "%5d : %f != %f", ii, U[ii], u[ii] );
      }
    if ( diff > 0 )
      writeln( "DIFFs ", diff );
  }

  /**
  */
  void cleanup()
  {
    foreach ( ref u ; U )
    {
      u = FLOAT_PRECISION.nan;
    }
  }

  FLOAT_PRECISION C( uint d, uint k, uint j, uint i )
  {
    return coef[d * zdim * ydim * xdim + k * ydim * xdim + j * xdim + i];
  }

  FLOAT_PRECISION V( uint k, uint j, uint i )
  {
    return v[k * ydim * xdim + j * xdim + i];
  }

  void sequential_stencil_7pt()
  {
    for ( uint jb = 1; jb < ydim - 1; jb += BLOCK_SIZE ) // blocking in Y
    {
      uint je = jb + BLOCK_SIZE < ydim - 1 ? jb + BLOCK_SIZE : ydim - 1;
      {
        for ( uint k = 1; k < zdim - 1; ++k )
          for ( uint j = jb; j < je; ++j )
            for ( uint i = 1; i < xdim - 1; ++i )
            {
              U[( k * ydim + j ) * xdim + i] = C(0,k,j,i) * V(k    ,j    ,i    )
                                             + C(1,k,j,i) * V(k    ,j    ,i - 1)
                                             + C(2,k,j,i) * V(k    ,j    ,i + 1)
                                             + C(3,k,j,i) * V(k    ,j - 1,i    )
                                             + C(4,k,j,i) * V(k    ,j + 1,i    )
                                             + C(5,k,j,i) * V(k - 1,j    ,i    )
                                             + C(6,k,j,i) * V(k + 1,j    ,i    );
            }
      }
    }
  }

  void sequential_stencil_25pt()
  {
    for ( uint jb = 4; jb < ydim - 4; jb += BLOCK_SIZE ) // blocking in Y
    {
      uint je = jb + BLOCK_SIZE < ydim - 4 ? jb + BLOCK_SIZE : ydim - 4;
      {
        for ( uint k = 4; k < zdim - 4; ++k )
          for ( uint j = jb; j < je; ++j )
            for ( uint i = 4; i < xdim - 4; ++i )
              U[( k * ydim + j ) * xdim + i] = C( 0,k,j,i) * ( V(k    ,j    ,i    )                        )
                                             + C( 1,k,j,i) * ( V(k    ,j    ,i + 1) + V(k    ,j    ,i - 1) )
                                             + C( 2,k,j,i) * ( V(k    ,j + 1,i    ) + V(k    ,j - 1,i    ) )
                                             + C( 3,k,j,i) * ( V(k + 1,j    ,i    ) + V(k - 1,j    ,i    ) )
                                             + C( 4,k,j,i) * ( V(k    ,j    ,i + 2) + V(k    ,j    ,i - 2) )
                                             + C( 5,k,j,i) * ( V(k    ,j + 2,i    ) + V(k    ,j - 2,i    ) )
                                             + C( 6,k,j,i) * ( V(k + 2,j    ,i    ) + V(k - 2,j    ,i    ) )
                                             + C( 7,k,j,i) * ( V(k    ,j    ,i + 3) + V(k    ,j    ,i - 3) )
                                             + C( 8,k,j,i) * ( V(k    ,j + 3,i    ) + V(k    ,j - 3,i    ) )
                                             + C( 9,k,j,i) * ( V(k + 3,j    ,i    ) + V(k - 3,j    ,i    ) )
                                             + C(10,k,j,i) * ( V(k    ,j    ,i + 4) + V(k    ,j    ,i - 4) )
                                             + C(11,k,j,i) * ( V(k    ,j + 4,i    ) + V(k    ,j - 4,i    ) )
                                             + C(12,k,j,i) * ( V(k + 4,j    ,i    ) + V(k - 4,j    ,i    ) );
      }
    }
  }

  double opencl_stencil_5pt_noblocks()
  {
    double result;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, null, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_5pt_noblocks, 0, cl_mem.sizeof, &du   );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_5pt_noblocks, 1, cl_mem.sizeof, &dv   );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_5pt_noblocks, 2, cl_mem.sizeof, &dc   );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );

      size_t[2] global = [8 * xdim - 2, 16 * ydim - 2];
      status = clEnqueueNDRangeKernel( runtime.queue              ,    //
                                       kernel_stencil_5pt_noblocks,    //
                                       2                          ,    //
                                       null                       ,    //
                                       global.ptr                 ,    //
                                       null                       ,    //
                                       0                          ,    //
                                       null                       ,    //
                                       &event                    );    //
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
      status = clWaitForEvents( 1, &event );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );

      cl_ulong start_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &start_time               , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      cl_ulong end_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &end_time                 , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      result = ( end_time - start_time ) / 1E9;

      status = clEnqueueReadBuffer( runtime.queue                    , //
                                    du                               , //
                                    CL_TRUE                          , //
                                    0                                , //
                                    FLOAT_PRECISION.sizeof * u.length, //
                                    u.ptr                            , //
                                    0                                , //
                                    null                             , //
                                    null                            ); //
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );

      status = clReleaseMemObject( du );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_5pt_noblocks );
      assert( status == CL_SUCCESS, "opencl_stencil_5pt_noblocks " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
    return result;
  }

  double opencl_stencil_7pt_noblocks()
  {
    double result;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, null, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 0, cl_mem.sizeof, &du   );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 1, cl_mem.sizeof, &dv   );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_noblocks, 2, cl_mem.sizeof, &dc   );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );

      size_t[3] global = [zdim - 2, ydim - 2, xdim - 2];
      status = clEnqueueNDRangeKernel( runtime.queue              ,    //
                                       kernel_stencil_7pt_noblocks,    //
                                       3                          ,    //
                                       null                       ,    //
                                       global.ptr                 ,    //
                                       null                       ,    //
                                       0                          ,    //
                                       null                       ,    //
                                       &event                    );    //
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clWaitForEvents( 1, &event );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );

      cl_ulong start_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &start_time               , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      cl_ulong end_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &end_time                 , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      result = ( end_time - start_time ) / 1E9;

      status = clEnqueueReadBuffer( runtime.queue                    , //
                                    du                               , //
                                    CL_TRUE                          , //
                                    0                                , //
                                    FLOAT_PRECISION.sizeof * u.length, //
                                    u.ptr                            , //
                                    0                                , //
                                    null                             , //
                                    null                            ); //
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );

      status = clReleaseMemObject( du );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_7pt_noblocks );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
    return result;
  }

  double opencl_stencil_7pt_blocks()
  {
    double result;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, null, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_blocks, 0, cl_mem.sizeof, &du );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_blocks, 1, cl_mem.sizeof, &dv );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_blocks, 2, cl_mem.sizeof, &dc );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );

      static if ( false )
      { // an example of getting the correct maximum work group
        // size. device info on OS X reports an incorrect value.
        size_t size;
        clGetKernelWorkGroupInfo( kernel_stencil_7pt_blocks,
                                  runtime.device,
                                  CL_KERNEL_WORK_GROUP_SIZE,
                                  size.sizeof,
                                  &size,
                                  null );
        writefln( "max work group size for kernel_stencil_7pt_blocks %d, actual work group size %d",
                  size, BLOCK_SIZE * BLOCK_SIZE * BLOCK_SIZE );
        clGetKernelWorkGroupInfo( kernel_stencil_7pt_blocks,
                                  runtime.device,
                                  CL_KERNEL_LOCAL_MEM_SIZE,
                                  size.sizeof,
                                  &size,
                                  null );
        writefln( "local mem size for kernel_stencil_7pt_blocks %d, allocated local mem size %d",
                  size, (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * FLOAT_PRECISION.sizeof );
      }

      size_t[3] global = [zdim, ydim / BLOCK_SIZE, xdim];
      size_t[3] local  = [BLOCK_SIZE, 1, BLOCK_SIZE];
      status = clEnqueueNDRangeKernel( runtime.queue            ,      // cl_command_queue command_queue
                                       kernel_stencil_7pt_blocks,      // cl_kernel        kernel
                                       3                        ,      // cl_uint          work_dim
                                       null                     ,      // const size_t*    global_work_offset
                                       global.ptr               ,      // const size_t*    global_work_size
                                       local.ptr                ,      // const size_t*    local_work_size
                                       0                        ,      // cl_uint          num_events_in_wait_list
                                       null                     ,      // const cl_event*  event_wait_list
                                       &event                  );      // cl_event*        event
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      status = clWaitForEvents( 1, &event );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );

      cl_ulong start_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &start_time               , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      cl_ulong end_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &end_time                 , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      result = ( end_time - start_time ) / 1E9;

      status = clEnqueueReadBuffer( runtime.queue                    , // cl_command_queue command_queue
                                    du                               , // cl_mem           buffer
                                    CL_TRUE                          , // cl_bool          blocking_read
                                    0                                , // size_t           offset
                                    FLOAT_PRECISION.sizeof * u.length, // size_t           size
                                    u.ptr                            , // void*            ptr
                                    0                                , // cl_uint          num_events_in_wait_list
                                    null                             , // const cl_event*  event_wait_list
                                    null                            ); // cl_event*        event
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( du );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_7pt_blocks );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_blocks " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
    return result;
  }

  double opencl_stencil_7pt_shared()
  {
    double result;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, null, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 0, cl_mem.sizeof, &du );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 1, cl_mem.sizeof, &dv );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared, 2, cl_mem.sizeof, &dc );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_7pt_shared,
                               3,
                               (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * FLOAT_PRECISION.sizeof,
                               null );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
/*
      status = clSetKernelArg( kernel_stencil_7pt_shared,
                               4,
                               (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) *
                               (BLOCK_SIZE + 2) * NUM_DOMAINS *
                               FLOAT_PRECISION.sizeof,
                               null );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
*/

      static if ( false )
      { // an example of getting the correct maximum work group
        // size. device info on OS X reports an incorrect value.
        size_t size;
        clGetKernelWorkGroupInfo( kernel_stencil_7pt_shared,
                                  runtime.device,
                                  CL_KERNEL_WORK_GROUP_SIZE,
                                  size.sizeof,
                                  &size,
                                  null );
        writefln( "max work group size for kernel_stencil_7pt_shared %d, actual work group size %d",
                  size, BLOCK_SIZE * BLOCK_SIZE * BLOCK_SIZE );
        clGetKernelWorkGroupInfo( kernel_stencil_7pt_shared,
                                  runtime.device,
                                  CL_KERNEL_LOCAL_MEM_SIZE,
                                  size.sizeof,
                                  &size,
                                  null );
        writefln( "local mem size for kernel_stencil_7pt_shared %d, allocated local mem size %d",
                  size, (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * (BLOCK_SIZE + 2) * FLOAT_PRECISION.sizeof );
      }

      size_t[3] global = [zdim, ydim, xdim];
      size_t[3] local  = [BLOCK_SIZE, BLOCK_SIZE, BLOCK_SIZE];
      status = clEnqueueNDRangeKernel( runtime.queue            ,      // cl_command_queue command_queue
                                       kernel_stencil_7pt_shared,      // cl_kernel        kernel
                                       3                        ,      // cl_uint          work_dim
                                       null                     ,      // const size_t*    global_work_offset
                                       global.ptr               ,      // const size_t*    global_work_size
                                       local.ptr                ,      // const size_t*    local_work_size
                                       0                        ,      // cl_uint          num_events_in_wait_list
                                       null                     ,      // const cl_event*  event_wait_list
                                       &event                  );      // cl_event*        event
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clWaitForEvents( 1, &event );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_noblocks " ~ cl_strerror( status ) );

      cl_ulong start_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &start_time               , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      cl_ulong end_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &end_time                 , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      result = ( end_time - start_time ) / 1E9;

      status = clEnqueueReadBuffer( runtime.queue                    , // cl_command_queue command_queue
                                    du                               , // cl_mem           buffer
                                    CL_TRUE                          , // cl_bool          blocking_read
                                    0                                , // size_t           offset
                                    FLOAT_PRECISION.sizeof * u.length, // size_t           size
                                    u.ptr                            , // void*            ptr
                                    0                                , // cl_uint          num_events_in_wait_list
                                    null                             , // const cl_event*  event_wait_list
                                    null                            ); // cl_event*        event
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( du );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_7pt_shared );
      assert( status == CL_SUCCESS, "opencl_stencil_7pt_shared " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
    return result;
  }

  double opencl_stencil_25pt_noblocks()
  {
    double result;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, null, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_noblocks, 0, cl_mem.sizeof, &du   );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_noblocks, 1, cl_mem.sizeof, &dv   );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_noblocks, 2, cl_mem.sizeof, &dc   );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );

      size_t[3] global = [zdim - 8, ydim - 8, xdim - 8];
      status = clEnqueueNDRangeKernel( runtime.queue              ,    //
                                       kernel_stencil_25pt_noblocks,   //
                                       3                          ,    //
                                       null                       ,    //
                                       global.ptr                 ,    //
                                       null                       ,    //
                                       0                          ,    //
                                       null                       ,    //
                                       &event                    );    //
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
      status = clWaitForEvents( 1, &event );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );

      cl_ulong start_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &start_time               , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      cl_ulong end_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &end_time                 , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      result = ( end_time - start_time ) / 1E9;

      status = clEnqueueReadBuffer( runtime.queue                    , //
                                    du                               , //
                                    CL_TRUE                          , //
                                    0                                , //
                                    FLOAT_PRECISION.sizeof * u.length, //
                                    u.ptr                            , //
                                    0                                , //
                                    null                             , //
                                    null                            ); //
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );

      status = clReleaseMemObject( du );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_25pt_noblocks );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_noblocks " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
    return result;
  }

  double opencl_stencil_25pt_blocks()
  {
    double result;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, null, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_blocks, 0, cl_mem.sizeof, &du   );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_blocks, 1, cl_mem.sizeof, &dv   );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_blocks, 2, cl_mem.sizeof, &dc   );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );

      size_t[3] global = [zdim, ydim / BLOCK_SIZE, xdim];
      size_t[3] local  = [BLOCK_SIZE, 1, BLOCK_SIZE];
      status = clEnqueueNDRangeKernel( runtime.queue              , //
                                       kernel_stencil_25pt_blocks , //
                                       3                          , //
                                       null                       , //
                                       global.ptr                 , //
                                       local.ptr                  , //
                                       0                          , //
                                       null                       , //
                                       &event                    ); //
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
      status = clWaitForEvents( 1, &event );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );

      cl_ulong start_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &start_time               , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      cl_ulong end_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &end_time                 , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "runtime copy benchmark " ~ cl_strerror( status ) );
      result = ( end_time - start_time ) / 1E9;

      status = clEnqueueReadBuffer( runtime.queue                    , //
                                    du                               , //
                                    CL_TRUE                          , //
                                    0                                , //
                                    FLOAT_PRECISION.sizeof * u.length, //
                                    u.ptr                            , //
                                    0                                , //
                                    null                             , //
                                    null                            ); //
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );

      status = clReleaseMemObject( du );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_25pt_blocks );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_blocks " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
    return result;
  }

  double opencl_stencil_25pt_shared()
  {
    double result;

    try
    {
      cl_int status;
      cl_event event;
      cl_mem_flags flags = CL_MEM_WRITE_ONLY;
      cl_mem du = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * u.length, null, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      flags = CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR;
      cl_mem dv = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * v.length, v.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      cl_mem dc = clCreateBuffer( runtime.context, flags, FLOAT_PRECISION.sizeof * coef.length, coef.ptr, &status );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_shared, 0, cl_mem.sizeof, &du );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_shared, 1, cl_mem.sizeof, &dv );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_shared, 2, cl_mem.sizeof, &dc );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      status = clSetKernelArg( kernel_stencil_25pt_shared,
                               3,
                               (BLOCK_SIZE + 8) * (BLOCK_SIZE + 8) * (BLOCK_SIZE + 8) * FLOAT_PRECISION.sizeof,
                               null );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );

      size_t[3] global = [zdim, ydim, xdim];
      size_t[3] local  = [BLOCK_SIZE, BLOCK_SIZE, BLOCK_SIZE];
      status = clEnqueueNDRangeKernel( runtime.queue                 , // cl_command_queue command_queue
                                       kernel_stencil_25pt_shared    , // cl_kernel        kernel
                                       3                             , // cl_uint          work_dim
                                       null                          , // const size_t*    global_work_offset
                                       global.ptr                    , // const size_t*    global_work_size
                                       local.ptr                     , // const size_t*    local_work_size
                                       0                             , // cl_uint          num_events_in_wait_list
                                       null                          , // const cl_event*  event_wait_list
                                       &event                       ); // cl_event*        event
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      status = clWaitForEvents( 1, &event );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );

      cl_ulong start_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_START, // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &start_time               , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      cl_ulong end_time;
      status = clGetEventProfilingInfo ( event                     , // cl_event          event
                                         CL_PROFILING_COMMAND_END  , // cl_profiling_info param_name
                                         cl_ulong.sizeof           , // size_t            param_value_size
                                         &end_time                 , // void*             param_value
                                         null                     ); // size_t*           param_value_size_ret
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      result = ( end_time - start_time ) / 1E9;

      status = clEnqueueReadBuffer( runtime.queue                    , // cl_command_queue command_queue
                                    du                               , // cl_mem           buffer
                                    CL_TRUE                          , // cl_bool          blocking_read
                                    0                                , // size_t           offset
                                    FLOAT_PRECISION.sizeof * u.length, // size_t           size
                                    u.ptr                            , // void*            ptr
                                    0                                , // cl_uint          num_events_in_wait_list
                                    null                             , // const cl_event*  event_wait_list
                                    null                            ); // cl_event*        event
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( du );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dv );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      status = clReleaseMemObject( dc );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
      status = clReleaseKernel( kernel_stencil_25pt_shared );
      assert( status == CL_SUCCESS, "opencl_stencil_25pt_shared " ~ cl_strerror( status ) );
    }
    catch( Exception e )
    {
      writeln( e );
    }
    return result;
  }

  /**
   */
  void run()
  {
    size_t size;
    StopWatch timer;
    TickDuration ticks;
    double benchmark = runtime.benchmark( (xdim - 2) * (ydim - 2) * (zdim - 2) );
    double rate;

    timer.start();
    rate = opencl_stencil_5pt_noblocks();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2d MI  5pt NO BLOCKS  %5.3f (%5.3f) [s], %7.2f MI/s, estimated %7.2f MI/s",
              xdim * ydim * 128 / ( 1024 * 1024 ),
              ticks.usecs / 1E6, rate,
              (8 * xdim - 2) * (16 * ydim - 2) / (1024 * 1024 * rate),
              2 * benchmark / 11 );

    writeln( "-----------------------------------------------------" );

    timer.reset();
    timer.start();
    sequential_stencil_7pt();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2d MI  7pt SEQUENTIAL %5.3f         [s], %7.2f MI/s",
              xdim * ydim * zdim / ( 1024 * 1024 ),
              ticks.usecs / 1E6,
              xdim * ydim * zdim * 1E6 / ( 1024 * 1024 * ticks.usecs ) );

    timer.reset();
    timer.start();
    rate = opencl_stencil_7pt_noblocks();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2d MI  7pt NO BLOCKS  %5.3f (%5.3f) [s], %7.2f MI/s, estimated %7.2f MI/s",
              xdim * ydim * zdim / ( 1024 * 1024 ),
              ticks.usecs / 1E6, rate,
              (xdim - 2) * (ydim - 2) * (zdim - 2) / (1024 * 1024 * rate),
              2 * benchmark / 15 );
    validate();

    clGetKernelWorkGroupInfo( kernel_stencil_7pt_blocks,
                              runtime.device,
                              CL_KERNEL_WORK_GROUP_SIZE,
                              size.sizeof,
                              &size,
                              null );
    if ( size >= BLOCK_SIZE * BLOCK_SIZE )
    {
      timer.reset();
      timer.start();
      rate = opencl_stencil_7pt_blocks();
      timer.stop();
      ticks = timer.peek();
      if ( 0 != rate )
      {
      writefln( "%2d MI  7pt BLOCKS     %5.3f (%5.3f) [s], %7.2f MI/s",
                xdim * ydim * zdim / ( 1024 * 1024 ),
                ticks.usecs / 1E6, rate,
                (xdim - 2) * (ydim - 2) * (zdim - 2) / (1024 * 1024 * rate) );
      validate();
      }
    }

    clGetKernelWorkGroupInfo( kernel_stencil_7pt_shared,
                              runtime.device,
                              CL_KERNEL_WORK_GROUP_SIZE,
                              size.sizeof,
                              &size,
                              null );
    if ( size >= BLOCK_SIZE * BLOCK_SIZE * BLOCK_SIZE )
    {
      timer.reset();
      timer.start();
      rate = opencl_stencil_7pt_shared();
      timer.stop();
      ticks = timer.peek();
      writefln( "%2d MI  7pt SHARED     %5.3f (%5.3f) [s], %7.2f MI/s",
                xdim * ydim * zdim / ( 1024 * 1024 ),
                ticks.usecs / 1E6, rate,
                (xdim - 2) * (ydim - 2) * (zdim - 2) / (1024 * 1024 * rate) );
      validate();
    }

    writeln( "-----------------------------------------------------" );

    cleanup();
    timer.start();
    sequential_stencil_25pt();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2d MI 25pt SEQUENTIAL %5.3f         [s], %7.2f MI/s",
              xdim * ydim * zdim / ( 1024 * 1024 ),
              ticks.usecs / 1E6,
              xdim * ydim * zdim * 1E6 / ( 1024 * 1024 * ticks.usecs ) );

    timer.reset();
    timer.start();
    rate = opencl_stencil_25pt_noblocks();
    timer.stop();
    ticks = timer.peek();
    writefln( "%2d MI 25pt NO BLOCKS  %5.3f (%5.3f) [s], %7.2f MI/s, estimated %7.2f MI/s",
              xdim * ydim * zdim / ( 1024 * 1024 ),
              ticks.usecs / 1E6, rate,
              (xdim - 8) * (ydim - 8) * (zdim - 8) / (1024 * 1024 * rate),
              2 * benchmark / 39 );
    validate();

    clGetKernelWorkGroupInfo( kernel_stencil_25pt_blocks,
                              runtime.device,
                              CL_KERNEL_WORK_GROUP_SIZE,
                              size.sizeof,
                              &size,
                              null );
    if ( size >= BLOCK_SIZE * BLOCK_SIZE )
    {
      timer.reset();
      timer.start();
      rate = opencl_stencil_25pt_blocks();
      timer.stop();
      ticks = timer.peek();
      if ( 0 != rate )
      {
      writefln( "%2d MI 25pt BLOCKS     %5.3f (%5.3f) [s], %7.2f MI/s",
                xdim * ydim * zdim / ( 1024 * 1024 ),
                ticks.usecs / 1E6, rate,
                xdim * ydim * zdim / (1024 * 1024 * rate) );
      validate();
      }
    }

    clGetKernelWorkGroupInfo( kernel_stencil_25pt_shared,
                              runtime.device,
                              CL_KERNEL_WORK_GROUP_SIZE,
                              size.sizeof,
                              &size,
                              null );
    if ( size >= BLOCK_SIZE * BLOCK_SIZE * BLOCK_SIZE )
    {
      timer.reset();
      timer.start();
      rate = opencl_stencil_25pt_shared();
      timer.stop();
      ticks = timer.peek();
      writefln( "%2d MI 25pt SHARED     %5.3f (%5.3f) [s], %7.2f MI/s",
                xdim * ydim * zdim / ( 1024 * 1024 ),
                ticks.usecs / 1E6, rate,
                (xdim - 8) * (ydim - 8) * (zdim - 8) / (1024 * 1024 * rate) );
      validate();
    }
  }
}

int
main( string[] args )
{
  uint[] platforms = runtime.get_platforms();
  for ( uint p = 0; p < platforms.length; ++p )
    foreach ( d; 0 .. platforms[p] )
    {
      try
      {
        runtime.init( p, d );
        writeln( "=====================================================" );
        auto app = new Application( args );
        app.run();
        writeln( "=====================================================" );
        runtime.shutdown();
      }
      catch ( Exception msg )
      {
        writeln( "STENCIL: ", msg );
        runtime.shutdown();
        return -1;
      }
    }
  return 0;
}
