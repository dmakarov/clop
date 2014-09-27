#include "data_structures.h"
//#include "iacaMarks.h"

#define U(i,j,k)         (u[((k)*(nny)+(j))*(nnx)+(i)])
#define V(i,j,k)         (v[((k)*(nny)+(j))*(nnx)+(i)])
#define ROC2(i,j,k)   (roc2[((k)*(nny)+(j))*(nnx)+(i)])
#define COEF(m,i,j,k) (coef[((k)*(nny)+(j))*(nnx)+(i)+((ln_domain)*(m))])

void
iso_ref_2space_1time_var_nosym( const int shape[3],
                                const int xb,
                                const int yb,
                                const int zb,
                                const int xe,
                                const int ye,
                                const int ze,
                                const FLOAT_PRECISION * restrict coef,
                                const FLOAT_PRECISION * restrict roc2,
                                const FLOAT_PRECISION * restrict v,
                                      FLOAT_PRECISION * restrict u,
                                      stencil_CTX stencil_ctx )
{
  int i, j, k, jb, je;
  int nny = shape[1];
  int nnx = shape[0];
  int ln_domain = shape[0] * shape[1] * shape[2];

  for ( jb = yb; jb < ye; jb += stencil_ctx.bs_y ) // blocking in Y
  {
    je = ( jb + stencil_ctx.bs_y ) < ( ye ) ? ( jb + stencil_ctx.bs_y ) : ( ye );
#pragma omp parallel num_threads(stencil_ctx.thread_group_size)
    {
#pragma omp for private(k,j,i) schedule(static,1)
      for ( k = zb; k < ze; k++ )
      {
        for ( j = jb; j < je; j++ )
        {
#pragma simd
          for ( i = xb; i < xe; i++ )
          {
//IACA_START
            U(i,j,k) = COEF(0,i,j,k) * V(i  ,j  ,k  )
                     + COEF(1,i,j,k) * V(i-1,j  ,k  )
                     + COEF(2,i,j,k) * V(i+1,j  ,k  )
                     + COEF(3,i,j,k) * V(i  ,j-1,k  )
                     + COEF(4,i,j,k) * V(i  ,j+1,k  )
                     + COEF(5,i,j,k) * V(i  ,j  ,k-1)
                     + COEF(6,i,j,k) * V(i  ,j  ,k+1);
          }
//IACA_END
        }
      }
    }
  }
}
