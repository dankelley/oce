#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* 
 * sfm_enu: convert starboard-forward-mast velocity components into east-nort-up components
 */
#define PI_OVER_180 0.0174532925199433
void sfm_enu(int *nhpr, double *heading, double *pitch, double *roll, 
        int *nsfm, double *starboard, double *forward, double *mast,
        double *east, double *north, double *up)
{
    //Rprintf("sfm_enu(nhpr=%d, heading=%f, pitch=%f, roll=%f, nsfm=%d, ...)\n",
    //        *nhpr, heading[0], pitch[0], roll[0], *nsfm);
    /* nhpr = length of heading, pitch, and roll
     * nsfm = length of starboard, forward, and mast (and of east, north and up)
     *
     * Note: heading, pitch, and roll values are recycled if (*nhpr) < (*nsfm).
     */
    if (*nhpr < 1)
        error("must have nhpr > 0");
    if (*nsfm < 1)
        error("must have nsfm > 0");
    if (*nsfm < *nhpr)
        error("must have nsfm >= nhpr");
    int i, j;
    /* if not recycling hpr, can save memory */
    if ((*nhpr) < (*nsfm)) {
        //Rprintf("nhpr < nsfm\n");
        double *CH = (double *) R_alloc(*nhpr, sizeof(double));
        double *SH = (double *) R_alloc(*nhpr, sizeof(double));
        double *CP = (double *) R_alloc(*nhpr, sizeof(double));
        double *SP = (double *) R_alloc(*nhpr, sizeof(double));
        double *CR = (double *) R_alloc(*nhpr, sizeof(double));
        double *SR = (double *) R_alloc(*nhpr, sizeof(double));
        for (j = 0; j < (*nhpr); j++) {
            //Rprintf("heading[%d]=%f, pitch[%d]=%f, roll[%d]=%f\n", j, heading[j], j, pitch[j], j, roll[j]);
            double h = PI_OVER_180 * heading[j];
            double p = PI_OVER_180 * pitch[j];
            double r = PI_OVER_180 * roll[j];
            CH[j] = cos(h);
            SH[j] = sin(h);
            CP[j] = cos(p);
            SP[j] = sin(p);
            CR[j] = cos(r);
            SR[j] = sin(r);
        }
        j = 0; /* see end of loop for recycling */
        for (i = 0; i < (*nsfm); i++) {
            east[i] =
                starboard[i] * ( CH[j] * CR[j] + SH[j] * SP[j] * SR[j] ) +
                forward[i]   * ( SH[j] * CP[j]                         ) +
                mast[i]      * ( CH[j] * SR[j] - SH[j] * SP[j] * CR[j] );
            north[i] =
                starboard[i] * (-SH[j] * CR[j] + CH[j] * SP[j] * SR[j] ) +
                forward[i]   * ( CH[j] * CP[j]                         ) +
                mast[i]      * (-SH[j] * SR[j] - CH[j] * SP[j] * CR[j] );
            up[i] =
                starboard[i] * (                        -CP[j] * SR[j] ) +
                forward[i]   * ( SP[j]                                 ) +
                mast[i]      * (                         CP[j] * CR[j] );
            if (++j == (*nhpr))
                j = 0;
        }
    } else {
        /* no need to allocate storage */
        for (i = 0; i < (*nsfm); i++) {
            double h = PI_OVER_180 * heading[i];
            double p = PI_OVER_180 * pitch[i];
            double r = PI_OVER_180 * roll[i];
            double CH = cos(h);
            double SH = sin(h);
            double CP = cos(p);
            double SP = sin(p);
            double CR = cos(r);
            double SR = sin(r);
            east[i] =
                starboard[i] * ( CH * CR + SH * SP * SR ) +
                forward[i]   * ( SH * CP                ) +
                mast[i]      * ( CH * SR - SH * SP * CR );
            north[i] =
                starboard[i] * (-SH * CR + CH * SP * SR ) +
                forward[i]   * ( CH * CP                ) +
                mast[i]      * (-SH * SR - CH * SP * CR );
            up[i] =
                starboard[i] * (               -CP * SR ) +
                forward[i]   * ( SP                     ) +
                mast[i]      * (                CP * CR );
        }
    }
}

#undef PI_OVER_180
