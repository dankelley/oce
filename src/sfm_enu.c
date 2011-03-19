#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* 
 * sfm_enu: convert starboard-forward-mast velocity components into east-nort-up components
 */
void sfm_enu(int *nhpr, double *heading, double *pitch, double *roll, 
        int *nsfm, double *starboard, double *forward, double *mast,
        double *east, double *north, double *up)
{
    /* nhpr = length of heading, pitch, and roll
     * nsfm = length of starboard, forward, and mast (and of east, north and up)
     *
     * Note: heading, pitch, and roll values are recycled if (*nhpr) < (*nsfm).
     */
    double *CH = (double *) R_alloc(*nhpr, sizeof(double));
    double *SH = (double *) R_alloc(*nhpr, sizeof(double));
    double *CP = (double *) R_alloc(*nhpr, sizeof(double));
    double *SP = (double *) R_alloc(*nhpr, sizeof(double));
    double *CR = (double *) R_alloc(*nhpr, sizeof(double));
    double *SR = (double *) R_alloc(*nhpr, sizeof(double));
    int i, j;
    for (j = 0; j < (*nhpr); j++) {
#define PI_OVER_180 0.0174532925199433
        double h = PI_OVER_180 * heading[j];
        double p = PI_OVER_180 * pitch[j];
        double r = PI_OVER_180 * roll[j];
#undef PI_OVER_180
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
}

