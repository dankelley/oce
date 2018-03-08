#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*
 * sfm_enu: convert starboard-forward-mast velocity components into east-nort-up components
 *
 * NOTE: n is the length of the input vectors (heading, pitch, roll,
 * starboard, forward, mast) and also the ouitput vectors (east, north
 * and up). Storage for all these things has to be set up in the R code
 * that calls this.
 *
 * HISTORY: until May 19, 2017, it was possible to have the input vectors of
 * one length and the output vectors of another, but that caused too much code
 * confusion (and an error, issue https://github.com/dankelley/oce/issues/1249)
 *
 */

void sfm_enu(int *n, double *heading, double *pitch, double *roll,
        double *starboard, double *forward, double *mast,
        double *east, double *north, double *up)
{
    const double PI_OVER_180 = atan2(1.0, 1.0) / 45.0;
    if (*n < 1)
        error("must have n >= 1");
    /* no need to allocate storage */
    for (int i = 0; i < (*n); i++) {
        if (0 == (i % 1000))
            R_CheckUserInterrupt();
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

