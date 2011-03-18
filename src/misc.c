#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define debug

/*
 * bisect_d (bisection search vector of doubles)
 * x = vector of doubles (must be in increasing order)
 * find = vector of doubles whose index to find
 * side = vector of 0 or 1 (to look to the left, or right, of find)
 * returns a vector of indices, or NA values
 *
 * TESTING:

 system("R CMD SHLIB misc.c") ; dyn.load("misc.so")

 x <- 0.1 + 1:1e7
 find <- c(10, 5e4)
 side <- c(0, 1)

 system.time({
 loc <- .Call("bisect_d", x, find, side);
 xx <- x[loc[1]:loc[2]]
 })

 user  system elapsed 
 0.021   0.000   0.021 

 system.time(xxx <- x[(find[1] < x) & (x < find[2])])

 user  system elapsed 
 0.266   0.001   0.264 

CONCLUSION: about 10 times faster than the straightforward method.
The latter might be an issue for deep use in loops, for large objects.

*/
SEXP bisect_d(SEXP x, SEXP find, SEXP side)
{
    PROTECT(x = AS_NUMERIC(x));
    double *px = NUMERIC_POINTER(x);
    PROTECT(find = AS_NUMERIC(find));
    double *pfind = NUMERIC_POINTER(find);
    PROTECT(side = AS_INTEGER(side));
    int *pside = INTEGER_POINTER(side);
    int nx = length(x);
    int nfind = length(find);
    int nside = length(side);
    if (nfind != nside)
        error("need length(find) = length(side)");
    char buf[1024]; /* for error messages */
    int i;
    SEXP res;
    PROTECT(res = NEW_INTEGER(nfind));
    /* ensure that x is in order */
    for (i = 1; i < nx; i++) {
        if (px[i-1] >= px[i]) {
            char buf[256];
            sprintf(buf, "x must be ordered from small to large; fails at x[%d]\n", i);
            error(buf);
        }
    }
    int *pres = INTEGER_POINTER(res);
    int left, right, middle, ifind;
    for (ifind = 0; ifind < nfind; ifind++) {
        double this_find = pfind[ifind];
#ifdef debug
        Rprintf("find[%d]=%f (%f <= x <= %f)\n", ifind, this_find, *(px), *(px+nx-1));
#endif
        /* trim indices to those of x (R notation) */
        if (this_find <= px[0]) {
            pres[ifind] = 1;
            continue;
        }
        if (this_find >= px[nx-1]) {
            pres[ifind] = nx;
            continue;
        }
        int p;
        left = 0;
        right = nx - 1;
        int halves = (int)(10 + log(0.0+nx) / log(2.0)); /* prevent inf loop from poor coding */
        pres[ifind] = NA_INTEGER;
        for (int half = 0; half < halves; half++) {
            middle = (int)floor(0.5 * (left + right));
            /* exact match to middle? */
            if (px[middle] == pfind[ifind]) {
#ifdef debug
                Rprintf("exact match at middle=%d\n", middle);
#endif
                pres[ifind] = middle;
                break;
            }
            /* in left half */
            if (px[left] <= this_find & this_find < px[middle]) {
#ifdef debug
                Rprintf("L %d %d\n", left, middle);
#endif
                right = middle;
                if (2 > (middle - left)) {
#ifdef debug
                    Rprintf("narrowed to left=%d and middle=%d\n", left, middle);
#endif
                    pres[ifind] = middle;
                    if (pside[ifind] == 0)
                        pres[ifind] = left + 1;
                    else
                        pres[ifind] = middle + 1;
                    break;
                }
            }
            /* in right half */
            if (px[middle] < this_find & this_find <= px[right]) {
#ifdef debug
                Rprintf("R %d %d %f %f\n", middle, right, px[middle], px[right]);
#endif
                left = middle;
                if (2 > (right - middle)) {
#ifdef debug
                    Rprintf("narrowed to middle=%d and right=%d\n", middle, right);
                    Rprintf("pside=%d\n", pside[ifind]);
#endif
                    if (pside[ifind] == 0)
                        pres[ifind] = middle + 1;
                    else
                        pres[ifind] = right + 1;
#ifdef debug
                    Rprintf("pres[ifind]=%d\n",pres[ifind]);
#endif
                    break;
                }
            }
        }
    }
    UNPROTECT(4);
    return(res);
}

SEXP matrix_smooth(SEXP mat)
{
    /* Note: the 2d data are stored in column order */
    SEXP res;
    int nrow = INTEGER(GET_DIM(mat))[0];
    int ncol = INTEGER(GET_DIM(mat))[1];
    int i, j;
    double *matp, *resp;
    if (!isMatrix(mat))
        error("'mat' must be a matrix");
    //if (isInteger(mat)) warning("'mat' is integer, but should be real");
    if (!isReal(mat))
        error("'mat' must be numeric, not integer");
    matp = REAL(mat);
    if (length(mat) != nrow * ncol)
        error("'nrow'*'ncol' must equal number of elements in 'mat'");
    PROTECT(res = allocMatrix(REALSXP, nrow, ncol));
    resp = REAL(res);
    // copy edges (change this, if filter size changes)
    for (j = 0; j < ncol; j++) {
        *(resp + j                    ) = *(matp + j                    );
        *(resp + j + ncol * (nrow - 1)) = *(matp + j + ncol * (nrow - 1));
    }
    for (i = 0; i < nrow; i++) {
        *(resp +      0     + ncol * i) = *(matp +      0     + ncol * i);
        *(resp + (nrow - 1) + ncol * i) = *(matp + (nrow - 1) + ncol * i);
    }
    // smooth middle 
    for (i = 1; i < nrow - 1; i++) {
        for (j = 1; j < ncol - 1; j++) {
            *(resp + j + ncol * i) = 
                (2*(*(matp + j     + ncol *    i    )) +
                 (  *(matp + j - 1 + ncol *    i    )) +
                 (  *(matp + j + 1 + ncol *    i    )) +
                 (  *(matp + j     + ncol * (i - 1) )) +
                 (  *(matp + j     + ncol * (i + 1) ))) / 6.0;
        }
    }
    UNPROTECT(1);
    return(res);
}

/*
Test with sleiwex 2008 m05 adp (sontek)

load("/data/archive/sleiwex/2008/moorings/m05/adp/sontek_c344/r/m05_adp_xyz.rda") 
load("/data/archive/sleiwex/2008/moorings/m05/adp/sontek_c344/r/m05_adp_enu.rda")

options(oce.flag1=1)
system.time(d <- xyz.to.enu.adp(m05.adp.xyz, declination=(-18.099))) 
png('newer.png'); plot(d, which='uv+ellipse+arrow', main='newer 6.114s');dev.off()


heading <- m05.adp.xyz$data$ts$heading[1:5]
pitch <- m05.adp.xyz$data$ts$pitch[1:5]
roll <- m05.adp.xyz$data$ts$roll[1:5]
heading <- heading - 90
heading <- heading + (-18.099)
pitch <- (-pitch)
roll <- (-roll)
starboard <- m05.adp.xyz$data$ma$v[1:5, 1, 1] # bin 1
forward <- -m05.adp.xyz$data$ma$v[1:5, 1, 2] # bin 1
mast <- -m05.adp.xyz$data$ma$v[1:5, 1, 3] # bin 1

system("R CMD SHLIB misc.c")
dyn.load("misc.so")
m <- .Call("sfm_enu", heading, pitch, roll, starboard, forward, mast)
print(m)
print(m05.adp.enu$data$ma$v[1:5,1,])

> m05.adp.enu$data$ma$v[1:5,1,]
                   [,1]               [,2]                 [,3]
[1,] 0.2267969684975225 0.2064642132398775 -0.04639371340121831
[2,] 0.1686621484404889 0.2148234570537008 -0.02718656614270807
[3,] 0.1356315034838943 0.2217491282924235 -0.03321269753774039
[4,] 0.1909154445231142 0.1523488598127603 -0.02908284786998954
[5,] 0.1529715003543281 0.1955612133135224 -0.03012186155548581

 */
#define mat_at(matrix, r, c, nr) *(matrix + r + c * nr)
#define check(v, vname, n) \
{ \
    if (!isVector(v)) \
        error("'%s' must be a vector\n", vname);\
    if (!isReal(v))\
        error("'%s' must have storage type 'numeric' (floating-point)\n", vname);\
    if (n == 0) {\
        n = GET_LENGTH(v);\
    } else {\
        if (n != GET_LENGTH(v))\
        error("length of '%s' is %d, but it should be %d to match length of 'heading'\n", vname, GET_LENGTH(v), n);\
    }\
}
SEXP sfm_enu(SEXP heading, SEXP pitch, SEXP roll, SEXP starboard, SEXP forward, SEXP mast)
{
    /* check lengths and storage types */
    int n = 0;
    check(heading, "heading", n);
    check(pitch, "pitch", n)
    check(roll, "roll", n)
    check(starboard, "starboard", n)
    check(forward, "forward", n)
    check(roll, "roll", n)
    /* calculate sines and cosines */
    double *CH = (double *) R_alloc(n, sizeof(double));
    double *SH = (double *) R_alloc(n, sizeof(double));
    double *CP = (double *) R_alloc(n, sizeof(double));
    double *SP = (double *) R_alloc(n, sizeof(double));
    double *CR = (double *) R_alloc(n, sizeof(double));
    double *SR = (double *) R_alloc(n, sizeof(double));
    double *heading_p = REAL(heading), *pitch_p = REAL(pitch), *roll_p = REAL(roll);
#define PI_OVER_180 0.0174532925199433
    double h, p, r;
    for (int i = 0; i < n; i++) {
        h = PI_OVER_180 * heading_p[i];
        p = PI_OVER_180 * pitch_p[i];
        r = PI_OVER_180 * roll_p[i];
        CH[i] = cos(h);
        SH[i] = sin(h);
        CP[i] = cos(p);
        SP[i] = sin(p);
        CR[i] = cos(r);
        SR[i] = sin(r);
    }
#undef PI_OVER_180
    double *starboard_p = REAL(starboard);
    double *forward_p = REAL(forward);
    double *mast_p = REAL(mast);
    SEXP enu;
    PROTECT(enu = allocMatrix(REALSXP, n, 3));
    double *enu_p = REAL(enu);
    for (int i = 0; i < n; i++) {
        double CHi, SHi, CPi, SPi, CRi, SRi, starboardi, forwardi, masti;
        starboardi = starboard_p[i];
        forwardi = forward_p[i];
        masti = mast_p[i];
        CHi = CH[i];
        SHi = SH[i];
        CPi = CP[i];
        SPi = SP[i];
        CRi = CR[i];
        SRi = SR[i];
        *(enu_p + i      ) = starboardi * ( CHi * CRi + SHi * SPi * SRi) + forwardi * (SHi * CPi) + masti * ( CHi * SRi - SHi * SPi * CRi);
        *(enu_p + i +   n) = starboardi * (-SHi * CRi + CHi * SPi * SRi) + forwardi * (CHi * CPi) + masti * (-SHi * SRi - CHi * SPi * CRi);
        *(enu_p + i + 2*n) = starboardi * (-CPi * SRi)                   + forwardi * SPi         + masti * ( CPi * CRi);
    }
    UNPROTECT(1);
    return(enu);
}
#undef check
#undef mat_l

void
sfm_enu2(int *n, double *heading, double *pitch, double *roll, double *starboard, double *forward, double *mast, double *east, double *north, double *up)
{
    /* calculate sines and cosines */
    double *CH = (double *) R_alloc(*n, sizeof(double));
    double *SH = (double *) R_alloc(*n, sizeof(double));
    double *CP = (double *) R_alloc(*n, sizeof(double));
    double *SP = (double *) R_alloc(*n, sizeof(double));
    double *CR = (double *) R_alloc(*n, sizeof(double));
    double *SR = (double *) R_alloc(*n, sizeof(double));
    double h, p, r;
    int nn = *n;
    for (int i = 0; i < nn; i++) {
#define PI_OVER_180 0.0174532925199433
        h = PI_OVER_180 * heading[i];
        p = PI_OVER_180 * pitch[i];
        r = PI_OVER_180 * roll[i];
#undef PI_OVER_180
        CH[i] = cos(h);
        SH[i] = sin(h);
        CP[i] = cos(p);
        SP[i] = sin(p);
        CR[i] = cos(r);
        SR[i] = sin(r);
        east[i]  = starboard[i] * ( CH[i] * CR[i] + SH[i] * SP[i] * SR[i]) + forward[i] * (SH[i] * CP[i]) + mast[i] * ( CH[i] * SR[i] - SH[i] * SP[i] * CR[i]);
        north[i] = starboard[i] * (-SH[i] * CR[i] + CH[i] * SP[i] * SR[i]) + forward[i] * (CH[i] * CP[i]) + mast[i] * (-SH[i] * SR[i] - CH[i] * SP[i] * CR[i]);
        up[i]    = starboard[i] * (-CP[i] * SR[i])                         + forward[i] * SP[i]           + mast[i] * ( CP[i] * CR[i]);
    }
}
