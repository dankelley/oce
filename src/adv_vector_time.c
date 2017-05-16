/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/*

system("R CMD SHLIB adv_vector_time.c"); dyn.load("adv_vector_time.so"); .Call("adv_vector_time", 1,2,3,4,5,6,7)

system("R CMD SHLIB adv_vector_time.c");dyn.load("adv_vector_time.so");.Call("adv_vector_time",5,c(1,3,6,10),3,4,5,0,7)

system("R CMD SHLIB adv_vector_time.c");dyn.load("adv_vector_time.so");.Call("adv_vector_time",5:6,c(1,3,7,10),3,4,5,0,7)
 
 
 */

SEXP adv_vector_time(SEXP vvdStart, SEXP vsdStart, SEXP vsdTime, SEXP vvdhStart, SEXP vvdhTime, SEXP n, SEXP f)
{
  // This is called by read.adv.nortek(), in adv.nortek.R, with arguments as follows
  //   vvdStart = indices of 'vector velocity data' (0xA5 ox10)
  //   vvdhStart = indices of headers for 'vector velocity data header' (0xA5 ox10)
  //   vvdhTime = POSIX times of vvdh
  //   vsdStart = indices of headers for 'vector system data' (0xA5 ox11)
  //   vsdTime = POSIX times of vsd
  //   n = samples expected (set to 0 for continous mode)
  //   f = sampling rate in Hz
  // and the result is a vector of times for the vvd items, which has length 
  // matching that of vvdStart.  The method works by left-bracketing
  // velocity data with vsd headers, and stepping forward thereafter
  // in times dt=1/f.
  PROTECT(vvdStart = AS_NUMERIC(vvdStart));
  PROTECT(vsdStart = AS_NUMERIC(vsdStart));
  PROTECT(vsdTime = AS_NUMERIC(vsdTime));
  PROTECT(vvdhStart = AS_NUMERIC(vvdhStart));
  PROTECT(vvdhTime = AS_NUMERIC(vvdhTime));
  PROTECT(f = AS_NUMERIC(f));
  double *vvdStartp = REAL(vvdStart);
  double *vsdStartp = REAL(vsdStart);
  double *vvdhStartp = REAL(vvdhStart);
  double *vsdTimep = REAL(vsdTime);
  double *vvdhTimep = REAL(vvdhTime);
  double *np = REAL(n);
  double *fp = REAL(f);
  int nvvd = LENGTH(vvdStart);
  int nvsd = LENGTH(vsdStart);
  int nvvdh = LENGTH(vvdhStart);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, nvvd));
  double *resp = REAL(res);
  unsigned long int ivvd, ivvdh = 0;
  double t = vvdhTimep[0];
  int nn = (int)floor(*np + 0.5);
  if (nn < 0)
    error("cannot have negative n (number of points), but got %d (after rounding from %f)", nn, np[0]);
  if (fp[0] < 0)
    error("cannot have negative f (sampling frequency), but got %f", fp[0]);
#ifdef DEBUG
  for (int iii=0; iii<nvvdh;iii++) Rprintf("nvvdhTime[%d] = %f\n", iii, vvdhTimep[iii]);
#endif
  double dt =  1.0 / fp[0];
  if (nn == 0) {
    // Continuous sampling
    //
    // 1. Move vsd pointer to point at vsd entry just preceding first vvd entry.
    int ivvd, ivsd=0;
#ifdef DEBUG
    Rprintf("continuous sampling\n");
    Rprintf("vvdStartp[0]=%f\n", vvdStartp[0]);
    Rprintf("vsdStartp[0]=%f\n", vsdStartp[0]);
#endif
    while(vvdStartp[0] > vsdStartp[ivsd]) {
      if (++ivsd >= nvsd)
        error("cannot interpret times for velocities, because no Vector System Data precede first velocity datum");
#ifdef DEBUG
      Rprintf("ivsd=%d\n", ivsd);
#endif
    }
    if (ivsd > 0)
      ivsd--;
#ifdef DEBUG
    Rprintf("got ivsd=%d\n",ivsd);
    Rprintf("vvdStartp[0]=%f\n", vvdStartp[0]);
    Rprintf("vsdStartp[%d]=%f\n", ivsd, vsdStartp[ivsd]);
#endif
    // 2. step through vvd, updating left-neighbor vsd when necessary
    double toffset = 0.0;
    for (ivvd = 0; ivvd < nvvd; ivvd++) {
      if (ivsd < (nvsd - 1) && vsdStartp[ivsd+1] < vvdStartp[ivvd]) {
        ivsd++; // enter new time era
        toffset = 0.0;
      }
      resp[ivvd] = vsdTimep[ivsd] + toffset;
      toffset += dt;
    }
  } else {
    // Burst sampling
#ifdef DEBUG
    Rprintf("burst sampling\n");
#endif
    // Pin time to start, if vvd precede vvdh (perhaps not possible).
    for (ivvd = 0; ivvd < nvvd; ivvd++) {
      if (vvdStartp[ivvd] < vvdhStartp[ivvdh]) {
        resp[ivvd] = NA_REAL;
      } else {
        break;
      }
    }
#ifdef DEBUG
    Rprintf("ivvd= %d (C notation)  dt= %.10f   f %f\n", ivvd, dt, fp[0]);
#endif
    if (ivvd < nvvd) {
      for (; ivvd < nvvd; ivvd++) {
        //#ifdef DEBUG
        //      Rprintf("ivvd=%d (%f)     ivvdh=%d (%f)\n", ivvd, vvdStartp[ivvd], ivvdh, vvdhStartp[ivvdh]);
        //#endif
        // use largest vvdh that is still has vvdhStart < vvdStart
        if (ivvdh < (nvvdh - 1) && vvdStartp[ivvd] > vvdhStartp[ivvdh + 1]) {
          ivvdh += 1;
          t = vvdhTimep[ivvdh];
#ifdef DEBUG
          Rprintf("ivvd = %d ; update to ivvdh = %d, yielding t=%f)\n", ivvd, ivvdh, t);
#endif
        }
        t += dt;
        resp[ivvd] = t;
      }
    }
  }
  UNPROTECT(7);
  return(res);
}
