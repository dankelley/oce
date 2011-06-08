/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

SEXP adv_vector_time(SEXP vvdStart, SEXP vvdhStart, SEXP vvdhTime, SEXP sampling_rate)
{
  // This is called by read.adv.nortek(), in adv.nortek.R, with arguments as follows
  //   vvdStart = indices of 'vector velocity data' (0xA5 ox10)
  //   vvdhStart = indices of 'vector velocity data header' (0xA5 ox10)
  //   vvdhTime = POSIX times of vvdh
  //   sampling_rate = samples/second
  // and the result is a vector of times for the vvd items, which has length 
  // matching that of vvdStart.  The method is to step forward from
  // each vvdhTime, incrementing time by 1/sampling_rate.
  PROTECT(vvdStart = AS_NUMERIC(vvdStart));
  PROTECT(vvdhStart = AS_NUMERIC(vvdhStart));
  PROTECT(vvdhTime = AS_NUMERIC(vvdhTime));
  PROTECT(sampling_rate = AS_NUMERIC(sampling_rate));
  double *vvdStartp = REAL(vvdStart);
  double *vvdhStartp = REAL(vvdhStart);
  double *vvdhTimep = REAL(vvdhTime);
  double *sampling_ratep = REAL(sampling_rate);
  int nvvd = LENGTH(vvdStart);
  int nvvdh = LENGTH(vvdhStart);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, nvvd));
  double *resp = REAL(res);
  int ivvd, ivvdh = 0;
  double t = vvdhTimep[0];
  
#ifdef DEBUG
  for (int iii=0; iii<nvvdh;iii++) Rprintf("iii=%d time %f\n", iii, vvdhTimep[iii]);
#endif
  // Pin time to start, if vvd precede vvdh (perhaps not possible).
  for (ivvd = 0; ivvd < nvvd; ivvd++) {
    if (vvdStartp[ivvd] < vvdhStartp[ivvdh]) {
      resp[ivvd] = NA_REAL;
    } else {
      break;
    }
  }
  double dt =  1.0 / *sampling_ratep;
  if (ivvd < nvvd) {
    for (; ivvd < nvvd; ivvd++) {
#ifdef DEBUG
      Rprintf("ivvd=%d (%f)     ivvdh=%d (%f)\n", ivvd, vvdStartp[ivvd], ivvdh, vvdhStartp[ivvdh]);
#endif
      // use largest vvdh that is still has vvdhStart < vvdStart
      if (ivvdh < (nvvdh - 1) && vvdStartp[ivvd] > vvdhStartp[ivvdh + 1]) {
	  ivvdh += 1;
	  t = vvdhTimep[ivvdh];
#ifdef DEBUG
	  Rprintf(" (update ivvdh to %d, yielding t=%f)\n", ivvdh, t);
#endif
      }
      t += dt;
      resp[ivvd] = t;
    }
  }
  UNPROTECT(5);
  return(res);
}
