/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* 
   library(oce)
   system("R CMD SHLIB stutter_time.c")

   dyn.load("stutter_time.so")
   .Call("stutter_time", 0:3, 8)

   system.time(as.vector(t(outer(seq(0,1e7), seq(0,7/8,1/8), "+"))))

   user  system elapsed 
   4.056   1.907   5.923

   system.time(.Call("stutter_time", 0:1e7, 8))

   user  system elapsed 
   0.599   0.308   0.899 

*/

/*#define DEBUG*/
SEXP stutter_time(SEXP t, SEXP f)
{
  PROTECT(t = AS_NUMERIC(t));
  PROTECT(f = AS_NUMERIC(f));
  double *tp = REAL(t);
  double *fp = REAL(f);         /* will be made into an integer */
  int tlen = LENGTH(t);
  int fi = (int)fabs(0.5 + *fp);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, tlen * fi));
  double *resp = REAL(res);
  int ifast = 0;
  int islow = 0;
  while (islow < tlen) {
    /* FIXME: check for NA, possibly */
    for (int j = 0; j < fi; j++) {
      resp[ifast++] = tp[islow] + (double)j / (double)fi;
    }
    islow++;
  }
  UNPROTECT(3);
  return(res);
}
