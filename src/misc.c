#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*

In shell:
R CMD SHLIB misc.c

In R:
dyn.load("misc.so");x<-1:3;y<-x*3;xout<-1:10;.Call("oce_approx",x,y,xout,1,2)
*/
SEXP oce_approx(SEXP x, SEXP y, SEXP xout, SEXP n, SEXP m)
{
  int x_len = length(x);
  int  y_len = length(y);
  int xout_len = length(xout);
  double *xp, *yp, *xoutp, *ansp;
  SEXP ans;
  if (x_len != y_len) error("lengths of x (%d) and y (%d) disagree", x_len, y_len);
  xp = REAL(x);
  xoutp = REAL(xout);
  yp = REAL(y);
  PROTECT(ans = allocVector(REALSXP, xout_len));
  ansp = REAL(ans);
  int i;
  for (i = 0; i < xout_len; i++) {
    *(ansp + i) = *(xoutp + i);
  }
  UNPROTECT(1);
  return(ans);
}

/* 
** compile from commandline:
R CMD SHLIB test.c
** test R code:
m <- matrix(rep(seq(0, 1, length.out=5), 5), nrow=5, byrow=TRUE)
m[3,3] <- 2
dyn.load("test.so")
m1 <- .Call("matrix_smooth", m)
m2 <- .Call("matrix_smooth", m1)
m3 <- .Call("matrix_smooth", m2)
par(mfrow=c(2,2))
image(m,  col=rainbow(100), zlim=c(0,4), main="raw")
image(m1, col=rainbow(100), zlim=c(0,4), main="smoothed 1 time")
image(m2, col=rainbow(100), zlim=c(0,4), main="smoothed 2 times")
image(m3, col=rainbow(100), zlim=c(0,4), main="smoothed 3 times")
*/

SEXP matrix_smooth(SEXP mat)
{
  /* Note: the 2d data are stored in column order */
  SEXP res;
  int nrow = INTEGER(GET_DIM(mat))[0];
  int ncol = INTEGER(GET_DIM(mat))[1];
  int i, j;
  double *matp, *resp;
  if (!isMatrix(mat)) error("'mat' must be a matrix");
  //if (isInteger(mat)) warning("'mat' is integer, but should be real");
  if (!isReal(mat)) error("'mat' must be numeric, not integer");
  matp = REAL(mat);
  if (length(mat) != nrow * ncol) error("'nrow'*'ncol' must equal number of elements in 'mat'");
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
	(2 * (*(matp +   j   + ncol *    i    )) +
	 1 * (*(matp + j - 1 + ncol *    i    )) +
	 1 * (*(matp + j + 1 + ncol *    i    )) +
	 1 * (*(matp +   j   + ncol * (i - 1) )) +
	 1 * (*(matp +   j   + ncol * (i + 1) ))) / 6.0;
    }
  }
  UNPROTECT(1);
  return(res);
}
