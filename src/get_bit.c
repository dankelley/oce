/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG
/*
   TEST

   system("R CMD shlib get_bit.c")
   dyn.load("get_bit.so")
   buf<-0x0a
   for(i in 7:0) cat(.Call("get_bit", buf, i))

   OUTPUT (correct)

   00001010

 */

SEXP get_bit(SEXP buf, SEXP bit) // bit=0 for rightmost bit, =7 for leftmost bit
{
  static unsigned char mask[] = {1, 2, 4, 8, 16, 32, 64, 128};
  PROTECT(buf = AS_RAW(buf));
  PROTECT(bit = AS_INTEGER(bit));
  int n = LENGTH(buf);
  unsigned char *bufp = RAW_POINTER(buf);
  int *bitp = INTEGER_POINTER(bit);
  if (*bitp < 0)
    error("cannot have bit number less than 0; got %d", *bitp);
  if (*bitp > 7)
    error("cannot have bit number greater than 7; got %d", *bitp);
  SEXP res;
  PROTECT(res = NEW_INTEGER(n));
  int *resp = INTEGER_POINTER(res);
  for (int i = 0; i < n; i++) {
    resp[i] = (bufp[i] & mask[*bitp]) != 0;
  }
  UNPROTECT(3);
  return(res);
}
