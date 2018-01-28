/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */
/*
 cd("/Users/kelley/internal-beach/work/data/R")
 source("m07_pcadp.R")
 dyn.load("~/src/R-kelley/oce/src/sontek_adp.so");.Call("ldc_sontek_adp", dan.buf, 0, 0, 0, 1, 10);print(dan.profile.start[1:10])
*/ 
/* REFERENCES
 *   1. see ADPManual_710.pdf, logical pages 82-86.
 *   2. hydratools20apr06/adp2cdf.m line 1360 re PCADP's extra header.
 */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
//#define DEBUG

SEXP ldc_sontek_adp(SEXP buf, SEXP Shave_ctd, SEXP Shave_gps, SEXP Shave_bottom_track, SEXP Spcadp, SEXP Smax)
{
  /* ldc = locate data chunk; _sontek_adp = for a SonTek ADV.
   * Arguments: 
   *   buf = buffer with data
   *   Shave_ctd = 1 if have CTD data (must be 0, for this version)
   *   Shave_gps = 1 if have GPS data (must be 0, for this version)
   *   Shave_bottom_track = 1 if have bottom-track data (must be 0, for this version)
   *   Spcadp = 1 if device is a PCADP (which has longer headers)
   *   Smax = number of profiles to get (set to <0 to get all)
   * 
   * Method:
   *   The code checks for bytes as follows, and does a checksum on
   *   results.  It has to determine ncell and nbeam first.
   *     BYTE   Contents
   *       1    0xA5 (flag 1)
   *       2    0x10 (flag 2)
   *       3    0x50 (decimal 80, number of bytes in header)
   *       4+   See ADPManual_710.pdf, logical page 84 et seq.
   *
   * Testing:
   R CMD SHLIB sontek_adp.o
   dyn.load("sontek_adp.so")
   */
  unsigned char byte1 = 0xA5;
  unsigned char byte2 = 0x10;
  unsigned char byte3 = 0x50; /* bytes in header (=80) */
  unsigned char *pbuf;
  PROTECT(buf = AS_RAW(buf));
  PROTECT(Shave_ctd = AS_INTEGER(Shave_ctd));
  PROTECT(Shave_gps = AS_INTEGER(Shave_gps));
  PROTECT(Shave_bottom_track = AS_INTEGER(Shave_bottom_track));
  PROTECT(Spcadp = AS_INTEGER(Spcadp));
  PROTECT(Smax = AS_INTEGER(Smax));
  int have_ctd = *INTEGER_POINTER(Shave_ctd);
  int have_bottom_track = *INTEGER_POINTER(Shave_bottom_track);
  int have_gps = *INTEGER_POINTER(Shave_gps);
  int pcadp = *INTEGER_POINTER(Spcadp);
  int max = *INTEGER_POINTER(Smax);
#ifdef DEBUG
  Rprintf("have_ctd=%d, have_bottom_track=%d, have_gps=%d, max=%d\n",have_ctd,have_bottom_track,have_gps,max);
#endif
  if (have_ctd != 0)
    error("cannot read SonTek ADP files with CTD data");
  if (have_bottom_track != 0)
    error("cannot read SonTek ADP files with bottom-track data");
  if (have_gps != 0)
    error("cannot read SonTek ADP files with GPS data");
  pbuf = RAW_POINTER(buf);
  int lbuf = LENGTH(buf);
  SEXP res;
#ifdef DEBUG
  Rprintf("lbuf=%d\n",lbuf);
#endif
  /* Count matches, so we can allocate the right length */
  unsigned int matches = 0;
  unsigned short int check_sum_start = ((unsigned short)0xa5<<8)  | ((unsigned short)0x96); /* manual p96 says 0xA596; assume little-endian */
  unsigned short int check_sum, desired_check_sum;
  if (max < 0)
    max = 0;
  /* scan first profile to determine ncell and nbeam */
  int first_look = 1000;
  if (first_look > lbuf)
    error("cannot read Sontek ADP from a buffer with fewer than 1000 bytes");
  int i;
  int ncell = -1, nbeam = -1;
  for (i = 0; i < first_look - 3; i++) { /* note that we don't look to the very end */
    //Rprintf(" %d: %x %x %x (%x %x %x)\n", i, pbuf[i], pbuf[i+1], pbuf[i+2], byte1, byte2, byte3);
    if (pbuf[i] == byte1 && pbuf[i+1] == byte2 && pbuf[i+2] == byte3) {
      nbeam = (int)pbuf[i + 26];
      ncell = ((unsigned short)pbuf[i+30]) | ((unsigned short)pbuf[i+31] << 8);
#ifdef DEBUG
      Rprintf("tentative first-profile at buf[%d], yielding nbeam=%d and ncell=%d\n",
          i, nbeam, ncell);
#endif
      if (nbeam < 2 || nbeam > 3)
        error("number of beams must be 2 or 3, but it is %d", nbeam);
      if (ncell < 1)
        error("number of cells cannot be less than 1, but it is %d", ncell);
      break;
    }
  }
  if (nbeam < 0 || ncell < 0)
    error("cannot determine #beams or #cells, based on first 1000 bytes in buffer");
  // The next line envisions more data streams, e.g. ctd.
  int chunk_length = 80 + (have_ctd?16:0) + (have_gps?40:0) + (have_bottom_track?18:0) + 4 * ncell * nbeam;
  // Next 2 lines acount for extra header in each PCADP profile; see ref 2.
  int max_beams = 4;
  int pcadp_extra_header_length = 2*(8+max_beams) + 2*max_beams + max_beams;
  if (pcadp)
    chunk_length += pcadp_extra_header_length;
#ifdef DEBUG
  Rprintf("bytes: 0x%x 0x%x 0x%x\n", byte1, byte2, byte3);
  Rprintf("chunk_length: %d\n", chunk_length);
  int bad, maxbad = 10;
#endif
  for (int i = 0; i < lbuf - 3 - chunk_length; i++) { // FIXME is 3 right, or needed?
    check_sum = check_sum_start;
    if (pbuf[i] == byte1 && pbuf[i+1] == byte2 && pbuf[i+2] == byte3) {
      for (int c = 0; c < chunk_length; c++)
        check_sum += (unsigned short int)pbuf[i + c];
      desired_check_sum = ((unsigned short)pbuf[i+chunk_length]) | ((unsigned short)pbuf[i+chunk_length+1] << 8);
      if (check_sum == desired_check_sum) {
        matches++;
#ifdef DEBUG
        Rprintf("good match i=%d check_sum=%d\n", i, check_sum);
#endif
        if (max != 0 && matches >= max)
          break;
      } else {
#ifdef DEBUG
        if (bad++ > maxbad)
          error("max bad\n");
        Rprintf("bad checksum i=%d\n", i);
#endif
      }
    }
  }
  /* allocate space, then run through whole buffer again, noting the matches */
  int lres = matches;
  if (lres > 0) {
    PROTECT(res = NEW_INTEGER(lres));
    int *pres = INTEGER_POINTER(res);
#ifdef DEBUG
    Rprintf("getting space for %d matches\n", lres);
#endif
    unsigned int ires = 0;
    for (int i = 0; i < lbuf - 3 - chunk_length; i++) { // FIXME is 3 right, or needed?
      check_sum = check_sum_start;
      if (pbuf[i] == byte1 && pbuf[i+1] == byte2 && pbuf[i+2] == byte3) {
        for (int c = 0; c < chunk_length; c++)
          check_sum += (unsigned short int)pbuf[i + c];
        desired_check_sum = ((unsigned short)pbuf[i+chunk_length]) | ((unsigned short)pbuf[i+chunk_length+1] << 8);
        if (check_sum == desired_check_sum)
          pres[ires++] = i + 1; /* the +1 is to get R pointers */
        if (ires > lres)        /* FIXME: or +1? */
          break;
      }
    }
  } else {
    PROTECT(res = NEW_INTEGER(1));
    int *pres = INTEGER_POINTER(res);
    pres[0] = 0;
  }
  UNPROTECT(7);
  return(res);
}
