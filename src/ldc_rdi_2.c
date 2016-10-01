/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
//#define SHOW(iensemble) (4 > abs((int)iensemble-29555))

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


/*

 TEMPORARY SANDBOX OF DK NOTES -- CONTINUALLY UPDATED< AND WILL BE REMOVED WHEN 
 NOT LONGER NEEDED

 There are 2 files in the test suite where ensembleStart (old method,
 from ldc_rdi.c) and ensemebleStart2 (new method, from present code)
 disagree. In both cases, diff(ensembleStart2) is uniform and
 diff(ensembleStart) varies, which suggests that ensembleStart2 is
 more accurate. Still, I want to get to the bottom of why they differ.
 Below is some of the output from the heavily instrumented (and
 varying) code


--- bad case 1 ---

file 'moored_workhorse_1200kHz/adp_rdi_1997.000' is adp/rdi
ERROR: >1 mismatch in lengths:
    length(ensembleStart)  =  81321 
    length(ensembleStart2) =  81319 
ERROR: found difference when comparing first  81319  entries of ensembleStart and ensembleStart2 
    firstMismatch= 29555  (note: there may be more later ... not checked, though) 
    following are some values in the mismatch neighborhood:
    ensembleStart       :  21688967 21689701 21690435 21691169 21691903 21692322 21692637 21693371 21694105 21694839 21695573 
    ensembleStart2      :  21688967 21689701 21690435 21691169 21691903 21692637 21693371 21694105 21694839 21695573 21696307 
    diff(ensembleStart) :  734 734 734 734 419 315 734 734 734 734 
    diff(ensembleStart2):  734 734 734 734 734 734 734 734 734 734 
read.adp.rdi(): subtracted a headingBias of  -18.1  degrees

--- bad case 2 ---


file 'vmdas_workhorse_300kHz_01/ADCP061_000000.ENR' is adp/rdi
ERROR: found difference when comparing first  2506  entries of ensembleStart and ensembleStart2 
    firstMismatch= 1944  (note: there may be more later ... not checked, though) 
    following are some values in the mismatch neighborhood:
    ensembleStart       :  2405059 2406300 2407541 2408782 2410023 2410610 2411264 2412505 2413746 2414987 2416228 
    ensembleStart2      :  2405059 2406300 2407541 2408782 2410023 2411264 2412505 2413746 2414987 2416228 2417469 
    diff(ensembleStart) :  1241 1241 1241 1241 587 654 1241 1241 1241 1241 
    diff(ensembleStart2):  1241 1241 1241 1241 1241 1241 1241 1241 1241 1241 
read.adp.rdi(): subtracted a headingBias of  -45  degrees
 

 */

//#define DEBUG

/*

library(testthat)
system("R CMD SHLIB ldc_rdi_2.c")
dyn.load('ldc_rdi_2.so')
filename <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
## next is 1.6s
system.time(d <- read.adp(filename, 1, 10))
# next is  < 0.2s (was 15.2s before some speedup work)
system.time(p <- .Call("ldc_rdi_2", filename))
expect_equal(length(p), 79134)
f <- file(filename, "rb")
buf<-readBin(f, "raw", 500000)
close(f)

*/


/*
 * old method, for the file used for data(adp): 1.6s elapsed
 *
 * this method, same file, simply scanning: 5.5s elapsed [but I have
 * some ideas for speedup, albeit with more alloc() complications.
 */
SEXP ldc_rdi_2(SEXP filename) // FIXME: add from,to args so we can look within
{
  /* Locate Data Chunk for RDI
   *   buf = buffer
   *   max = 0 in normal use, but can be >0 to test things manually
   *
   * Ref: WorkHorse Commands and Output Data Format_Nov07.pdf
   * p124: header structure (note that 'number of bytes in ensemble'
   *       does *not* count the first 2 bytes; it's really an offset to the
   *       checksum)
   * p158 (section 5.8) checksum
   */
  const char *filenamestring = CHAR(STRING_ELT(filename, 0));
  FILE *fp = fopen(filenamestring, "rb");
  if (!fp)
    error("cannot open file '%s'\n", filenamestring);
  int c, clast=0x00;
  int byte1 = 0x7f;
  int byte2 = 0x7f;
  unsigned short int check_sum, desired_check_sum;
  unsigned int bytes_to_check = 0;
  unsigned long int cindex = 0; // character index
  clast = fgetc(fp);
  cindex++;
  if (clast == EOF)
    error("empty file '%s'", filenamestring);

  // Growable buffer for ensemble starting points
  unsigned long int nensembles = 100000;
  int *ensembles;
  unsigned long int iensemble = 0;
  ensembles = (int *)Calloc((size_t)nensembles, int);
  if (ensembles == NULL)
    error("cannot set up the buffer used to store adp/rdi ensemble pointers");

  // Growable buffer for the present ensemble.
  unsigned long int nebuf = 30000;
  unsigned char *ebuf = (unsigned char *)Calloc((size_t)nebuf, unsigned char);
  if (ebuf == NULL)
    error("cannot set up the buffer used to store data within an individual adp/rdi ensemble");

  unsigned int FIXME = 0;
  int b1, b2;
  long int last_start = 0;

  while (1) {
    c = fgetc(fp); 
    if (c == EOF) break;
    cindex++;
    if (clast == byte1 && c == byte2) {
      last_start = cindex - 1;
      // The checksum includes the starting 0x7f, 0x7f pair, and also
      // the two bytes that specify the number of bytes in the
      // ensemble. So we start by adding these four bytes.
      check_sum = (unsigned short int)byte1;
      check_sum += (unsigned short int)byte2;
      b1 = fgetc(fp);
      if (b1 == EOF) break;
      cindex++;
      check_sum += (unsigned short int)b1;
      b2 = fgetc(fp);
      if (b2 == EOF) break;
      cindex++;
      check_sum += (unsigned short int)b2;
      // Now we are ready to look at the rest of the bytes. Note that
      // our loop starts at index 4, because we have already handled
      // those 4 bytes of the ensemble (i.e. those 4 bytes are include
      // in the bytes_to_check value that we now calculate).
      bytes_to_check = (unsigned int)b1 + 256 * (unsigned int)b2;
      //if (SHOW(iensemble)) Rprintf("NEW cindex=%d iensemble=%d bytes_to_check=%d\n",
      // cindex, iensemble, bytes_to_check);
      if (bytes_to_check < 5) { // this will only happen in error; we check so bytes_to_read won't be crazy
	Free(ensembles);
	Free(ebuf);
	error("cannot decode the length of ensemble number %d", iensemble);
      }
      unsigned int bytes_to_read = bytes_to_check - 4; // check_sum has used first 4 bytes already

      // Read the bytes in one operation, because fgetc() is too slow.
      if (bytes_to_read > nebuf) {
	  ebuf = (unsigned char *)Realloc(ebuf, bytes_to_read, unsigned char);
	  if (ebuf== NULL)
	    error("cannot enlarge the buffer used to store data within an individual adp/rdi ensemble; trying to enlarge to %d bytes", bytes_to_read);
	  nebuf = bytes_to_read;
      }
      fread(ebuf, bytes_to_read, sizeof(unsigned char), fp);
      if (feof(fp)) {
	//Free(ensembles);
	//Free(ebuf);
	//Rprintf("NEW: end of file while reading ensemble number %d, at byte %d\n", iensemble+1, cindex);
	break;
      }
      cindex += bytes_to_read;
      for (int ib = 0; ib < bytes_to_read; ib++) {
	check_sum += (unsigned short int)ebuf[ib];
	//if (SHOW(iensemble)) Rprintf("NEW iensemble=%d ib=%d check_sum=%d\n", iensemble, ib, check_sum);
      }
      int cs1, cs2;
      cs1 = fgetc(fp);
      if (cs1 == EOF) break;
      cindex++;
      cs2 = fgetc(fp);
      if (cs2 == EOF) break;
      cindex++;
      desired_check_sum = ((unsigned short int)cs1) | ((unsigned short int)(cs2 << 8));
      //if (SHOW(iensemble)) Rprintf("NEW iensemble=%d icindex=%d check_sum %d desired_check_sum=%d b1=%d b2=%d bytes_to_check=%d\n",
      // iensemble, cindex, check_sum, desired_check_sum, b1, b2, bytes_to_check);
      if (check_sum == desired_check_sum) {
	if (iensemble >= nensembles) {
	  //Rprintf("iensemble=%d : present  storage starts at 0x%x and can contain %d elements...\n", iensemble, ensembles, nensembles);
	  ensembles = (int *)Realloc(ensembles, 2*nensembles, int);
	  if (ensembles == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi ensemble pointers; trying to enlarge to %d bytes", 2*nensembles);
	  nensembles = 2 * nensembles;
	  //Rprintf("            : upgraded storage starts at 0x%x and can contain %d elements...\n", ensembles, nensembles);
	}
	ensembles[iensemble] = last_start;
	iensemble++;
      } // if it doesn't match the check_sum, we just ignore it as a coincidence of a 0x7f 0x7f pair
      R_CheckUserInterrupt(); // only check once per ensemble, for speed
      clast = c;
      FIXME++;
    }
    clast = c;
    c = fgetc(fp); 
    if (c == EOF) break;
    cindex++;
  }
  fclose(fp);
  SEXP res;
  PROTECT(res = NEW_INTEGER(iensemble));
  int *pres = INTEGER_POINTER(res);
  for (long int i = 0; i < iensemble; i++)
    pres[i] = ensembles[i];
  Free(ensembles);
  Free(ebuf);
  UNPROTECT(1);
  return(res);
}

