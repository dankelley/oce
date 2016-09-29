/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/*

library(testthat)
system("R CMD SHLIB ldc_rdi_2.c")
dyn.load('ldc_rdi_2.so')
filename <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
## next is 1.6s
system.time(d <- read.adp(filename, 1, 10))
# next is  5.5s using EOF; was 15.2s using feof()
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
  //PROTECT(filename = AS_CHARACTER(filename));
  //unsigned char *pfilename = CHARACTER_POINTER(filename); //*CHAR(STRING_ELT(filename, 0));
  const char *filenamestring = CHAR(STRING_ELT(filename, 0));
  FILE *fp = fopen(filenamestring, "rb");
  if (!fp)
    error("cannot open file '%s'\n", filenamestring);
  int c, clast=0x00;
  int byte1 = 0x7f;
  int byte2 = 0x7f;
  unsigned short int check_sum, desired_check_sum;
  unsigned int bytes_to_check = 0;
  unsigned int cindex = 0; // character index
  clast = fgetc(fp);
  cindex++;
  if (clast == EOF)
    error("empty file '%s'", filenamestring);
  // Growable buffer for ensemble starting points
  unsigned long int nensembles = 100000;
  int *ensembles;
  unsigned long int iensemble = 0;
  ensembles = (int *)Calloc((size_t)nensembles, int);
  // Growable buffer for the present ensemble.
  unsigned long int nebuf = 30000;
  unsigned char *ebuf = (unsigned char *)Calloc((size_t)nebuf, unsigned char);

  unsigned int FIXME = 0;
  int b1, b2;
  int last_start = 0;

  while (1) { // new 
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
      if (bytes_to_check < 5) {
	Free(ensembles);
	Free(ebuf);
	error("cannot decode the length of ensemble number %d", iensemble);
      }
      unsigned int bytes_to_read = bytes_to_check - 4; // check_sum has used first 4 bytes already

      // Read the bytes in one operation, because fgetc() is too slow.
      if (bytes_to_read > nebuf) {
	  ebuf = (unsigned char *)Realloc(ebuf, bytes_to_read + 1000, unsigned char); // ask for extra space
	  nebuf = bytes_to_read + 1000;
      }
      fread(ebuf, bytes_to_read, sizeof(unsigned char), fp);
      cindex += bytes_to_check;
      if (feof(fp)) {
	Free(ensembles);
	Free(ebuf);
	error("end of file while looking for a checksum"); // this exits, I think, so we must Free() first
      }
      for (int ib = 0; ib < bytes_to_read; ib++) {
	check_sum += (unsigned short int)ebuf[ib];
      }
      int cs1, cs2;
      cs1 = fgetc(fp);
      if (cs1 == EOF) break;
      cindex++;
      cs2 = fgetc(fp);
      if (cs2 == EOF) break;
      cindex++;
      desired_check_sum = ((unsigned short int)cs1) | ((unsigned short int)(cs2 << 8));
      if (FIXME < 3) Rprintf("  icindex=%d, cs1=%d cs2=%d check_sum %d; desired_check_sum=%d\n",
	  cindex, cs1, cs2, check_sum, desired_check_sum);
      if (check_sum == desired_check_sum) {
	if (iensemble >= nensembles) {
	  //Rprintf("iensemble=%d : present  storage starts at 0x%x and can contain %d elements...\n", iensemble, ensembles, nensembles);
	  ensembles = (int *)Realloc(ensembles, 2*nensembles, int);
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
  for (int i = 0; i < iensemble; i++)
    pres[i] = ensembles[i];
  Free(ensembles);
  Free(ebuf);
  UNPROTECT(1);
  return(res);
}

