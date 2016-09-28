/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/*

system("R CMD SHLIB ldc_rdi_2.c")
dyn.load('ldc_rdi_2.so')
filename <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
d <- read.adp(filename, 1, 10)
np <- .Call("ldc_rdi_2", filename)
message("got ", np, " profiles")
buf<-readBin(file(filename,"rb"), "raw", 500000)



## for reference:
## 79135 profiles
## > buf[1835+0:3]
## [1] 7f 7f 28 07

## for reference:
## > d <- read.adp(filename, 1, 10)
##   ic 0  byte 127  check_sum 127
##   ic 1  byte 127  check_sum 254
##   ic 2  byte 40  check_sum 294
##   ic 3  byte 7  check_sum 301
##   ic 4  byte 0  check_sum 301
##   ic 5  byte 6  check_sum 307
##   ic 6  byte 18  check_sum 325
##   ic 7  byte 0  check_sum 325
##   ic 8  byte 77  check_sum 402
##   ic 9  byte 0  check_sum 402
 
*/


/*
 * old method, for the file used for data(adp): 1.2s elapsed
 *
 * this method, same file, simply scanning: 9.9s elapsed [but I have
 * some ideas for speedup, albeit with more alloc() complications.
 */
SEXP ldc_rdi_2(SEXP filename)
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
  SEXP res;
  PROTECT(res = NEW_INTEGER(1));
  int *pres = INTEGER_POINTER(res);
  int lres = 0;
  unsigned char c, clast=0x00;
  unsigned char byte1 = 0x7f;
  unsigned char byte2 = 0x7f;
  unsigned short int check_sum, desired_check_sum;
  unsigned int bytes_to_check = 0;
  // // check that we can read a file
  // while (1) {
  //   c = fgetc(fp);
  //   if (feof(fp))
  //     break;
  //   lres++;
  // }
  // pres[0] = lres;
  unsigned int cindex = 0; // character index
  clast = fgetc(fp);
  if (feof(fp))
    error("empty file '%s'", filenamestring);
  cindex++;
  pres[0] = 0;
  unsigned int matches = 0;
  unsigned int FIXME = 0;
  unsigned char b1, b2;
  while (1) { // new 
    c = fgetc(fp); 
    cindex++;
    if (feof(fp))
      break;
    if (clast == byte1 && c == byte2) {
      check_sum = byte1;
      if (FIXME < 3) Rprintf("  cindex %d, byte 0x%02x, check_sum %d [NEW METHOD a]\n", cindex-2, byte1, check_sum);
      check_sum += byte2;
      if (FIXME < 3) Rprintf("  cindex %d, byte 0x%02x, check_sum %d [NEW METHOD b]\n", cindex-1, byte2, check_sum);
      unsigned char b1, b2;
      b1 = fgetc(fp);
      check_sum += (unsigned short int)b1;
      if (FIXME < 3) Rprintf("  cindex %d, byte 0x%02x, check_sum %d [NEW METHOD c]\n", cindex, b1, check_sum);
      cindex++;
      if (feof(fp)) break;
      b2 = fgetc(fp);
      check_sum += (unsigned short int)b2;
      if (FIXME < 3) Rprintf("  cindex %d, byte 0x%02x, check_sum %d [NEW METHOD d]\n", cindex, b2, check_sum);
      cindex++;
      if (feof(fp)) break;
      bytes_to_check = (unsigned int)b1 + 256 * (unsigned int)b2; // we do the first 4 manually; see next 4 lines
      if (FIXME < 5) Rprintf("bytes_to_check %d; matches=%d\n", bytes_to_check, matches);
      // FIXME: consider reading all bytes in the chunk into a buffer,
      // which may speed things up.
      for (int ic = 4; ic < bytes_to_check; ic++) { // note starting index. we already have 4 bytes done
	unsigned char ccheck = fgetc(fp);
	cindex++;
	check_sum += (unsigned short int)ccheck;
	if (FIXME < 1 && (ic < 5 || ic > (bytes_to_check-5)))
	  Rprintf("  cindex %d, ic %d, byte 0x%02x, check_sum %d [NEW METHOD]\n", cindex, ic, ccheck, check_sum);
	if (feof(fp))
	  error("end of file while looking for a checksum");
      }
      unsigned char cs1, cs2;
      cs1 = fgetc(fp);
      cindex++;
      if (feof(fp))
	break;
      cs2 = fgetc(fp);
      cindex++;
      if (feof(fp))
	break;
      // new
      desired_check_sum = ((unsigned short int)cs1) | ((unsigned short int)(cs2 << 8));
      if (FIXME < 3) Rprintf("  icindex=%d, cs1=%d cs2=%d check_sum %d; desired_check_sum=%d\n",
	  cindex, cs1, cs2, check_sum, desired_check_sum);
      if (check_sum == desired_check_sum) {
	matches++;
	if (FIXME < 5) Rprintf("   MATCH\n");
      } else {
	if (FIXME < 5) Rprintf("   NO MATCH\n");
      }
      R_CheckUserInterrupt(); // only check once per profile, for speed
      clast = c;
      FIXME++;
    }
    clast = c;
    c = fgetc(fp); 
    cindex++;
    if (feof(fp))
      break;
  }
  pres[0] = matches;
  UNPROTECT(1);
  fclose(fp);
  Rprintf("DEBUG: ldc_rdi_2 is only returning # of profiles (and that's ok for data(adp) file)\n");
  return(res);
}

