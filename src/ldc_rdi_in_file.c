/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
//#define SHOW(iensemble) (4 > abs((int)iensemble-29555))

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG


/*

Locate Data Chunk for RDI

@description

Read an RDI adp file, finding the start indices of ensembles (or
"profiles", in other words).
Use `from` and `to` to extract a subset of the file. (Possibly a
`by` argument will get added later.) Setting `from=0`
and `to=0` means to read the entire file.

@details

In the name, "ldc" refers to "Locate Data Chunk", and "rdi" refers to
RD Instruments.  Note that an earlier version of this function, used
before October 2016, was passed a buffer that contained the whole file
contents. This earlier scheme was dropped because of limitations on
the size of vectors in R, which is set by the R use of 32-bit
integers. (Note: 2^32-1 corresponds to a data file of roughly 4.3Gb.)

@param filename name of an RDI adp file

@param from first ensemble (AKA profile) to get, in R notation, i.e. 1
means the first ensemble in the file. (Possibly this will later be
permitted to be negative, e.g. from=-10 with to=0 might be used to get
the last 10 ensembles in the file. But this is not set up yet, because
I do not see the need for it.)

@param to last ensemble to get.  Setting this to 0 will retrieve *all*
the data within the file.  If `to` exceeds the number of ensembles in
the file, then the returned value will just be the actual number of
ensembles.

@value a vector of indices where enembles start, in R notation, i.e.
if the file starts with an ensemble (flagged by the first two bytes
being 0x7f) then the first value in the returned result is 1.

@examples

R CMD SHLIB ldc_rdi_in_file.c
f <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
dyn.load("src/ldc_rdi_in_file.so")
a <- .Call("ldc_rdi_in_file", f, 1, 0)  # whole file
b <- .Call("ldc_rdi_in_file", f, 1, 10) # first 10 ensembles
stopifnot(all.equal(length(a), 79134))
stopifnot(all.equal(a[1:10], b))

@references

1. WorkHorse Commands and Output Data Format_Nov07.pdf

2. p124 of [1]: header structure (note that 'number of bytes in
ensemble' does *not* count the first 2 bytes; it's really an offset to
the checksum)

3. p158 (section 5.8) of [1] defines how to handle checksums

@author

Dan Kelley

*/

SEXP ldc_rdi_in_file(SEXP filename, SEXP from, SEXP to)
{
  /*
     
# Test R code, used by developers whilst debugging:

system("R CMD SHLIB src/ldc_rdi_in_file.c")
f <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
dyn.load("src/ldc_rdi_2.so")
a <- .Call("ldc_rdi_2", f, 1, 0)
b <- .Call("ldc_rdi_2", f, 1, 10)
stopifnot(all.equal(length(a), 79134))
stopifnot(all.equal(a[1:10], b))
    
   */

  const char *filenamestring = CHAR(STRING_ELT(filename, 0));
  FILE *fp = fopen(filenamestring, "rb");
  if (!fp)
    error("cannot open file '%s'\n", filenamestring);
  PROTECT(from = AS_INTEGER(from));
  int from_value = *INTEGER_POINTER(from);
  if (from_value < 0)
    error("'from' must be positive");
  PROTECT(to = AS_INTEGER(to));
  int to_value = *INTEGER_POINTER(to);
  if (to_value < 0)
    error("'to' must be positive");

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
  unsigned long int in_ensemble = 0, out_ensemble = 0;

  int *ensembles = (int *)Calloc((size_t)nensembles, int);
  if (ensembles == NULL)
    error("cannot set up the buffer used to store adp/rdi ensembles");
  int *bytes = (int *)Calloc((size_t)nensembles, int);
  if (bytes == NULL)
    error("cannot set up the buffer used to store adp/rdi bytes");
  unsigned char *years = (unsigned char *)Calloc((size_t)nensembles, unsigned char);
  if (years == NULL)
    error("cannot set up the buffer used to store adp/rdi years");
  unsigned char *months = (unsigned char *)Calloc((size_t)nensembles, unsigned char);
  if (months == NULL)
    error("cannot set up the buffer used to store adp/rdi months");
  unsigned char *days = (unsigned char *)Calloc((size_t)nensembles, unsigned char);
  if (days == NULL)
    error("cannot set up the buffer used to store adp/rdi days");
  unsigned char *hours = (unsigned char *)Calloc((size_t)nensembles, unsigned char);
  if (hours == NULL)
    error("cannot set up the buffer used to store adp/rdi hours");
  unsigned char *minutes = (unsigned char *)Calloc((size_t)nensembles, unsigned char);
  if (minutes == NULL)
    error("cannot set up the buffer used to store adp/rdi minutes");
  unsigned char *seconds = (unsigned char *)Calloc((size_t)nensembles, unsigned char);
  if (seconds == NULL)
    error("cannot set up the buffer used to store adp/rdi seconds");
  unsigned char *sec100s = (unsigned char *)Calloc((size_t)nensembles, unsigned char);
  if (sec100s == NULL)
    error("cannot set up the buffer used to store adp/rdi sec100s");
  // Growable buffer for the present ensemble.
  unsigned long int nebuf = 5000;
  unsigned char *ebuf = (unsigned char *)Calloc((size_t)nebuf, unsigned char);
  if (ebuf == NULL)
    error("cannot set up the buffer used to store data within an individual adp/rdi ensemble");

  int b1, b2;
  long int last_start = 0;

  while (1) {
    c = fgetc(fp);
    if (c == EOF) break;
    cindex++;
    // Try to locate "ensemble starts", spots where a 0x7f is followed by a second 0x7f,
    // then followed by data that match a checksum.
    if (clast == byte1 && c == byte2) {
      last_start = cindex - 1;
      // The checksum includes the starting 0x7f, 0x7f pair, and also
      // the two bytes that specify the number of bytes in the
      // ensemble. So we start by adding these four bytes.
      check_sum = (unsigned short int)byte1;
      check_sum += (unsigned short int)byte2;
      b1 = fgetc(fp);
      if (b1 == EOF)
	break;
      cindex++;
      check_sum += (unsigned short int)b1;
      b2 = fgetc(fp);
      if (b2 == EOF)
	break;
      cindex++;
      check_sum += (unsigned short int)b2;
      // Now we are ready to look at the rest of the bytes. Note that
      // our loop starts at index 4, because we have already handled
      // those 4 bytes of the ensemble (i.e. those 4 bytes are include
      // in the bytes_to_check value that we now calculate).
      bytes_to_check = (unsigned int)b1 + 256 * (unsigned int)b2;
      //if (SHOW(in_ensemble)) Rprintf("NEW cindex=%d in_ensemble=%d bytes_to_check=%d\n",
      // cindex, in_ensemble, bytes_to_check);
      if (bytes_to_check < 5) { // this will only happen in error; we check so bytes_to_read won't be crazy
	Free(ensembles);
	Free(years);
	Free(months);
	Free(days);
	Free(hours);
	Free(minutes);
	Free(seconds);
	Free(sec100s);
	Free(ebuf);
	error("cannot decode the length of ensemble number %d", in_ensemble);
      }
      unsigned int bytes_to_read = bytes_to_check - 4; // check_sum has used first 4 bytes already

      // Expand the ensemble buffer, ebuf, if need be.
      if (bytes_to_read > nebuf) {
	  Rprintf("increasing 'ebuf' buffer size from %d bytes to %d bytes\n", nebuf, bytes_to_read);
	  ebuf = (unsigned char *)Realloc(ebuf, bytes_to_read, unsigned char);
	  if (ebuf== NULL)
	    error("cannot enlarge the buffer used to store data within an individual adp/rdi ensemble; trying to enlarge to %d bytes", bytes_to_read);
	  nebuf = bytes_to_read;
      }
      // Read the bytes in one operation, because fgetc() is too slow.
      size_t tmp; // prevent compiler warnings with fread
      tmp = fread(ebuf, bytes_to_read, sizeof(unsigned char), fp);
      if (feof(fp)) {
	//Free(ensembles);
	//Free(ebuf);
	//Rprintf("NEW: end of file while reading ensemble number %d, at byte %d\n", in_ensemble+1, cindex);
	break;
      }
      cindex += bytes_to_read;
      for (int ib = 0; ib < bytes_to_read; ib++) {
	check_sum += (unsigned short int)ebuf[ib];
	//if (SHOW(in_ensemble)) Rprintf("NEW in_ensemble=%d ib=%d check_sum=%d\n", in_ensemble, ib, check_sum);
      }
      
      int cs1, cs2;
      cs1 = fgetc(fp);
      if (cs1 == EOF) break;
      cindex++;
      cs2 = fgetc(fp);
      if (cs2 == EOF) break;
      cindex++;
      desired_check_sum = ((unsigned short int)cs1) | ((unsigned short int)(cs2 << 8));
      //if (SHOW(in_ensemble)) Rprintf("NEW in_ensemble=%d icindex=%d check_sum %d desired_check_sum=%d b1=%d b2=%d bytes_to_check=%d\n",
      // in_ensemble, cindex, check_sum, desired_check_sum, b1, b2, bytes_to_check);
      if (check_sum == desired_check_sum) {
	// The check_sum is ok, so we can store the results, after
	// enlarging the buffers, if they are full.
	if (out_ensemble >= nensembles) {
	  // Enlarge the buffer
	  Rprintf("in_ensemble=%d : present  storage starts at 0x%x and can contain %d elements...\n", in_ensemble, ensembles, nensembles);
	  ensembles = (int *)Realloc(ensembles, 2*nensembles, int);
	  if (ensembles == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi ensembles; trying to enlarge to %d bytes", 2*nensembles);
	  bytes = (int *)Realloc(bytes, 2*nensembles, int);
	  if (bytes == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi bytes; trying to enlarge to %d bytes", 2*nensembles);
	  years = (unsigned char *)Realloc(years, 2*nensembles, unsigned char);
	  if (years == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi years; trying to enlarge to %d bytes", 2*nensembles);
	  months = (unsigned char *)Realloc(months, 2*nensembles, unsigned char);
	  if (months == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi months; trying to enlarge to %d bytes", 2*nensembles);
	  days = (unsigned char *)Realloc(days, 2*nensembles, unsigned char);
	  if (days == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi days; trying to enlarge to %d bytes", 2*nensembles);
	  hours = (unsigned char *)Realloc(hours, 2*nensembles, unsigned char);
	  if (hours == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi hours; trying to enlarge to %d bytes", 2*nensembles);
	  minutes = (unsigned char *)Realloc(minutes, 2*nensembles, unsigned char);
	  if (minutes == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi minutes; trying to enlarge to %d bytes", 2*nensembles);
	  seconds = (unsigned char *)Realloc(seconds, 2*nensembles, unsigned char);
	  if (seconds == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi seconds; trying to enlarge to %d bytes", 2*nensembles);
	  sec100s = (unsigned char *)Realloc(sec100s, 2*nensembles, unsigned char);
	  if (sec100s == NULL)
	    error("cannot enlarge the buffer used to store adp/rdi sec100s; trying to enlarge to %d bytes", 2*nensembles);
	  nensembles = 2 * nensembles;
	  //Rprintf("            : upgraded storage starts at 0x%x and can contain %d elements...\n", ensembles, nensembles);
	}
	if (from_value > 0 && in_ensemble >= (from_value-1)) { // the -1 is because R index starts at 1
	  ensembles[out_ensemble] = last_start;
	  bytes[out_ensemble] = 6 + bytes_to_read; // 2 for 0x7f, 2 for length, 2 for checksum
	  // profileStart <- ensembleStart + as.numeric(buf[ensembleStart[1]+8]) + 256*as.numeric(buf[ensembleStart[1]+9])
	  //unsigned int timePointer = (unsigned int)ebuf[7] + 256 * (unsigned int) ebuf[8];
	  unsigned int timePointer = (unsigned int)ebuf[4] + 256 * (unsigned int) ebuf[5];
	  //Rprintf("ebuf[1:4] %d %d %d %d, timePointer %d; ebuf[timePointer+5] %d\n",
	  //   (unsigned int) ebuf[0],
	  //   (unsigned int) ebuf[1],
	  //   (unsigned int) ebuf[2],
	  //   (unsigned int) ebuf[3],
	  //   (unsigned int) ebuf[4],
	  //   timePointer, (unsigned int)ebuf[timePointer+1]); 
	  years[out_ensemble] = ebuf[timePointer+0];
	  months[out_ensemble] = ebuf[timePointer+1];
	  days[out_ensemble] = ebuf[timePointer+2];
	  hours[out_ensemble] = ebuf[timePointer+3];
	  minutes[out_ensemble] = ebuf[timePointer+4];
	  seconds[out_ensemble] = ebuf[timePointer+5];
	  sec100s[out_ensemble] = ebuf[timePointer+6];
	  out_ensemble++;
	}
	in_ensemble++;
	// If 'max' is positive, check that we return only that many
	// ensemble pointers.
	if (to_value > 0 && out_ensemble > (to_value-from_value+1)) { // FIXME: seems to be off by 1
	  //Rprintf("breaking at out_ensemble=%d, in_ensemble=%d, from=%d, to=%d\n",
	  //    out_ensemble, in_ensemble, from_value, to_value);
	  break;
	}
      } // if it doesn't match the check_sum, we just ignore it as a coincidence of a 0x7f 0x7f pair
      R_CheckUserInterrupt(); // only check once per ensemble, for speed
      clast = c;
    }
    clast = c;
    c = fgetc(fp);
    if (c == EOF) break;
    cindex++;
  }
  fclose(fp);
  
  // Storage for ensembleStart, year, month, day, hour, second,
  // sec100, all returned in a list.
  SEXP es, by, year, month, day, hour, minute, second, sec100;
  PROTECT(es = NEW_INTEGER(out_ensemble)); // "es" = "ensembleStart"
  PROTECT(by = NEW_INTEGER(out_ensemble)); // "by" = "bytes"
  PROTECT(year = NEW_RAW(out_ensemble));
  PROTECT(month = NEW_RAW(out_ensemble));
  PROTECT(day = NEW_RAW(out_ensemble));
  PROTECT(hour = NEW_RAW(out_ensemble));
  PROTECT(minute = NEW_RAW(out_ensemble));
  PROTECT(second = NEW_RAW(out_ensemble));
  PROTECT(sec100 = NEW_RAW(out_ensemble));

  int *pes = INTEGER_POINTER(es);
  int *pby = INTEGER_POINTER(by);
  unsigned char *pyear = RAW_POINTER(year);
  unsigned char *pmonth = RAW_POINTER(month);
  unsigned char *pday = RAW_POINTER(day);
  unsigned char *phour = RAW_POINTER(hour);
  unsigned char *pminute = RAW_POINTER(minute);
  unsigned char *psecond = RAW_POINTER(second);
  unsigned char *psec100 = RAW_POINTER(sec100);

  for (long int i = 0; i < out_ensemble; i++) {
    pes[i] = ensembles[i];
    pby[i] = bytes[i];
    pyear[i] = years[i];
    pmonth[i] = months[i];
    pday[i] = days[i];
    phour[i] = hours[i];
    pminute[i] = minutes[i];
    psecond[i] = seconds[i];
    psec100[i] = sec100s[i];
  }
  //Rprintf("freeing ensembles\n");
  Free(ensembles);
  //Rprintf("freeing bytes\n");
  Free(bytes);
  //Rprintf("freeing years\n");
  Free(years);
  //Rprintf("freeing months\n");
  Free(months);
  //Rprintf("freeing days\n");
  Free(days);
  //Rprintf("fre;eing hours\n");
  Free(hours);
  //Rprintf("free;ing minutes\n");
  Free(minutes);
  //Rprintf("freei;ng seconds\n");
  Free(seconds);
  //Rprintf("freeing sec100s\n");
  Free(sec100s);
  //Rprintf("freeing ebuf\n");
  Free(ebuf);
  //Rprintf("freeing; -- all done\n");

  SEXP lres;
  SEXP lres_names;
  PROTECT(lres = allocVector(VECSXP, 9));
  PROTECT(lres_names = allocVector(STRSXP, 9));
  SET_VECTOR_ELT(lres, 0, es);
  SET_STRING_ELT(lres_names, 0, mkChar("ensembleStart"));
  SET_VECTOR_ELT(lres, 1, by);
  SET_STRING_ELT(lres_names, 1, mkChar("bytes"));
  SET_VECTOR_ELT(lres, 2, year);
  SET_STRING_ELT(lres_names, 2, mkChar("year"));
  SET_VECTOR_ELT(lres, 3, month);
  SET_STRING_ELT(lres_names, 3, mkChar("month"));
  SET_VECTOR_ELT(lres, 4, day);
  SET_STRING_ELT(lres_names, 4, mkChar("day"));
  SET_VECTOR_ELT(lres, 5, hour);
  SET_STRING_ELT(lres_names, 5, mkChar("hour"));
  SET_VECTOR_ELT(lres, 6, minute);
  SET_STRING_ELT(lres_names, 6, mkChar("minute"));
  SET_VECTOR_ELT(lres, 7, second);
  SET_STRING_ELT(lres_names, 7, mkChar("second"));
  SET_VECTOR_ELT(lres, 8, sec100);
  SET_STRING_ELT(lres_names, 8, mkChar("sec100"));
  setAttrib(lres, R_NamesSymbol, lres_names);
  UNPROTECT(13); 
  return(lres);
}

