/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
//#define SHOW(iensemble) (4 > abs((int)iensemble-29555))

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <time.h>

//#define DEBUG


/*

Locate Data Chunk for RDI

@description

Read an RDI adp file, finding the start indices of ensembles (or
"profiles", in other words).  Use `from` and `to` to extract a subset
of the file.  Setting `from=1` and `to=0` means to read the entire
file.

@details

In the name, "ldc" refers to "Locate Data Chunk", and "rdi" refers to
RD Instruments.

@section history:

A version of this function, used before October 2016, was passed a
buffer that contained the whole file contents. This scheme was dropped
because of limitations on the size of vectors in R, which is set by
the R use of 32-bit integers. (Note: 2^32-1 corresponds to a data file
of roughly 4.3Gb.)

@param filename character string indicating the name of an RDI adp
file.

@param from integer giving the index of the first ensemble (AKA
profile) to retrieve. The R notation is used, i.e. from=1 means the
first profile.

@param to integer giving the index of the last ensemble to retrieve.
As a special case, setting this to 0 will retrieve *all* the data
within the file.

@param by integer giving increment of the sequence, as for seq(), i.e.
a value of 1 means to retrieve all the profiles, while a value of 2
means to get every second profile.

@param mode integer, 0 if 'from' etc are profile numbers or 1 if they
are the numerical values of unix times.

@value a list containing "ensembleStart", "time", "sec100", and "outbuf",
all used in the calling R function, read.adp.rdi().
 
@examples

R CMD SHLIB ldc_rdi_in_file.c
f <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
dyn.load("src/ldc_rdi_in_file.so")
a <- .Call("ldc_rdi_in_file", f, 1, 0, 1) # whole file
b <- .Call("ldc_rdi_in_file", f, 1, 10, ) # first 10 ensembles
stopifnot(all.equal(length(a$ensembleStart), 79134))
stopifnot(all.equal(a$ensembleStart[1:10], b$ensembleStart))

@references

1. WorkHorse Commands and Output Data Format_Nov07.pdf

2. p124 of [1]: header structure (note that 'number of bytes in
ensemble' does *not* count the first 2 bytes; it's really an offset to
the checksum)

3. p158 (section 5.8) of [1] defines how to handle checksums

@author

Dan Kelley

*/

SEXP ldc_rdi_in_file(SEXP filename, SEXP from, SEXP to, SEXP by, SEXP mode)
{
  struct tm etime; // time of the ensemble under examination
  time_t ensemble_time = 0; // integer-ish form of the above (only calculated if mode=1)
  time_t ensemble_time_last = 0; // we use this for 'by', if mode is 1

  /*
     
# Test R code, used by developers whilst debugging:

system("R CMD SHLIB src/ldc_rdi_in_file.c")
f <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
dyn.load("src/ldc_rdi_2.so")
a <- .Call("ldc_rdi_2", f, 1, 0, 0)
b <- .Call("ldc_rdi_2", f, 1, 10, 0)
stopifnot(all.equal(length(a), 79134))
stopifnot(all.equal(a[1:10], b))
    
   */

  const char *filenamestring = CHAR(STRING_ELT(filename, 0));
  FILE *fp = fopen(filenamestring, "rb");
  if (!fp)
    error("cannot open file '%s'\n", filenamestring);
  PROTECT(from = AS_INTEGER(from));
  PROTECT(to = AS_INTEGER(to));
  PROTECT(by = AS_INTEGER(by));
  PROTECT(mode = AS_INTEGER(mode));

  int from_value = *INTEGER_POINTER(from);
  if (from_value < 0)
    error("'from' must be positive");
  int to_value = *INTEGER_POINTER(to);
  if (to_value < 0)
    error("'to' must be positive");
  int by_value = *INTEGER_POINTER(by);
  if (by_value < 0)
    error("'by' must be positive");
  int mode_value = *INTEGER_POINTER(mode);
  if (mode_value != 0 && mode_value != 1)
    error("'mode' must be 0 or 1");

  //Rprintf("from=%d, to=%d, by=%d, mode_value=%d\n", from_value, to_value, by_value, mode_value);

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

  // Growable buffers; see 'Realloc()' and 'Free()' calls later in the code.
  // Note that we do not check the Calloc() results because the R docs say that
  // Calloc() performs tests and R handles any problems.
  unsigned long int nensembles = 100000;
  int *ensembles = (int *)Calloc((size_t)nensembles, int);
  int *times = (int *)Calloc((size_t)nensembles, int);
  unsigned char *sec100s = (unsigned char *)Calloc((size_t)nensembles, unsigned char);
  unsigned long int nebuf = 5000;
  unsigned char *ebuf = (unsigned char *)Calloc((size_t)nebuf, unsigned char);
  
  // outbuf holds the output. It is growable
  unsigned long int iobuf = 0;
  unsigned long int nobuf = 100000;
  unsigned char *obuf = (unsigned char *)Calloc((size_t)nobuf, unsigned char);

  unsigned long int in_ensemble = 0, out_ensemble = 0;
  int b1, b2;
  long int last_start = 0;


  unsigned long int counter = 0, counter_last = 0;
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
	Free(times);
	Free(sec100s);
	Free(ebuf);
	Free(obuf);
	error("cannot decode the length of ensemble number %d", in_ensemble);
      }
      unsigned int bytes_to_read = bytes_to_check - 4; // check_sum has used first 4 bytes already

      // Expand the ensemble buffer, ebuf, if need be.
      if (bytes_to_read > nebuf) {
	  Rprintf("increasing 'ebuf' buffer size from %d bytes to %d bytes\n", nebuf, bytes_to_read);
	  ebuf = (unsigned char *)Realloc(ebuf, bytes_to_read, unsigned char);
	  nebuf = bytes_to_read;
      }
      // Read the bytes in one operation, because fgetc() is too slow.
      size_t tmp; // prevent compiler warnings with fread
      tmp = fread(ebuf, bytes_to_read, sizeof(unsigned char), fp);
      if (feof(fp)) {
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
	// The check_sum is ok, so we may want to store the results for
	// this profile.
	//
	// First, ensure that there will be sufficient storage to store results.
	// We do this before checking to see if we are actually going
	// to store the results, so possibly this might get done one
	// more time than required, before this function returns.
	if (out_ensemble >= nensembles) {
	  // Enlarge the buffer. We do not check the Realloc() result, because this
	  // is an R macro that is supposed to check for errors and handle them.
	  ensembles = (int *) Realloc(ensembles, 2*nensembles, int);
	  times = (int *) Realloc(times, 2*nensembles, int);
	  sec100s = (unsigned char *)Realloc(sec100s, 2*nensembles, unsigned char);
	  nensembles = 2 * nensembles;
	  //Rprintf("            : upgraded storage starts at 0x%x and can contain %d elements...\n", ensembles, nensembles);
	}
	// We will decide whether to keep this ensemble, based on ensemble
	// number, if mode_value==0 or on time, if mode_value==1. That
	// means we only need to compute a time if mode_value==1.
	unsigned int time_pointer = (unsigned int)ebuf[4] + 256 * (unsigned int) ebuf[5];
	etime.tm_year = 100 + (int) ebuf[time_pointer+0];
	etime.tm_mon = -1 + (int) ebuf[time_pointer+1];
	etime.tm_mday = (int) ebuf[time_pointer+2];
	etime.tm_hour = (int) ebuf[time_pointer+3];
	etime.tm_min = (int) ebuf[time_pointer+4];
	etime.tm_sec = (int) ebuf[time_pointer+5];
	etime.tm_isdst = 0;
	ensemble_time = timegm(&etime);
	//Rprintf(" estimet %d %s after_from=%d before_to=%d",
	//    ensemble_time, ctime(&ensemble_time),
	//    ensemble_time > from_value, ensemble_time < to_value);


	// See whether we are past the 'from' condition. Note the "-1"
	// for the ensemble case, because R starts counts at 1, not 0,
	// and the calling R code is (naturally) in R notation.
	if ((mode_value == 0 && in_ensemble >= (from_value-1)) ||
	    (mode_value == 1 && ensemble_time >= from_value)) {

	  // Handle the 'by' value.
	  //
	  // FIXME: best to have a 'last' variable and to count from
	  // FIXME: that, instead of using the '%' method'
	  if ((mode_value == 0 && (counter - counter_last) >= by_value) ||
	      (mode_value == 1 && (ensemble_time - ensemble_time_last) >= by_value)) {
	    // Expand the output buffer if needed.
	    if ((iobuf + 6 + bytes_to_read) >= nobuf) {
	      nobuf = 2 * nobuf;
	      // Rprintf("growing obuf (iobuf=%d, bytes_to_read=%d; new nobuf=%d)\n", 
	      //         iobuf, bytes_to_read, nobuf);
	      obuf = (unsigned char *)Realloc(obuf, nobuf, unsigned char);
	    }
	    // Copy ensemble to output buffer, after 6 bytes of header
	    //Rprintf("starting outbuf chunk at iobuf=%d, value 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x\n",
		//iobuf, byte1, byte2, b1, b2, cs1, cs2);

	    ensembles[out_ensemble] = iobuf + 1; // the +1 puts in R notation
	    times[out_ensemble] = ensemble_time;
	    obuf[iobuf++] = byte1; // 0x7f
	    obuf[iobuf++] = byte2; // 0x7f
	    obuf[iobuf++] = b1; // this and b2 yield bytes_to_read
	    obuf[iobuf++] = b2;
	    //obuf[iobuf++] = cs1; // this and cs2 form the checksum
	    //obuf[iobuf++] = cs2;
	    for (unsigned int i = 0; i < 6 + bytes_to_read; i++) { // FIXME: 4 here, or 6, or maybe 2???
	      obuf[iobuf++] = ebuf[i];
	    }
	    //Rprintf("AFTER saving, iobuf=%d, nobuf=%d, bytes_to_read=%d\n", iobuf, nobuf, bytes_to_read);
	    // Increment counter (can be of two types)
	    if (mode_value == 1) {
	      ensemble_time_last = ensemble_time;
	    } else {
	      counter_last = counter;
	    }
	    //Rprintf("saving at in_ensemble=%d, counter=%d, by=%d\n", in_ensemble, counter, by_value);
	    //	    ensembles[out_ensemble] = last_start;
	    unsigned int timePointer = (unsigned int)ebuf[4] + 256 * (unsigned int) ebuf[5];
	    sec100s[out_ensemble] = ebuf[timePointer+6];
	    out_ensemble++;
	  } else {
	    //Rprintf("skipping at in_ensemble=%d, counter=%d, by=%d\n", in_ensemble, counter, by_value);
	  }
	  counter++;
	}
	in_ensemble++;
	// If 'max' is positive, check that we return only that many
	// ensemble pointers.
	if ((mode_value == 0 && (to_value > 0 && in_ensemble > to_value)) ||
	    (mode_value == 1 && (ensemble_time > to_value))) {
	  //Rprintf("breaking at out_ensemble=%d, in_ensemble=%d, from=%d, to=%d, ensemble_time=%d, mode_value=%d\n",
	  //    out_ensemble, in_ensemble, from_value, to_value, ensemble_time, mode_value);
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
  
  // We will not return the whole buffers, but only the fraction that
  // stores data.
  SEXP ensemble;
  PROTECT(ensemble = NEW_INTEGER(out_ensemble));
  SEXP time;
  PROTECT(time = NEW_INTEGER(out_ensemble));
  SEXP sec100;
  PROTECT(sec100 = NEW_RAW(out_ensemble));

  SEXP outbuf;
  PROTECT(outbuf = NEW_RAW(iobuf)); // FIXME: check ok

  int *pensemble = INTEGER_POINTER(ensemble);
  unsigned char *psec100 = RAW_POINTER(sec100);
  int *ptime = INTEGER_POINTER(time);
  unsigned char *poutbuf = RAW_POINTER(outbuf);

  for (long int i = 0; i < out_ensemble; i++) {
    pensemble[i] = ensembles[i];
    ptime[i] = times[i];
    psec100[i] = sec100s[i];
  }
  for (long int i = 0; i < iobuf; i++) {
    poutbuf[i] = obuf[i];
  }
  Free(ensembles);
  Free(times);
  Free(sec100s);
  Free(ebuf);
  Free(obuf);

  SEXP lres;
  SEXP lres_names;
  PROTECT(lres = allocVector(VECSXP, 4));
  PROTECT(lres_names = allocVector(STRSXP, 4));
  int i = 0;
  SET_VECTOR_ELT(lres, i, ensemble);
  SET_STRING_ELT(lres_names, i, mkChar("ensembleStart"));
  SET_VECTOR_ELT(lres, ++i, time);
  SET_STRING_ELT(lres_names, i, mkChar("time"));
  SET_VECTOR_ELT(lres, ++i, sec100);
  SET_STRING_ELT(lres_names, i, mkChar("sec100"));
  SET_VECTOR_ELT(lres, ++i, outbuf);
  SET_STRING_ELT(lres_names, i, mkChar("outbuf"));
  setAttrib(lres, R_NamesSymbol, lres_names);
  UNPROTECT(10); 
  return(lres);
}

