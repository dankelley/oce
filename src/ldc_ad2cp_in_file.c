/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <time.h>

#define debug 1

// The next items are specific to ad2cp, as is the whole
// format, but I want to define these here to make the
// code clearer. See [1 sec 6.1].
#define SYNC 0xA5
#define HEADER_SIZE 10
#define FAMILY 0x10

//#define DEBUG


/*

   Locate (header+data) for Nortek ad2cp

   @param filename character string indicating the file name.

   @param from integer giving the index of the first ensemble (AKA
   profile) to retrieve. The R notation is used, i.e. from=1 means the
   first profile.

   @param to integer giving the index of the last ensemble to retrieve.
   As a special case, setting this to 0 will retrieve *all* the data
   within the file.

   @param by integer giving increment of the sequence, as for seq(), i.e.
   a value of 1 means to retrieve all the profiles, while a value of 2
   means to get every second profile.

   @value a list containing 'index', 'type', 'length'.

   @examples

   system("R CMD SHLIB ldc_ad2cp_in_file.c")
   f <- "/Users/kelley/Dropbox/oce_ad2cp/labtestsig3.ad2cp"
   dyn.load("ldc_ad2cp_in_file.so")
   a <- .Call("ldc_ad2cp_in_file", f, 1, 0, 1)

@section: notes

Table 6.1 (header definition):

+--------------------------------------------------------------------------------------------
|Sync	         | 8 bits             | Always 0xA5
+----------------|--------------------|------------------------------------------------------
|Header Size     | 8 bits (unsigned)  | Size (number of bytes) of the Header.
+----------------|--------------------|------------------------------------------------------
|ID              | 8 bits             | Defines type of the following Data Record
|                |                    | 0x16 – Average Data Record.
|                |                    | 0x17 – Bottom Track Data Record.
|                |                    | 0x18 – Interleaved Burst Data Record (beam 5).
|                |                    | 0xA0 - String Data Record, eg. GPS NMEA data,
|                |                    |        comment from the FWRITE command.
+----------------|--------------------|------------------------------------------------------
|Family          | 8 bits             | Defines the Instrument Family. 0x10 – AD2CP Family
+----------------|--------------------|------------------------------------------------------
|Data Size       | 16 bits (unsigned) | Size (number of bytes) of the following Data Record.
+----------------|--------------------|------------------------------------------------------
|Data Checksum   | 16 bits            | Checksum of the following Data Record.
+----------------|--------------------|------------------------------------------------------
|Header Checksum | 16 bits            | Checksum of all fields of the Header
|                |                    | (excepts the Header Checksum itself).
+-------------------------------------_------------------------------------------------------

@references

1. "Integrators Guide AD2CP_A.pdf", provided to me privately by
(person 1) in early April of 2017.

@author

Dan Kelley

*/

// Check the header checksum.
//
// The code for this differs from that suggested by Nortek,
// because we don't use a specific (msoft) compiler, 
// which evidently provides misaligned_load16().
unsigned short cs(unsigned char *data, unsigned short size)
{
  //unsigned short checksum = 0xB58C;
  unsigned short checksum = (unsigned short)0xB5 + 256*(unsigned short)0x8C;
  //Rprintf("checksum %d (initial)\n", checksum);
  if (2 * (size / 2) != size)
    error("HEADER_SIZE should be an even number but it is %d\n", size);
  for (int i = 0; i < size; i += 2) {
    checksum += (unsigned short)data[i] + 256*(unsigned short)data[i+1];
    //Rprintf("checksum=%d (at i=%d)\n", checksum, i);
  }
  //Rprintf("checksum=%d (final)\n", checksum);
  //FIXME: use an extra byte (can be, with data)
  return(checksum);
}
unsigned short cs2(unsigned char *data, unsigned short size)
{
  //unsigned short checksum = 0xB58C;
  unsigned short checksum = 256*(unsigned short)0xB5 + (unsigned short)0x8C;
  //Rprintf("checksum %d (initial)\n", checksum);
  if (2 * (size / 2) != size)
    error("HEADER_SIZE should be an even number but it is %d\n", size);
  for (int i = 0; i < size; i += 2) {
    checksum += 256*(unsigned short)data[i] + (unsigned short)data[i+1];
    //Rprintf("checksum=%d (at i=%d)\n", checksum, i);
  }
  //Rprintf("checksum=%d (final)\n", checksum);
  //FIXME: use an extra byte (can be, with data)
  return(checksum);
}



SEXP ldc_ad2cp_in_file(SEXP filename, SEXP from, SEXP to, SEXP by)
{
  const char *filenamestring = CHAR(STRING_ELT(filename, 0));
  FILE *fp = fopen(filenamestring, "rb");
  if (!fp)
    error("cannot open file '%s'\n", filenamestring);
  PROTECT(from = AS_INTEGER(from));
  PROTECT(to = AS_INTEGER(to));
  PROTECT(by = AS_INTEGER(by));

  int from_value = *INTEGER_POINTER(from);
  if (from_value < 0)
    error("'from' must be positive but it is %d", from_value);
  int to_value = *INTEGER_POINTER(to);
  if (to_value < 0)
    error("'to' must be positive but it is %d", to_value);
  int by_value = *INTEGER_POINTER(by);
  if (by_value < 0)
    error("'by' must be positive but it is %d", by_value);
  if (debug > 1) Rprintf("from=%d, to=%d, by=%d\n", from_value, to_value, by_value);

  int n=10;

  int c;
  // 305988694 from C
  // 305988694 from R
  fseek(fp, 0L, SEEK_END);
  unsigned long int fileSize = ftell(fp);
  fseek(fp, 0L, SEEK_SET);
  if (debug > 3) Rprintf("fileSize=%d\n", fileSize);
  unsigned long int chunk = 0;
  unsigned long int cindex = 0;

  // Ensure that the first byte we point to equals SYNC.
  // In a conentional file, starting with a SYNC char, this
  // just gets a byte and puts it back, leaving cindex=0.
  // But if the file does not start with a SYNC char, this works
  // along the file until it finds one, and adjusts cindex
  // appropriately.
  while (1) {
    c = getc(fp);
    if (EOF == c) {
      error("this file does not contain a single 0x", SYNC, " byte");
      break;
    }
    if (debug > 2) Rprintf("< c=0x%x\n", c);
    if (SYNC == c) {
      fseek(fp, -1, SEEK_CUR);
      break;
    }
    cindex++;
  }

  Rprintf("%6s %6s %6s %6s %6s %6s\n", 
      "cindex", "chunk", "id", "dsize", "dcs", "hcs");
  // The table in [1 sec 6.1] says header pieces are 10 bytes
  // long, so once we get an 0xA5, we'll try to get 9 more bytes.
  unsigned char hbuf[HEADER_SIZE]; // header buffer
  unsigned int dbuflen = 10; // may be increased later
  unsigned char *dbuf = (unsigned char *)Calloc((size_t)dbuflen, unsigned char);
  while (chunk < 1) {// FIXME: use whole file here
    int ID, dataSize, dataChecksum, headerChecksum;
    size_t bytes_read;
    bytes_read = fread(hbuf, 1, HEADER_SIZE, fp);
    if (bytes_read != HEADER_SIZE) {
      Rprintf("malformed header at cindex=%d", cindex);
      break; // FIXME: maybe we should stop here, actually
    }
    cindex += bytes_read;
    if (debug > 1) 
      Rprintf("buf: 0x%02x 0x%02x 0x%02x 0x%02x 0x%02x 0x%02x 0x%02x 0x%02x 0x%02x 0x%02x\n", 
	  hbuf[0], hbuf[1], hbuf[2], hbuf[3], hbuf[4],
	  hbuf[5], hbuf[6], hbuf[7], hbuf[8], hbuf[9]); 
    // It's prudent to check.
    if (hbuf[0] != SYNC) {
      error("coding error in reading the header at cindex=%d; expecting 0x%x but found 0x%x\n",
	  cindex, SYNC, hbuf[0]);
    }
    // Rprintf("\t*%d %d %d %d %d %d %d %d %d |", 
    //     buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7], buf[8], buf[9]); 
    // Rprintf("headerSize=%d ID=%d family=%d\n", buf[0], buf[1], buf[2]);

    // Check that it's an actual header
    if (hbuf[1] == HEADER_SIZE && hbuf[3] == FAMILY) {
      chunk++;
      ID = (int)hbuf[2];
      dataSize = hbuf[4] + 256L * hbuf[5];
      //Rprintf("\n\tdataSize=%5d ", dataSize);
      dataChecksum = (unsigned short)hbuf[6] + 256*(unsigned short)hbuf[7];
      //Rprintf(" dataChecksum=%5d", dataChecksum);
      headerChecksum = (unsigned short)hbuf[8] + 256*(unsigned short)hbuf[9];

      // Check the header checksum.
      // try two different ways
      unsigned short hbufcs = cs(hbuf, HEADER_SIZE);
      Rprintf("\t\t\t\t\thbufcs %d\n", hbufcs);
      Rprintf("\t\t\t\t\tdesired hchecksum %d (?)\n", (unsigned short)hbuf[8] + 256*(unsigned short)hbuf[9]);
      // data cs
      unsigned short dbufcs = cs(dbuf, dataSize);
      Rprintf("\t\t\t\t\tdbufcs %d\n", dbufcs);
      unsigned short dbufcs2 = cs2(dbuf, dataSize);
      Rprintf("\t\t\t\t\tdbufcs2 %d\n", dbufcs2);
      Rprintf("\t\t\t\t\tdesired dchecksum %d (?)\n", (unsigned short)hbuf[6] + 256*(unsigned short)hbuf[7]);
      Rprintf("\t\t\t\t\tdesired dchecksum %d (?)\n", 256*(unsigned short)hbuf[6] + (unsigned short)hbuf[7]);
      //Rprintf(" headerChecksum=%5d\n", headerChecksum);
      Rprintf("%6d %6d %6d %6d %6d %6d\n", cindex, chunk, ID, dataSize, dataChecksum, headerChecksum);
      if (dataSize > dbuflen) { // expand the buffer if required
	dbuflen = dataSize;
	dbuf = (unsigned char *)Realloc(dbuf, dbuflen, unsigned char);
	if (debug > 1) Rprintf("\n *** increased dbuflen to %d\n", dbuflen);
      }
      if (debug > 2) Rprintf("about to read data; cindex=%d now\n", cindex);
      bytes_read = fread(dbuf, 1, dataSize, fp);
      if (bytes_read != dataSize)
	error("ran out of file on data chunk near cindex=%d; wanted %d bytes but got only %d\n",
	    cindex, dataSize, bytes_read);
      cindex += dataSize;
      if (debug > 2) Rprintf("got data; cindex=%d now\n", cindex);
    }
  }
  Rprintf("\nlast chunk=%d; cindex=%d\n", chunk, cindex);

  SEXP res;
  PROTECT(res = NEW_INTEGER(n));
  int *pres = INTEGER_POINTER(res);
  for (int i = 0; i < n; i++) {
    pres[i] = i;
  }
  Rprintf("B in ldc_ad2cp.c\n");
  UNPROTECT(4);
  Rprintf("C in ldc_ad2cp.c\n");
  return(res);
}
#if 0
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

int mode_value = *INTEGER_POINTER(mode);
if (mode_value != 0 && mode_value != 1)
  error("'mode' must be 0 or 1");

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


  unsigned long int counter = 0, counter_last = 0;
  while (1) {
    c = fgetc(fp);
    if (c == EOF) break;
    cindex++;
    // Try to locate "ensemble starts", spots where a 0x7f is followed by a second 0x7f,
    // then followed by data that match a checksum.
    if (clast == byte1 && c == byte2) {
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
      fread(ebuf, bytes_to_read, sizeof(unsigned char), fp);
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
	// below should work even with windows
	ensemble_time = oce_timegm(&etime);
	//Rprintf("C %d\n", ensemble_time);
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
#endif

