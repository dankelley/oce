/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <Rcpp.h>
using namespace Rcpp;
//#define DEBUG

// Convert Epic time (julian day along with millisecond in that day)
// into year, month, day, hour, second.
/*

   library(oce)
   library(Rcpp)
   sourceCpp("time.cpp")
   now <- Sys.time()
   julianDay <- as.integer(julianDay(now))     # julian day 
   sec <- 3600                            # seconds (made into milliseconds below)
   sec <- seq(0, 86400, 3600/2)
   julianDay <- rep(julianDay, length(sec))
   millisecond <- 1000 * sec
   res <- epic_time_to_ymdhms(julianDay, millisecond)
   t <- ISOdatetime(res$year, res$month, res$day, res$hour, res$minute, res$second, tz="UTC")
   x <- seq_along(t)
   oce.plot.ts(t, x, type='p')

*/

#define JULGREG   2299161

// [[Rcpp::export]]
List epic_time_to_ymdhms(IntegerVector julianDay, IntegerVector millisecond)
{
  int n = julianDay.size(); // The R code ensures length matches that of millisecond
  IntegerVector year(n), month(n), day(n), hour(n), minute(n);
  NumericVector second(n);

  long int ja, jalpha, jb, jc, jd, je;
  for (int i = 0; i < n; i++) {
#ifdef DEBUG
    Rprintf("julianDay[%d]=%d, milliscond[%d]=%d\n", i, julianDay[i], i, millisecond[i]);
#endif
    while(millisecond[i] >= 86400000) { /* increament days if ms larger then one day */
      julianDay[i] = julianDay[i] + 1;
      millisecond[i] = millisecond[i] - 86400000;
#ifdef DEBUG
      Rprintf("Altered to julianDay[i]=%d millisecond[i]=%d\n", julianDay[i], millisecond[i]);
#endif
    }
    if(julianDay[i] >= JULGREG) {
      jalpha = (long)(((double) (julianDay[i] - 1867216) - 0.25) / 36524.25);
      ja = julianDay[i] + 1 + jalpha - (long)(0.25*jalpha);
    } else {
      ja = julianDay[i];
    }
    jb = ja+1524;
    jc = (long)(6680.0+((double)(jb-2439870)-122.1)/365.25);
    jd = (long)(365*jc + (0.25*jc));
    je = (long)((jb - jd)/30.6001);
    day[i] = jb - jd - (long int)(30.6001*je);
    month[i] = je - 1;
    if(month[i] > 12)
      month[i] -= 12;
    year[i] = jc - 4715;
    if(month[i] > 2)
      year[i] = year[i] - 1;
    if(year[i] <= 0)
      year[i] = year[i] - 1;
    ja = millisecond[i] / 1000;
    hour[i] = ja / 3600;
    minute[i] = (ja - (hour[i]) * 3600) / 60;
    second[i] = (double)(millisecond[i] - ((hour[i])*3600 + (minute[i])*60)*1000)/1000.0;
  }
  return(List::create(Named("year")=year,
        Named("month")=month,
        Named("day")=day,
        Named("hour")=hour,
        Named("minute")=minute,
        Named("second")=second));
}
#if 0
library(oce)
## next two lines would be taken care of within oce
  system("R CMD shlib ep.c")
  dyn.load("ep.so")

## construct fake data, half-hourly on the present
now <- Sys.time()
  jday <- as.integer(julianDay(now))     # julian day 
  sec <- 3600                            # seconds (made into milliseconds below)
  sec <- seq(0, 86400, 3600/2)
jday <- rep(jday, length(sec))

  n <- length(jday)
if (length(sec) != n)
  stop("bad setup; lengths of jday and sec must match but they are ", n, " and ", length(sec))
  res <- .C("ep_time_to_mdyhms",
      as.integer(n),
      jday=as.integer(jday), ms=as.integer(1000*sec),
      month=integer(n), day=integer(n), year=integer(n),
      hour=integer(n), minute=integer(n), second=double(n),
      NAOK=TRUE)
  t <- ISOdatetime(res$year, res$month, res$day, res$hour, res$minute, res$second, tz="UTC")
x <- seq_along(t)
  oce.plot.ts(t, x, type='b')

#endif
#if 0
  // https://www.pmel.noaa.gov/epic/eps-manual/epslib_toc.html#SEC47
  // ftp://ftp.pmel.noaa.gov/epic//epic_src.tar.Z
  // epic_src/eps/fil_time.c
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <math.h>
#define JULGREG   2299161

  //#define DEBUG

void ep_time_to_mdyhms(int *n,
    int *jday, int *ms,
    int *mon, int *day, int *yr, int *hour, int *min, double *sec)
{
  for (int i = 0; i < *n; i++) {
    long int ja, jalpha, jb, jc, jd, je;
#ifdef DEBUG
    Rprintf("in C, jday[%d]=%d ms[%d]=%d\n", i, jday[i], i, ms[i]);
#endif

    while(ms[i] >= 86400000) { /* increament days if ms larger then one day */
      jday[i] = jday[i] + 1;
      ms[i] = ms[i] - 86400000;
#ifdef DEBUG
      Rprintf("Altered to jday[i]=%d ms[i]=%d\n", jday[i], ms[i]);
#endif
    }

    if(jday[i] >= JULGREG) {
      jalpha=(long)(((double) (jday[i]-1867216)-0.25)/36524.25);
      ja=jday[i]+1+jalpha-(long)(0.25*jalpha);
    } else {
      ja=jday[i];
    }
    jb=ja+1524;
    jc=(long)(6680.0+((double)(jb-2439870)-122.1)/365.25);
    jd=(long)(365*jc+(0.25*jc));
    je=(long)((jb-jd)/30.6001);
    day[i] = jb-jd-(long int)(30.6001*je);
    mon[i] = je - 1;
    if(mon[i] > 12)
      mon[i] -= 12;
    yr[i] = jc-4715;
    if(mon[i] > 2)
      yr[i] = yr[i] - 1;
    if(yr[i] <=0)
      yr[i] = yr[i] - 1;
    ja = ms[i] / 1000;
    hour[i] = ja / 3600;
    min[i] = (ja - (hour[i]) * 3600) / 60;
    sec[i] = (double)(ms[i] - ((hour[i])*3600 + (min[i])*60)*1000)/1000.0;
  }
}


#endif

