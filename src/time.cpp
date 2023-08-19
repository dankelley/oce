/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <Rcpp.h>
using namespace Rcpp;

//#define DEBUG

// Convert Epic time (julian day along with millisecond in that day)
// into year, month, day, hour, second.
#define JULGREG   2299161

// [[Rcpp::export]]
List do_epic_time_to_ymdhms(IntegerVector julianDay, IntegerVector millisecond)
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
  return(List::create(
    Named("year")=year,
    Named("month")=month,
    Named("day")=day,
    Named("hour")=hour,
    Named("minute")=minute,
    Named("second")=second));
}
