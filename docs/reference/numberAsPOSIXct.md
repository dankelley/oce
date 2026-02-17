# Convert a Numeric Time to a POSIXct Time

This converts numerical values into POSIXct times. There are many
schemes for doing this, with the `type` parameter being used to select
between them. See “Details” for a listing, broken down by scheme.

## Usage

``` r
numberAsPOSIXct(t, type = "unix", tz = "UTC", leap = TRUE)
```

## Arguments

- t:

  an integer corresponding to a time, in a way that depends on `type`.

- type:

  character value indicating the time type. The permitted values are
  `"argo"`, `"epic"`, `"excel"`, `"gps"`, `"matlab"`, `"ncep1"`,
  `"ncep2"`, `"sas"`, `"spss"`, `"unix"`, and `"yearday"`, the first of
  these being the default.

- tz:

  a string indicating the time zone, by default `"UTC"`.

- leap:

  a logical value, TRUE by default, that applies only if `type` is
  `"gps"`. If `leap` is TRUE, then the built-in dataset named
  [.leap.seconds](https://rdrr.io/r/base/DateTimeClasses.html) is
  consulted to find of the number of leap seconds between 1980 (when the
  GPS program started) and the time computed from the other parameters,
  and the return value is decreased accordingly (see Example 3).

## Value

A [`POSIXct()`](https://rdrr.io/r/base/DateTimeClasses.html) time
vector.

## Details

The possible choices for `type` are as listed below.

- `"unix"` handles Unix times, measured in seconds since the start of
  the year 1970.

- `"matlab"` handles Matlab times, measured in days since what MathWorks
  (reference 1) calls “January 0, 0000” (i.e.
  `ISOdatetime(0, 1, 1, 0, 0, 0)` in R notation).

- `"gps"` handles the Global Positioning System convention. The scheme
  is complicated, owing to hardware limitations of GPS satellites. As
  illustrated in Example 3, `t` may be a matrix with either 2 or 3
  columns. In the 2-column format, the first column holds the number of
  weeks after 1999-08-22, modulo 1024 (approximately 19.6 years), and
  the second column (here and also in the 3-column format) holds the
  number of seconds in the referenced week, with leap seconds being
  handled with the `leap` parameter. The modulo calculation is required
  because GPS satellites dedicate only 10 bits to the week number. The
  resultant ambiguity (e.g. a rollover in 2019-04-07) is addressed in
  the 3-column format, in which the last column holds the number of
  1024-week rollover events since 1980-01-06. Users should set this
  column to 0 for times prior to 1999-08-22, to 1 for later times prior
  to 2019-04-07, to 2 for later times prior to 2038-11-21, etc. However,
  there will be an exception to this rule, when satellites start
  dedicating 12 bits to the week value. For such data, the third column
  will need to be 0 for all times prior to 2137-01-06.

- `"argo"` handles Argo times, measured in days since the start of the
  year 1900.

- `"excel"` handles Excel times, measured in days since the start of the
  year 1900. (Note that excel incorrectly regards 1900 as a leap year,
  so 1 day is subtracted from `t` unless the time is less than or equal
  to 1900 Feb 28. Note that NA is returned for the day 60, which is what
  excel codes for "Feb 29, 1900", the non-existing day that excel
  accepts.

- `"ncep1"` handles NCEP times, measured in hours since the start of the
  year 1800.

- `"ncep2"` handles NCEP times, measured in days since the start of the
  year 1. (Note that, for reasons that are unknown at this time, a
  simple R expression of this definition is out by two days compared
  with the UDUNITS library, which is used by NCEP. Therefore, a two-day
  offset is applied. See references 2 and 3.)

- `"sas"` handles SAS times, indicated by `type="sas"`, have origin at
  the start of 1960.

- `"spss"` handles SPSS times, in seconds after 1582-10-14.

- `"yearday"` handles a convention in which `t` is a two-column matrix,
  with the first column being the year, and the second the yearday
  (starting at 1 for the first second of January 1, to match the
  convention used by Sea-Bird CTD software).

- `"epic"` handles a convention used in the EPIC software library, from
  the Pacific Marine Environmental Laboratory, in which `t` is a
  two-column matrix, with the first column being the julian Day (as
  defined in
  [`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
  for example), and with the second column being the millisecond within
  that day. See reference 4.

- `"vms"` handles a convention used in the VMS operating system and for
  Modified Julian Day, in which `t` is the number of seconds past
  1859-11-17T00:00:00 UTC. See reference 5.

## References

1.  Matlab times:
    `https://www.mathworks.com/help/matlab/ref/datenum.html`

2.  NCEP times: `https://psl.noaa.gov/data/gridded/faq.html`

3.  Problem with NCEP times:
    `https://github.com/dankelley/oce/issues/738`

4.  EPIC times: software and manuals at
    `https://www.pmel.noaa.gov/epic/download/index.html#epslib`; see
    also Denbo, Donald W., and Nancy N. Soreide. “EPIC.” Oceanography 9
    (1996).
    [doi:10.5670/oceanog.1996.10](https://doi.org/10.5670/oceanog.1996.10)

5.  VMS times: https://en.wikipedia.org/wiki/Epoch\_(computing)

6.  GPS times: https://www.labsat.co.uk/index.php/en/gps-time-calculator

## See also

Other things related to time:
[`ctimeToSeconds()`](https://dankelley.github.io/oce/reference/ctimeToSeconds.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`numberAsHMS()`](https://dankelley.github.io/oce/reference/numberAsHMS.md),
[`secondsToCtime()`](https://dankelley.github.io/oce/reference/secondsToCtime.md),
[`unabbreviateYear()`](https://dankelley.github.io/oce/reference/unabbreviateYear.md)

Other things related to time:
[`ctimeToSeconds()`](https://dankelley.github.io/oce/reference/ctimeToSeconds.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`numberAsHMS()`](https://dankelley.github.io/oce/reference/numberAsHMS.md),
[`secondsToCtime()`](https://dankelley.github.io/oce/reference/secondsToCtime.md),
[`unabbreviateYear()`](https://dankelley.github.io/oce/reference/unabbreviateYear.md)

## Author

Dan Kelley

## Examples

``` r
# Example 1. default (unix)
numberAsPOSIXct(0)
#> [1] "1970-01-01 UTC"

# Example 2. Matlab
numberAsPOSIXct(1, type = "matlab")
#> [1] "0000-01-01 UTC"

# Example 3. GPS with default week rollover or with no rollover (Canada Day, year 2010)
numberAsPOSIXct(cbind(566, 345615), type = "gps")
#> [1] "2010-07-01 UTC"
numberAsPOSIXct(cbind(566, 345615, 1), type = "gps")
#> [1] "2010-07-01 UTC"
numberAsPOSIXct(cbind(1024 + 566, 345615, 0), type = "gps")
#> [1] "2010-07-01 UTC"
# Show how to deal with leap seconds (15 of them, in this case)
sum(as.POSIXct("1980-01-01") < .leap.seconds & .leap.seconds <= as.POSIXct("2010-07-01"))
#> [1] 15
-15 + numberAsPOSIXct(cbind(1024 + 566, 345615, 0), type = "gps", leap = FALSE)
#> [1] "2010-07-01 UTC"

# Example 4. yearday
numberAsPOSIXct(cbind(2013, 1), type = "yearday") # start of 2013
#> [1] "2013-01-01 UTC"

# Example 5. Epic time, one hour into Canada Day of year 2018. In computing the
# Julian day, note that this starts at noon.
jd <- julianDay(as.POSIXct("2018-07-01 12:00:00", tz = "UTC"))
numberAsPOSIXct(cbind(jd, 1e3 * 1 * 3600), type = "epic", tz = "UTC")
#> [1] "2018-07-01 01:00:00 UTC"

# Example 6. Julian day, note that this starts at noon.
jd <- julianDay(as.POSIXct("2018-07-01 12:00:00", tz = "UTC"))
numberAsPOSIXct(cbind(jd, 1e3 * 1 * 3600), type = "epic", tz = "UTC")
#> [1] "2018-07-01 01:00:00 UTC"
```
