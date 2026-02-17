# Parse a Latitude or Longitude String

Parse a latitude or longitude string, e.g. as in the header of a CTD
file The following formats are understood (for, e.g. latitude):

    ** NMEA Latitude = 47 54.760 N
    ** Latitude: 47 53.27 N

Note that [`iconv()`](https://rdrr.io/r/base/iconv.html) is called to
convert the string to ASCII before decoding, to change any degree (or
other non-ASCII) symbols to blanks.

## Usage

``` r
parseLatLon(line, debug = getOption("oceDebug"))
```

## Arguments

- line:

  a character string containing an indication of latitude or longitude.

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

## Value

A numerical value of latitude or longitude.

## See also

Used by
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md).

## Author

Dan Kelley
