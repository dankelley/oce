# Oce Version of as.POSIXct

Each format in `timeFormats` is used in turn as the `format` argument to
[`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html), and the first
that produces a non-`NA` result is used. If `timeFormats` is missing,
the following formats are tried, in the stated order:

## Usage

``` r
decodeTime(time, timeFormats, tz = "UTC")
```

## Arguments

- time:

  Character string with an indication of the time.

- timeFormats:

  Optional vector of time formats to use, as for
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html).

- tz:

  Time zone.

## Value

A time as returned by
[`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html).

## Details

- `"\%b \%d \%Y \%H:\%M:\%S"` (e.g. `"Jul 1 2013 01:02:03"`)

- `"\%b \%d \%Y"` (e.g. `"Jul 1 2013"`)

- `"\%B \%d \%Y \%H:\%M:\%S"` (e.g. `"July 1 2013 01:02:03"`)

- `"\%B \%d \%Y"` (e.g. `"July 1 2013"`)

- `"\%d \%b \%Y \%H:\%M:\%S"` (e.g. `"1 Jul 2013 01:02:03"`)

- `"\%d \%b \%Y"` (e.g. `"1 Jul 2013"`)

- `"\%d \%B \%Y \%H:\%M:\%S"` (e.g. `"1 July 2013 01:02:03"`)

- `"\%d \%B \%Y"` (e.g. `"1 July 2013"`)

- `"\%Y-\%m-\%d \%H:\%M:\%S"` (e.g. `"2013-07-01 01:02:03"`)

- `"\%Y-\%m-\%d"` (e.g. `"2013-07-01"`)

- `"\%Y-\%b-\%d \%H:\%M:\%S"` (e.g. `"2013-July-01 01:02:03"`)

- `"\%Y-\%b-\%d"` (e.g. `"2013-Jul-01"`)

- `"\%Y-\%B-\%d \%H:\%M:\%S"` (e.g. `"2013-July-01 01:02:03"`)

- `"\%Y-\%B-\%d"` (e.g. `"2013-July-01"`)

- `"\%d-\%b-\%Y \%H:\%M:\%S"` (e.g. `"01-Jul-2013 01:02:03"`)

- `"\%d-\%b-\%Y"` (e.g. `"01-Jul-2013"`)

- `"\%d-\%B-\%Y \%H:\%M:\%S"` (e.g. `"01-July-2013 01:02:03"`)

- `"\%d-\%B-\%Y"` (e.g. `"01-July-2013"`)

- `"\%Y/\%b/\%d \%H:\%M:\%S"` (e.g. `"2013/Jul/01 01:02:03"`)

- `"\%Y/\%b/\%d"` (e.g. `"2013/Jul/01"`)

- `"\%Y/\%B/\%d \%H:\%M:\%S"` (e.g. `"2013/July/01 01:02:03"`)

- `"\%Y/\%B/\%d"` (e.g. `"2013/July/01"`)

- `"\%Y/\%m/\%d \%H:\%M:\%S"` (e.g. `"2013/07/01 01:02:03"`)

- `"\%Y/\%m/\%d"` (e.g. `"2013/07/01"`)

## Author

Dan Kelley

## Examples

``` r
decodeTime("July 1 2013 01:02:03")
#> [1] "2013-07-01 01:02:03 UTC"
decodeTime("Jul 1 2013 01:02:03")
#> [1] "2013-07-01 01:02:03 UTC"
decodeTime("1 July 2013 01:02:03")
#> [1] "2013-07-01 01:02:03 UTC"
decodeTime("1 Jul 2013 01:02:03")
#> [1] "2013-07-01 01:02:03 UTC"
decodeTime("2013-07-01 01:02:03")
#> [1] "2013-07-01 01:02:03 UTC"
decodeTime("2013/07/01 01:02:03")
#> [1] "2013-07-01 01:02:03 UTC"
decodeTime("2013/07/01")
#> [1] "2013-07-01 UTC"
```
