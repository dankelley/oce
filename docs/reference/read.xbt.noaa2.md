# Read an xbt File in UBT (Universal BathyThermograph) Format

This is preliminary function that might be changed through the month of
February 2025. Note that, at present, this function returns a list of
[xbt](https://dankelley.github.io/oce/reference/xbt-class.md) objects,
as opposed to a single
[xbt](https://dankelley.github.io/oce/reference/xbt-class.md) object.
This is because the data format holds one profile per line. Also, please
note that the documentation used to frame the function (Reference 1)
does not accurately describe test files (Reference 2), owing to a change
of year format from the documented two-character form to a more modern
4-character form. Other aspects of the documented format are also at
odds with the data (see issue 2289 on the github site for oce).

## Usage

``` r
read.xbt.noaa2(
  file,
  debug = getOption("oceDebug"),
  missingValue = -9.99,
  encoding = "latin1",
  processingLog
)
```

## Arguments

- file:

  character value naming a file, or a file connection, containing the
  data, with each line corresponding to an XBT profile.

- debug:

  a flag that turns on debugging. The value indicates the depth within
  the call stack to which debugging applies.

- missingValue:

  ignored.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- processingLog:

  ignored.

## Value

A list containing
[xbt](https://dankelley.github.io/oce/reference/xbt-class.md) objects,
one per line in `file`.

## References

1.  https://www.ncei.noaa.gov/data/oceans/nodc/formats/UBT_Universal_Bathythermograph.html

2.  https://data.noaa.gov/onestop/collections/details/22c9b8d0-c32b-4824-815b-04ce80078d10

## See also

Other things related to xbt data:
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
`[[<-,xbt-method`,
[`as.xbt()`](https://dankelley.github.io/oce/reference/as.xbt.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`read.xbt()`](https://dankelley.github.io/oce/reference/read.xbt.md),
[`read.xbt.noaa1()`](https://dankelley.github.io/oce/reference/read.xbt.noaa1.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md),
[`summary,xbt-method`](https://dankelley.github.io/oce/reference/summary-xbt-method.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md),
[`xbt.edf`](https://dankelley.github.io/oce/reference/xbt.edf.md),
[`xbt2.edf`](https://dankelley.github.io/oce/reference/xbt2.edf.md)

## Author

Dan Kelley

## Examples

``` r
# Plot the 2 profiles in a built-in data file
library(oce)
xbts <- read.xbt.noaa2(system.file("extdata", "xbt_noaa2", package = "oce"))
par(mfrow = c(1, 2))
for (xbt in xbts) {
    summary(xbt)
    plot(xbt)
}
#> xbt summary
#> -----------
#> 
#> * Longitude:          -77.25
#> * Latitude:           43.5833333333333
#> * Time:               1972-07-11 03:02:00
#> * Data Overview
#> 
#>                 Min. Mean   Max.   Dim. NAs
#>     depth       0    34.083 136    12   0  
#>     temperature 3.8  7.9417 15.1   12   0  
#>     pressure    0    34.369 137.16 12   0  
#> 
#> * Processing Log
#> 
#>     - 2026-03-16 15:50:02 UTC: `create 'xbt' object`
#> xbt summary
#> -----------
#> 
#> * Longitude:          -77.25
#> * Latitude:           43.4833333333333
#> * Time:               1972-07-11 04:12:00
#> * Data Overview
#> 
#>                 Min. Mean   Max.   Dim. NAs
#>     depth       0    47.455 187    11   0  
#>     temperature 3.9  6.9818 14.6   11   0  
#>     pressure    0    47.859 188.62 11   0  
#> 
#> * Processing Log
#> 
#>     - 2026-03-16 15:50:02 UTC: `create 'xbt' object`

```
