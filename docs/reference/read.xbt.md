# Read an xbt file

Three file types are handled: (1) the `"sippican"` format of Sippican
XBT files with space-separated data columns, (2) a related `"sippican2`
format, (also known as MK21 export format) in which data columns are
separated by tab characters, and (3) the `"noaa1"` format. These three
types are handled either by setting `type` to the named string, or by
directly calling
[`read.xbt.edf()`](https://dankelley.github.io/oce/reference/read.xbt.edf.md),
[`read.xbt.edf2()`](https://dankelley.github.io/oce/reference/read.xbt.edf2.md),
or
[`read.xbt.noaa1()`](https://dankelley.github.io/oce/reference/read.xbt.noaa1.md).

## Usage

``` r
read.xbt(
  file,
  type = "sippican",
  longitude,
  latitude,
  encoding = "latin1",
  debug = getOption("oceDebug"),
  processingLog
)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- type:

  character string indicating type of file, with valid choices being
  `"sippican"`, `"sippican2"`, and `"noaa1"`.

- longitude, latitude:

  optional signed numbers indicating the longitude in degrees East and
  latitude in degrees North. These values are used if `type="sippican"`,
  but ignored if `type="noaa1"`, because those files contain location
  information.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- debug:

  a flag that turns on debugging. The value indicates the depth within
  the call stack to which debugging applies.

- processingLog:

  if provided, the action item to be stored in the log. This parameter
  is typically only provided for internal calls; the default that it
  provides is better for normal calls by a user.

## Value

An [xbt](https://dankelley.github.io/oce/reference/xbt-class.md) object.

## References

1.  Sippican, Inc. "Bathythermograph Data Acquisition System:
    Installation, Operation and Maintenance Manual (P/N 308195, Rev.
    A)," 2003.
    https://pages.uoregon.edu/drt/MGL0910_Science_Report/attachments/MK21_ISA_Manual_Rev_A.pdf.

## See also

Other things related to xbt data:
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
`[[<-,xbt-method`,
[`as.xbt()`](https://dankelley.github.io/oce/reference/as.xbt.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`read.xbt.noaa1()`](https://dankelley.github.io/oce/reference/read.xbt.noaa1.md),
[`read.xbt.noaa2()`](https://dankelley.github.io/oce/reference/read.xbt.noaa2.md),
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
library(oce)
xbt <- read.xbt(system.file("extdata", "xbt.edf", package = "oce"))
summary(xbt)
#> xbt summary
#> -----------
#> 
#> * File source:        "/private/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T/RtmpA4dTGC/temp_libpath116d56d689885/oce/extdata/xbt.edf"
#> * Serial Number:      0
#> * Longitude:          4.005
#> * Latitude:           -4
#> * Time:               2000-10-10 08:49:38
#> * Data Overview
#> 
#>                              Min.   Mean   Max.   Dim. NAs OriginalName    
#>     depth [m]                5.4    7.0333 8.7    6    0   "Depth"         
#>     temperature [°C, ITS-90] 20.9   20.905 20.91  6    0   "Temperature"   
#>     soundSpeed [m/s]         1575.3 1575.3 1575.4 6    0   "Sound Velocity"
#> 
#> * Processing Log
#> 
#>     - 2026-04-09 19:44:49 UTC: `create 'xbt' object`
#>     - 2026-04-09 19:44:49 UTC: `read.xbt.edf(file = file, longitude = longitude, latitude = latitude,     encoding = encoding, debug = debug - 1L, processingLog = processingLog)`
plot(xbt)

```
