# Read an xbt File in Sippican Format Type 1

The function was written by inspection of a particular file, and might
be wrong for other files; see “Details” for a note on character
translation. The format uses space-separated data columns, unlike the
tab- separated columns of
[`read.xbt.edf2()`](https://dankelley.github.io/oce/reference/read.xbt.edf2.md).

## Usage

``` r
read.xbt.edf(
  file,
  longitude = NA,
  latitude = NA,
  encoding = "latin1",
  debug = getOption("oceDebug"),
  processingLog
)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- longitude:

  optional signed number indicating the longitude in degrees East.

- latitude:

  optional signed number indicating the latitude in degrees North.

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

## Details

The header is converted to ASCII format prior to storage in the
`metadata` slot, so that e.g. a degree sign in the original file will
become a `?` character in the `header`. This is to prevent problems with
submission of `oce` to the CRAN system, which produces NOTEs about UTF-8
strings in data (on some build machines, evidently depending on the
locale on those machines). This character substitution is at odds with
the `oce` philosophy of leaving data intact, so it will be reverted, if
CRAN policy changes or if the developers can find a way to otherwise
silence the NOTE.

## Author

Dan Kelley

## Examples

``` r
library(oce)
xbt <- read.oce(system.file("extdata", "xbt.edf", package = "oce"))
summary(xbt)
#> xbt summary
#> -----------
#> 
#> * File source:        "/private/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T/RtmpdNg0iG/temp_libpath32578b0e13b/oce/extdata/xbt.edf"
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
#>     - 2026-02-17 20:58:06 UTC: `create 'xbt' object`
#>     - 2026-02-17 20:58:06 UTC: `read.xbt.edf(file = file, encoding = encoding)`
```
