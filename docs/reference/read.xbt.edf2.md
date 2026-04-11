# Read an xbt File in Sippican Format Type 2

The function was written by inspection of a particular file provided by
a user in late 2024. The format has been referred to as MK21 export
format, and a key difference to the format handled by
[`read.xbt.edf()`](https://dankelley.github.io/oce/reference/read.xbt.edf.md)
is that data columns are separated by tab characters, not spaces. The
reading of header data is more rudimentary than is the case for
[`read.xbt.edf()`](https://dankelley.github.io/oce/reference/read.xbt.edf.md),
because the sample data file made available to the author did not seem
to have much useful information in its header.

## Usage

``` r
read.xbt.edf2(
  file,
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

## Author

Dan Kelley

## Examples

``` r
library(oce)
xbt2 <- read.xbt(system.file("extdata", "xbt2.edf", package = "oce"), type = "sippican2")
summary(xbt2)
#> xbt summary
#> -----------
#> 
#> * File source:        "/private/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T/RtmpA4dTGC/temp_libpath116d56d689885/oce/extdata/xbt2.edf"
#> * Instrument type:    T-xx
#> * Serial Number:      1
#> * Longitude:          -50.5
#> * Latitude:           40.5
#> * Time:               2025-01-07 12:01:02
#> * Data Overview
#> 
#>                      Min. Mean   Max. Dim. NAs OriginalName    
#>     time [s]         0    0.05   0.1  2    0   "Time"          
#>     resistance [ohm] 6600 6600.5 6601 2    0   "Resistance"    
#>     depth [m]        0    0.25   0.5  2    0   "Depth"         
#>     temperature [°C] 17.9 17.95  18   2    0   "Temperature"   
#>     soundSpeed [m/s] 1520 1520.5 1521 2    0   "Sound Velocity"
#> 
#> * Processing Log
#> 
#>     - 2026-04-09 19:44:49 UTC: `create 'xbt' object`
```
