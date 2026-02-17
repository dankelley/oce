# Read a NOAA Ocean Index File

Read an ocean index file, in the format used by NOAA.

## Usage

``` r
read.index(
  file,
  format,
  missingValue,
  tz = getOption("oceTz"),
  encoding = "latin1",
  debug = getOption("oceDebug")
)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load. May be a URL.

- format:

  optional character string indicating the format type. If not supplied,
  a guess will be made, based on examination of the first few lines of
  the file. If supplied, the possibilities are `"noaa"` and `"ucar"`.
  See “Details”.

- missingValue:

  If supplied, this is a numerical value that indicates invalid data. In
  some datasets, this is -99.9, but other values may be used. If
  `missingValue` is not supplied, any data that have value equal to or
  less than -99 are considered invalid. Set `missingValue` to `TRUE` to
  turn of the processing of missing values.

- tz:

  character string indicating time zone to be assumed in the data.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- debug:

  a flag that turns on debugging, ignored in the present version of the
  function.

## Value

A data frame containing `t`, a POSIX time, and `index`, the numerical
index. The times are set to the 15th day of each month, which is a guess
that may need to be changed if so indicated by documentation (yet to be
located).

## Details

Reads a text-format index file, in either of two formats. If `format` is
missing, then the first line of the file is examined. If that line
contains 2 (whitespace-separated) tokens, then `"noaa"` format is
assumed. If it contains 13 tokens, then `"ucar"` format is assumed.
Otherwise, an error is reported.

In the `"noaa"` format, the two tokens on the first line are taken to be
the start year and the end year, respectively. The second line must
contain a single token, the missing value. All further lines must
contain 12 tokens, for the values in January, February, etc.

In the `"ucar"` format, all data lines must contain the 13 tokens, the
first of which is the year, with the following 12 being the values for
January, February, etc.

## Sample of Usage

    library(oce)
    par(mfrow=c(2, 1))
    # 1. AO, Arctic oscillation
    #  Note that data used to be at https://www.esrl.noaa.gov/psd/data/correlation/ao.data
    ao <- read.index("https://psl.noaa.gov/data/correlation/ao.data")
    aorecent <- subset(ao, t > as.POSIXct("2000-01-01"))
    oce.plot.ts(aorecent$t, aorecent$index)
    # 2. SOI, probably more up-to-date then data(soi, package="ocedata")
    soi <- read.index("https://www.cgd.ucar.edu/cas/catalog/climind/SOI.signal.ascii")
    soirecent <- subset(soi, t > as.POSIXct("2000-01-01"))
    oce.plot.ts(soirecent$t, soirecent$index)

## References

See `https://psl.noaa.gov/data/climateindices/list/` for a list of
indices.

## Author

Dan Kelley
