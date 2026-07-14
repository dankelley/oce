# Read an Oceanographic Data File

Read an oceanographic data file, auto-discovering the file type from the
first line of the file. This function tries to infer the file type from
the first line, using
[`oceMagic()`](https://dankelley.github.io/oce/reference/oceMagic.md).
If it can be discovered, then an instrument-specific file reading
function is called, with the `file` and with any additional arguments
being supplied.

## Usage

``` r
read.oce(file, ..., encoding = "latin1")
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- ...:

  arguments to be handed to whichever instrument-specific reading
  function is selected, based on the header.

- encoding:

  a character string giving the file encoding. This defaults to
  `"latin1`", which seems to work for files available to the authors,
  but be aware that a different setting may be required for files that
  contain unusual accents or characters. (Try `"UTF-8"` if the default
  produces errors.) Note that `encoding` is ignored in binary files, and
  also in some text-based files, as well.

## Value

An [oce](https://dankelley.github.io/oce/reference/oce-class.md) object
of that is specialized to the data type, e.g.
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md), if the
data file contains `ctd` data.

## See also

The file type is determined by
[`oceMagic()`](https://dankelley.github.io/oce/reference/oceMagic.md).
If the file type can be determined, then one of the following is called:
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md),
[`read.coastline()`](https://dankelley.github.io/oce/reference/read.coastline.md)
[`read.lobo()`](https://dankelley.github.io/oce/reference/read.lobo.md),
[`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md),
[`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md),
etc.

## Author

Dan Kelley

## Examples

``` r
library(oce)
x <- read.oce(system.file("extdata", "ctd.cnv.gz", package = "oce"))
#> Warning: suspicious startTime 1903-10-15 11:38:38 changed to 2003-10-15 11:38:38; see 'start_time' in file header
#> Warning: file has temperature in IPTS-68 so this is stored as-is, but note that [["temperature"]] and sw* functions autoconvert to ITS-90 to match modern conventions
plot(x) # summary with TS and profiles

plotTS(x) # just the TS
```
