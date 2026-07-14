# Read a topo File

Read a file that contains topographic data in the ETOPO dataset, as was
once provided by the NOAA website (see
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md)
for a good server for such files. (As of May, 2020, there does not seem
to be a way to download these files from the NOAA website.)

## Usage

``` r
read.topo(file, encoding = "latin1", debug = getOption("oceDebug"))
```

## Arguments

- file:

  Name of a file containing an ETOPO-format dataset. Three types are
  permitted; see “Details”.

- encoding:

  ignored.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A [topo](https://dankelley.github.io/oce/reference/topo-class.md)
object.

## Details

The three permitted file types are as follows.

1.  An ascii type in which line 1 holds a label (which is ignored),
    whitespace, and then the number of columns in the matrix (i.e. the
    number of longitude values), line 2 is similar but for latitude,
    line 3 is similar but for the westernmost longitude, line 4 is
    similar but for southernmost latitude, line 5 is similar but for
    cell size, and lines after that hold the grid.

2.  A NetCDF format that was once described by NOAA as "GMT NetCDF".

3.  A NetCDF format that was once described by NOAA as "NetCDF".

## Sample of Usage


    library(oce)
    topoMaritimes <- read.topo("topoMaritimes.asc")
    plot(topographyMaritimes)

## See also

Other things related to topo data:
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
`[[<-,topo-method`,
[`as.topo()`](https://dankelley.github.io/oce/reference/as.topo.md),
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`summary,topo-method`](https://dankelley.github.io/oce/reference/summary-topo-method.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`topoInterpolate()`](https://dankelley.github.io/oce/reference/topoInterpolate.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md)

## Author

Dan Kelley
