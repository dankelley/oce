# Download a coastline File

Constructs a query to the NaturalEarth server (see reference 1) to
download coastline data (or lake data, river data, etc) in any of three
resolutions.

## Usage

``` r
download.coastline(
  resolution,
  item = "coastline",
  destdir = ".",
  destfile,
  server = "naturalearth",
  debug = getOption("oceDebug")
)
```

## Arguments

- resolution:

  A character value specifying the desired resolution. The permitted
  choices are `"10m"` (for 1:10M resolution, the most detailed), `"50m"`
  (for 1:50M resolution) and `"110m"` (for 1:110M resolution). If
  `resolution` is not supplied, `"50m"` will be used.

- item:

  A character value indicating the quantity to be downloaded. This is
  normally one of `"coastline"`, `"land"`, `"ocean"`,
  `"rivers_lakes_centerlines"`, or `"lakes"`, but the NaturalEarth
  server has other types, and advanced users can discover their names by
  inspecting the URLs of links on the NaturalEarth site, and use them
  for `item`. If `item` is not supplied, it defaults to `"coastline"`.

- destdir:

  Optional string indicating the directory in which to store downloaded
  files. If not supplied, `"."` is used, i.e. the data file is stored in
  the present working directory.

- destfile:

  Optional string indicating the name of the file. If not supplied, the
  file name is constructed from the other parameters of the function
  call, so subsequent calls with the same parameters will yield the same
  result, thus providing the key to the caching scheme.

- server:

  A character value specifying the server that is to supply the data. At
  the moment, the only permitted value is `"naturalearth"`, which is the
  default if `server` is not supplied.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A character value indicating the filename of the result; if there is a
problem of any kind, the result will be the empty string.

## Non-Executable Examples


    library(oce)
    # User must create directory ~/data/coastline first.
    # As of September 2016, the downloaded file, named
    # "ne_50m_coastline.zip", occupies 443K bytes.
    filename <- download.coastline(destdir="~/data/coastline")
    coastline <- read.coastline(filename)
    plot(coastline)

## References

1.  The NaturalEarth server is at `https://www.naturalearthdata.com`

## See also

The work is done with
[`utils::download.file()`](https://rdrr.io/r/utils/download.file.html).

Other functions that download files:
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
[`download.met()`](https://dankelley.github.io/oce/reference/download.met.md),
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md)

Other things related to coastline data:
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
`[[<-,coastline-method`,
[`as.coastline()`](https://dankelley.github.io/oce/reference/as.coastline.md),
[`coastline-class`](https://dankelley.github.io/oce/reference/coastline-class.md),
[`coastlineBest()`](https://dankelley.github.io/oce/reference/coastlineBest.md),
[`coastlineCut()`](https://dankelley.github.io/oce/reference/coastlineCut.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`plot,coastline-method`](https://dankelley.github.io/oce/reference/plot-coastline-method.md),
[`read.coastline.openstreetmap()`](https://dankelley.github.io/oce/reference/read.coastline.openstreetmap.md),
[`read.coastline.shapefile()`](https://dankelley.github.io/oce/reference/read.coastline.shapefile.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`summary,coastline-method`](https://dankelley.github.io/oce/reference/summary-coastline-method.md)

## Author

Dan Kelley
