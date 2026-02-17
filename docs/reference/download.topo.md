# Download and Cache a topo File

Topographic data are downloaded from a data server that holds the ETOPO1
dataset (Amante, C. and B.W. Eakins, 2009), and saved as a netCDF file
whose name specifies the data request, if a file of that name is not
already present on the local file system. The return value is the name
of the data file, and its typical use is as the filename for a call to
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md).
Given the rules on file naming, subsequent calls to `download.topo` with
identical parameters will simply return the name of the cached file,
assuming the user has not deleted it in the meantime. Note that
`download.topo` uses the `"terra"` and `"ncdf4"` packages, so an error
is reported if they are not available.

## Usage

``` r
download.topo(
  west,
  east,
  south,
  north,
  resolution = 4,
  destdir = ".",
  destfile,
  format,
  server = "https://gis.ngdc.noaa.gov",
  debug = getOption("oceDebug")
)
```

## Arguments

- west, east:

  numeric values for the limits of the data-selection box, in degrees.
  These are converted to the -180 to 180 degree notation, if needed.
  Then, `west` is rounded down to the nearest 1/100th degree, and `east`
  is rounded up to the the nearest 1/100th degree. The results of these
  operations are used in constructing the query for the NOAA data
  server.

- south, north:

  latitude limits, treated in a way that corresponds to the longitude
  limits.

- resolution:

  numeric value of grid spacing, in geographical minutes. The default
  value is 4 minutes, corresponding to 4 nautical miles (approx. 7.4km)
  in the north-south direction, and less in the east-west direction.

- destdir:

  Optional string indicating the directory in which to store downloaded
  files. If not supplied, `"."` is used, i.e. the data file is stored in
  the present working directory.

- destfile:

  Optional string indicating the name of the file. If not supplied, the
  file name is constructed from the other parameters of the function
  call, so subsequent calls with the same parameters will yield the same
  result, thus providing the key to the caching scheme.

- format:

  Deprecated, and ignored, as of June 2020.

- server:

  character value specifying the base from which a download URL will be
  constructed. It is unlikely that any value other than the default will
  work, unless it is a similarly-constructed mirrored site.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

String indicating the full pathname to the downloaded file.

## Details

The specified longitude and latitude limits are rounded to 2 digits
(corresponding to a footprint of approximately 1km), and these are used
in the server request. If the resultant request would generate under 1
row or column in the result, `download.topo` generates an error message
and stops.

## Historical note relating to NOAA server changes

2022 November 13: updated to new NOAA database, with 1/4-minute
resolution (a marked improvement over the previous 1-minute resolution).
The revision was framed along similar changes to
`marmap::getNOAAbathy()` made earlier today. Thanks to Clark Richards
for pointing this out!

2020 May 31: updated for a change in the NOAA query structure, taking
hints from `marmap::getNOAAbathy()`.

## Sample of Usage

    library(oce)
    topoFile <- download.topo(west=-66, east=-60, south=43, north=47,
        resolution=1, destdir="~/data/topo")
    topo <- read.topo(topoFile)
    imagep(topo, zlim=c(-400, 400), col=oceColorsTwo, drawTriangles=TRUE)
    if (requireNamespace("ocedata", quietly=TRUE)) {
        data(coastlineWorldFine, package="ocedata")
        lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
    }

## References

- Amante, C. and B.W. Eakins, 2009. ETOPO1 1 Arc-Minute Global Relief
  Model: Procedures, Data Sources and Analysis. NOAA Technical
  Memorandum NESDIS NGDC-24. National Geophysical Data Center, NOAA.
  doi:10.7289/V5C8276M

## See also

Other functions that download files:
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
[`download.coastline()`](https://dankelley.github.io/oce/reference/download.coastline.md),
[`download.met()`](https://dankelley.github.io/oce/reference/download.met.md)

Other things related to topo data:
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
`[[<-,topo-method`,
[`as.topo()`](https://dankelley.github.io/oce/reference/as.topo.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`summary,topo-method`](https://dankelley.github.io/oce/reference/summary-topo-method.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`topoInterpolate()`](https://dankelley.github.io/oce/reference/topoInterpolate.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md)

## Author

Dan Kelley
