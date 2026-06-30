# Read a g1sst File

Read a G1SST file in the NetCDF format provided by the ERDDAP server
(see reference 1).

## Usage

``` r
read.g1sst(file, encoding = NA)
```

## Arguments

- file:

  character value containing the name of a NetCDF file containing G1SST
  data.

- encoding:

  ignored.

## Value

A [g1sst](https://dankelley.github.io/oce/reference/g1sst-class.md)
object.

## Details

As noted in the documentation for the
[g1sst](https://dankelley.github.io/oce/reference/g1sst-class.md) class,
one must be aware of the incorporation of model simulations in the
`g1sst` product. For example, the code presented below might lead one to
believe that the mapped field represents observations, whereas in fact
it can be verified by consulting reference 2 (clicking and unclicking
the radio button to show just the data) that the field mostly derives
from simulation.

## Sample of Usage


    # Construct query, making it easier to understand and modify.
    day <- "2016-01-02"
    lon0 <- -66.5
    lon1 <- -64.0
    lat0 <- 44
    lat1 <- 46
    source <- paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/",
        "jplG1SST.nc?",
        "SST
        "
        "
        "
    if (!length(list.files(pattern="^a.nc$")))
        download.file(source, "a.nc")
    d <- read.g1sst("a.nc")
    plot(d, "SST", col=oceColorsTemperature)
    if (requireNamespace("ocedata", quietly=TRUE)) {
        data(coastlineWorldFine, package="ocedata")
        lines(coastlineWorldFine[["longitude"]],coastlineWorldFine[["latitude"]])
    }

## References

1.  ERDDAP Portal `https://coastwatch.pfeg.noaa.gov/erddap/`

2.  JPO OurOcean Portal `https://ourocean.jpl.nasa.gov/SST/`

## See also

Other things related to g1sst data:
[`[[,g1sst-method`](https://dankelley.github.io/oce/reference/sub-sub-g1sst-method.md),
`[[<-,g1sst-method`,
[`g1sst-class`](https://dankelley.github.io/oce/reference/g1sst-class.md)

## Author

Dan Kelley
