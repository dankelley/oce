# Global Topographic Data (at Half-degree Resolution)

Global topographic dataset at half-degree resolution, downloaded from a
NOAA server on May 18, 2019. Longitude, accessible as
`topoWorld[["longitude"]]`, ranges from -179.75 to 129.75 degrees north.
Latitude (`topoWorld[["latitude"]]`) ranges from -89.75 to 89.75 degrees
east. Height (`topoWorld[["z"]]`) is measured in metres above nominal
sea level.

The coarse resolution can be a problem in plotting depth contours along
with coastlines in regions of steep topography. For example, near the
southeast corner of Newfoundland, a 200m contour will overlap a
coastline drawn with `coastlineWorldFine` from the
[ocedata](https://CRAN.R-project.org/package=ocedata) package. The
solution in such cases is to download a higher-resolution topography
file, perhaps using
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md),
and then use
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md)
to create another `topo` object. (With other data sources,
[`as.topo()`](https://dankelley.github.io/oce/reference/as.topo.md) may
be helpful.)

## Usage

``` r
data(topoWorld)
```

## Source

This is created with
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md),
using a file downloaded with

    topoFile <- download.topo(west=-180, east=180, south=-90, north=90,
       resolution=30, destdir=".")

## Historical note

From late 2009 until May 18, 2019, the `topoWorld` dataset was created
with a fairly complicated code that read a binary file downloaded from
NOAA (`http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5`),
decoded, decimated from 1/12th degree resolution to 1/2 degree
resolution, and passed through
[`matrixShiftLongitude()`](https://dankelley.github.io/oce/reference/matrixShiftLongitude.md)
to put longitude between -180 and 180 degrees. The new scheme for
creating the dataset, (see “Source”) is much simpler, and also a much
better model of how users are likely to deal with topography files in
the more modern netCDF format. Note that the new version differs from
the old one in longitude and latitude being shifted by 1/4 degree, and
by a mean elevation difference of under 10m. The old and new versions
appear identical when plotted at the global scale that is the
recommended for such a coarse topographic file.

## Sample of Usage

    library(oce)
    data(topoWorld)
    par(mfrow=c(2, 1))
    plot(topoWorld, location=NULL)
    imagep(topoWorld)

## See also

Other datasets provided with oce:
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`lisst`](https://dankelley.github.io/oce/reference/lisst.md),
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`ocecolors`](https://dankelley.github.io/oce/reference/ocecolors.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`wind`](https://dankelley.github.io/oce/reference/wind.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md)

Other things related to topo data:
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
`[[<-,topo-method`,
[`as.topo()`](https://dankelley.github.io/oce/reference/as.topo.md),
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`summary,topo-method`](https://dankelley.github.io/oce/reference/summary-topo-method.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`topoInterpolate()`](https://dankelley.github.io/oce/reference/topoInterpolate.md)
