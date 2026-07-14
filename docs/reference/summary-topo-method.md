# Summarize a topo Object

Pertinent summary information is presented, including the longitude and
latitude range, and the range of elevation.

## Usage

``` r
# S4 method for class 'topo'
summary(object, ...)
```

## Arguments

- object:

  A [topo](https://dankelley.github.io/oce/reference/topo-class.md)
  object.

- ...:

  Further arguments passed to or from other methods.

## Value

A matrix containing statistics of the elements of the `data` slot.

## See also

Other things related to topo data:
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
`[[<-,topo-method`,
[`as.topo()`](https://dankelley.github.io/oce/reference/as.topo.md),
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`topoInterpolate()`](https://dankelley.github.io/oce/reference/topoInterpolate.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(topoWorld)
summary(topoWorld)
#> 
#> Topo dataset
#> ------------
#> * Source:           ./topo_180W_180E_90S_90N_30min_netcdf.nc 
#> * Data Overview
#> 
#>                    Min.    Mean    Max.   Dim.      NAs OriginalName
#>     longitude [°N] -179.75 0       179.75 720       0   "lon"       
#>     latitude [°E]  -89.75  0       89.75  360       0   "lat"       
#>     z [m]          -10471  -1892.3 6147   "720x360" 0   "Band1"     
#> 
#> * Processing Log
#> 
#>     - 2019-05-21 12:55:46 UTC: `create 'topo' object`
#>     - 2019-05-21 12:55:46 UTC: `as.topo(longitude = longitude, latitude = latitude, z = z, filename = file)`
#>     - 2019-05-21 12:55:46 UTC: `read.topo(file = topoFile, debug = 5)`
```
