# Read an argo File in Copernicus Format

This function was added to read a particular file downloaded from the
Fleet Monitoring website (Reference 1). The format was inferred through
examination of the file and a brief study of a document (Reference 2)
that describes the format. Not all fields are read by this function; see
Reference 3 for a full list and note that the author would be happy to
add new entries (but not to spend hours entering then all).

## Usage

``` r
read.argo.copernicus(
  file,
  encoding = NA,
  debug = getOption("oceDebug"),
  processingLog,
  ...
)
```

## Arguments

- file:

  A character string giving the name of the file to load.

- encoding:

  ignored.

- debug:

  A flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or 0 (the default) for silent operation.

- processingLog:

  ignored.

- ...:

  ignored.

## References

1.  `https://fleetmonitoring.euro-argo.eu/float/4902489`

2.  Copernicus Marine In Situ Tac Data Management Team. Copernicus
    Marine In Situ NetCDF Format Manual (version V1.43). Pdf. Copernicus
    in situ TAC, 2021. `https://doi.org/10.13155/59938` (link checked
    2022-04-11).

3.  Variable names are provided in files at
    `https://doi.org/10.13155/53381` (link checked 2022-04-12)

## See also

Other things related to argo data:
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
`[[<-,argo-method`,
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`argoGrid()`](https://dankelley.github.io/oce/reference/argoGrid.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`as.argo()`](https://dankelley.github.io/oce/reference/as.argo.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

## Author

Dan Kelley
