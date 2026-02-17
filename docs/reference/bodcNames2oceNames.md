# Determine oce Variable Names from an NERC/BODC Names

Translate names in the NERC/BODC vocabulary to oce names, primarily so
that
[`read.netcdf()`](https://dankelley.github.io/oce/reference/read.netcdf.md)
can produce more easily interpreted results. Please note that
`bodcNames2oceNames()` handles only a tiny subset of the huge number of
names in the NERC/BODC vocabulary (see Reference 1). To see the names
that the function handles currently, type `bodcNames2oceNames` in an R
session.

## Usage

``` r
bodcNames2oceNames(bodcNames, unduplicate = TRUE)
```

## Arguments

- bodcNames:

  character vector that specifies variable names that use the NERC/BODC
  convention.

- unduplicate:

  logical value indicating whether to rename repeated entries, so that
  e.g. if `"temperature` occurs twice, the second instance will be
  changed to `"temperature2"`. Set this to FALSE if you plan are calling
  `bodcNames2oceNames` in a renaming function of your own, and call
  [`unduplicateNames()`](https://dankelley.github.io/oce/reference/unduplicateNames.md)
  at the end of your function; see Example 2.

## Value

`bodcNames2oceNames` returns a vector of the same length as its first
argument, with translations to oce names, as appropriate. Note that the
usual oce convention for handling duplicates is used, with the first
name that maps to temperature being set to `temperature`, the next to
`temperature2`, etc.

## References

1.  The NERC Environmental Data Server.
    <http://vocab.nerc.ac.uk/collection/P01/current/>

## See also

Other functions that convert variable names to the oce convention:
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`metNames2oceNames()`](https://dankelley.github.io/oce/reference/metNames2oceNames.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md)

## Author

Dan Kelley

## Examples

``` r
# Example 1: typical usage
bodcNames2oceNames(c("PSALST01", "TEMPP901", "PRESPR01"))
#> [1] "salinity"    "temperature" "pressure"   

# Example 2: extend to add new variables
BODC2 <- function(originalNames) {
    rval <- bodcNames2oceNames(originalNames, unduplicate = FALSE)
    rval[rval == "bowler hat"] <- "hat"
    rval[rval == "top hat"] <- "hat"
    unduplicateNames(rval)
}
BODC2(c("PSALST01", "TEMPP901", "PRESPR01", "bowler hat", "top hat"))
#> [1] "salinity"    "temperature" "pressure"    "hat"         "hat2"       
```
