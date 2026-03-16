# Add an Item to a Processing Log

Add an Item to a Processing Log

## Usage

``` r
processingLog(x) <- value
```

## Arguments

- x:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- value:

  A character string with the description of the logged activity.

## See also

Other things related to processing logs:
[`processingLogAppend()`](https://dankelley.github.io/oce/reference/processingLogAppend.md),
[`processingLogItem()`](https://dankelley.github.io/oce/reference/processingLogItem.md),
[`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)

## Examples

``` r
data(ctd)
processingLogShow(ctd)
#> * Processing Log
#> 
#>     - 2018-11-14 20:03:47 UTC: `create 'ctd' object`
#>     - 2018-11-14 20:03:47 UTC: `read.ctd.sbe(file = file, debug = 10, processingLog = processingLog)`
#>     - 2018-11-14 20:03:47 UTC: `oce.edit(x = ctd, item = "startTime", value = as.POSIXct(gsub("1903",     "2003", format(ctd[["startTime"]])), tz = "UTC") + 4 * 3600,     reason = "file had year=1903, instead of 2003", person = "Dan Kelley")`
processingLog(ctd) <- "test"
processingLogShow(ctd)
#> * Processing Log
#> 
#>     - 2018-11-14 20:03:47 UTC: `create 'ctd' object`
#>     - 2018-11-14 20:03:47 UTC: `read.ctd.sbe(file = file, debug = 10, processingLog = processingLog)`
#>     - 2018-11-14 20:03:47 UTC: `oce.edit(x = ctd, item = "startTime", value = as.POSIXct(gsub("1903",     "2003", format(ctd[["startTime"]])), tz = "UTC") + 4 * 3600,     reason = "file had year=1903, instead of 2003", person = "Dan Kelley")`
#>     - 2026-03-16 12:26:33 UTC: `test`
```
