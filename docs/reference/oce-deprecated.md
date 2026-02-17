# Deprecated and Defunct Elements of the oce Package

Certain functions and function arguments are still provided for
compatibility with older versions of
[oce](https://CRAN.R-project.org/package=oce), but will be removed soon.
The [oce](https://CRAN.R-project.org/package=oce) scheme for removing
functions is similar to that used by `Bioconductor`: items are marked as
"deprecated" in one release, marked as "defunct" in the next, and
removed in the next after that. This goal is to provide a gentle
migration path for users who keep their packages reasonably up-to-date.

## Details

The following are marked "deprecated" in the present CRAN release of
[oce](https://CRAN.R-project.org/package=oce). Please use the
replacement functions as listed below. The upcoming CRAN release of
[oce](https://CRAN.R-project.org/package=oce) will mark these as
"defunct", which is the last step before outright removal.

|                |                 |                |             |             |
|----------------|-----------------|----------------|-------------|-------------|
| **Deprecated** | **Replacement** | **Deprecated** | **Defunct** | **Removed** |

The following are marked "defunct", so calling them in the the present
version produces an error message that hints at a replacement function.
Once a function is marked "defunct" on one CRAN release, it will be
slated for outright deletion in some subsequent release.

|             |                 |             |
|-------------|-----------------|-------------|
| **Defunct** | **Replacement** | **Version** |

The following functions were removed after having been marked as
"deprecated" in at least one CRAN release, and possibly as "defunct" in
at least one CRAN release. (The version number in the table is the first
version to lack the named function.)

|  |  |  |
|----|----|----|
| **Function** | **Replacement** | **Version** |
| `addColumn()` | [`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md) | 1.1-2 |
| `ctdAddColumn()` | [`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md) | 1.1-2 |
| `ctdUpdateHeader()` | [`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md) | 1.1-2 |
| `findInOrdered()` | [`findInterval()`](https://rdrr.io/r/base/findInterval.html) | 1.1-2 |
| `makeSection()` | [`as.section()`](https://dankelley.github.io/oce/reference/as.section.md) | 0.9.24 |
| `mapMeridians()` | [`mapGrid()`](https://dankelley.github.io/oce/reference/mapGrid.md) | 1.1-2 |
| `mapZones()` | [`mapGrid()`](https://dankelley.github.io/oce/reference/mapGrid.md) | 1.1-2 |
| `oce.as.POSIXlt()` | [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html) | 1.1-2 |
| `renameData()` | [`oceRenameData()`](https://dankelley.github.io/oce/reference/oceRenameData.md) | 1.7-9 |
| `trimString()` | [`trimws()`](https://rdrr.io/r/base/trimws.html) | 1.8-2 |

Several “oce” function arguments are considered "defunct", which means
they will be removed in the next CRAN release. They are as follows.

- The `fill` argument of
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  was confusing to users, so it was designated as deprecated in
  June 2016. (The confusion stemmed from subtle differences between
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`polygon()`](https://rdrr.io/r/graphics/polygon.html), and the
  problem is that
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  can use either of these functions, according to whether coastlines are
  to be filled.) The functionality is preserved, in the `col` argument.

## See also

The “Bioconductor” scheme for removing functions is described at
`https://www.bioconductor.org/developers/how-to/deprecation/` and it is
extended here to function arguments.
