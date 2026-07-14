# Read a coastline File in Openstreetmap Format

Read coastline data stored in the openstreetmap format.

## Usage

``` r
read.coastline.openstreetmap(
  file,
  lonlim = c(-180, 180),
  latlim = c(-90, 90),
  monitor = FALSE,
  encoding = NA,
  debug = getOption("oceDebug"),
  processingLog
)
```

## Arguments

- file:

  name of file containing coastline data (a file ending in `.shp`) or a
  zipfile that contains such a file, with a corresponding name. The
  second scheme is useful for files downloaded from the NaturalEarth
  website (see reference 2).

- lonlim, latlim:

  numerical vectors specifying the west and east edges (and south and
  north edges) of a focus window. Coastline polygons that do not
  intersect the defined box are skipped, which can be useful in
  narrowing high-resolution world-scale data to a local application.

- monitor:

  Logical indicating whether to print an indication of progress through
  the file.

- encoding:

  ignored.

- debug:

  set to TRUE to print information about the header, etc.

- processingLog:

  if provided, the action item to be stored in the log. (Typically only
  provided for internal calls; the default that it provides is better
  for normal calls by a user.)

## Value

a
[coastline](https://dankelley.github.io/oce/reference/coastline-class.md)
object.

## See also

Other things related to coastline data:
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
`[[<-,coastline-method`,
[`as.coastline()`](https://dankelley.github.io/oce/reference/as.coastline.md),
[`coastline-class`](https://dankelley.github.io/oce/reference/coastline-class.md),
[`coastlineBest()`](https://dankelley.github.io/oce/reference/coastlineBest.md),
[`coastlineCut()`](https://dankelley.github.io/oce/reference/coastlineCut.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`download.coastline()`](https://dankelley.github.io/oce/reference/download.coastline.md),
[`plot,coastline-method`](https://dankelley.github.io/oce/reference/plot-coastline-method.md),
[`read.coastline.shapefile()`](https://dankelley.github.io/oce/reference/read.coastline.shapefile.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`summary,coastline-method`](https://dankelley.github.io/oce/reference/summary-coastline-method.md)

## Author

Dan Kelley
