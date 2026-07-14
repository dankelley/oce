# Read a coastline File in Shapefile Format

Read coastline data stored in the shapefile format (see reference 1).

## Usage

``` r
read.coastline.shapefile(
  file,
  lonlim = c(-180, 180),
  latlim = c(-90, 90),
  encoding = NA,
  monitor = FALSE,
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

- encoding:

  ignored.

- monitor:

  Logical indicating whether to print an indication of progress through
  the file.

- debug:

  set to TRUE to print information about the header, etc.

- processingLog:

  if provided, the action item to be stored in the log. (Typically only
  provided for internal calls; the default that it provides is better
  for normal calls by a user.)

## Value

x a
[coastline](https://dankelley.github.io/oce/reference/coastline-class.md)
object.

## A hack for depth contours

The following demonstrates that this code is getting close to working
with depth contours. This should be handled more internally, and a new
object for depth contours should be constructed, of which coastlines
could be a subset.

## References

1.  The “shapefile” format is described in *ESRI Shapefile Technical
    Description*, March 1998, available at
    `https://www.esri.com/content/dam/esrisites/sitecore-archive/Files/Pdfs/library/whitepapers/pdfs/shapefile.pdf`
    (last checked 2021-03-24).

2.  The NaturalEarth website
    `https://www.naturalearthdata.com/downloads/` provides coastline
    datasets in three resolutions, along with similar files lakes and
    rivers, for borders, etc. It is highly recommended.

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
[`read.coastline.openstreetmap()`](https://dankelley.github.io/oce/reference/read.coastline.openstreetmap.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`summary,coastline-method`](https://dankelley.github.io/oce/reference/summary-coastline-method.md)

## Author

Dan Kelley
