# Read a coastline File

Read a coastline file in R, Splus, mapgen, shapefile, or openstreetmap
format. The S and R formats are identical, and consist of two columns,
lon and lat, with land-jump segments separated by lines with two NAs.
The MapGen format is of the form

     # -b -16.179081 28.553943
    -16.244793 28.563330 

BUG: the 'arc/info ungenerate' format is not yet understood.

## Usage

``` r
read.coastline(
  file,
  type = c("R", "S", "mapgen", "shapefile", "openstreetmap"),
  encoding = "latin1",
  monitor = FALSE,
  debug = getOption("oceDebug"),
  processingLog
)
```

## Arguments

- file:

  name of file containing coastline data. If this is a shapefile, then
  the other parameters (except `debug`) are ignored, and the results of
  a call to
  [`read.coastline.shapefile()`](https://dankelley.github.io/oce/reference/read.coastline.shapefile.md)
  are returned.

- type:

  type of file, one of `"R"`, `"S"`, `"mapgen"`, `"shapefile"` or
  `"openstreetmap"`.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- monitor:

  print a dot for every coastline segment read (ignored except for
  reading "shapefile" type)

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

## Author

Dan Kelley
