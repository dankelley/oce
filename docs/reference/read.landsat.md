# Read a landsat File Directory

Read a landsat data file, producing an object of
[landsat](https://dankelley.github.io/oce/reference/landsat-class.md).
The actual reading is done with
[`tiff::readTIFF()`](https://rdrr.io/pkg/tiff/man/readTIFF.html) in the
[tiff](https://CRAN.R-project.org/package=tiff) package, so that package
must be installed for `read.landsat` to work.

## Usage

``` r
read.landsat(
  file,
  band = "all",
  emissivity = 0.984,
  decimate,
  encoding = "latin1",
  debug = getOption("oceDebug")
)
```

## Arguments

- file:

  A connection or a character string giving the name of the file to
  load. This is a directory name containing the data.

- band:

  The bands to be read, by default all of the bands. Use `band=NULL` to
  skip the reading of bands, instead reading only the image metadata,
  which is often enough to check if the image is of interest in a given
  study. See “Details” for the names of the bands, some of which are
  pseudo-bands, computed from the actual data.

- emissivity:

  Value of the emissivity of the surface, stored as `emissivity` in the
  `metadata` slot of the resultant object. This is used in the
  calculation of surface temperature, as explained in the discussion of
  accessor functions for
  [landsat](https://dankelley.github.io/oce/reference/landsat-class.md).
  The default value is from Konda et al. (1994). These authors suggest
  an uncertainty of 0.04, but a wider range of values can be found in
  the literature. The value of `metadata$emissivity` is easy to alter,
  either as a single value or as a matrix, yielding flexibility of
  calculation.

- decimate:

  optional positive integer indicating the degree to which the data
  should be subsampled after reading and before storage. Setting this to
  10 can speed up reading by a factor of 3 or more, but higher values
  have diminishing effect. In exploratory work, it is useful to set
  `decimate=10`, to plot the image to determine a subregion of interest,
  and then to use
  [`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md)
  to trim the image.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

## Value

A [landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
object, with the conventional Oce slots `metadata`, `data` and
`processingLog`. The `metadata` is mainly intended for use by Oce
functions, but for generality it also contains an entry named `header`
that represents the full image header in a list (with names made
lower-case). The `data` slot holds matrices of the data in the requested
bands, and users may add extra matrices if desired, e.g. to store
calculated quantities.

## Details

Landsat data are provided in directories that contain TIFF files and
header information, and `read.landsat` relies on a strict convention for
the names of the files in those directories. Those file names were found
by inspection of some data, on the assumption that similar patterns will
hold for other datasets for any given satellite. This is a brittle
approach and it should be born in mind if `read.landsat` fails for a
given dataset.

For Landsat 8, there are 11 bands, with names `"aerosol"` (band 1),
`"blue"` (band 2), `"green"` (band 3), `"red"` (band 4), `"nir"` (band
5), `"swir1"` (band 6), `"swir2"` (band 7), `"panchromatic"` (band 8),
`"cirrus"` (band 9), `"tirs1"` (band 10), and `"tirs2"` (band 11). In
addition to the above, setting `band="terralook"` may be used as an
abbreviation for `band=c("red", "green", "nir")`.

For Landsat 7, there 8 bands, with names `"blue"` (band 1), `"green"`
(band 2), `"red"` (band 3), `"nir"` (band 4), `"swir1"` (band 5),
`"tir1"` (band 6A), `"tir2"` (band 6B), `"swir2"` (band 7) and
`"panchromatic"` (band 8).

For Landsat 4 and 5, the bands similar to Landsat 7 but without
`"panchromatic"` (band 8).

## Storage requirements

Landsat data files (directories, really) are large, accounting for
approximately 1 gigabyte each. The storage of the Oce object is similar
(see
[landsat](https://dankelley.github.io/oce/reference/landsat-class.md)).
In R, many operations involving copying data, so that dealing with
full-scale landsat images can overwhelm computers with storage under
8GB. For this reason, it is typical to read just the bands that are of
interest. It is also helpful to use
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md)
to trim the data to a geographical range, or to use
[`decimate()`](https://dankelley.github.io/oce/reference/decimate.md) to
get a coarse view of the domain, especially early in an analysis.

## References

1.  Konda, M. Imasato N., Nishi, K., and T. Toda, 1994. Measurement of
    the Sea Surface Emissivity. *Journal of Oceanography*, 50, 17:30.
    [doi:10.1007/BF02233853](https://doi.org/10.1007/BF02233853)

## See also

See the documentation for the
[landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
class for more information on `landsat` objects, especially band
information. Use
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md)
to trim Landsat objects geographically and
[`landsatAdd()`](https://dankelley.github.io/oce/reference/landsatAdd.md)
to add new “bands.” The accessor operator (`[[`) is used to access band
information, full or decimated, and to access certain derived
quantities. A sample dataset named
[`landsat()`](https://dankelley.github.io/oce/reference/landsat.md) is
provided by the [oce](https://CRAN.R-project.org/package=oce) package.

Other things related to landsat data:
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md),
`[[<-,landsat-method`,
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`landsat-class`](https://dankelley.github.io/oce/reference/landsat-class.md),
[`landsatAdd()`](https://dankelley.github.io/oce/reference/landsatAdd.md),
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md),
[`plot,landsat-method`](https://dankelley.github.io/oce/reference/plot-landsat-method.md),
[`summary,landsat-method`](https://dankelley.github.io/oce/reference/summary-landsat-method.md)

## Author

Dan Kelley
