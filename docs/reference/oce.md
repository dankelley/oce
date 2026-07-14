# A Package for Oceanographic Analysis

The oce package provides functions for working with Oceanographic data,
for calculations that are specific to Oceanography, and for producing
graphics that match the conventions of the field.

## Details

Over a dozen specialized data types are handled by oce, with generic
plots and summaries for each, along with the specialized functions
needed for typical Oceanographic analysis.

See [oce](https://dankelley.github.io/oce/reference/oce-class.md) for a
summary of the class structure and links to documentation for the many
subclasses of oce objects, each aligned with a class of instrument or or
type of dataset. For a more task-oriented approach, see the several
vignettes that are provided with oce, and a book (Kelley, Dan E.
Oceanographic Analysis with R. New York: Springer-Verlag, 2018.
https://link.springer.com/book/10.1007/978-1-4939-8844-0) written by one
of the oce co-authors.

## Specialized Functions

A key function is
[`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md),
which will attempt to read Oceanographic data in raw format. This uses
[`oceMagic()`](https://dankelley.github.io/oce/reference/oceMagic.md) to
try to detect the file type, based on the file name and contents. If
this detection is not possible, users will need to go beyond
[`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md),
using a more specialized function, e.g.
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
for CTD files,
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
for Teledyne-Seabird files, etc.

## Generic Methods

A list of the generic methods in oce is provided by
[methods](https://rdrr.io/r/utils/methods.html)`(class="oce")`; a few
that are used frequently are as follows.

- `[[` Finds the value of an item in the object's `metadata` or `data`
  slot. If the item does not exist, but can be calculated from the other
  items, then the calculated value is returned. As an example of the
  latter, consider the built-in `ctd` dataset, which does not contain
  potential temperature, "`theta`". Using `ctd[["theta"]]` therefore
  causes
  [`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md) to
  be called, to calculate `theta`. See
  [\[\[,oce-method](https://dankelley.github.io/oce/reference/sub-sub-oce-method.md)
  or type
  [`?"[[,oce-method"`](https://dankelley.github.io/oce/reference/sub-sub-oce-method.md)
  to learn more about general functioning, or a specialized method like
  [\[\[,ctd-method](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md)
  for CTD data, etc.

- `[[<-` Alters the named item in the object's `metadata` or `data`
  slot. If the item does not exist, it is created. See
  \[\[\<-,oce-method or type `?"[[<-,oce-method"` to learn more about
  the general methodology, or a specialized method like
  \[\[\<-,ctd-method for CTD data, etc.

- [`summary()`](https://rdrr.io/r/base/summary.html) Displays some
  information about the object named as an argument, including a few
  elements from its `metadata` slot and some statistics of the contents
  of its `data` slot. See
  [summary,oce-method](https://dankelley.github.io/oce/reference/summary-oce-method.md)
  or type
  [`?"summary,oce-method"`](https://dankelley.github.io/oce/reference/summary-oce-method.md)
  to learn more about general functioning, or a specialized method like
  [summary,ctd-method](https://dankelley.github.io/oce/reference/summary-ctd-method.md)
  for CTD data, etc.

- [`subset()`](https://rdrr.io/r/base/subset.html) Takes a subset of an
  oce object. See
  [subset,oce-method](https://dankelley.github.io/oce/reference/subset-oce-method.md)
  or type
  [`?"subset,oce-method"`](https://dankelley.github.io/oce/reference/subset-oce-method.md)
  to learn more about general functioning, or a specialized method like
  [subset,ctd-method](https://dankelley.github.io/oce/reference/subset-ctd-method.md)
  for CTD data, etc.

## See also

Useful links:

- <https://dankelley.github.io/oce/>

- Report bugs at <https://github.com/dankelley/oce/issues>

## Author

**Maintainer**: Dan Kelley <Dan.Kelley@Dal.Ca>
([ORCID](https://orcid.org/0000-0001-7808-5911))

Authors:

- Clark Richards <clark.richards@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-7833-206X))

Other contributors:

- Chantelle Layton <chantelle.layton@dal.ca>
  ([ORCID](https://orcid.org/0000-0002-3199-5763)) (curl() coauthor)
  \[contributor\]

- British Geological Survey (magnetic-field subroutine) \[contributor,
  copyright holder\]
