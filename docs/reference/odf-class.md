# Class to Store ODF Data

This class is for data stored in a format used at Canadian Department of
Fisheries and Oceans laboratories. It is somewhat similar to the
[bremen](https://dankelley.github.io/oce/reference/bremen-class.md)
class, in the sense that it does not apply just to a particular
instrument.

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `odf` objects is a
  [list](https://rdrr.io/r/base/list.html) containing the main data for
  the object.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `odf` objects is a
  [list](https://rdrr.io/r/base/list.html) containing information about
  the `data` or about the object itself.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `odf` objects
  is a [list](https://rdrr.io/r/base/list.html) with entries describing
  the creation and evolution of the object. The contents are updated by
  various `oce` functions to keep a record of processing steps. Object
  summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
odf objects (see `[[<-,odf-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a odf object may
be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md)
operator can also be used to retrieve items from within the `data` and
`metadata` slots. For example, `o[["temperature"]]` can be used to
retrieve temperature from an object containing that quantity. The rule
is that a named quantity is sought first within the object's `metadata`
slot, with the `data` slot being checked only if `metadata` does not
contain the item. This `[[` method can also be used to get certain
derived quantities, if the object contains sufficient information to
calculate them. For example, an object that holds (practical) salinity,
temperature and pressure, along with longitude and latitude, has
sufficient information to compute Absolute Salinity, and so `o[["SA"]]`
will yield the calculated Absolute Salinity.

It is also possible to find items more directly, using
[`oceGetData()`](https://dankelley.github.io/oce/reference/oceGetData.md)
and
[`oceGetMetadata()`](https://dankelley.github.io/oce/reference/oceGetMetadata.md),
but neither of these functions can retrieve derived items.

## References

1.  Anthony W. Isenor and David Kellow, 2011. *ODF Format Specification
    Version 2.0*. (This is a .doc file obtained in June 2011 by Dan
    Kelley, which no longer seems to be made available at any DFO
    website.)

2.  (Unknown authors), October 2014. *ODF Format Description (MLI)*,
    `https://ogsl.ca/wp-content/uploads/ODF_format_desc_en_0.pdf`, (Link
    worked early on March 16, 2022, but failed later that day.)

3.  A sample ODF file in the DFO format is available at
    `system.file("extdata","CTD_BCD2014666_008_1_DN.ODF.gz",package="oce")`

4.  A sample ODF file in the MLI format may be available at
    `https://ogsl.ca/wp-content/uploads/ODF_file_example_en_0.pdf`.
    (Link worked early on March 16, 2022, but failed later that day.)

## See also

Other things related to odf data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md),
[`ODFListFromHeader()`](https://dankelley.github.io/oce/reference/ODFListFromHeader.md),
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
`[[<-,odf-method`,
[`plot,odf-method`](https://dankelley.github.io/oce/reference/plot-odf-method.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`summary,odf-method`](https://dankelley.github.io/oce/reference/summary-odf-method.md)

Other classes provided by oce:
[`adp-class`](https://dankelley.github.io/oce/reference/adp-class.md),
[`adv-class`](https://dankelley.github.io/oce/reference/adv-class.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`bremen-class`](https://dankelley.github.io/oce/reference/bremen-class.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`coastline-class`](https://dankelley.github.io/oce/reference/coastline-class.md),
[`ctd-class`](https://dankelley.github.io/oce/reference/ctd-class.md),
[`lisst-class`](https://dankelley.github.io/oce/reference/lisst-class.md),
[`lobo-class`](https://dankelley.github.io/oce/reference/lobo-class.md),
[`met-class`](https://dankelley.github.io/oce/reference/met-class.md),
[`oce-class`](https://dankelley.github.io/oce/reference/oce-class.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`section-class`](https://dankelley.github.io/oce/reference/section-class.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`windrose-class`](https://dankelley.github.io/oce/reference/windrose-class.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md)

## Author

Dan Kelley
