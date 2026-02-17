# Class to Store Sealevel Data

This class stores sealevel data, e.g. from a tide gauge.

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `sealevel` objects is a
  [list](https://rdrr.io/r/base/list.html) containing the main data for
  the object. The key items stored in this slot are `time` and
  `elevation`.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `sealevel` objects
  is a [list](https://rdrr.io/r/base/list.html) containing information
  about the `data` or about the object itself. An example of the former
  might be the location at which a `sealevel` measurement was made,
  stored in `longitude` and `latitude`, and of the latter might be
  `filename`, the name of the data source.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `sealevel`
  objects is a [list](https://rdrr.io/r/base/list.html) with entries
  describing the creation and evolution of the object. The contents are
  updated by various `oce` functions to keep a record of processing
  steps. Object summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
sealevel objects (see `[[<-,sealevel-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a sealevel
object may be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md)
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

## See also

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
[`odf-class`](https://dankelley.github.io/oce/reference/odf-class.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`section-class`](https://dankelley.github.io/oce/reference/section-class.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`windrose-class`](https://dankelley.github.io/oce/reference/windrose-class.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md)

Other things related to sealevel data:
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
`[[<-,sealevel-method`,
[`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md),
[`plot,sealevel-method`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md),
[`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`summary,sealevel-method`](https://dankelley.github.io/oce/reference/summary-sealevel-method.md)

## Author

Dan Kelley
