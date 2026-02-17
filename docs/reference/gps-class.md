# Class to Store GPS Data

This class stores GPS data. These objects may be read with
[`read.gps()`](https://dankelley.github.io/oce/reference/read.gps.md) or
assembled with
[`as.gps()`](https://dankelley.github.io/oce/reference/as.gps.md).

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `gps` objects is a
  [list](https://rdrr.io/r/base/list.html) containing the main data for
  the object.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `gps` objects is a
  [list](https://rdrr.io/r/base/list.html) containing information about
  the `data` or about the object itself.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `gps` objects
  is a [list](https://rdrr.io/r/base/list.html) with entries describing
  the creation and evolution of the object. The contents are updated by
  various `oce` functions to keep a record of processing steps. Object
  summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
gps objects (see `[[<-,gps-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a gps object may
be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,gps-method`](https://dankelley.github.io/oce/reference/sub-sub-gps-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,gps-method`](https://dankelley.github.io/oce/reference/sub-sub-gps-method.md)
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

Other things related to gps data:
[`[[,gps-method`](https://dankelley.github.io/oce/reference/sub-sub-gps-method.md),
`[[<-,gps-method`,
[`as.gps()`](https://dankelley.github.io/oce/reference/as.gps.md),
[`plot,gps-method`](https://dankelley.github.io/oce/reference/plot-gps-method.md),
[`read.gps()`](https://dankelley.github.io/oce/reference/read.gps.md),
[`summary,gps-method`](https://dankelley.github.io/oce/reference/summary-gps-method.md)

## Author

Dan Kelley
