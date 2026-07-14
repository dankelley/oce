# Class to Store Echosounder Data

This class stores echosounder data. Echosounder objects may be read with
[`read.echosounder()`](https://dankelley.github.io/oce/reference/read.echosounder.md),
summarized with
[`summary,echosounder-method()`](https://dankelley.github.io/oce/reference/summary-echosounder-method.md),
and plotted with
[`plot,echosounder-method()`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md).
The
[`findBottom()`](https://dankelley.github.io/oce/reference/findBottom.md)
function infers the ocean bottom from tracing the strongest reflector
from ping to ping.

## Details

- An infrequently updated record of the instrument position, in
  `timeSlow`, `longitudeSlow` and `latitudeSlow`. These are used in
  plotting maps with
  [`plot,echosounder-method()`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md).

- An interpolated record of the instrument position, in `time`,
  `longitude`, and `latitude`. Linear interpolation is used to infer the
  longitude and latitude from the variables listed above.

- `depth`, vector of depths of echo samples (measured positive downwards
  in the water column). This is calculated from the inter-sample time
  interval and the sound speed provided as the `soundSpeed` argument to
  [`read.echosounder()`](https://dankelley.github.io/oce/reference/read.echosounder.md),
  so altering the value of the latter will alter the echosounder plots
  provided by
  [`plot,echosounder-method()`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md).

- The echosounder signal amplitude `a`, a matrix whose number of rows
  matches the length of `time`, etc., and number of columns equal to the
  length of `depth`. Thus, for example, `a[100,]` represents the
  depth-dependent amplitude at the time of the 100th ping.

- A matrix named `b` exists for dual-beam and split-beam cases. For
  dual-beam data, this is the wide-beam data, whereas `a` is the
  narrow-beam data. For split-beam data, this is the x-angle data.

- A matrix named `c` exists for split-beam data, containing the y-angle
  data.

- In addition to these matrices, ad-hoc calculated matrices named `Sv`
  and `TS` may be accessed as explained in the next section.

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `echosounder` objects
  is a [list](https://rdrr.io/r/base/list.html) containing the main data
  for the object.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `echosounder`
  objects is a [list](https://rdrr.io/r/base/list.html) containing
  information about the `data` or about the object itself.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `echosounder`
  objects is a [list](https://rdrr.io/r/base/list.html) with entries
  describing the creation and evolution of the object. The contents are
  updated by various `oce` functions to keep a record of processing
  steps. Object summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
echosounder objects (see `[[<-,echosounder-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a echosounder
object may be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,echosounder-method`](https://dankelley.github.io/oce/reference/sub-sub-echosounder-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,echosounder-method`](https://dankelley.github.io/oce/reference/sub-sub-echosounder-method.md)
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

Other things related to echosounder data:
[`[[,echosounder-method`](https://dankelley.github.io/oce/reference/sub-sub-echosounder-method.md),
`[[<-,echosounder-method`,
[`as.echosounder()`](https://dankelley.github.io/oce/reference/as.echosounder.md),
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`findBottom()`](https://dankelley.github.io/oce/reference/findBottom.md),
[`plot,echosounder-method`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md),
[`read.echosounder()`](https://dankelley.github.io/oce/reference/read.echosounder.md),
[`subset,echosounder-method`](https://dankelley.github.io/oce/reference/subset-echosounder-method.md),
[`summary,echosounder-method`](https://dankelley.github.io/oce/reference/summary-echosounder-method.md)

## Author

Dan Kelley
