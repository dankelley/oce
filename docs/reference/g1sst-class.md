# Class to Store G1SST Satellite/Model Data

This class stores G1SST model-satellite products.

## Details

G1SST is an acronym for global 1-km sea surface temperature, a product
that combines satellite data with the model output. It is provided by
the JPO ROMS (Regional Ocean Modelling System) modelling group. See the
JPL website (reference 1) to learn more about the data, and see the
[`read.g1sst()`](https://dankelley.github.io/oce/reference/read.g1sst.md)
documentation for an example of downloading and plotting.

It is important not to regard G1SST data in the same category as, say,
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md) data,
because the two products differ greatly with respect to cloud cover. The
satellite used by
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md) has the
ability to sense water temperature even if there is cloud cover, whereas
`g1sst` fills in cloud gaps with model simulations. It can be helpful to
consult reference 1 for a given time, clicking and then unclicking the
radio button that turns off the model-based filling of cloud gaps.

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `g1sst` objects is a
  [list](https://rdrr.io/r/base/list.html) containing the main data for
  the object.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `g1sst` objects is
  a [list](https://rdrr.io/r/base/list.html) containing information
  about the `data` or about the object itself.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `g1sst`
  objects is a [list](https://rdrr.io/r/base/list.html) with entries
  describing the creation and evolution of the object. The contents are
  updated by various `oce` functions to keep a record of processing
  steps. Object summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
g1sst objects (see `[[<-,g1sst-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a g1sst object
may be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,g1sst-method`](https://dankelley.github.io/oce/reference/sub-sub-g1sst-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,g1sst-method`](https://dankelley.github.io/oce/reference/sub-sub-g1sst-method.md)
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

1.  JPO OurOcean Portal `https://ourocean.jpl.nasa.gov/SST/` (link
    worked in 2016 but was seen to fail 2017 Feb 2).

## See also

Other classes holding satellite data:
[`amsr-class`](https://dankelley.github.io/oce/reference/amsr-class.md),
[`landsat-class`](https://dankelley.github.io/oce/reference/landsat-class.md),
[`satellite-class`](https://dankelley.github.io/oce/reference/satellite-class.md)

Other things related to g1sst data:
[`[[,g1sst-method`](https://dankelley.github.io/oce/reference/sub-sub-g1sst-method.md),
`[[<-,g1sst-method`,
[`read.g1sst()`](https://dankelley.github.io/oce/reference/read.g1sst.md)

## Author

Dan Kelley
