# Class to Store AMSR-2 Satellite Data

This class stores data from the AMSR-2 satellite.

## Details

The Advanced Microwave Scanning Radiometer (AMSR-2) is in current
operation on the Japan Aerospace Exploration Agency (JAXA) GCOM-W1 space
craft, launched in May 2012. Data are processed by Remote Sensing
Systems. The satellite completes an ascending and descending pass during
local daytime and nighttime hours respectively. Each daily file contains
7 daytime and 7 nighttime maps of variables named as follows within the
`data` slot of amsr objects: `timeDay`, `SSTDay`, `LFwindDay` (wind at
10m sensed in the 10.7GHz band), `MFwindDay` (wind at 10m sensed at
18.7GHz), `vaporDay`, `cloudDay`, and `rainDay`, along with
similarly-named items that end in `Night`. See reference 1 for
additional information on the instrument, how to cite the data source in
a paper, etc.

The bands are stored in [`raw()`](https://rdrr.io/r/base/raw.html) form,
to save storage. The accessor function
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md)
can provide these values in `raw` form or in physical units;
[`plot,amsr-method()`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
and
[`summary,amsr-method()`](https://dankelley.github.io/oce/reference/summary-amsr-method.md)
work with physical units.

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `amsr` objects is a
  [list](https://rdrr.io/r/base/list.html) containing the main data for
  the object.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `amsr` objects is a
  [list](https://rdrr.io/r/base/list.html) containing information about
  the `data` or about the object itself. Examples that are of common
  interest include `longitude` and `latitude`, which define the grid.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `amsr` objects
  is a [list](https://rdrr.io/r/base/list.html) with entries describing
  the creation and evolution of the object. The contents are updated by
  various `oce` functions to keep a record of processing steps. Object
  summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
amsr objects (see `[[<-,amsr-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a amsr object
may be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md)
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

1.  Information on the satellite, how to cite the data, etc. is provided
    at `http://www.remss.com/missions/amsr/`.

2.  A simple interface for viewing and downloading data is at
    `http://images.remss.com/amsr/amsr2_data_daily.html`.

## See also

Other classes holding satellite data:
[`g1sst-class`](https://dankelley.github.io/oce/reference/g1sst-class.md),
[`landsat-class`](https://dankelley.github.io/oce/reference/landsat-class.md),
[`satellite-class`](https://dankelley.github.io/oce/reference/satellite-class.md)

Other things related to amsr data:
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
`[[<-,amsr-method`,
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`composite,amsr-method`](https://dankelley.github.io/oce/reference/composite-amsr-method.md),
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`summary,amsr-method`](https://dankelley.github.io/oce/reference/summary-amsr-method.md)

## Author

Dan Kelley and Chantelle Layton
