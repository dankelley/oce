# Class to Store Hydrographic Section Data

This class stores data from oceanographic section surveys.

## Details

Sections can be read with
[`read.section()`](https://dankelley.github.io/oce/reference/read.section.md)
or created with
[`read.section()`](https://dankelley.github.io/oce/reference/read.section.md)
or created from CTD objects by using
[`as.section()`](https://dankelley.github.io/oce/reference/as.section.md)
or by adding a ctd station to an existing section with
[`sectionAddStation()`](https://dankelley.github.io/oce/reference/sectionAddStation.md).

Sections may be sorted with
[`sectionSort()`](https://dankelley.github.io/oce/reference/sectionSort.md),
subsetted with
[`subset,section-method()`](https://dankelley.github.io/oce/reference/subset-section-method.md),
smoothed with
[`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md),
and gridded with
[`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md).
A "spine" may be added to a section with
[`addSpine()`](https://dankelley.github.io/oce/reference/addSpine.md).
Sections may be summarized with
[`summary,section-method()`](https://dankelley.github.io/oce/reference/summary-section-method.md)
and plotted with
[`plot,section-method()`](https://dankelley.github.io/oce/reference/plot-section-method.md).

The sample dataset
[`section()`](https://dankelley.github.io/oce/reference/section.md)
contains data along WOCE line A03.

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `section` objects is a
  [list](https://rdrr.io/r/base/list.html) containing the main data for
  the object.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `section` objects
  is a [list](https://rdrr.io/r/base/list.html) containing information
  about the `data` or about the object itself. Examples that are of
  common interest include `stationId`, `longitude`, `latitude` and
  `time`.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `section`
  objects is a [list](https://rdrr.io/r/base/list.html) with entries
  describing the creation and evolution of the object. The contents are
  updated by various `oce` functions to keep a record of processing
  steps. Object summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
section objects (see `[[<-,section-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a section object
may be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md)
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
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`windrose-class`](https://dankelley.github.io/oce/reference/windrose-class.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md)

Other things related to section data:
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md),
`[[<-,section-method`,
[`as.section()`](https://dankelley.github.io/oce/reference/as.section.md),
[`handleFlags,section-method`](https://dankelley.github.io/oce/reference/handleFlags-section-method.md),
[`initializeFlagScheme,section-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-section-method.md),
[`plot,section-method`](https://dankelley.github.io/oce/reference/plot-section-method.md),
[`read.section()`](https://dankelley.github.io/oce/reference/read.section.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`sectionAddStation()`](https://dankelley.github.io/oce/reference/sectionAddStation.md),
[`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md),
[`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md),
[`sectionSort()`](https://dankelley.github.io/oce/reference/sectionSort.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`summary,section-method`](https://dankelley.github.io/oce/reference/summary-section-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(section)
plot(section[["station", 1]])

pairs(cbind(z = -section[["pressure"]], T = section[["temperature"]], S = section[["salinity"]]))

# T profiles for first few stations in section, at common scale
par(mfrow = c(3, 3))
Tlim <- range(section[["temperature"]])
ylim <- rev(range(section[["pressure"]]))
for (stn in section[["station", 1:9]]) {
    plotProfile(stn, xtype = "potential temperature", ylim = ylim, Tlim = Tlim)
}

```
