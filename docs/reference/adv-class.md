# Class to Store Acoustic-Doppler Velocimeter Data

This class holds data from acoustic-Doppler velocimeters.

## Details

A file containing ADV data is usually recognized by Oce, and so
[`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md)
will usually read the data. If not, one may use the general ADV function
[`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md) or
specialized variants
[`read.adv.nortek()`](https://dankelley.github.io/oce/reference/read.adv.nortek.md),
[`read.adv.sontek.adr()`](https://dankelley.github.io/oce/reference/read.adv.sontek.adr.md)
or
[`read.adv.sontek.text()`](https://dankelley.github.io/oce/reference/read.adv.sontek.text.md).

ADV data may be plotted with
[`plot,adv-method()`](https://dankelley.github.io/oce/reference/plot-adv-method.md)
function, which is a generic function so it may be called simply as
`plot(x)`, where `x` is an adv object.

Statistical summaries of ADV data are provided by the generic function
[`summary,adv-method()`](https://dankelley.github.io/oce/reference/summary-adv-method.md).

Conversion from beam to xyz coordinates may be done with
[`beamToXyzAdv()`](https://dankelley.github.io/oce/reference/beamToXyzAdv.md),
and from xyz to enu (east north up) may be done with
[`xyzToEnuAdv()`](https://dankelley.github.io/oce/reference/xyzToEnuAdv.md).
[`toEnuAdv()`](https://dankelley.github.io/oce/reference/toEnuAdv.md)
may be used to transfer either beam or xyz to enu. Enu may be converted
to other coordinates (e.g. aligned with a coastline) with
[`enuToOtherAdv()`](https://dankelley.github.io/oce/reference/enuToOtherAdv.md).

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `adv` objects is a
  [list](https://rdrr.io/r/base/list.html) containing the main data for
  the object. The key items stored in this slot include `time` and `v`.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `adv` objects is a
  [list](https://rdrr.io/r/base/list.html) containing information about
  the `data` or about the object itself. Examples that are of common
  interest include `frequency`, `oceCordinate`, and `frequency`.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `adv` objects
  is a [list](https://rdrr.io/r/base/list.html) with entries describing
  the creation and evolution of the object. The contents are updated by
  various `oce` functions to keep a record of processing steps. Object
  summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
adv objects (see `[[<-,adv-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a adv object may
be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md)
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
[`section-class`](https://dankelley.github.io/oce/reference/section-class.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`windrose-class`](https://dankelley.github.io/oce/reference/windrose-class.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md)

Other things related to adv data:
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
`[[<-,adv-method`,
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`advSontekAdrFileTrim()`](https://dankelley.github.io/oce/reference/advSontekAdrFileTrim.md),
[`applyMagneticDeclination,adv-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adv-method.md),
[`beamName()`](https://dankelley.github.io/oce/reference/beamName.md),
[`beamToXyz()`](https://dankelley.github.io/oce/reference/beamToXyz.md),
[`enuToOther()`](https://dankelley.github.io/oce/reference/enuToOther.md),
[`enuToOtherAdv()`](https://dankelley.github.io/oce/reference/enuToOtherAdv.md),
[`plot,adv-method`](https://dankelley.github.io/oce/reference/plot-adv-method.md),
[`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md),
[`read.adv.nortek()`](https://dankelley.github.io/oce/reference/read.adv.nortek.md),
[`read.adv.sontek.adr()`](https://dankelley.github.io/oce/reference/read.adv.sontek.adr.md),
[`read.adv.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adv.sontek.serial.md),
[`read.adv.sontek.text()`](https://dankelley.github.io/oce/reference/read.adv.sontek.text.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`summary,adv-method`](https://dankelley.github.io/oce/reference/summary-adv-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdv()`](https://dankelley.github.io/oce/reference/toEnuAdv.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md),
[`xyzToEnuAdv()`](https://dankelley.github.io/oce/reference/xyzToEnuAdv.md)

## Examples

``` r
data(adv)
adv[["v"]] <- 0.001 + adv[["v"]] # add 1mm/s to all velocity components
```
