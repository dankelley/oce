# Replace Parts of an adv Object

Generally, the `[[` method lets users extract information from `oce`
objects, without having to know the details of the internal storage. For
many `oce` sub-classes, `[[` can also return quantities that are
computed from the object's contents.

## Usage

``` r
# S4 method for class 'adv'
x[[i, j, ...]] <- value
```

## Arguments

- x:

  an [adv](https://dankelley.github.io/oce/reference/adv-class.md)
  object.

- i:

  character value indicating the name of an item to extract.

- j:

  optional additional information on the `i` item.

- ...:

  ignored.

- value:

  The value to be inserted into `x`.

## Details

If the `adv` object holds slow variables (i.e. if `timeSlow` is in the
`data` slot), then assigning to .e.g. `heading` will not actually assign
to a variable of that name, but instead assigns to `headingSlow`. To
catch misapplication of this rule, an error message will be issued if
the assigned value is not of the same length as `timeSlow`.

A two-step process is used to try to find the requested information.
First, a class-specific function is used (see “Details of the
Specialized Method”). If this yields nothing, then a general method is
used (see “Details of the General Method”). If both methods fail, then
`[[` returns NULL.

Some understanding of the subclass is required to know what can be
retrieved with `[[`. When dealing with an unfamiliar subclass, it can be
useful to first use `x[["?"]]` to get a listing of the retrievable
items. See “Details of the Specialized Method” for more information.

## Details of the General Method

Note: the text of this section is identical for all `oce` subclasses,
and so some of what you read here may not be relevant to the class being
described in this help page.

If the specialized method produces no matches, the following generalized
method is applied. As with the specialized method, the procedure hinges
first on the values of `i` and, optionally, `j`. The work proceeds in
steps, by testing a sequence of possible conditions in sequence.

1.  A check is made as to whether `i` names one of the standard `oce`
    slots. If so, `[[` returns the slot contents of that slot. Thus,
    `x[["metadata"]]` will retrieve the `metadata` slot, while
    `x[["data"]]` and `x[["processingLog"]]` return those slots.

2.  If `i` is a string ending in the `"Unit"`, then the characters
    preceding that string are taken to be the name of an item in the
    data object, and a list containing the unit is returned (or `NULL`
    if there is no such unit). This list consists of an item named
    `unit`, which is an
    [`expression()`](https://rdrr.io/r/base/expression.html), and an
    item named `scale`, which is a string describing the measurement
    scale. If the string ends in `" unit"`, e.g.
    `x[["temperature unit"]]` (note the space), then just the expression
    is returned, and if it ends in `" scale"`, then just the scale is
    returned.

3.  If `i` is a string ending in `"Flag"`, then the corresponding
    data-quality flag is returned (or `NULL` if there is no such flag).

4.  If the object holds hydrographic information (pressure, salinity,
    temperature, longitude and latitude) then another set of
    possibilities arises. If `i` is `"sigmaTheta"`, then the value of
    [`swSigmaTheta()`](https://dankelley.github.io/oce/reference/swSigmaTheta.md)
    is called with `x` as the sole argument, and the results are
    returned. Similarly,
    [`swSigma0()`](https://dankelley.github.io/oce/reference/swSigma0.md)
    is used if `i="sigma0"`, and
    [`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md)
    is used if `i="spice"`. Of course, these actions only make sense for
    objects that contain the relevant items within their `data` slot.

5.  After these possibilities are eliminated, the action depends on
    whether `j` has been provided. If `j` is not provided, or is the
    string `""`, then `i` is sought in the `metadata` slot, and then in
    the `data` slot, returning whichever is found first. In other words,
    if `j` is not provided, the `metadata` slot takes preference over
    the `data` slot. However, if `j` is provided, then it must be either
    the string `"metadata"` or `"data"`, and it directs where to look.

6.  If none of the above-listed conditions holds, then `NULL` is
    returned.

## See also

Other functions that extract parts of oce objects:
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
[`[[,bremen-method`](https://dankelley.github.io/oce/reference/sub-sub-bremen-method.md),
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
[`[[,echosounder-method`](https://dankelley.github.io/oce/reference/sub-sub-echosounder-method.md),
[`[[,g1sst-method`](https://dankelley.github.io/oce/reference/sub-sub-g1sst-method.md),
[`[[,gps-method`](https://dankelley.github.io/oce/reference/sub-sub-gps-method.md),
[`[[,ladp-method`](https://dankelley.github.io/oce/reference/sub-sub-ladp-method.md),
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md),
[`[[,lisst-method`](https://dankelley.github.io/oce/reference/sub-sub-lisst-method.md),
[`[[,lobo-method`](https://dankelley.github.io/oce/reference/sub-sub-lobo-method.md),
[`[[,met-method`](https://dankelley.github.io/oce/reference/sub-sub-met-method.md),
[`[[,oce-method`](https://dankelley.github.io/oce/reference/sub-sub-oce-method.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md),
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
[`[[,windrose-method`](https://dankelley.github.io/oce/reference/sub-sub-windrose-method.md),
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md)

Other things related to adv data:
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`adv-class`](https://dankelley.github.io/oce/reference/adv-class.md),
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

## Author

Dan Kelley
