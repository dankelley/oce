# Extract Something From an argo Object

Generally, the `[[` method lets users extract information from `oce`
objects, without having to know the details of the internal storage. For
many `oce` sub-classes, `[[` can also return quantities that are
computed from the object's contents.

## Usage

``` r
# S4 method for class 'argo'
x[[i, j, ...]]
```

## Arguments

- x:

  an [argo](https://dankelley.github.io/oce/reference/argo-class.md)
  object.

- i:

  character value indicating the name of an item to extract.

- j:

  optional additional information on the `i` item.

- ...:

  ignored.

## Details

A two-step process is used to try to find the requested information.
First, a class-specific function is used (see “Details of the
Specialized Method”). If this yields nothing, then a general method is
used (see “Details of the General Method”). If both methods fail, then
`[[` returns NULL.

Some understanding of the subclass is required to know what can be
retrieved with `[[`. When dealing with an unfamiliar subclass, it can be
useful to first use `x[["?"]]` to get a listing of the retrievable
items. See “Details of the Specialized Method” for more information.

## Details of the Specialized Method

Note that
[argo](https://dankelley.github.io/oce/reference/argo-class.md) data may
contain both unadjusted data and adjusted data. By default, this
extraction function refers to the former, but a preference for the
latter may be set with
[`preferAdjusted()`](https://dankelley.github.io/oce/reference/preferAdjusted.md),
the documentation of which explains (fairly complex) details.

The results from `argo[[i]]` or `argo[[i,j]]` depend on the nature of
`i` and (if provided) `j`. The details are as follows.

- If `i` is `"?"`, then the return value is a list containing four
  items, each of which is a character vector holding the names of things
  that can be accessed with `[[`. The `data` and `metadata` items hold
  the names of entries in the object's data and metadata slots,
  respectively. The `dataDerived` and `metadataDerived` items hold the
  names of things that can be inferred from the object's contents, e.g.
  `"SA"` is named in `dataDerived`, indicating that `argo[["SA"]]` is
  permitted (to compute Absolute Salinity).

- If `i` is `"profile"` and `j` is an integer vector, then an argo
  object is returned, as specified by `j`. For example,
  `argo[["profile", 2:5]]` is equivalent to
  `subset(argo, profile %in% 2:5)`.

- If `i` is `"CT"`, then Conservative Temperature is returned, as
  computed with
  [`gsw::gsw_CT_from_t`](http://teos-10.github.io/GSW-R/reference/gsw_CT_from_t.md)`(SA,t,p)`,
  where first `SA` is computed as explained in the next item, `t` is
  in-situ temperature, and `p` is pressure.

- If `i` is `"N2"`, then the square of buoyancy is returned, as computed
  with [`swN2()`](https://dankelley.github.io/oce/reference/swN2.md).

- If `i` is `"SA"`, then Absolute Salinity is returned, as computed with
  [`gsw::gsw_SA_from_SP()`](http://teos-10.github.io/GSW-R/reference/gsw_SA_from_SP.md).

- If `i` is `"sigmaTheta"`, then potential density anomaly (referenced
  to zero pressure) is computed, with
  [`swSigmaTheta()`](https://dankelley.github.io/oce/reference/swSigmaTheta.md),
  where the equation of state is taken to be
  [getOption](https://rdrr.io/r/base/options.html)`("oceEOS", default="gsw")`.

- If `i` is `"sigma0"`, `"sigma1"`, `"sigma2"`, `"sigma3"` or
  `"sigma4"`, then the associated function in the
  [gsw](https://CRAN.R-project.org/package=gsw) package. For example,
  `"sigma0"` uses
  [`gsw::gsw_sigma0()`](http://teos-10.github.io/GSW-R/reference/gsw_sigma0.md),
  which returns potential density anomaly referenced to 0 dbar,
  according to the gsw equation of state.

- If `i` is `"theta"`, then potential temperature (referenced to zero
  pressure) is computed, with
  [`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md),
  where the equation of state is taken to be
  [getOption](https://rdrr.io/r/base/options.html)`("oceEOS", default="gsw")`.

- If `i` is `"depth"`, then a matrix of depths is returned.

- If `i` is `"id"` or `"ID"`, then the `id` element within the
  `metadata` slot is returned.

- If `i` is in the `data` slot of `x`, then it is returned, otherwise if
  it is in the `metadata` slot, then that is returned, otherwise `NULL`
  is returned.

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
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
`[[<-,adv-method`

Other things related to argo data:
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
`[[<-,argo-method`,
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`argoGrid()`](https://dankelley.github.io/oce/reference/argoGrid.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`as.argo()`](https://dankelley.github.io/oce/reference/as.argo.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md),
[`read.argo.copernicus()`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

## Author

Dan Kelley

## Examples

``` r
data(argo)
# 1. show that dataset has 223 profiles, each with 56 levels
dim(argo[["temperature"]])
#> [1]  56 223

# 2. show importance of focussing on data flagged 'good'
fivenum(argo[["salinity"]], na.rm = TRUE)
#> [1]  0.000 34.890 34.926 35.042 47.899
fivenum(argo[["salinity"]][argo[["salinityFlag"]] == 1], na.rm = TRUE)
#> [1] 32.850 34.890 34.926 35.042 35.842
```
