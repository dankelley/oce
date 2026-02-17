# Extract Something From a landsat Object

Generally, the `[[` method lets users extract information from `oce`
objects, without having to know the details of the internal storage. For
many `oce` sub-classes, `[[` can also return quantities that are
computed from the object's contents.

## Usage

``` r
# S4 method for class 'landsat'
x[[i, j, ...]]
```

## Arguments

- x:

  a
  [landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
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

If `i` is `"?"`, then the return value is a list containing four items,
each of which is a character vector holding the names of things that can
be accessed with `[[`. The `data` and `metadata` items hold the names of
entries in the object's data and metadata slots, respectively. The
`data` entries are difficult to deal with directly, and so users are
advised to use `dataDerived` instead.

*Accessing band data.* The data may be accessed with e.g.
`landsat[["panchromatic"]]`, for the panchromatic band. If a new “band”
is added with
[`landsatAdd()`](https://dankelley.github.io/oce/reference/landsatAdd.md),
it may be referred by name. In all cases, a second argument can be
provided, to govern decimation. If this is missing, all the relevant
data are returned. If this is present and equal to `TRUE`, then the data
will be automatically decimated (subsampled) to give approximately 800
elements in the longest side of the matrix. If this is present and
numerical, then its value governs decimation. For example,
`landsat[["panchromatic",TRUE]]` will auto-decimate, typically reducing
the grid width and height from 16000 to about 800. Similarly,
`landsat[["panchromatic",10]]` will reduce width and height to about
1600. On machines with limited RAM (e.g. under about 6GB), decimation is
a good idea in almost all processing steps. It also makes sense for
plotting, and in fact is done through the \`decimate\` argument of
[`plot,landsat-method()`](https://dankelley.github.io/oce/reference/plot-landsat-method.md).

*Accessing derived data.* One may retrieve several derived quantities
that are calculated from data stored in the object:
`landsat[["longitude"]]` and `landsat[["latitude"]]` give pixel
locations. Accessing `landsat[["temperature"]]` creates an estimate of
ground temperature as follows (see reference 4). First, the “count
value” in band 10, denoted \\b\_{10}\\ say, is scaled with coefficients
stored in the image metadata using \\\lambda_L=b\_{10}M_L+A_L\\ where
\\M_L\\ and \\A_L\\ are values stored in the metadata (e.g. the first in
`landsat@metadata$header$radiance_mult_band_10`) Then the result is
used, again with coefficients in the metadata, to compute Celcius
temperature \\T=K_2/ln(\epsilon K_1/\lambda_L+1)-273.15\\. The value of
the emissivity \\\epsilon\\ is set to unity by
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md),
although it can be changed easily later, by assigning a new value to
\`landsat@metadata\$emissivity\`. The default emissivity value set by
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md)
is from reference 11, and is within the oceanic range suggested by
reference 5. Adjustment is as simple as altering
\`landsat@metadata\$emissivity\`. This value can be a single number
meant to apply for the whole image, or a matrix with dimensions matching
those of band 10. The matrix case is probably more useful for images of
land, where one might wish to account for the different emissivities of
soil and vegetation, etc.; for example, Table 4 of reference 9 lists
0.9668 for soil and 0.9863 for vegetation, while Table 5 of reference 10
lists 0.971 and 0.987 for the same quantities.

*Accessing metadata.* Anything in the metadata can be accessed by name,
e.g. `landsat[["time"]]`. Note that some items are simply copied over
from the source data file and are not altered by e.g. decimation. An
exception is the lat-lon box, which is altered by
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md).

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

Other things related to landsat data: `[[<-,landsat-method`,
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`landsat-class`](https://dankelley.github.io/oce/reference/landsat-class.md),
[`landsatAdd()`](https://dankelley.github.io/oce/reference/landsatAdd.md),
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md),
[`plot,landsat-method`](https://dankelley.github.io/oce/reference/plot-landsat-method.md),
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md),
[`summary,landsat-method`](https://dankelley.github.io/oce/reference/summary-landsat-method.md)

## Author

Dan Kelley
