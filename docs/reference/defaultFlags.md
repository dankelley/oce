# Suggest a Default Flag Vector for Bad or Suspicious Data

`defaultFlags` tries to suggest a reasonable default `flag` scheme for
use by
[`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md).
It does this by looking for an item named `flagScheme` in the `metadata`
slot of `object`. If `flagScheme` is found, and if the scheme is
recognized, then a numeric vector is returned that indicates bad or
questionable data. If `flagScheme$default` exists, then that scheme is
returned. However, if that does not exist, and if `flagScheme$name` is
recognized, then a pre-defined (very conservative) scheme is used, as
listed below.

## Usage

``` r
defaultFlags(object)
```

## Arguments

- object:

  An oce object

## Value

A vector of one or more flag values, or `NULL` if `object` `metadata`
slot lacks a `flagScheme` as set by
[`initializeFlagScheme()`](https://dankelley.github.io/oce/reference/initializeFlagScheme.md),
or if it has a scheme that is not in the list provide in “Description”.

## Details

- for `argo`, the default is `c(0,3,4,6,7,9)`, meaning to act upon
  `not_assessed` (0), `probably_bad` (3), `bad` (4), `not_used_6` (6),
  `not_used_7` (7) and `missing` (9). See Section 3.2.2 of Carval et al.
  (2019).

- for `BODC`, the default is `c(0,2,3,4,5,6,7,8,9)`, i.e. all flags
  except `good`.

- for `DFO`, the default is `c(0,2,3,4,5,8,9)`, i.e. all flags except
  `appears_correct`.

- for `WHP bottle`, the default is `c(1,3,4,5,6,7,8,9)`, i.e. all flags
  except `no_problems_noted`.

- for `WHP ctd`, the default is `c(1,3,4,5,6,7,9)`, i.e. all flags
  except `acceptable`.

## References

- Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida,
  Stephen Loch Loch, Claudia Schmid, and Roger Goldsmith. Argo User's
  Manual V3.3. Ifremer, 2019.
  [doi:10.13155/29825](https://doi.org/10.13155/29825)

## See also

Other functions relating to data-quality flags:
[`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md),
[`handleFlags,adp-method`](https://dankelley.github.io/oce/reference/handleFlags-adp-method.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`handleFlags,ctd-method`](https://dankelley.github.io/oce/reference/handleFlags-ctd-method.md),
[`handleFlags,oce-method`](https://dankelley.github.io/oce/reference/handleFlags-oce-method.md),
[`handleFlags,section-method`](https://dankelley.github.io/oce/reference/handleFlags-section-method.md),
[`initializeFlagScheme()`](https://dankelley.github.io/oce/reference/initializeFlagScheme.md),
[`initializeFlagScheme,ctd-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-ctd-method.md),
[`initializeFlagScheme,oce-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-oce-method.md),
[`initializeFlagScheme,section-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-section-method.md),
[`initializeFlagSchemeInternal()`](https://dankelley.github.io/oce/reference/initializeFlagSchemeInternal.md),
[`initializeFlags()`](https://dankelley.github.io/oce/reference/initializeFlags.md),
[`initializeFlags,adp-method`](https://dankelley.github.io/oce/reference/initializeFlags-adp-method.md),
[`initializeFlags,oce-method`](https://dankelley.github.io/oce/reference/initializeFlags-oce-method.md),
[`initializeFlagsInternal()`](https://dankelley.github.io/oce/reference/initializeFlagsInternal.md),
[`setFlags()`](https://dankelley.github.io/oce/reference/setFlags.md),
[`setFlags,adp-method`](https://dankelley.github.io/oce/reference/setFlags-adp-method.md),
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`setFlags,oce-method`](https://dankelley.github.io/oce/reference/setFlags-oce-method.md)
