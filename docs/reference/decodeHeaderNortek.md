# Decode a Nortek Header

Decode data in a Nortek ADV or ADP header.

## Usage

``` r
decodeHeaderNortek(
  buf,
  type = c("aquadoppHR", "aquadoppProfiler", "aquadopp", "aquadoppPlusMagnetometer",
    "vector"),
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- buf:

  a “raw” buffer containing the header

- type:

  type of device

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- ...:

  additional arguments, passed to called routines.

## Value

A list containing elements `hardware`, `head`, `user` and `offset`. The
easiest way to find the contents of these is to run this function with
`debug=3`.

## Details

Decodes the header in a binary-format Nortek ADV/ADP file. This function
is designed to be used by
[`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md)
and
[`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md),
but can be used directly as well. The code is based on information in
the Nortek System Integrator Guide (2008) and on postings on the Nortek
“knowledge center” discussion board. One might assume that the latter is
less authoritative than the former. For example, the inference of cell
size follows advice found at
https://www.nortekusa.com/en/knowledge-center/forum/hr-profilers/736804717,
which contains a typo in an early posting that is corrected later on.

## References

1.  Information on Nortek profilers (including the System Integrator
    Guide, which explains the data format byte-by-byte) is available at
    `https://www.nortekusa.com/usa?set_language=usa` after login.

2.  The Nortek Knowledge Center
    https://www.nortekusa.com/en/knowledge-center may be of help if
    problems arise in dealing with data from Nortek instruments.

3.  Nortek, "Classic Integrators Guide: Aquadopp \| Aquadopp DW \|
    Aquadopp Profiler \| HQ Aquadopp Profiler \| Vector \| AWAC." Nortek
    AS, 2022.

## See also

Most users should employ the functions
[`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md)
and
[`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md)
instead of this one.

## Author

Dan Kelley and Clark Richards
