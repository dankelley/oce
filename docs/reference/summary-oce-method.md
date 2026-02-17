# Summarize an oce Object

Provide a textual summary of some pertinent aspects of the object,
including selected components of its `metadata` slot, statistical and
dimensional information on the entries in the `data` slot, and a listing
of the contents of its `processingLog` slot. The details depend on the
class of the object, especially for the `metadata` slot, so it can help
to consult the specialized documentation, e.g.
[summary,ctd-method](https://dankelley.github.io/oce/reference/summary-ctd-method.md)
for CTD objects (i.e. objects inheriting from the
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) class.) It
is important to note that this is not a good way to learn the details of
the object contents. Instead, for an object named `object`, say, one
might use `str(object)` to learn about all the contents, or
`str(object[["metadata"]])` to learn about the `metadata`, etc.

## Usage

``` r
# S4 method for class 'oce'
summary(object, ...)
```

## Arguments

- object:

  The object to be summarized.

- ...:

  Extra arguments (ignored)

## Examples

``` r
o <- new("oce")
summary(o)
#> * Processing Log
#> 
#>     - 2026-02-17 20:58:32 UTC: `Create oce object`
```
