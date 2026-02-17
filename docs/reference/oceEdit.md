# Edit an Oce Object

Edit an element of an oce object, inserting a note in the processing log
of the returned object.

## Usage

``` r
oceEdit(
  x,
  item,
  value,
  action,
  reason = "",
  person = "",
  debug = getOption("oceDebug")
)
```

## Arguments

- x:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object. The exact action of `oceEdit()` depends on the sub-class of
  `x`.

- item:

  if supplied, a character string naming an item in the object's
  `metadata` or `data` slot, the former being checked first. An
  exception is if `item` starts with `"data@"` or `"metadata@"`, in
  which case the named slot is updated with a changed value of the
  contents of `item` after the `@` character.

- value:

  new value for `item`, if both supplied.

- action:

  optional character string containing R code to carry out some action
  on the object.

- reason:

  character string giving the reason for the change.

- person:

  character string giving the name of person making the change.

- debug:

  an integer that specifies a level of debugging, with 0 or less
  indicating no debugging, and 1 or more indicating debugging.

## Value

A [oce](https://dankelley.github.io/oce/reference/oce-class.md) object,
altered appropriately, and with a log item indicating the nature of the
alteration.

## Details

There are several ways to use this function.

1.  If both an `item` and `value` are supplied, then either the object's
    metadata or data slot may be altered. There are two ways in which
    this can be done.

    - Case 1A. If the `item` string does not contain an `@` character,
      then the `metadata` slot is examined for an entry named `item`,
      and that is modified if so. Alternatively, if `item` is found in
      `metadata`, then that value is modified. However, if `item` is not
      found in either `metadata` or `data`, then an error is reported
      (see 1B for how to add something that does not yet exist).

    - Case 1B. If the `item` string contains the `@` character, then the
      text to the left of that character must be either `"metadata"` or
      `"data"`, and it names the slot in which the change is done. In
      contrast with case 1A, this will *create* a new item, if it is not
      already in existence.

2.  If `item` and `value` are not supplied, then `action` must be
    supplied. This is a character string specifying some action to be
    performed on the object, e.g. a manipulation of a column. The action
    must refer to the object as `x`, as in Example 2.

In any case, a log entry is stored in the object, to document the
change. Indeed, this is the main benefit to using this function, instead
of altering the object directly. The log entry will be most useful if it
contains a brief note on the `reason` for the change, and the name of
the `person` doing the work.

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(ctd)
# Example 1: change latitude
ctd2 <- oceEdit(ctd,
    item = "latitude", value = 47.8879,
    reason = "illustration", person = "Dan Kelley"
)
# Example 2: add 0.1 dbar to pressure
ctd3 <- oceEdit(ctd, action = "x@data$pressure<-x@data$pressure+0.1")
```
