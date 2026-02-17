# Append an Item to a Processing Log

Append an Item to a Processing Log

## Usage

``` r
processingLogAppend(h, value = "")
```

## Arguments

- h:

  either the `processingLog` slot of an object, or an `oce` object from
  which the processingLog will be extracted

- value:

  A string indicating the text of the log entry.

## Value

An [`list()`](https://rdrr.io/r/base/list.html) containing items named
`time` and `value`, i.e. the times of entries and the text notations of
those entries..

## See also

Other things related to processing logs: `processingLog<-()`,
[`processingLogItem()`](https://dankelley.github.io/oce/reference/processingLogItem.md),
[`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
