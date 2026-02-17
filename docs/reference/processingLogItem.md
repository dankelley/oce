# Create an Item That can be Inserted into a Processing Log

A function is used internally to initialize processing logs. Users will
probably prefer to use
[`processingLogAppend()`](https://dankelley.github.io/oce/reference/processingLogAppend.md)
instead.

## Usage

``` r
processingLogItem(value = "")
```

## Arguments

- value:

  A string that will be used for the item.

## Value

A [`list()`](https://rdrr.io/r/base/list.html) containing `time`, which
is the time in UTC (calculated with
[`presentTime()`](https://dankelley.github.io/oce/reference/presentTime.md))
at the moment the function is called and `value`, a string that is set
to the argument of the same name.

## See also

Other things related to processing logs: `processingLog<-()`,
[`processingLogAppend()`](https://dankelley.github.io/oce/reference/processingLogAppend.md),
[`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
