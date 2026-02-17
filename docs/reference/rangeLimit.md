# Substitute NA for Data Outside a Range

Substitute NA for data outside a range, e.g. to remove wild spikes in
data.

## Usage

``` r
rangeLimit(x, min, max)
```

## Arguments

- x:

  vector of values

- min:

  minimum acceptable value. If not supplied, and if `max` is also not
  supplied, a `min` of the 0.5 percentile will be used.

- max:

  maximum acceptable value. If not supplied, and if `min` is also not
  supplied, a `min` of the 0.995 percentile will be used.

## Author

Dan Kelley

## Examples

``` r
ten.to.twenty <- rangeLimit(1:100, 10, 20)
```
