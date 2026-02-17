# Get First Finite Value in a Vector or Array.

If x is a vector, this is straightforward. If x is anything else, it is
first converted to a vector with
[`as.vector()`](https://rdrr.io/r/base/vector.html), so the first value
will be with respect to storage by columns, for a matrix, etc.

## Usage

``` r
firstFinite(v)
```

## Arguments

- v:

  A numerical vector or array.

## Value

The first finite value, or NULL if there are no finite values.
