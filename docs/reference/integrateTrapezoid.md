# Trapezoidal Integration

Estimate the integral of one-dimensional function using the trapezoidal
rule.

## Usage

``` r
integrateTrapezoid(x, y, type = c("A", "dA", "cA"), xmin, xmax)
```

## Arguments

- x, y:

  vectors of x and y values. In the normal case, these vectors are both
  supplied, and of equal length. There are also two special cases.
  First, if `y` is missing, then `x` is taken to be `y`, and a new `x`
  is constructed as
  [`seq_along`](https://rdrr.io/r/base/seq.html)`(y)1. Second, if `length(x)`is 1 and`length(y)`exceeds 1, then`x`is replaced by`x\*`[`seq_along`]`(y)\`.

- type:

  Flag indicating the desired return value (see “Value”).

- xmin, xmax:

  Optional numbers indicating the range of the integration. These values
  may be used to restrict the range of integration, or to extend it; in
  either case, [`approx()`](https://rdrr.io/r/stats/approxfun.html) with
  `rule=2` is used to create new x and y vectors.

## Value

If `type="A"` (the default), a single value is returned, containing the
estimate of the integral of `y=y(x)`. If `type="dA"`, a numeric vector
of the same length as `x`, of which the first element is zero, the
second element is the integral between `x[1]` and `x[2]`, etc. If
`type="cA"`, the result is the cumulative sum (as in
[`cumsum()`](https://rdrr.io/r/base/cumsum.html)) of the values that
would be returned for `type="dA"`. See “Examples”.

## Bugs

There is no handling of `NA` values.

## Author

Dan Kelley

## Examples

``` r
x <- seq(0, 1, length.out = 10) # try larger length.out to see if area approaches 2
y <- 2 * x + 3 * x^2
A <- integrateTrapezoid(x, y)
dA <- integrateTrapezoid(x, y, "dA")
cA <- integrateTrapezoid(x, y, "cA")
print(A)
#> [1] 2.006173
print(sum(dA))
#> [1] 2.006173
print(tail(cA, 1))
#> [1] 2.006173
print(integrateTrapezoid(diff(x[1:2]), y))
#> [1] 2.006173
print(integrateTrapezoid(y))
#> [1] 18.05556
```
