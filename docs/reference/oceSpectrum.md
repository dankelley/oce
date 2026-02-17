# Normalize a Spectrum

This is a wrapper around the R
[`spectrum()`](https://rdrr.io/r/stats/spectrum.html) function, which
returns spectral values that are adjusted so that the integral of those
values equals the variance of the input `x`.

## Usage

``` r
oceSpectrum(x, ...)
```

## Arguments

- x:

  a univariate or multivariate time series, as for
  [`spectrum()`](https://rdrr.io/r/stats/spectrum.html).

- ...:

  extra arguments passed on to
  [`spectrum()`](https://rdrr.io/r/stats/spectrum.html).

## Value

A spectrum that has values that integrate to the variance.

## See also

[`spectrum()`](https://rdrr.io/r/stats/spectrum.html).

## Author

Dan Kelley

## Examples

``` r
x <- rnorm(1e3)
s <- spectrum(x, plot = FALSE)
ss <- oce.spectrum(x, plot = FALSE)
cat("variance of x=", var(x), "\n")
#> variance of x= 0.9728356 
cat("integral of     spectrum=", sum(s$spec) * diff(s$freq[1:2]), "\n")
#> integral of     spectrum= 0.4780713 
cat("integral of oce.spectrum=", sum(ss$spec) * diff(ss$freq[1:2]), "\n")
#> integral of oce.spectrum= 0.9728356 
```
