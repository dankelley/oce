# Bilinear Interpolation Within a Grid

This is used by
[`topoInterpolate`](https://dankelley.github.io/oce/reference/topoInterpolate.md).

## Usage

``` r
bilinearInterp(x, y, gx, gy, g)
```

## Arguments

- x:

  vector of x values at which to interpolate

- y:

  vector of y values at which to interpolate

- gx:

  vector of x values for the grid

- gy:

  vector of y values for the grid

- g:

  matrix of the grid values

## Value

vector of interpolated values
