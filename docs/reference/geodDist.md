# Compute Geodesic Distance on Surface of Earth

This calculates geodesic distance, in km, between points on the earth,
i.e. distance measured along the (presumed ellipsoidal) surface. The
method involves the solution of the geodetic inverse problem, using
Vincenty's (1975) modification of Rainsford's method with Helmert's
elliptical terms.

## Usage

``` r
geodDist(
  longitude1,
  latitude1 = NULL,
  longitude2 = NULL,
  latitude2 = NULL,
  alongPath = FALSE
)
```

## Arguments

- longitude1:

  longitude or a vector of longitudes, *or* a `section` object, from
  which longitude and latitude are extracted and used instead of the
  next three arguments

- latitude1:

  latitude or vector of latitudes (ignored if `longitude1` is a
  `section` object)

- longitude2:

  optional longitude or vector of longitudes (ignored if
  `alongPath=TRUE`)

- latitude2:

  optional latitude or vector of latitudes (ignored if `alongPath=TRUE`)

- alongPath:

  boolean indicating whether to compute distance along the path, as
  opposed to distance from the reference point. If `alongPath=TRUE`, any
  values provided for `latitude2` and `longitude2` will be ignored.

## Value

Vector of distances in kilometres.

## Details

The function may be used in several different ways.

Case 1: `longitude1` is a `section` object. The values of `latitude1`,
`longitude2`, and `latitude2` arguments are ignored, and the behaviour
depends on the value of the `alongPath` argument. If `alongPath=FALSE`,
the return value contains the geodetic distances of each station from
the first one. If `alongPath=TRUE`, the return value is the geodetic
distance along the path connecting the stations, in the order in which
they are stored in the section.

Case 2: `longitude1` is a vector. If `longitude2` and `latitude2` are
not given, then the return value is a vector containing the distances of
each point from the first one, *or* the distance along the path
connecting the points, according to the value of `alongPath`. On the
other hand, if both `longitude2` and `latitude2` are specified, then the
return result depends on the length of these arguments. If they are each
of length 1, then they are taken as a reference point, from which the
distances to `longitude1` and `latitude1` are calculated (ignoring the
value of `alongPath`). However, if they are of the same length as
`longitude1` and `latitude1`, then the return value is the distance
between corresponding (`longitude1`,`latitude1`) and
(`longitude2`,`latitude2`) values.

## References

Vincenty, T. "Direct and Inverse Solutions of Geodesics on the Ellipsoid
with Application of Nested Equations." Survey Review 23, no. 176 (April
1, 1975): 88-93. https://doi.org/10.1179/sre.1975.23.176.88.

## See also

[`geodXy()`](https://dankelley.github.io/oce/reference/geodXy.md)

Other functions relating to geodesy:
[`geodGc()`](https://dankelley.github.io/oce/reference/geodGc.md),
[`geodXy()`](https://dankelley.github.io/oce/reference/geodXy.md),
[`geodXyInverse()`](https://dankelley.github.io/oce/reference/geodXyInverse.md)

## Author

Dan Kelley based this on R code sent to him by Darren Gillis, who in
2003 had modified Fortran code that, according to comments in the
source, had been written in 1974 by L. Pfeifer and J. G. Gergen.

## Examples

``` r
library(oce)
km <- geodDist(100, 45, 100, 46)
data(section)
geodDist(section)
#>   [1]    0.00000   19.32429   41.97088   55.38400   77.12095   97.43228
#>   [7]  140.86337  236.68579  288.31159  341.09128  391.72386  445.05214
#>  [13]  497.71719  549.70283  601.72487  655.01069  708.59668  760.25064
#>  [19]  814.20562  866.36956  919.08447  971.64219 1076.76938 1130.27545
#>  [25] 1182.65028 1289.00714 1341.45528 1394.73699 1448.93931 1500.14559
#>  [31] 1553.69120 1607.12013 1659.27943 1711.43964 1764.28095 1816.50871
#>  [37] 1869.14069 1922.63185 1975.25273 2028.54549 2080.42703 2133.23082
#>  [43] 2185.44805 2238.44463 2290.01922 2343.28305 2395.13015 2448.12865
#>  [49] 2500.85347 2552.86513 2605.39232 2658.30164 2711.51496 2763.34652
#>  [55] 2816.13791 2868.73222 2922.74667 2972.57341 3025.06989 3077.58318
#>  [61] 3126.21092 3179.06544 3230.14997 3336.07435 3388.24838 3493.29516
#>  [67] 3543.42063 3595.29304 3648.19007 3700.12404 3752.64077 3804.68050
#>  [73] 3859.43021 3914.55123 3968.84559 4024.20490 4080.45748 4136.26199
#>  [79] 4190.68385 4245.85274 4299.61905 4356.68235 4408.77946 4464.74619
#>  [85] 4514.56278 4567.35792 4623.31159 4680.32529 4736.04351 4790.14602
#>  [91] 4845.35484 4899.20736 4953.59714 5007.88376 5059.92465 5116.18357
#>  [97] 5224.57696 5278.47381 5330.56753 5347.43198 5361.75044 5376.43180
#> [103] 5388.12780 5402.31973 5415.84740 5429.59164 5443.72655 5457.30296
#> [109] 5470.12311 5479.22626 5488.57772 5504.57747 5518.33874 5530.83750
#> [115] 5542.29346 5553.12372 5565.24674 5576.51940 5587.84941 5599.75751
#> [121] 5610.04172 5621.55739 5629.16006 5633.49267
geodDist(section, alongPath = TRUE)
#>   [1]    0.00000   19.32429   41.98025   55.45124   77.19517  110.46626
#>   [7]  165.53704  270.97198  324.62785  379.07516  430.79723  485.08130
#>  [13]  538.55673  591.26800  643.88027  697.78690  752.07840  804.06096
#>  [19]  858.28302  910.91394  964.04121 1017.01676 1123.51893 1177.28079
#>  [25] 1230.08210 1337.29373 1390.21243 1444.00195 1498.32245 1550.42178
#>  [31] 1604.48322 1658.24865 1710.92352 1763.59800 1817.12368 1869.69358
#>  [37] 1923.21041 1977.10211 2030.23927 2084.16611 2136.69018 2190.36711
#>  [43] 2243.09385 2296.82917 2349.08925 2403.02461 2455.63789 2509.39317
#>  [49] 2563.01465 2616.13733 2669.43418 2723.19853 2777.12313 2830.11382
#>  [55] 2883.69215 2937.44869 2991.73271 3043.56085 3097.04814 3150.62606
#>  [61] 3200.33847 3254.74428 3306.99942 3414.90302 3468.58088 3576.36715
#>  [67] 3627.93229 3681.32923 3735.60106 3789.20771 3843.14810 3896.99974
#>  [73] 3953.77147 4010.86373 4066.91757 4123.85351 4181.90090 4240.29531
#>  [79] 4296.21556 4354.78728 4410.24474 4469.41248 4523.69794 4582.00096
#>  [85] 4634.87666 4689.75722 4747.45577 4806.86398 4865.47849 4922.65818
#>  [91] 4980.92893 5037.32056 5094.77974 5151.26056 5206.85970 5265.74388
#>  [97] 5380.67252 5437.79073 5493.26410 5516.47719 5538.32842 5559.84386
#> [103] 5581.10886 5603.01334 5623.06788 5643.77294 5665.47216 5686.53871
#> [109] 5707.28098 5723.54327 5740.23088 5760.01226 5778.91461 5797.41508
#> [115] 5814.27408 5831.22599 5849.15121 5866.66683 5883.59620 5901.68615
#> [121] 5918.58447 5935.68413 5946.96298 5953.85721
```
