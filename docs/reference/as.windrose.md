# Create a windrose Object

Create a wind-rose object, typically for plotting with
[`plot,windrose-method()`](https://dankelley.github.io/oce/reference/plot-windrose-method.md).

## Usage

``` r
as.windrose(x, y, dtheta = 15, debug = getOption("oceDebug"))
```

## Arguments

- x:

  The x component of wind speed (or stress) *or* an object of class
  `met` (see
  [met](https://dankelley.github.io/oce/reference/met-class.md)), in
  which case the `u` and `v` components of that object are used for the
  components of wind speed, and `y` here is ignored.

- y:

  The y component of wind speed (or stress).

- dtheta:

  The angle increment (in degrees) within which to classify the data.

- debug:

  A flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

## Value

A
[windrose](https://dankelley.github.io/oce/reference/windrose-class.md)
object, with `data` slot containing

|  |  |
|----|----|
| **Item** | **Meaning** |
| `n` | the number of `x` values |
| `x.mean` | the mean of the `x` values |
| `y.mean` | the mean of the `y` values |
| `theta` | the central angle (in degrees) for the class |
| `count` | the number of observations in this class |
| `mean` | the mean of the observations in this class |
| `fivenum` | the [`fivenum()`](https://rdrr.io/r/stats/fivenum.html) vector for observations in this class (the min, the lower hinge, the median, the upper hinge, and the max) |

## See also

Other things related to windrose data:
[`[[,windrose-method`](https://dankelley.github.io/oce/reference/sub-sub-windrose-method.md),
`[[<-,windrose-method`,
[`plot,windrose-method`](https://dankelley.github.io/oce/reference/plot-windrose-method.md),
[`summary,windrose-method`](https://dankelley.github.io/oce/reference/summary-windrose-method.md),
[`windrose-class`](https://dankelley.github.io/oce/reference/windrose-class.md)

## Author

Dan Kelley, with considerable help from Alex Deckmyn.

## Examples

``` r
library(oce)
set.seed(1234)
theta <- seq(0, 360, 0.25)
x <- 1 + cos(pi / 180 * theta) + rnorm(theta)
y <- sin(pi / 180 * theta) + rnorm(theta)
wr <- as.windrose(x, y)
summary(wr)
#> Windrose data
#> -------------
#> 
#> * Have n= 24 angles, separated by dtheta= 15 
#> 
#> * Data Overview
#> 
#>            Min.     Mean       Max.     Dim.   NAs
#>     n      1441     1441       1441     1      0  
#>     x.mean 0.99206  0.99206    0.99206  1      0  
#>     y.mean 0.013422 0.013422   0.013422 1      0  
#>     theta  -172.5   1.1842e-14 172.5    24     0  
#>     count  14       60.042     142      24     0  
#>     mean   1.0569   1.584      2.1238   24     0  
#>     fives  0.043288 1.6777     4.9429   "24x5" 0  
#> 
#> * Processing Log
#> 
#>     - 2026-02-17 19:25:02 UTC: `create 'windrose' object`
#>     - 2026-02-17 19:25:02 UTC: `as.windrose(x = x, y = y)`
```
