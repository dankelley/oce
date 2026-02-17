# Summarize a lisst Object

Summarizes some of the data in a `lisst` object, presenting such
information as the station name, sampling location, data ranges, etc.

## Usage

``` r
# S4 method for class 'lisst'
summary(object, ...)
```

## Arguments

- object:

  a [lisst](https://dankelley.github.io/oce/reference/lisst-class.md)
  object.

- ...:

  Ignored.

## See also

Other things related to lisst data:
[`[[,lisst-method`](https://dankelley.github.io/oce/reference/sub-sub-lisst-method.md),
`[[<-,lisst-method`,
[`as.lisst()`](https://dankelley.github.io/oce/reference/as.lisst.md),
[`lisst-class`](https://dankelley.github.io/oce/reference/lisst-class.md),
[`plot,lisst-method`](https://dankelley.github.io/oce/reference/plot-lisst-method.md),
[`read.lisst()`](https://dankelley.github.io/oce/reference/read.lisst.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(lisst)
summary(lisst)
#> LISST Summary
#> -------------
#> 
#> * File source:        (constructed)
#> * Time: 2012-01-01 to 2012-01-01 00:00:59 (91 samples, mean increment 0.6570842 s)
#> * Data Overview
#> 
#>                      Min.       Mean       Max.       Dim. NAs
#>     C1               0.0010652  0.15425    0.6978     91   0  
#>     C2               0.006291   0.24195    0.88089    91   0  
#>     C3               0.0021988  0.24053    0.95345    91   0  
#>     C4               0.0041735  0.22599    1.0271     91   0  
#>     C5               0.003596   0.27159    1.1656     91   0  
#>     C6               0.0084791  0.35829    1.391      91   0  
#>     C7               0.00017494 0.32218    1.4376     91   0  
#>     C8               0.0011839  0.33932    1.5601     91   0  
#>     C9               0.0053039  0.35563    1.6479     91   0  
#>     C10              0.00027554 0.43936    1.7989     91   0  
#>     C11              0.0037151  0.47023    1.8696     91   0  
#>     C12              0.00081108 0.44499    2.0161     91   0  
#>     C13              0.008303   0.53889    2.2436     91   0  
#>     C14              0.0061296  0.63101    2.4211     91   0  
#>     C15              0.0081711  0.6101     2.4778     91   0  
#>     C16              0.001358   0.54999    2.4576     91   0  
#>     C17              0.0040694  0.54498    2.552      91   0  
#>     C18              0.0044594  0.60613    2.8146     91   0  
#>     C19              0.00055307 0.63606    2.9199     91   0  
#>     C20              0.001207   0.70448    3.0498     91   0  
#>     C21              2.4262e-05 0.6655     3.0745     91   0  
#>     C22              0.0020885  0.77344    3.3258     91   0  
#>     C23              0.00037555 0.7105     3.2838     91   0  
#>     C24              0.00090739 0.72498    3.2742     91   0  
#>     C25              0.0065367  0.85793    3.6554     91   0  
#>     C26              0.0015493  0.78833    3.6585     91   0  
#>     C27              0.007094   0.85212    3.792      91   0  
#>     C28              0.024247   0.95463    4.0537     91   0  
#>     C29              0.00164    0.85932    4.0159     91   0  
#>     C30              0.0026813  0.89955    4.1353     91   0  
#>     C31              0.0006224  0.98356    4.3578     91   0  
#>     C32              0.01269    1.0482     4.4591     91   0  
#>     lts              0          0          0          91   0  
#>     voltage          3.8248     3.899      3.9978     91   0  
#>     aux              0.07       0.07       0.07       91   0  
#>     lrs              3.9045     4.0007     4.0993     91   0  
#>     pressure [dbar]  5.0068     5.6479     6.0193     91   0  
#>     temperature [°C] 9.9432     14.331     16.879     91   0  
#>     dayhour          0          2.5385     6          91   0  
#>     minutesecond     0          2769.2     5600       91   0  
#>     transmission     0.0010149  0.20525    0.45733    91   0  
#>     beam             30.853     35.895     39.98      91   0  
#>     time             1325376000 1325376030 1325376059 91   0  
#> 
#> * Processing Log
#> 
#>     - 2018-04-07 18:46:34 UTC: `create 'lisst' object with filename="(constructed)", longitude=NA, latitude=NA`
#>     - 2018-04-07 18:46:34 UTC: `as.lisst(data = data, filename = "(constructed)", year = 2012,     tz = "UTC")`
```
