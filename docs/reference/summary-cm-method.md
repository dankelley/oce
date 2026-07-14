# Summarize a cm Object

Summarizes some of the data in a `cm` object, presenting such
information as the station name, sampling location, data ranges, etc.

## Usage

``` r
# S4 method for class 'cm'
summary(object, ...)
```

## Arguments

- object:

  A [cm](https://dankelley.github.io/oce/reference/cm-class.md) object.

- ...:

  Further arguments passed to or from other methods.

## See also

The documentation for the
[cm](https://dankelley.github.io/oce/reference/cm-class.md) class
explains the structure of `cm` objects, and also outlines the other
functions dealing with them.

Other things related to cm data:
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
`[[<-,cm-method`,
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`as.cm()`](https://dankelley.github.io/oce/reference/as.cm.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`plot,cm-method`](https://dankelley.github.io/oce/reference/plot-cm-method.md),
[`read.cm()`](https://dankelley.github.io/oce/reference/read.cm.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(cm)
summary(cm)
#> Cm summary
#> ----------
#> 
#> * File source:   "/Users/kelley/Dropbox/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab"
#> * Instr. type:   (2Hz)S4
#> * Serial Num.:   8111786
#> * Version:       2.399
#> * North:         magnetic
#> * Time: 2008-06-26 to 2008-06-26 23:59:00 (1440 samples, mean increment 1 min)
#> * Data Overview
#> 
#>                              Min.       Mean       Max.       Dim. NAs  OriginalName
#>     Sample..                 842        1561.5     2281       1440 0    "Sample #"  
#>     Date                     NA         NA         NA         1440 0    "Date"      
#>     Time                     NA         NA         NA         1440 0    "Time"      
#>     dec.S                    0          0          0          1440 0    "dec S"     
#>     v [m/s]                  -0.862     -0.088736  0.74       1440 0    "Vnorth"    
#>     u [m/s]                  -0.478     0.02761    0.654      1440 0    "Veast"     
#>     Speed                    1.44       53.569     98.29      1440 0    "Speed"     
#>     Dir                      7          126.99     352.1      1440 0    "Dir"       
#>     Vref                     NA         NA         NA         1440 1440 "Vref"      
#>     Hx                       -210       -31.052    177        1440 0    "Hx"        
#>     Hy                       -212       -0.58889   211        1440 0    "Hy"        
#>     conductivity [mS/cm]     36.8       36.924     37.2       1440 0    "Cond"      
#>     temperature [°C, ITS-90] 1.204      2.0773     3.065      1440 0    "T-Temp"    
#>     Depth                    73.242     74.767     77.148     1440 0    "Depth"     
#>     none                     NA         NA         NA         1440 1440 "none"      
#>     none.1                   NA         NA         NA         1440 1440 "none"      
#>     Hdg                      0          175.79     359.2      1440 0    "Hdg"       
#>     X                        NA         NA         NA         1440 1440 "-"         
#>     salinity [PSS-78]        41.307     42.71      44.205     1440 0    "Sal"       
#>     Dens                     1033.3     1034.5     1035.8     1440 0    "Dens"      
#>     SV                       1467.9     1469.8     1472.2     1440 0    "SV"        
#>     X.1                      NA         NA         NA         1440 1440 "-"         
#>     N.S.Dist                 -2535.2    4159.5     10349      1440 0    "N-S Dist"  
#>     E.W.Dist                 -2033.5    893.81     4687.5     1440 0    "E-W Dist"  
#>     X.2                      NA         NA         NA         1440 1440 "-"         
#>     SRB.Date                 NA         NA         NA         1440 1440 "SRB Date"  
#>     SRB.Time                 NA         NA         NA         1440 0    "SRB Time"  
#>     Vref.1                   1.226      1.2263     1.23       1440 1152 "Vref"      
#>     Hx.1                     NA         NA         NA         1440 1440 "Hx"        
#>     Hy.1                     NA         NA         NA         1440 1440 "Hy"        
#>     Cond.1                   NA         NA         NA         1440 1440 "Cond"      
#>     T.Temp.1                 NA         NA         NA         1440 1440 "T-Temp"    
#>     Depth.1                  NA         NA         NA         1440 1440 "Depth"     
#>     none.2                   NA         NA         NA         1440 1440 "none"      
#>     none.3                   NA         NA         NA         1440 1440 "none"      
#>     Hdg.1                    NA         NA         NA         1440 1440 "Hdg"       
#>     X.3                      NA         NA         NA         1440 1440 "-"         
#>     Sal.1                    NA         NA         NA         1440 1440 "Sal"       
#>     Dens.1                   NA         NA         NA         1440 1440 "Dens"      
#>     SV.1                     NA         NA         NA         1440 1440 "SV"        
#>     pressure [dbar]          73.855     75.392     77.794     1440 0    "-"         
#>     time                     1214438400 1214481570 1214524740 1440 0    "-"         
#> 
#> * Processing Log
#> 
#>     - 2023-02-10 12:50:57 UTC: `create 'cm' object`
#>     - 2023-02-10 12:50:57 UTC: `read.oce("~/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab", ...)`
#>     - 2023-02-10 12:50:57 UTC: `subset.cm(x, subset=time < as.POSIXct("2008-06-27 00:00:00", tz = "UTC"))`
```
