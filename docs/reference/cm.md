# Sample cm Data

The result of using
[`read.cm()`](https://dankelley.github.io/oce/reference/read.cm.md) on a
current meter file holding measurements made with an Interocean S4
device. See
[`read.cm()`](https://dankelley.github.io/oce/reference/read.cm.md) for
some general cautionary notes on reading such files. Note that the
salinities in this sample dataset are known to be incorrect, perhaps
owing to a lack of calibration of an old instrument that had not been
used in a long time.

## Usage

``` r
data(cm)
```

## See also

Other datasets provided with oce:
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`lisst`](https://dankelley.github.io/oce/reference/lisst.md),
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`ocecolors`](https://dankelley.github.io/oce/reference/ocecolors.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md),
[`wind`](https://dankelley.github.io/oce/reference/wind.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md)

Other things related to cm data:
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
`[[<-,cm-method`,
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`as.cm()`](https://dankelley.github.io/oce/reference/as.cm.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`plot,cm-method`](https://dankelley.github.io/oce/reference/plot-cm-method.md),
[`read.cm()`](https://dankelley.github.io/oce/reference/read.cm.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md),
[`summary,cm-method`](https://dankelley.github.io/oce/reference/summary-cm-method.md)

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
plot(cm)

```
