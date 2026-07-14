# Determine Available Derived Water Properties

This determines what things can be derived from the supplied variables.
For example, if `salinity`, `temperature`, and `pressure` are supplied,
then potential temperature, sound speed, and several other things can be
derived. If, in addition, `longitude` and `latitude` are supplied, then
Absolute Salinity, Conservative Temperature, and some other things can
be derived. Similarly, `nitrate` can be computed from `NO2+NO3` together
with `nitrate`, and `nitrite` can be computed from `NO2+NO3` together
with `nitrate`. See the “Examples” for a full listing.

## Usage

``` r
computableWaterProperties(x)
```

## Arguments

- x:

  a specification of the names of known variables. This may be (a) an
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) object,
  in which case the names are determined by calling
  [`names()`](https://rdrr.io/r/base/names.html) on the `data` slot of
  `x`, or (b) a vector of character values indicating the names.

## Value

`computableWaterProperties()` returns a sorted character vector holding
the names of computable water properties, or NULL, if there are no
computable values.

## See also

Other functions that calculate seawater properties:
[`T68fromT90()`](https://dankelley.github.io/oce/reference/T68fromT90.md),
[`T90fromT48()`](https://dankelley.github.io/oce/reference/T90fromT48.md),
[`T90fromT68()`](https://dankelley.github.io/oce/reference/T90fromT68.md),
[`locationForGsw()`](https://dankelley.github.io/oce/reference/locationForGsw.md),
[`swAbsoluteSalinity()`](https://dankelley.github.io/oce/reference/swAbsoluteSalinity.md),
[`swAlpha()`](https://dankelley.github.io/oce/reference/swAlpha.md),
[`swAlphaOverBeta()`](https://dankelley.github.io/oce/reference/swAlphaOverBeta.md),
[`swBeta()`](https://dankelley.github.io/oce/reference/swBeta.md),
[`swCSTp()`](https://dankelley.github.io/oce/reference/swCSTp.md),
[`swConservativeTemperature()`](https://dankelley.github.io/oce/reference/swConservativeTemperature.md),
[`swDepth()`](https://dankelley.github.io/oce/reference/swDepth.md),
[`swDynamicHeight()`](https://dankelley.github.io/oce/reference/swDynamicHeight.md),
[`swLapseRate()`](https://dankelley.github.io/oce/reference/swLapseRate.md),
[`swN2()`](https://dankelley.github.io/oce/reference/swN2.md),
[`swPressure()`](https://dankelley.github.io/oce/reference/swPressure.md),
[`swRho()`](https://dankelley.github.io/oce/reference/swRho.md),
[`swRrho()`](https://dankelley.github.io/oce/reference/swRrho.md),
[`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md),
[`swSR()`](https://dankelley.github.io/oce/reference/swSR.md),
[`swSTrho()`](https://dankelley.github.io/oce/reference/swSTrho.md),
[`swSigma()`](https://dankelley.github.io/oce/reference/swSigma.md),
[`swSigma0()`](https://dankelley.github.io/oce/reference/swSigma0.md),
[`swSigma1()`](https://dankelley.github.io/oce/reference/swSigma1.md),
[`swSigma2()`](https://dankelley.github.io/oce/reference/swSigma2.md),
[`swSigma3()`](https://dankelley.github.io/oce/reference/swSigma3.md),
[`swSigma4()`](https://dankelley.github.io/oce/reference/swSigma4.md),
[`swSigmaT()`](https://dankelley.github.io/oce/reference/swSigmaT.md),
[`swSigmaTheta()`](https://dankelley.github.io/oce/reference/swSigmaTheta.md),
[`swSoundAbsorption()`](https://dankelley.github.io/oce/reference/swSoundAbsorption.md),
[`swSoundSpeed()`](https://dankelley.github.io/oce/reference/swSoundSpeed.md),
[`swSpecificHeat()`](https://dankelley.github.io/oce/reference/swSpecificHeat.md),
[`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md),
[`swSpiciness0()`](https://dankelley.github.io/oce/reference/swSpiciness0.md),
[`swSpiciness1()`](https://dankelley.github.io/oce/reference/swSpiciness1.md),
[`swSpiciness2()`](https://dankelley.github.io/oce/reference/swSpiciness2.md),
[`swSstar()`](https://dankelley.github.io/oce/reference/swSstar.md),
[`swTFreeze()`](https://dankelley.github.io/oce/reference/swTFreeze.md),
[`swTSrho()`](https://dankelley.github.io/oce/reference/swTSrho.md),
[`swThermalConductivity()`](https://dankelley.github.io/oce/reference/swThermalConductivity.md),
[`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md),
[`swViscosity()`](https://dankelley.github.io/oce/reference/swViscosity.md),
[`swZ()`](https://dankelley.github.io/oce/reference/swZ.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Example 1
data(ctd)
computableWaterProperties(ctd)
#>  [1] "Absolute Salinity"        "CT"                      
#>  [3] "Conservative Temperature" "N2"                      
#>  [5] "Rrho"                     "RrhoSF"                  
#>  [7] "SA"                       "SP"                      
#>  [9] "SR"                       "Sstar"                   
#> [11] "cabbeling"                "density"                 
#> [13] "potential temperature"    "sigma0"                  
#> [15] "sigma1"                   "sigma2"                  
#> [17] "sigma3"                   "sigma4"                  
#> [19] "sigmaTheta"               "sound speed"             
#> [21] "spice"                    "spiciness0"              
#> [23] "spiciness1"               "spiciness2"              
#> [25] "theta"                    "z"                       
# Example 2: nothing an be computed from just salinity
computableWaterProperties("salinity")
#> NULL
# Example 3: quite a lot can be computed from this trio of values
computableWaterProperties(c("salinity", "temperature", "pressure"))
#>  [1] "N2"                    "Rrho"                  "RrhoSF"               
#>  [4] "SP"                    "density"               "depth"                
#>  [7] "potential temperature" "sigmaTheta"            "sound speed"          
#> [10] "spice"                 "theta"                 "z"                    
# Example 4: now we can get TEOS-10 values as well
computableWaterProperties(c(
    "salinity", "temperature", "pressure",
    "longitude", "latitude"
))
#>  [1] "Absolute Salinity"        "CT"                      
#>  [3] "Conservative Temperature" "N2"                      
#>  [5] "Rrho"                     "RrhoSF"                  
#>  [7] "SA"                       "SP"                      
#>  [9] "SR"                       "Sstar"                   
#> [11] "cabbeling"                "density"                 
#> [13] "depth"                    "potential temperature"   
#> [15] "sigma0"                   "sigma1"                  
#> [17] "sigma2"                   "sigma3"                  
#> [19] "sigma4"                   "sigmaTheta"              
#> [21] "sound speed"              "spice"                   
#> [23] "spiciness0"               "spiciness1"              
#> [25] "spiciness2"               "theta"                   
#> [27] "z"                       
```
