# Create Color Functions

This function generates other functions that are used to specify colors.
It is used within oce to create
[`oceColorsTemperature()`](https://dankelley.github.io/oce/reference/oceColorsTemperature.md)
and its many cousins. Users may also find it helpful, for creating
custom color schemes (see “Examples”).

## Usage

``` r
oceColorsClosure(spec)
```

## Arguments

- spec:

  Specification of the color scheme. This may be a character string, in
  which case it must be the name of an item stored in `data(ocecolors)`,
  or either a 3-column data frame or matrix, in which case the columns
  specify red, green and blue values (in range from 0 to 1).

## Sample of Usage

    # Update oxygen color scheme to latest matplotlib value.
    library(oce)
    oxy <- "https://raw.githubusercontent.com/matplotlib/cmocean/master/cmocean/rgb/oxy-rgb.txt"
    oxyrgb <- read.table(oxy, header=FALSE)
    oceColorsOxygenUpdated <- oceColorsClosure(oxyrgb)
    par(mfrow=c(1, 2))
    m <- matrix(1:256)
    imagep(m, col=oceColorsOxygen, zlab="oxygen")
    imagep(m, col=oceColorsOxygenUpdated, zlab="oxygenUpdated")

## See also

Other things related to colors:
[`colormap()`](https://dankelley.github.io/oce/reference/colormap.md),
[`colormapGMT()`](https://dankelley.github.io/oce/reference/colormapGMT.md),
[`oceColors9B()`](https://dankelley.github.io/oce/reference/oceColors9B.md),
[`oceColorsCDOM()`](https://dankelley.github.io/oce/reference/oceColorsCDOM.md),
[`oceColorsChlorophyll()`](https://dankelley.github.io/oce/reference/oceColorsChlorophyll.md),
[`oceColorsCubeHelix()`](https://dankelley.github.io/oce/reference/oceColorsCubeHelix.md),
[`oceColorsDensity()`](https://dankelley.github.io/oce/reference/oceColorsDensity.md),
[`oceColorsFreesurface()`](https://dankelley.github.io/oce/reference/oceColorsFreesurface.md),
[`oceColorsGebco()`](https://dankelley.github.io/oce/reference/oceColorsGebco.md),
[`oceColorsJet()`](https://dankelley.github.io/oce/reference/oceColorsJet.md),
[`oceColorsOxygen()`](https://dankelley.github.io/oce/reference/oceColorsOxygen.md),
[`oceColorsPAR()`](https://dankelley.github.io/oce/reference/oceColorsPAR.md),
[`oceColorsPalette()`](https://dankelley.github.io/oce/reference/oceColorsPalette.md),
[`oceColorsPhase()`](https://dankelley.github.io/oce/reference/oceColorsPhase.md),
[`oceColorsSalinity()`](https://dankelley.github.io/oce/reference/oceColorsSalinity.md),
[`oceColorsTemperature()`](https://dankelley.github.io/oce/reference/oceColorsTemperature.md),
[`oceColorsTurbidity()`](https://dankelley.github.io/oce/reference/oceColorsTurbidity.md),
[`oceColorsTurbo()`](https://dankelley.github.io/oce/reference/oceColorsTurbo.md),
[`oceColorsTwo()`](https://dankelley.github.io/oce/reference/oceColorsTwo.md),
[`oceColorsVelocity()`](https://dankelley.github.io/oce/reference/oceColorsVelocity.md),
[`oceColorsViridis()`](https://dankelley.github.io/oce/reference/oceColorsViridis.md),
[`oceColorsVorticity()`](https://dankelley.github.io/oce/reference/oceColorsVorticity.md),
[`ocecolors`](https://dankelley.github.io/oce/reference/ocecolors.md)
