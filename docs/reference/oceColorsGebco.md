# Create Colors in a GEBCO-like Scheme

The colours were determined by examination of paper charts printed
during the GEBCO Fifth Edition era. The hues range from dark blue to
light blue, then from light brown to dark brown. If used to show
topography in scheme centred on z=0, this means that near-coastal
regions are light in tone, with darker colours representing both
mountains and the deep sea.

## Usage

``` r
oceColorsGebco(
  n = 9,
  region = c("water", "land", "both"),
  type = c("fill", "line"),
  debug = getOption("oceDebug")
)
```

## Arguments

- n:

  Number of colors to return

- region:

  String indicating application region, one of `"water"`, `"land"`, or
  `"both"`.

- type:

  String indicating the purpose, one of `"fill"` or `"line"`.

- debug:

  a flag that turns on debugging.

## See also

Other things related to colors:
[`colormap()`](https://dankelley.github.io/oce/reference/colormap.md),
[`colormapGMT()`](https://dankelley.github.io/oce/reference/colormapGMT.md),
[`oceColors9B()`](https://dankelley.github.io/oce/reference/oceColors9B.md),
[`oceColorsCDOM()`](https://dankelley.github.io/oce/reference/oceColorsCDOM.md),
[`oceColorsChlorophyll()`](https://dankelley.github.io/oce/reference/oceColorsChlorophyll.md),
[`oceColorsClosure()`](https://dankelley.github.io/oce/reference/oceColorsClosure.md),
[`oceColorsCubeHelix()`](https://dankelley.github.io/oce/reference/oceColorsCubeHelix.md),
[`oceColorsDensity()`](https://dankelley.github.io/oce/reference/oceColorsDensity.md),
[`oceColorsFreesurface()`](https://dankelley.github.io/oce/reference/oceColorsFreesurface.md),
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

## Examples

``` r
library(oce)
imagep(volcano, col = oceColorsGebco(128, region = "both"))

```
