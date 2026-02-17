# Create Colors in the Cube Helix Style

This is a colour scheme that renders well in black and white, that has
colours that are reasonably distinct for many viewers, and that may be
good for persons with colour deficiencies. For more on such matters, the
underlying theory, and the meanings of the parameter values, please see
Green (2011). That R code for the present function is a rewriting of
Fortran code provided in an appendix of that paper.

## Usage

``` r
oceColorsCubeHelix(
  n = 256,
  start = 0.5,
  rotations = -1.5,
  hue = 1.2,
  gamma = 1
)
```

## Arguments

- n:

  number of colors to create, with default 256.

- start:

  numeric value indicating the starting colour. The default, 0.5,
  corresponds to purpose, as in Green (2011).

- rotations:

  numeric value for number of rotations of the helix, with default -1.5
  as in Green (2011).

- hue:

  numeric value with default 1.2, as in Green (2011).

- gamma:

  numeric value, with default 1.

## References

1.  Green, D.A. “A Colour Scheme for the Display of Astronomical
    Intensity Images.” Bulletin of the Astronomical Society of India, 39
    (2011): 289–95.

## See also

Other things related to colors:
[`colormap()`](https://dankelley.github.io/oce/reference/colormap.md),
[`colormapGMT()`](https://dankelley.github.io/oce/reference/colormapGMT.md),
[`oceColors9B()`](https://dankelley.github.io/oce/reference/oceColors9B.md),
[`oceColorsCDOM()`](https://dankelley.github.io/oce/reference/oceColorsCDOM.md),
[`oceColorsChlorophyll()`](https://dankelley.github.io/oce/reference/oceColorsChlorophyll.md),
[`oceColorsClosure()`](https://dankelley.github.io/oce/reference/oceColorsClosure.md),
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

## Author

Dan Kelley based on Fortran code by D.A. Green (2011).

## Examples

``` r
library(oce)
# Example 1: normal (dark for low values)
imagep(volcano, col = oceColorsCubeHelix)

# Example 2: reversed (dark for high values)
imagep(volcano, col = \(n) rev(oceColorsCubeHelix(n)))

```
