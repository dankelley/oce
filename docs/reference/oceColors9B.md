# Create Colors in a Red-Yellow-Blue Color Scheme

The results are similar to those of
[`oceColorsJet()`](https://dankelley.github.io/oce/reference/oceColorsJet.md),
but with white hues in the centre, rather than green ones. The scheme
may be useful in displaying signed quantities, and thus is somewhat
analogous to
[`oceColorsTwo()`](https://dankelley.github.io/oce/reference/oceColorsTwo.md),
except that some viewers may be able to distinguish more colors with
`oceColors9B`.

## Usage

``` r
oceColors9B(n)
```

## Arguments

- n:

  number of colors

## References

The following references provide information on choosing colour schemes,
that are suitable for viewers who have colour deficiencies.

Light, Adam, and Patrick J. Bartlein. "The End of the Rainbow? Color
Schemes for Improved Data Graphics." *Eos, Transactions American
Geophysical Union* 85, no. 40 (2004): 385. DOI: 10.1029/2004EO400002

Stephenson, David B. "Comment on 'Color Schemes for Improved Data
Graphics', by A Light and P.J. Bartlein." *Eos, Transactions American
Geophysical Union* 86, no. 20 (2005): 196. DOI: 10.1029/2005EO200005

Light, Adam, and Patrick J. Bartlein. "Reply to 'Comment on Color
Schemes for Improved Data Graphics,' by A. Light and P.J. Bartlein'."
*Eos, Transactions American Geophysical Union* 86, no. 20 (2005):
196–196. DOI: 10.1029/2005EO200006

## See also

Other things related to colors:
[`colormap()`](https://dankelley.github.io/oce/reference/colormap.md),
[`colormapGMT()`](https://dankelley.github.io/oce/reference/colormapGMT.md),
[`oceColorsCDOM()`](https://dankelley.github.io/oce/reference/oceColorsCDOM.md),
[`oceColorsChlorophyll()`](https://dankelley.github.io/oce/reference/oceColorsChlorophyll.md),
[`oceColorsClosure()`](https://dankelley.github.io/oce/reference/oceColorsClosure.md),
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

## Examples

``` r
library(oce)
imagep(volcano,
    col = oceColors9B(128),
    zlab = "oceColors9B"
)

```
