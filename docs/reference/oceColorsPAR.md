# Create Colors Suitable for PAR Fields

Create a set of colors for displaying PAR values, based on the scheme
devised by Thyng et al. (2016) and presented in a python package by
Thyng (2019). The color specifications were transliterated from python
to R on 2015-09-29, but have not been adjusted since, even though the
python source has changed. This is to prevent breaking old `oce` code.
To get the latest versions of these colours or other colours, use the
[cmocean](https://CRAN.R-project.org/package=cmocean) R package (Thyng,
Richards, and Krylov, 2019) directly, as is illustrated (with the
"matter" scheme) in Example 2. Note that the
[cmocean](https://CRAN.R-project.org/package=cmocean) core functions
provide a way to select between various versions of the colour schemes.
It is also worth considering the palettes provided by the
[viridis](https://CRAN.R-project.org/package=viridis) package, as
illustrated (with the "inferno" scheme) in Example 3.

## Usage

``` r
oceColorsPAR(n)
```

## Arguments

- n:

  number of colors to create.

## Value

A vector of color specifications.

## References

- Thyng, Kristen, Chad Greene, Robert Hetland, Heather Zimmerle, and
  Steven DiMarco. “True Colors of Oceanography: Guidelines for Effective
  and Accurate Colormap Selection.” Oceanography 29, no. 3 (September 1,
  2016): 9–13.
  [doi:10.5670/oceanog.2016.66](https://doi.org/10.5670/oceanog.2016.66)

- Thyng, Kristen. Kthyng/Cmocean. Python, 2019.
  `https://github.com/kthyng/cmocean`.

- Thyng, Kristen, Clark Richards, and Ivan Krylov. Cmocean: Beautiful
  Colour Maps for Oceanography (version 0.2), 2019.
  `https://CRAN.R-project.org/package=cmocean`.

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
[`oceColors9B()`](https://dankelley.github.io/oce/reference/oceColors9B.md),
[`oceColorsCDOM()`](https://dankelley.github.io/oce/reference/oceColorsCDOM.md),
[`oceColorsChlorophyll()`](https://dankelley.github.io/oce/reference/oceColorsChlorophyll.md),
[`oceColorsClosure()`](https://dankelley.github.io/oce/reference/oceColorsClosure.md),
[`oceColorsCubeHelix()`](https://dankelley.github.io/oce/reference/oceColorsCubeHelix.md),
[`oceColorsDensity()`](https://dankelley.github.io/oce/reference/oceColorsDensity.md),
[`oceColorsFreesurface()`](https://dankelley.github.io/oce/reference/oceColorsFreesurface.md),
[`oceColorsGebco()`](https://dankelley.github.io/oce/reference/oceColorsGebco.md),
[`oceColorsJet()`](https://dankelley.github.io/oce/reference/oceColorsJet.md),
[`oceColorsOxygen()`](https://dankelley.github.io/oce/reference/oceColorsOxygen.md),
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

Krysten M. Thyng (Python version), Dan Kelley (R transliteration)

## Examples

``` r
library(oce)

# Example 1
imagep(volcano, col=oceColorsPAR(128),
    zlab="oceColorsPAR")

if (FALSE) { # \dontrun{
# Example 2 (requires the cmocean package)
imagep(volcano, col=cmocean::cmocean("matter"),
    zlab="cmocean::cmocean(\"matter\")")} # }

if (FALSE) { # \dontrun{
# Example 3 (requires the viridis package)
imagep(volcano, col=viridis::inferno,
    zlab="viridis::inferno")} # }
```
