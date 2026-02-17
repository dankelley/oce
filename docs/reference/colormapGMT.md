# Create a GMT-type (CPT) Colormap

`colormapGMT` creates colormaps in the Generic Mapping Tools (GMT)
scheme (see References 1 to 4). A few such schemes are built-in, and may
be referred to by name (`"gmt_gebco"`, `"gmt_globe"`, `"gmt_ocean"`, or
`"gmt_relief"`) while others are handled by reading local files that are
in GMT format, or URLs providing such files (see Reference 3).

## Usage

``` r
colormapGMT(name, debug = getOption("oceDebug"))
```

## Arguments

- name:

  character value specifying the GMT scheme, or a source for such a
  scheme. Four pre-defined schemes are available, accessed by setting
  `name` to `"gmt_gebco"`, `"gmt_globe"`, `"gmt_ocean"`, or
  `"gmt_relief"`. If `name` is not one of these values, then it is taken
  to be the name of a local file in GMT format or, if no such file is
  found, a URL holding such a file.

- debug:

  integer that, if positive, indicates to print some debugging output

## Value

`colormap` returns a list, in the same format as the return value for
[`colormap()`](https://dankelley.github.io/oce/reference/colormap.md).

## Details

The GMT files understood by colormapGMT are what GMT calls "Regular CPT
files" (see reference 4). This is a text format that can be read and
(with care) edited in a text editor. There are three categories of lines
within this file. (1) Any line starting with the `"#"` character is a
comment, and is ignored by colormapGMT. (2) Lines with 8 numbers specify
colour bands. The first number is a z value, and the three numbers after
that are red, green and blue values in the range from 0 to 255. This set
of 4 numbers is followed on the same line with similar values. Think of
this sequence as describing a band of colours between two z values. (3)
Lines starting with a character, followed by three numbers, specify
particular codings. The character `"B"` specifies background colour,
while `"F"` specifies foreground colour, and `"N"` specifies the colour
to be used for missing data (the letter stands for not-a-number). Only
`"N"` is used by colormapGMT, and it takes on the role that the
`missingColor` argument would otherwise have. (This is why
`missingColor` is not permitted if `name` is given.)

## References

1.  General overview of GMT system
    `https://www.generic-mapping-tools.org`.

2.  Information on GMT color schemes
    `https://docs.generic-mapping-tools.org/dev/cookbook/cpts.html`

3.  Source of GMT specification files
    `https://beamreach.org/maps/gmt/share/cpt/`

4.  CPT (color palette table) format
    `https://www.soest.hawaii.edu/gmt/gmt/html/GMT_Docs.html#x1-820004.15`

## See also

Other things related to colors:
[`colormap()`](https://dankelley.github.io/oce/reference/colormap.md),
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

Dan Kelley
