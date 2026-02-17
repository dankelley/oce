# Add a Spine to a section Object

The purpose of this is to permit plotting with `xtype="spine"`, so that
the section plot will display the distance of stations projected onto
the spine.

## Usage

``` r
addSpine(section, spine, debug = getOption("oceDebug"))
```

## Arguments

- section:

  a
  [section](https://dankelley.github.io/oce/reference/section-class.md)
  object.

- spine:

  either a list or a data frame, containing numeric items named
  `longitude` and `latitude`, defining a path along the spine.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A [section](https://dankelley.github.io/oce/reference/section-class.md)
object with a spine added.

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(section)
eastern <- subset(section, longitude < (-65))
spine <- list(
    longitude = c(-74.5, -69.2, -55),
    latitude = c(38.6, 36.25, 36.25)
)
easternWithSpine <- addSpine(eastern, spine)
# plot(easternWithSpine, which="map")
# plot(easternWithSpine, xtype="distance", which="temperature")
# plot(easternWithSpine, xtype="spine", which="temperature")
```
