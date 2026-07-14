# Create a ctd Object from an rsk Object

A new `ctd` object is assembled from the contents of the `rsk` object.
The data and metadata are mostly unchanged, with an important exception:
the `pressure` item in the `data` slot may altered, because `rsk`
instruments measure total pressure, not sea pressure; see “Details”.

## Usage

``` r
rsk2ctd(
  x,
  pressureAtmospheric = 0,
  longitude = NULL,
  latitude = NULL,
  ship = NULL,
  cruise = NULL,
  station = NULL,
  deploymentType = NULL,
  debug = getOption("oceDebug")
)
```

## Arguments

- x:

  an [rsk](https://dankelley.github.io/oce/reference/rsk-class.md)
  object.

- pressureAtmospheric:

  A numerical value (a constant or a vector), that is subtracted from
  the pressure in `object` before storing it in the return value.

- longitude:

  numerical value of longitude, in degrees East.

- latitude:

  numerical value of latitude, in degrees North.

- ship:

  optional string containing the ship from which the observations were
  made.

- cruise:

  optional string containing a cruise identifier.

- station:

  optional string containing a station identifier.

- deploymentType:

  character string indicating the type of deployment (see
  [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md)).

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Details

The `pressureType` element of the `metadata` of `rsk` objects defines
the pressure type, and this controls how pressure is set up in the
returned object. If `object@metadata$pressureType` is `"absolute"` (or
`NULL`) then the resultant pressure will be adjusted to make it into
`"sea"` pressure. To do this, the value of
`object@metadata$pressureAtmospheric` is inspected. If this is present,
then it is subtracted from `pressure`. If this is missing, then standard
pressure (10.1325 dbar) will be subtracted. At this stage, the pressure
should be near zero at the ocean surface, but some additional adjustment
might be necessary, and this may be indicated by setting the argument
`pressureAtmospheric` to a non-zero value to be subtracted from
pressure.
