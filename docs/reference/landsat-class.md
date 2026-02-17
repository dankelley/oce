# Class to Store Landsat Satellite Data

This class holds landsat data. Such are available at several websites
(e.g. reference 1). Although the various functions may work for other
satellites, the discussion here focusses on Landsat 8 and Landsat 7.

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `landsat` objects is a
  [list](https://rdrr.io/r/base/list.html) containing the main data for
  the object.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `landsat` objects
  is a [list](https://rdrr.io/r/base/list.html) containing information
  about the `data` or about the object itself.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `landsat`
  objects is a [list](https://rdrr.io/r/base/list.html) with entries
  describing the creation and evolution of the object. The contents are
  updated by various `oce` functions to keep a record of processing
  steps. Object summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
landsat objects (see `[[<-,landsat-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a landsat object
may be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md)
operator can also be used to retrieve items from within the `data` and
`metadata` slots. For example, `o[["temperature"]]` can be used to
retrieve temperature from an object containing that quantity. The rule
is that a named quantity is sought first within the object's `metadata`
slot, with the `data` slot being checked only if `metadata` does not
contain the item. This `[[` method can also be used to get certain
derived quantities, if the object contains sufficient information to
calculate them. For example, an object that holds (practical) salinity,
temperature and pressure, along with longitude and latitude, has
sufficient information to compute Absolute Salinity, and so `o[["SA"]]`
will yield the calculated Absolute Salinity.

It is also possible to find items more directly, using
[`oceGetData()`](https://dankelley.github.io/oce/reference/oceGetData.md)
and
[`oceGetMetadata()`](https://dankelley.github.io/oce/reference/oceGetMetadata.md),
but neither of these functions can retrieve derived items.

## Data storage

The data are stored with 16-bit resolution. Oce breaks these 16 bits up
into most-significant and least-significant bytes. For example, the
aerosol band of a Landsat object named `x` are contained within
`x@data$aerosol$msb` and `x@data$aerosol$lsb`, each of which is a matrix
of raw values. The results may be combined as e.g.

    256L*as.integer(x@data[[i]]$msb) + as.integer(x@data[[i]]$lsb)

and this is what is returned by executing `x[["aerosol"]]`.

Landsat data files typically occupy approximately a gigabyte of storage.
That means that corresponding Oce objects are about the same size, and
this can pose significant problems on computers with less than 8GB of
memory. It is sensible to specify bands of interest when reading data
with
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md),
and also to use
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md)
to isolate geographical regions that need processing.

Experts may need to get direct access to the data, and this is easy
because all Landsat objects (regardless of satellite) use a similar
storage form. Band information is stored in byte form, to conserve
space. Two bytes are used for each pixel in Landsat-8 objects, with just
one for other objects. For example, if a Landsat-8 object named `L`
contains the `tirs1` band, the most- and least-significant bytes will be
stored in matrices `L@data$tirs1$msb` and `L@data$tirs1$lsb`. A similar
Landsat-7 object would have the same items, but `msb` would be just the
value `0x00`.

Derived bands, which may be added to a landsat object with
[`landsatAdd()`](https://dankelley.github.io/oce/reference/landsatAdd.md),
are not stored in byte matrices. Instead they are stored in numerical
matrices, which means that they use 4X more storage space for Landsat-8
images, and 8X more storage space for other satellites. A computer needs
at least 8GB of RAM to work with such data.

## Landsat 8

The Landsat 8 satellite has 11 frequency bands, listed below (see
reference 2\]).

    .------------------------------------------------------------------------------.
    | Band | Band                      | Band         | Wavelength    | Resolution |
    | No.  | Contents                  | Name         | (micrometers) |   (meters) |
    |------+---------------------------+--------------+---------------+------------|
    |    1 | Coastal aerosol           | aerosol      |  0.43 -  0.45 |         30 |
    |    2 | Blue                      | blue         |  0.45 -  0.51 |         30 |
    |    3 | Green                     | green        |  0.53 -  0.59 |         30 |
    |    4 | Red                       | red          |  0.64 -  0.67 |         30 |
    |    5 | Near Infrared (NIR)       | nir          |  0.85 -  0.88 |         30 |
    |    6 | SWIR 1                    | swir1        |  1.57 -  1.65 |         30 |
    |    7 | SWIR 2                    | swir2        |  2.11 -  2.29 |         30 |
    |    8 | Panchromatic              | panchromatic |  0.50 -  0.68 |         15 |
    |    9 | Cirrus                    | cirrus       |  1.36 -  1.38 |         30 |
    |   10 | Thermal Infrared (TIRS) 1 | tirs1        | 10.60 - 11.19 |        100 |
    |   11 | Thermal Infrared (TIRS) 2 | tirs2        | 11.50 - 12.51 |        100 |
    .------------------------------------------------------------------------------.

In addition to the above, setting `band="terralook"` may be used as an
abbreviation for `band=c("red", "green", "nir")`.

Band 8 is panchromatic, and has the highest resolution. For convenience
of programming,
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md)
subsamples the `tirs1` and `tirs2` bands to the 30m resolution of the
other bands. See Reference 3 for information about the evolution of
Landsat 8 calibration coefficients, which as of summer 2014 are still
subject to change.

## Landsat 7

Band information is as follows (from reference 8). The names are not
official, but are set up to roughly correspond with Landsat-8 names,
according to wavelength. An exception is the Landsat-7 bands named
`tirs1` and `tirs2`, which are at two different gain settings, with
identical wavelength span for each, which roughly matches the range of
the Landsat-8 bands `tirs1` and `tirs2` combined. This may seem
confusing, but it lets code like `plot(im, band="tirs1")` to work with
both Landsat-8 and Landsat-7.

    .------------------------------------------------------------------------------.
    | Band | Band                      | Band         | Wavelength    | Resolution |
    | No.  | Contents                  | Name         | (micrometers) |   (meters) |
    |------+---------------------------+--------------+---------------+------------|
    |    1 | Blue                      | blue         |  0.45 -  0.52 |         30 |
    |    2 | Green                     | green        |  0.52 -  0.60 |         30 |
    |    3 | Red                       | red          |  0.63 -  0.69 |         30 |
    |    4 | Near IR                   | nir          |  0.77 -  0.90 |         30 |
    |    5 | SWIR                      | swir1        |  1.55 -  1.75 |         30 |
    |    6 | Thermal IR                | tirs1        | 10.4  - 12.50 |         30 |
    |    7 | Thermal IR                | tirs2        | 10.4  - 12.50 |         30 |
    |    8 | SWIR                      | swir2        |  2.09 -  2.35 |         30 |
    |    9 | Panchromatic              | panchromatic |  0.52 -  0.90 |         15 |
    .------------------------------------------------------------------------------.

## References

1.  See the USGS "glovis" web site.

2.  see landsat.gsfc.nasa.gov/?page_id=5377

3.  see landsat.usgs.gov/calibration_notices.php

4.  `https://dankelley.github.io/r/2014/07/01/landsat.html`

5.  `https://scienceofdoom.com/2010/12/27/emissivity-of-the-ocean/`

6.  see landsat.usgs.gov/Landsat8_Using_Product.php

7.  see landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf

8.  see landsat.usgs.gov/band_designations_landsat_satellites.php

9.  Yu, X. X. Guo and Z. Wu., 2014. Land Surface Temperature Retrieval
    from Landsat 8 TIRS-Comparison between Radiative Transfer
    Equation-Based Method, Split Window Algorithm and Single Channel
    Method, *Remote Sensing*, 6, 9829-9652.
    `https://www.mdpi.com/2072-4292/6/10/9829`

10. Rajeshwari, A., and N. D. Mani, 2014. Estimation of land surface
    temperature of Dindigul district using Landsat 8 data.
    *International Journal of Research in Engineering and Technology*,
    3(5), 122-126.
    `http://www.academia.edu/7655089/ESTIMATION_OF_LAND_SURFACE_TEMPERATURE_OF_DINDIGUL_DISTRICT_USING_LANDSAT_8_DATA`

11. Konda, M. Imasato N., Nishi, K., and T. Toda, 1994. Measurement of
    the Sea Surface Emissivity. *Journal of Oceanography*, 50, 17:30.
    [doi:10.1007/BF02233853](https://doi.org/10.1007/BF02233853)

## See also

Data from AMSR satellites are handled with
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md).

A file containing Landsat data may be read with
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md)
or
[`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md),
and one such file is provided by the
[ocedata](https://CRAN.R-project.org/package=ocedata) package as a
dataset named `landsat`.

Plots may be made with
[`plot,landsat-method()`](https://dankelley.github.io/oce/reference/plot-landsat-method.md).
Since plotting can be quite slow, decimation is available both in the
plotting function and as the separate function
[`decimate()`](https://dankelley.github.io/oce/reference/decimate.md).
Images may be subsetted with
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md).

Other classes holding satellite data:
[`amsr-class`](https://dankelley.github.io/oce/reference/amsr-class.md),
[`g1sst-class`](https://dankelley.github.io/oce/reference/g1sst-class.md),
[`satellite-class`](https://dankelley.github.io/oce/reference/satellite-class.md)

Other things related to landsat data:
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md),
`[[<-,landsat-method`,
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`landsatAdd()`](https://dankelley.github.io/oce/reference/landsatAdd.md),
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md),
[`plot,landsat-method`](https://dankelley.github.io/oce/reference/plot-landsat-method.md),
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md),
[`summary,landsat-method`](https://dankelley.github.io/oce/reference/summary-landsat-method.md)

## Author

Dan Kelley and Clark Richards
