# oce 1.8.2

* Add `package?oce` documentation (had been missing with a Roxygen2 change).
* Add `read.ctd.saiv()` to read data from SAIV instruments (issue 2141).
* Remove many dontrun examples from the docs (issue 2152).
* Remove `trimString()`, after it had been deprecated for a long time (issue 2123).
* Remove use of the deprecated `sp` package (issue 2154).
* Change `amsr` dataset and functions to handle new format (issues 2124 to 2133, 2147).
* Change `as.ctd()` to handle multi-profile Argo better (issue 2173).
* Change `as.tidem()` to handle NOAA constituent names and frequencies (issues 2143, 2144 and 2146).
* Change C/C++ `sprintf()` calls so all formats match arguments (issue 2172).
* Change `computableWaterProperties` to handle a vector of variable names.
* Change `plot.adp()` to obey `xlab` if x axis represents time (issue 2162).
* Change `plot.cm()` to obey `xlim`, `ylim`, `xaxs` and `yaxs` (issue 2121).
* Change `plotTS()` and `plotProfile()` to accept `type="b"`.
* Change `plot.windrose()` for `type="fivenum"` (issue 2164).

# oce 1.8.1 (on CRAN)

* Change `as.ctd()` to handle `startTime`, `PRES`, `PSAL` and `TEMP` better.
* Change `binCount1D()` + 6 related functions by adding `include.lowest` parameter (issue 2113).
* Change C++ files to avoid an error relating to include statements (issue 2119).
* Change `imagep()` to ensure `z` (if provided) is a matrix (issue 2091).
* Change `plot.section()` to skip the kriging example (issue 2080).
* Change `plot.section()` to show palette triangles if necessary (issue 2083).
* Change `plot.section()` to show data and bottom correctly (issue 2092).
* Change `plotTS()` to obey the `rho1000` parameter again (issue 2085).
* Change `read.argo()` to translate more variable names.
* Change `read.netcdf()` to read more items.
* Change `read.rsk()` to obey the `tz` parameter (issue 2108).
* Change `summary.adp()` to handle the new format for AD2CP data (issue 2087).

# oce 1.8.0 (on CRAN)

* Add `advSontekAdrFileTrim()` (issue 1994).
* Add `ctdFindProfilesRBR()` (issue 2027).
* Change `applyMagneticDeclination()` to also handle `adp` and `adv` (issue 2038).
* Change `download.topo()` to handle new NOAA database (issue 2015).
* Change `inst/extdata` by compressing some files.
* Change `mapPlot()` to remove an infrequent low-level error (issue 2036).
* Change `mapPlot()` to require `projection` to be a string (issue 2076).
* Change `numberAsPOSIXct(t, type="gps")` to handle week rollover (issue 2077).
* Change `plot.echosounder()` to use `oceColorsViridis()` by default (issue 2060).
* Change `plotProfile()` to create xlab on vector input (issue 2047).
* Change `plotTS()` to compute isopycnals more accurately (issue 2046).
* Change `plotTS()` to handle lobo objects directly.
* Change `plot.tidem()` to obey `...` parameter (issue 2035).
* Change `read.adp.ad2cp()`, and structure of AD2CP objects (issue 2005).
* Change `read.rsk()` to read geographic information (issue 2024).
* Change `sectionSmooth()` to handle `method="kriging"` again
* Change `tidem()` (and summary method) to handle 6-hourly data (issue 2034).
* Remove package dependence on `rgeos` and `raster` packages (issue 2028).
* Remove `plotAD2CP()`, now superseded by generic `plot()` method (issue 2005).
* Remove plotting tests but retain in developer suite (issue 2073).

# oce 1.7.10 (on CRAN)

* Fix a typo in `swThermalConductivity()` doc.

# oce 1.7.9

* Improve the handling of file encodings.
* Make the argo `[[` method handle spiciness.
* Improve `read.adp.ad2cp()` extensively, breaking some old behaviours.
* Add `oceFileTrim()`, `adpAd2cpFileTrim()` and `adpRdiFileTrim()`.
* Remove `renameData()`, which had been flagged as defunct in multiple CRAN releases.
* Deprecate `trimString()`.

# oce 1.7.8

* Fix compiler warning of comparing signed and unsigned quantities.

# oce 1.7.7

* Add `gappyIndex()`.
* Fix encoding problems that will occur in the upcoming version of R.

# oce 1.7.6

* Permit acoustic-Doppler files in excess of 4Gb.

# oce 1.7.5

* Remove a stray file.

# oce 1.7.4

* Change `mapImage()` to use `interp` instead of `akima`, as required by CRAN.
* Improve `read.ctd.sbe()` for interpreting Date in headers.
* Improve `as.ctd()` for Argo data.

# oce 1.7.3

* Add `read.ctd.ssda()`.
* Add `read.argo.copernicus()`.
* Add `sectionSort()` parameter `decreasing` (thanks to M. Renner).
* Address unused-variable warnings in C++ code.
* Address byte-order mark problem in `read.met()` tests.
* Change most documentation hyperlinks to plain-text.
* Change `read.met()` file encoding; remove sample data.
* Speed up some tests.

# Version 1.7.2

* Remove a link to `https://pubs.usgs.gov/pp/1395/report.pdf`, which was
  reported as unreachable during the attempted CRAN submission of version
  1.7.1.

# Version 1.7.1

* Fix error in comparing a `class()` result with a string, which was reported
  as an error during the attempted CRAN submission of version 1.7.0.

# Version 1.7.0

* Fix CRAN-check warning about 2 uninitialized variables on the
  `r-devel-linux-x86_64-debian-gcc` test system.  (We thank K. Hornik, for an
  email dated 2022-03-16 notifying us about the problem.)
* Fix CRAN-check error on the `r-devel-linux-x86_6-debian-clang`,
  `r-devel-linux-x86_6-fedora-clang` and `r-devel-linux-x86_6-fedora-gcc` test
  systems, relating to a byte-order-mark in a test file, which is evidently
  treated differently on different systems.
* Remove a kriging example because it causes CRAN-check problems on
  `r-devel-linux-*` machines (but, curiously, no other machines or systems).
* Change `mapTissot()` to trim shapes to earth limits.
* Alter this `NEWS.md` file so `pkgdown::build_news()` will notice the
  subsections.
* Add `read.ctd.aml()`.
* Change `read.met()` to handle a new `encoding` argument.
* Fix `drawPalette()` to obey the `at` and `labels` arguments.
* Fix `plot.ctd()` to recognize `which=13` for spice.

# Version 1.6.1

* Remove a journal link that has become unavailable.
* Fix "length_1" error (thanks to B. Ripley for the heads-up).

# 1.6.0

* Fix image-size declarations in 3 man pages (required by CRAN).
* Improve sub-class vignette.

# 1.5.0

* Add `[["?"]]` facility to all objects.
* Add `showNA` argument to `vectorShow()`.
* Add `adpConvertRawToNumeric()`.
* Add `adpFlagPastBoundary()`.
* Add `ctdRepair()`.
* Add `labelWithUnit()`.
* Add `swSR()`.
* Add `swSstar()`.
* Change `as.ctd()` to drop `other` parameter, deprecated 4y ago.
* Change `read.odf()` to handle more CODE and UNIT values.
* Change `oce.plot.ts()` by adding `simplify` argument.
* Change `plot.section()`, correcting temperature label to "T" and
  adding many more plot types.
* Change `pwelch()`, improving low-frequency results.
* Fix `[[` to yield ITS90 temperature for all classes, not just `ctd`.
* Fix `as.ctd()` handling of temperature scale of first argument.
* Fix `as.section()` handling of list of `argo` objects as first argument.
* Fix `colormap()` handling of `name` argument.
* Mark `byteToBinary()` as defunct (slated for removal in 1.6.0).
* Mark `renameData()` as defunct (slated for removal in 1.6.0).

# 1.4.0

* `colormap()` uses `oceColorsViridis` as default colour scheme.
* `download.topo()` uses new NOAA server.
* `mapPlot()` docs warn about problems with version 0.9-8 of sf package.
* `plotProfile()` handles units of added columns.
* `read.argo()` reads more metadata.

# 1.3.0

* Remove a broken link (OK locally but failing in CRAN tests).
* Fix an error in the example for `setFlags.ctd`.
* Add `addSpine()` for defining section spines.
* Add `angle2hms()`.
* Add `argoJuldToTime()`.
* Add `data(amsr)` and improve `subset.amsr()`.
* Add `data(tidalCurrent)` dataset of tidal currents from Foreman.
* Add `oceAxis()`.
* Add `preferAdjusted()` for `argo-class` data.
* Add `read.ctd.odv()`.
* Add `snakeToCamel()` to convert variable names in e.g. Argo netcdf files.
* Add `timeToArgoJuld()`.
* Change `drawPalette()` default colour palette to `oce.colorsViridis()`.
* Change `imagep()` default colour palette to `oce.colorsViridis()`.
* Change `plot.amsr()` default colour palette to `oce.colorsViridis()`.
* Change `plot.section()` default colour palette to `oce.colorsViridis()`.
* Change `plotTS()` to trim isopycnals to realistic salinities and temperatures.
* Change `read.argo()` and `data(argo)` to use camelCase in all metadata.
* Change user-oriented github website generator to `pkgdown`.
* Extend `[[,argo-method` to accept `"ID"` as an alternative to `"id"`.
* Extend `as.cm()` to accept `adp` and `adv` objects.
* Extend `oce.plot.ts()`, adding argument `logStyle`.
* Extend `read.adp.rdi()` to handle 38kHz RDI adp files.
* Extend `read.argo()` and `data(argo)` by adding three more `CYCLE` variables.
* Extend `read.ctd.sbe()` to handle 4 new (Beckman) oxygen variables.
* Extend `read.ctd.sbe()` to handle `.btl` files.
* Fix `plot.coastline` error in box drawing.
* Fix `plotTS` error in auto-scaling if S and T have mixed NA status.
* Fix `subset.argo()` error in `flags`, `location`, and `*QC` in `metadata`.
* Remove dependence on `rgdal` package, using `sf` for map projections.

# 1.2.0

* Address argument doc/code warning that blocked CRAN acceptance.
* Accommodate new ocedata (needed to meet new CRAN dependency rule).
* Add `oceRenameData()` and `oceRenamemetadata()`.
* Deprecate `renameData()`.
* Improve axis control for `mapPlot()` and `plot.coastline`.
* Remove `addColumn()`, `ctdAddColumn()`, `ctdUpdateHeader()`,
  `findInOrdered()`, `mapMeridians()`, `mapZones()`, and
  `oce.as.POSIXlt()`, all of which have been marked as
  "Deprecated" for the past two CRAN releases.
* Extend `read.met()` to read a new Environment Canada data format.
* Handle some upcoming rgdal/R-devel changes.
* Extend `sectionSmooth()` to handle grids beyond data range.
* Extend `read.adp.rdi()` to trim `time` if it longer than other data.
* Improve `geodDist()` to handle second location correctly when `alongPath=FALSE`.
* Improve vignettes.
* Fix itemized-list documentation problem pointed out by Kurt Hornik.

# 1.1-1

* Address "link-time-optimization" issues for CRAN submission.

# 1.1-0

* fix argument error in a .C() call
* Extend `read.odf()` to store ODF header as a list in `metadata`.
* Add `presentTime()`.
* Add vignette on subclassing oce.
* Extend `sectionSmooth()` to handle output grid better, and offer 2 new methods.
* Extend `read.ctd.sbe()` to handle more column names.
* Extend mapping code to support i386/windows.
* Extend `imagep()` to handle combined `flipy` and `ylim` arguments better.

# 1.0-1

* Renamed 0.9-24, released with OAR book publication.

# Version 0.9-24

* Extend `read.odf()` to store ODF header in `metadata`.
* Remove `makeSection()`, after its deprecate/defunct period ended.
* Make defunct: `addColumn()`, `byteToBinary()`, ` ctdAddColumn()`,
  `ctdUpdateHeader()`, `findInOrdered()`, `mapZones()`, `mapMeridians()`,
`oce.as.POSIXlt()`, and `oce.magic()`
* Add `as.tidem()`.
* Extend `read.odf()` to handle multiple `NULL_VALUE`s.
* Improve `handleFlags()`.
* Add `rotateAboutZ()`.
* Add `concatenate()`.
* Add `read.woa()`.
* Extend `colormap()` to include `colfunction` in return value.

# Version 0.9-23

* `tidem()` permits inferred constituents
* `tidem()` follows T_TIDE phase convention
* adorn argument removed from some plot functions (after being defunct 1+ year)
* deprecate `findInOrdered()`

# Version 0.9-22

* `plot.section()` can use external bathymetry
* `lowpass()` added
* `ctdTrim()` can isolate upcasts
* deprecate `byteToBinary()`
* address a CRAN NOTE about UTF-8 strings in data

# Version 0.9-21

* `adpEnsembleAverage()` added
* `mapCoordinateSystem()` added
* woce-exchange missing-value inference
* woce-exchange autodetection, plus read all data types
* `download.amsr()` updated for data-server change
* `download.met()` added
* `as.ctd()` can specify columns within argo data
* `read.adp.rdi()` handles more file types and larger RDI files
* `read.odf()` handles contradictory metadata better
* `oce.as.POSIXlt()` deprecated
* `oceSetData()` handles units better
* address `R_registerRoutines` build-time warning
* fix memory-usage error in `coastlineCut()`

# Version 0.9-20

* remove observatory object type (changed format, seldom-used type)
* remove pangaea object type (undocumented format, seldom-used type)
* improvements for BioArgo data
* Spanish translation by Pablo Valdés
* make `read.gps()` handle more files
* deprecate `ctdAddColumn()` and `ctdUpdateHeader()`
* `download.topo()` added and `read.topo()` updated to handle new formats
* `download.amsr()` added
* `composite()` added
* `addColumn()` marked as deprecated
* `read.cm.s4()` reads all data in file
* `as.cm()` added
* `read.odf()` handles Aanderaa current meter data
* `mapPlot()` and `mapAxis()` get cex.axis parameter
* `subset(amsr)` added; various other amsr improvements

# Version 0.9-19

* deprecate `adorn` argument in plot functions
* make `read.ctd()` obey `missingValue` (renamed)
* improve map projections
* add `renameData()`
* improve support for SBE files
* remove `is.beam()`, `is.xyz()`, `is.enu()` and `coordinate()`, since
  `x[["coordinate"]]` now permits these simply
* replace several function-style accessors with `[[` style
* fix test-suite error relating to an `rgdal` change
* add `handleFlags()` as generic plus specifics for ctd and argo classes
* use S4 documentation, e.g. `?"plot.ctd"` instead of `?plot.ctd`
* add `subset(argo, "adjusted")`
* make `read.argo()` read all documented data and metadata fields
* add `trimString()`

# Version 0.9-18

* improve `plot.coastline()` and `mapPlot()`
* add support for G1SST satellite
* all objects now have metadata items for units and flags
* `ctdTrim()` method renamed: old A and B are new A; old C is new B
* support more channels and features of rsk files
* `as.adp()` added
* convert argo objects to sections
* `makeSection()` deprecated; use as.section() instead
* `read.adp.rdi()` handles Teledyne/RDI vsn 23.19 bottom-track data
* `geodXyInverse()` added; geod functions now spell out longitude etc
* `read.odf()` speeded up by a factor of about 30
* add colour palettes from the `cmocean` Python package by Kristen Thyng
* add `as.oce()`
* rename `drifter` class as `argo` to recognize what it actually handles
* add `oceColorsViridis()`
* `interpBarnes()` has new argument `pregrid`
* `binMean2D()` has new argument `flatten`
* `data(topoWorld)` now has longitude from -179.5 to 180
* `ODF2oce()` added
* `read.odf()` handles more data types
* `read.adp.rdi()` reads more VmDas (navigational) data
* ITS-90 is now the default temperature unit
* ctd objects can have vector longitude and latitude
* logger class renamed to rsk
* `bremen` class added
* `coastlineCut()` added
* `rgdal` package used instead of local PROJ.4 source code
* `mapproj`-style map projections eliminated

# Version 0.9-17

* remove an `exit()` call in a C function

# Version 0.9-16

* cite PROJ.4 contributors in DESCRIPTION
* address package-build warnings; improve DESCRIPTION (thanks, BR)

# Version 0.9-15

* `plot.echosounder()` gets new argument `drawPalette`
* `data(landsat)` taken from ocedata (and shrunk)
* `data(nao)` and data(soi) moved to ocedata
* `mapTissot()` added
* `read.logger()` with ctd-type data infers salinity if necessary
* `ctdTrim()` can have "method" as a function
* `as.topo()` can convert "bathy" objects from the marmap package
* `"+.section"` renamed sectionAddStation()
* `oceApprox()` renamed oce.approx(), with an alias for compatibility
* `oce.grid()` added
* `"tdr"` class renamed `"logger"`
* `swCSTp()` added
* `swConductivity()` now `swThermalConductivity()`; using Caldwell (1974).
* many `sw()` functions handle eos="gsw"
* `library(gsw)` replaces `teos()`
* `curl()` added
* handle new NOAA ascii topography dataset type
* `mapGrid()` added
* `data(soi)` moved to ocedata package
* `lonlat2map()` added
* `map2lonlat()` made more accurate
* permit `proj4` style map projections
* landsat-7 (and possibly 4 and 5) support
* landsat decimation/plotting improvements
* `drifterGrid()` added
* `mapDirectionField()` added
* `mapArrows()` added
* `read.index()` added
* `utm2lonlat()` and `lonlat2utm()` added
* `data(met)` changed to time of Hurricane Juan

# Version 0.9-14

* fix compile-time warning
* `colormap()` added, and functionality added to `imagep()` and `drawPalette()`
* `imagep()` and `drawPalette()` new arg. `axisPalette` (suggested C. Richards)
* `drawPalette()` has new args: plot, pos, levels, and cex.axis
* `ctdDecimate()` permits user-supplied method function
* `data(nao)` added; data(soi) updated, and names improved in latter
* landsat support added
* `plotProfile()` and `plotTS()` get new argument `pt.bg`
* `plot.section()` grids the data if needed
* rename `sealevelHalifax` dataset as `sealevel`
* translate some axis names (Spanish, French, German and Mandarin)
* `plotProfile()` now has `ytype="depth"`
* `mapImage()` and friends now demand a map exists first
* `runlm()` added
* `mapScalebar()` added
* `subset.topo()` added
* `read.topo()` now handles GEBCO netcdf files
* `decimate()` now handles topo objects
* reverse oce.colorsGebco colours for water
* `drawIsopycnals()` and `plotTS()`: improve isopycnal labels
* `ctdDecimate()` handles new method `unesco`
* coastline improvements (now 3 resolutions)
* `webtide()` improvements
* `read.observatory()` added
* `read.ctd.itp()` added
* `mapImage()` speeded up (60X in 1deg SST example)
* `binApply1D()` and `binApply2D()` added
* `binCount1D()`, `binMean1D()`, `binCount2D()`, and `binMean2D()` added
* `numberAsHMS()` added
* `gps` data type added (provisionally)
* various functions: reorder arguments putting longitude before latitude
* `magneticField()` replaces `magneticDeclination()`
* `ungrid()` added
* `mapPlot()`: improve zone and meridian aesthetics
* `detrend()` returns list with detrend vector and coefs
* `decodeTime()` improvements
* `interpBarnes()` has new argument `trim`
* `standardDepths()` added
* `mapLongitudeLatitudeXY()` added
* `read.ctd.woce()` now accepts a format used once by Arctic scientists
* `swRrho()` added
* `plotProfile()` now accepts type="o"
* `read.adv.nortek()`: read salinity
* `ctdDecimate()`: fix bug for NULL columns [issue 327]
* `plot.section()` now chooses best coastline file
* `swPressure()` added
* `ctdFindProfiles()` added
* `as.ctd()`: new arguments filename etc
* remove `oceBisect()`, which merely reproduces `uniroot()`
* move large/specialized datasets to library(ocedata)

# Version 0.9-13

* prepare for an upcoming release of R-devel

# Version 0.9-12

* move vignette source directory

# Version 0.9-11

* `numberAsPOSIXct()` now handles types "sas" and "spss"
* `data(turbulence)` added
* `plot.echosounder()`: use white for below-threshold values
* `plot.echosounder()`: add arg `beam` to e.g. display Sv
* `read.echosounder()`: handle dual-beam and split-beam data
* `read.echosounder()`: decode bottom-pick
* `swSoundAbsorption()`
* `read.adp()` doc improved regarding variable orientation of RDI files

# Version 0.9-10

* improve line breaks in some help pages (thanks, B. Ripley)

# Version 0.9-9

* `mapPlot()` uses ... graphical parameters (e.g. cex) in labelling axes
* fix recent bug in reading CNV files (issue 318)
* `plotTS(...,mar=NULL)` now avoids setting `par(mar)`

# Version 0.9-8

* fix memory error in teos handler (thanks, B. Ripley)

# Version 0.9-7

* make `ctdTrim()` work in R 3.0
* support for adp vmdas nav data (coded by Clark Richards)
* `approx3d()`: added
* `drawPalette()`: new arg `fullpage`
* `plot.lobo()`: add arg `which`
* `read.coastline.openstreetmap()` added
* `data(levitus)` added
* `plot.coastline()` gets new arguments `projection` etc to use `mapPlot()`
* `mapContour()` permits first argument to be a `topo` object
* `mapPlot()` gets new arguments `axes`, `bg`, `fill`, and `drawBox`

# Version 0.9-6

* address compilation error on Windows-64 platform
* make some long-running operations more interruptible
* `oceApprox()`: permit equal x values [issue 279]

# Version 0.9-5

* halve package check time

# Version 0.9-4

* `as.section()` added
* add `data(endeavour)`
* add map projections, with provisional functions `mapPlot()` etc
* `plot.ctd()`: add argument `add`
* add crude mapping support
* `interpBarnes()`: add args `xgl` and `ygl`
* `read.section()`: add `directory` argument
* `sectionSmooth()`: add `barnes` method
* `oce.plot.ts()` and `oce.axis.POSIXct()`: add tformat argument
* `read.aquadopp()` and cousins added [issues 253 and 258]
* `read.ctd()` guesses waterDepth if not supplied [issue 253]
* `plotTS()`: extend isopycnal range [issue 252]
* `plot.section()` contouring improved
* `data(oceans)` added
* `data(papa)` added
* `fillGap()` works on matrices as well as vectors
* `read.lobo()` is more flexible on column names
* `imagep(..., axes=FALSE)`: no longer plots axis box [issue 249]
* `oce.plot.ts()`: alter time auto-ranging method
* `oceBisect()`: increase generality of function whose root is sought
* `plotTS()`: clean axes after isopycnals drawn
* `teos10`: update test values (with help from PB)
* `read.adp.nortek()`: handle missing `to` argument as documented
* `beamUnattenuateAdp()` renamed `beamUnspreadAdp()`
* `plotTS()` and `plotProfile()`: permit type=`n`
* `plotProfile()` with `ytype="z"`: fix bug in y label
* improve error message if TEOS-10 is missing

# Version 0.9-3

* `example(riley)`: remove error in R-devel
* `plot.ctd()` with `which=7` improvement
* `swSCTp()`: accept conductivity in mS/cm and S/m
* `as.ctd()`: accept TEOS-10 values SA and CT
* make TEOS routines more resistant to unphysical values

# Version 0.9-2

* remove unwanted files during build process

# Version 0.9-1

* `pwelch()`: fix bug in frequency
* `tidem()`: add argument `regress`
* rename oce.plot.sticks() as `plotSticks()`; delete `stickplot()`
* use S4 style for all plot() methods
* `data(giss)` added
* `data(topo2)` added
* `plotPolar()` added
* `oce.magic()` new name for `magic();` renamed `oceMagic()` in 0.9-15
* `errorBars()` added
* `numberAsPOSIXct()` add a leap second for July 2012

# Version 0.8-10

* `plot.topo()`: automatic cut-point shift
* add support for Nortek aquadopp-profiler instruments
* `oce.contour()` added
* lisst class added
* `plotTaylor()` added
* `oce.as.raw()` added
* `binmap()` added

# Version 0.8-9

* make the newly-added TEOS-10 functions compile on Windows

# Version 0.8-8

* `data(cm)` added
* `plot.coastline()`: improve argument `geographical`
* add `prettyPosition()`
* add `grad()`
* add (preliminary) support for TEOS-10, installed separately
* add `integrateTrapezoid()`
* `plotTS()`: add argument teos
* add `teos()` as an interface to the TEOS-10 library
* add `moonAngle()` plus other astronomy functions, e.g. julianDay()

# Version 0.8-7

* `plotInset()`: remove `bg` and `fg` arguments
* `plotTS()`: add `bg` argument
* rename `dt` object and functions to `tdr`
* `plotTS()`: add argument `inset`
* `read.adp.rdi()`: fix a bug in bottom-tracking; add soundSpeed vector
* `data(echosounder)` created
* `formatPosition()` created
* `plot(echosounder)`: new arguments atTop and labelsTop
* adp objects now also hold `percent good` for RDI instruments
* `plot.drifter()`: improve multi-panel plots
* `imagep()`: add argument `missingColor`
* `imagep()` and `drawPalette()`: add arguments `labels` and `at`

# Version 0.8-6

* `imagep()`: permit POSIXt type for zlim

# Version 0.8-5

* reconstruct data(ctd) so plot will focus maps better
* `plotInset()` added
* `grid()` works for oce-based time axes
* `imagep(..., filledContours)`: obey ylab argument
* `subset()`: permit subsetting ADP by pressure
* `oce.plot.ts()`: obey arguments xaxt and yaxt
* improve support for shapefile coastlines

# Version 0.8-4

* replace `center` argument for plotting topography and coastline with
  `clatitude` and `clongitude`
* add `echosounder` class
* `fullFilename()`: handle URLs properly
* ADV objects: add access to slow variables (e.g. `headingSlow`)

# Version 0.8-3

* add `met` class
* `subset.oce()` handles section objects
* `draw.section()`: permit `xtype="latitude"` or `"longitude"`
* `imagep()` has drawPalette="space" option

# Version 0.8-2

* fix bug in read.coastline(), reversing lon and lat (issue 162)
* fix bug in S4 adp validity checker
* `geodDist()`: add argument `alongPath`
* remove compilation warning about doc for summary.ctd()

# Version 0.8-1

* switch to S4 classes, and add new accessors and `show` functions.
* add support for RBR `rsk` files
* `plot.section()` now uses `col` if supplied

# Version 0.7-1

* improve `oce.colorsJet()`
* improve `plotProfile()` vertical range with missing data
* add `data(drag)`
* `plotProfile()' and `plot.ctd()`: add `xtype="spice"`
* add `data(geosecs235)`
* `plotProfile()` and `plot.ctd()`: add argument `keepNA`
* `as.ctd()`: add arguments `other` and `missingValue`
* `read.lobo()`: tolerate more formats, including missing velocities
* add `data(schmitt)`
* add accessor `spice()`
* handle O2 and nutrients in CTD data and sections
* `swSCTp()`: use surface pressure as a default
* `plotTS()`: add arguments `inSitu` and `referencePressure.
* `plotTS()`: handle section objects
* fix bugs reported in issues 150, 151, 153, 155, 156
* `imagep()`: change default `drawContours` to FALSE

# Version 0.6-1

* add `adv` dataset
* generalize `swN2()` by adding new arg `derivs`
* `oce.plot.ts()` gets new arg `axes`
* add accessor `distance()`
* rename to `plotTS()` and `plotScan()` to avoid S3 check warnings
* switch `makeFilter()` argument `asKernel` default to TRUE

# Version 0.5-1

* add support for ARGO drifters
* `latitude()` and `longitude()`: new argument to repeat b`yDepth`
* `addCtdColumn()`: replace if column already exists
* add CTD accessors for lat, lon, pressure, salinity, temperature

# Version 0.4-1

* `plot.sealevel()`: only show 3 panels, for clarity
* `plot.ctd()`: add argument `type`
* `imagep()`: handle x, y, and z as `image()` does
* `oce.plot.ts()`: inferred ylim matches data range within provided xlim
* `read.adv.nortek()`: handle extra analog data
* add CTD accessors `longitude()`, `latitude()`, `time()`, `elevation()`, etc

# Version 0.3-1

* add `read.oce.odf()`
* add findInOrdered
* generalize `tidem()` argument list
* `read.adv.nortek()`: handle burst-mode data
* add datasets wilson, redfieldNP, redfieldNC, redfieldPlankton, and riley
* change `data(ctd)` to be derived from `data(ctdRaw)`
* change to monitor=FALSE for all reading functions
* add accessor functions `heading()`, `latitude()`, `longitude()`, `pitch()`,
  `pressure()`, `processingLog()`, `roll()`, `salinity()`, `temperature()`,
  `time()`, and `velocity()`
* rename history as `processingLog.
* flatten data objects to be 1 level thick (for future matlab exports)
* add `threenum()` added, and use it in all summaries (for speed)

# Version 0.2-3

* `head.adp()` added
* `tail.adp()` added
* `extract()` added

# Version 0.2-2

* `velocityStatistics()` added
* new names: `oceApprox()` `integerToAscii()` `rangeLimit()` `ctdRaw`
* `topoInterpolate()` added
* `numberAsPOSIXct()`: add type `argos`
* rename `beamAttenuateAdp()` as `beamUnattenuateAdp()`

# Version 0.2-1

* camel-case function and argument names (and the NEWS file)

# Version 0.1-83

* add `binAverage()`
* `fillGap()`: add `rule` argument, analogous to same for `approx()`
* add `rescale()`
* `read.pt()`: fix timing error; use as.pt() to create return value
* add `as.pt()`
* `read.adp.rdi()` uses checksums
* `number.as.POSIXct(type="gps")`: account for leap seconds

# Version 0.1-82

* remove warning that occurs for R 2.13.0 (alpha)
* `number.as.POSIXct()`: accept GPS times
* rename e.g. `adv.2enu()` as `to.enu.adv()`
* speed up conversion of ADP and ADV to enu by more than a factor of 10
* change RDI coordinate handling
* rename `match.bytes()` as `matchBytes()
* add `drawPalette()`
* remove `matlab2POSIXt()`, now a sub-case of `number.as.POSIXct()`
* `number.as.POSIXct()`: added
* `oceBiset()`: added
* `despike()`: add argument `action`
* `detrend()`: added
* `read.adp.sontek()`: handle PCADP type
* `read.adp.sontek.serial()`: added
* `read.section()`: handle WOCE quality flags for salinity
* `retime()`: added
* `read.adv.sontek.serial()`: correct error in class of `a` and `c`
* `despike()`: use string value for argument `method`
* `logger.toc()`: fix bug in discovering files ranging over days
* `as.ctd()`: make temperature and salinity into vectors, if not already
* `plot.TS()`: add argument `use.smoothScatter`
* `oce.debug()`: flush the console after printing a message
* `sw.theta()`: rename the method possibilities, to lower case
* in some sw functions, rename `pref` as `reference.pressure`
* `read.ctd()`: fix bug in getting start.time for some time formats
* `pwelch()` added
* `plot.sealevel()`: make y axis obey range in data subset, if xlim is given
* `adp.2enu()` added
* `coastline.world`: improve resolution by a factor of 4 or so
* `read.ctd.woce()`: infer water depth as max(pressure) if not in header
* `section.smooth()`: handle misordered stations; handle missing values better
* `plot.section()`: allow strings for `which`; improve contouring
* `plot.ctd()` and `plot.profile()`: add `use.smoothScatter` argument
* `coriolis()`: improve omega value

# Version 0.1-81

* add `despike()`
* add `range.limit()`
* add `unabbreviate.time()`
* add support for bottom-tracking RDI ADCPs
* add support for interocean `s4` current meters
* add `unwrap.angle()`
* `read.adv.nortek()`: detect the velocity range
* add processing.log.add(), an alternative to processing.log.append()
* `plot.ctd()`: add more plot types; which=9 for salinity, not density
* `as.ctd()`: improve flexibilty
* `predict.tidem()`: add `newdata` argument
* `adv.2enu()` added
* `adp.xyz2enu()`: make it correct for heading.bias
* `make.filter()`: can now produce tskernel type
* improve (but temporarily limit) fill.gap
* `plot.adp()`: add `use.layout` argument
* `window.oce()` added
* make objects remember full filename, not just local filename (issue8)
* `plot.tidem()`: remove argument `plot.type`, using `which` instead
* `read.pt()`: add arguments from, by, and to
* fix issue 57; `read.adp.rdi()` read the heading incorrectly
* add "rr" method to `ctdDecimate()`
* `oceApprox()` added
* topoWorld dataset added
* `plot.ctd()`: by default, make coastlines extend to box boundaries
* reformulate sun.angle in R
* fix google-code issue 56; `plot.topo()` should accept land.z=NULL]
* fix google-code issue 55; `plot.topo()` could go past poles]
* fix google-code issue 54; `plot.topo()` was resetting par() on exit]
* fix google-code issue 53; `interp.barnes()` gives poor error msg for
  mismatched x and y
* fix google-code issue 52; `plot.section()` draws incorrect bottom shape

# Version 0.1-80

* add sun.angle; not tested yet; needs R reformulation
* fix google-code issue 51; `summary.section()` does not report water depth
* fix google-code issue 50; non-bug relating to sun elevation
* fix google-code issue 49; `demo(TS)` broken

# Version 0.1-79

* fix google-code issue 48; `oce.plot.sticks()` ignored page ratio
* fix google-code issue 47; cannot read new MEDS sealevel format

# Version 0.1-78

* add `data(RRprofile)`
* to `plot.TS()`, add arguments lwd.rho and lty.rho

# Version 0.1-77

* refactor adp code wrt transformation matrices
* add `matrix.smooth()`
* improve labels for `oce.axis.POSIXct()`
* set default for `debug` argument to getOption("oceDebug") in all functions
* `plot.profile()`: add argument ytype; change `type` to `xtype`
* add `swZ()
* add `oce.smooth()`
* add `read.adv()`, etc., supporting Nortek and Sontek devices
* add `read.adp()`, etc., supporting Nortek, RDI and Sontek devices
* add `oce.plot.ts()`
* add `imagep()`
* add `bcd2integer()`
* add `matlab2POSIXt()`
* to most plot functions, add `mgp` and `mar` arguments
* `plot.pt()`: add plim and Tlim arguments.
* `gravity()`: give default for latitude argument.
* `plot.sealevel()`: remove argument focus.time and add argument which.
* rename tdr functions as pt, to reflect common phrasing.
* add `geod.xy()`.
* add argument `mgp` to all plotting functions.
* use abbreviations for axis names if space is tight.
* add argument `adorn` to all `plot()` functions.
* add `oce.colorsTwo()`, `oce.colorsJet()`, and `oce.colorsPalette()`.
* add `byte2binary()`
* add "lty.grid" to all CTD plotting functions
* add "+.section", a more convenient way to build sections from stations
* rework `summary()` for all existing objects
* `as.sealevel()`: rename sampling.interval as `deltat`

# Version 0.1-76

* `sw.N2()`: make it tolerate NAs; adjust df to make result smoother
* add `makeFilter()`
* add `decimate()`, which may eventually replace ctd.decimate()
* `read.tdr()`: handle 5-column data files; add tz argument
* `plot.profile()`: add types sigma+dpdt and sigma+time
* `ctdTrim()`: add pmin parameter
* `plot.ctd()`: add Slim, Tlim, plim, lonlim, latlim args; add maps
* add coastline.sle dataset
* `plot.TS()`: fix isopycnal labels for fresh water; add args Slim, Tlim
* `read.ctd()`: make it understand another SBE format
* add `parseLatlon()`
* `oce.edit()`: add argument `action`
* add `oce.write.`table()
* add `fillGap()`

# Version 0.1-75

* add `addColumn()`
* add `undriftTime(`)
* add `tdrPatm()`
* make `readTdr()` gather serial no. info; show this on `plot.pt()`
* switch to recommended version-number format
* remove `section.subset()`, replaced by `subset()`
* improve log items in functions that read and assemble oce objects
* add `subset.oc`e()
* add `header()`
* add argument `which` to `read.rbrdtr()`
* make `read.rbrdtr()` understand headers better
* add arguments `xtype' and `ytype` to `plot.section()`
* improve accuracy of bottom drawn by `plot.section()`
* add `section.smooth()`
* fix bug making `section.smooth()` fail if <4 good data at a level
* fix bug making `summary.ctd()` fail for results of section.smooth()
* fix bug making `sw.dynamic.height()` choke on empty stations
* fix bug making `plot.section()` die if x is unordered
* rename rbrpt functions to tdr functions, improving generality

# Version 0.1.74

* add argument "src" to as.ctd()
* `plot.section()` RHS axis needs tics
* trim axis whitespace in plot.profile() and plot.TS()
* make `read.oce()` understand WOCE section type
* speed up `swSpice()`
* add `read.pt()`, `summary.pt()`, `plot.pt()`, and `ptTrim()`
* make `plot.TS()` isopycnal label size be same on top and right sides
* add lwd argument to `plot.profile()`
* give `plot.section()` ability to control contour levels and labels
* give `plot.section()` tics for station-location

# Version 0.1.73

* `plot.topo()` narrows autoscale to xlim-ylim region, if provided
* add `plot.ctd()` arg `textpanel`, and put the profiles in the same row
* fix bug in `plot.profile()` to let it take Slim,..., as args
* fix bug in `plot.profile()` font size
* fix bug in `plot.profile(type="S_T")` positioning
* fix bug in section plot (bottom was missing)
* fix bug in size of salinity axis label for plot.ctd()
* use pch=21 for TS plots (so data density is more visible)

# Version 0.1.72

* add `interp.barnes()`

# Version 0.1.71

* fix bug in `makeSection()` (ignored the list, if a list provided)
* add `oce.edit()`, later renamed `oceEdit()`
* add topoMaritimes dataset
* add `read.topo()`, `plot.topo()`, and `summary.topo()`
* add `gebcoColors()` ... renamed `oce.colorsGebco()` in version 0.1.77
* make `plot.section()` check that pressures coincide

# Version 0.1.70

* `plot.TS()`: make isopycnal list work better for nearly-fresh water
* `trimCtd()`: improve equilibration-phase deletion
* `read.ctd()`: handle cases without cruise information or scan column
* permit setting xlab and ylab in `plot.TS()`
* make processing log timestamps be in GMT
* add `as.windrose()` and `plot.windrose()`
* add `sealevel.tuk` sea-level dataset
* add `tidem()`, `predict.tide()`, `summary.tide()`, `plot.tide()`, data(tidedata)`
* establish a uniform form for objects created by "read" and "as"
* let `swRho()` and similar functions take a ctd object as a single argument
* add `swDynamicHeight()`
* make `section.grid() and `ctd.decimate()` extrapolate to surface
* add `summary.oce()`
* make all objects inherit from a new class "oce"
* fix google-code issue 12: `read.sealevel()` had a hard-wired filename

# Version 0.1.69

* modify documentation slightly
* start a migration to more uniform object structures
* rename `as.CTD()` to `as.ctd()`, to make it consistent with similar functions
* add `read.section()`
* add `sectionGrid()`, which only grids in p at the moment
* add a03 dataset (renamed `section` in 0.9-13)
* fix SF bug 1833719: warnings from read.ctd()
* fix google-code issue 8: read.coastline() not producing data
* improve ability of trimCtd() to ignore spurious initial data
* add "connectPoints" option to plot.TS()
* add "station" item to "ctd" object
* change `as.CTD()` to produce sigma.theta instead of sigma

# Version 0.1.68

* add coastline.world; increase resolution of coastline.maritimes
* `plot.TS()` no longer rotates the RH margin isopycnal labels

# Version 0.1.67

* allow `swRho()` and similar take matrices as arguments for S, T, etc

# Version 0.1.66

* fix bug in `plot.TS()` that prevented it from showing some isopycnals

# Version 0.1.65

* switch to UNESCO formulation for sw potential temperature by default
* prevent warnings with C90 compiler on some machines

# Version 0.1.64

* add `read.oce()`, a generic function for reading several oceanographic files
* make `read.ctd()` understand WOCE-exchange files
* make `read.sealevel()` understand comma-separated data from MEDS
* make `plot.sealevel()` skip spectral graphs if timeseries has NA values
* improve aesthetics of `plot.ctd()`
* extend `plot.sealevel()` by adding focus.time argument
* add `ctdDecimate()`
* add `oce.as.POSIXlt()`
* add `latlonFormat()`, `latFormat()`, and `lonFormat()`
* add `as.coastline()`
* add `make.section()`
* add `summary.section()`
* add `plot.section()`
* add `data(section)`

# Version 0.1.63

* add `swSoundSpeed()`

# Version 0.1.62

* add `historyAppend()` (later named `processingLogAppend()`) and related code
* improve `ctdTrim()`

# Version 0.1.61

* add rudimentary read/summary/plot functions for lobo datasets

# Version 0.1.60

* add vignette; address some build-stage warning messages

# Version 0.1.59

* set up data files to permit latin1 encoding of some content

# Version 0.1.58

* rename all seawater-related functions to e.g. `swTheta()`, to allow
  for atmospheric analogs

# Version 0.1.57

* make `as.CTD()` accept `length(p)=1` e.g. for surface plots

# Version 0.1.56

* make `read.coastline()` handle S files

# Version 0.1.55

* keep `swTheta()` from complaining if `length(p)` equals 1

# Version 0.1.54

* add `swAlpha() `and `swBeta()`

# Version 0.1.53

* add `swAlphaOverBeta()`

# Version 0.1.52

* document the use of `df` in `swN2()`, `plot.ctd()`, etc.

# Version 0.1.51

* permit `swRho()` (and similar) to take NAs in args (bug B32)

# Version 0.1.50

* permit `swRho()` and descendants to take scalar pressure.

# Version 0.1.49

* add a`s.CTD()`

# Version 0.1.48

* add `"UNESCO1983"` formulation to `swTheta()` as an alternative

# Version 0.1.47

* speed up `swTheta()` by moving vector work from R to C

# Version 0.1.46

* make `geodDist()` accept a vector for location 1 and a scalar for location 2

# Version 0.1.45

* make `read.coastline()` understand matlab and Splus formats

# Version 0.1.44

* make `plot.coastline()` scale lat and lon correctly

# Version 0.1.43

* add `read.coastline()`, `summary.coastline()`, and `plot.coastline()`

# Version 0.1.42

* add `as.sealevel()`

# Version 0.1.41

* add `read.sealevel()`, `summary.sealevel()`, and `plot.sealevel()`

# Version 0.1.40

* add name option to `plot.ctd.scan()`

# Version 0.1.39

* add `lapse.rate()`

# Version 0.1.38

* fix B27 (header not updated for columns added during read.ctd)

# Version 0.1.37

* rename as `ctdTrim()`
* rename to `ctdWrite()`

# Version 0.1.36

* add `write.ctd()`
* add `ctdAddColumn()`
* add `ctdUpdateHeader()`

# Version 0.1.35

* add `plot.ctd.scan()`

# Version 0.1.34

* make `read.ctd()` calculate S if missing but C, T, and p are given
* add `swSCTp()`

# Version 0.1.33

* add `swConductivity()`

# Version 0.1.32

* rename `oceRho()` to `swRho()`

# Version 0.1.31

* use namespace

# Version 0.1.30

* add `oce.viscosity()`

# Version 0.1.29

* fix B23 (not done yet)
* fix B22 (not done within oce; done at system level)

# Version 0.1.28

* add pressure method to `trimCtd()`

# Version 0.1.27

* vectorize `swRho()` since it was too slow on large datasets

# Version 0.1.26

* make `demo(oce.ctd)` print data

# Version 0.1.25

* fix some documentation errors that the 2.0.0 CHECK noticed

# Version 0.1.24

* profiles have correct y-axis
* OSX port, but must first do e.g. `export PKG_LIBS="-L/sw/lib"`

# Version 0.1.23

* OSX port (you must first do `export PKG_LIBS="-L/sw/lib"`)

# Version 0.1.22

* add `trim.ctd()`

# Version 0.1.21

* permit lat/lon with the "N"-type indicator at start

# Version 0.1.20

* fix bug B16 (label the isopycnals on TS diagram)

# Version 0.1.19

* make `read.ctd()` handle more file types

# Version 0.1.18

* add `oceCoriolis()`, `oceDepth()`, `oceGravity()`, `oceTFreeze()`

# Version 0.1.17

* add `oceSpecificHeat()` and `oce.N2()`; fix bug B18

# Version 0.1.16

* get `data(ctd)` working

# Version 0.1.15

* add `oceSTRho()`

# Version 0.1.14

* fix bug B15 (maybe)

# Version 0.1.13

* add oceTSRho

# Version 0.1.12

* fix bugs B10, B11, and B13

# Version 0.1.11

* fix bugs B6, B7, and B12

# Version 0.1.10

* add `geodDist()`.
* add `plot.ctd()`, which is rudimentary for now.
* fix bug B8.

# Version 0.1.9

* add `spice()` function

# Version 0.1.8

* permit all eos functions to handle missing values
* fix bug B4

# Version 0.1.7

* add `summary.ctd()` method.
* make `read.ctd()` return an object of type ctd.
* get tests/ working.

# Version 0.1.6

* create tests/oce.test1.R

# Version 0.1.5

* add `oceSigma()`, `oceSigmaT()`, and `oceSigmaTheta()`

# Version 0.1.4

* permit various EOS functions to handle lists
* fix bug B3

# Version 0.1.3

* add `ocerho()`

# Version 0.1.2

* fix file-path bug
* fix bug B2

# Version 0.1.1

* add `theta()`
* fix bug B1

# Version 0.1.0

* add `read.ctd()`

