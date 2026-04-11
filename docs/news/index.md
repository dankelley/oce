# Changelog

## oce 1.8.4

- Add
  [`bodcNames2oceNames()`](https://dankelley.github.io/oce/reference/bodcNames2oceNames.md)
  to rename NERC/BODC variables (issue
  [\#2235](https://github.com/dankelley/oce/issues/2235)).
- Add `D4902337_219.nc` built-in Argo profile data file (issue
  [\#2272](https://github.com/dankelley/oce/issues/2272)). (issue
  [\#2331](https://github.com/dankelley/oce/issues/2331)).
- Add
  [`netcdfTOC()`](https://dankelley.github.io/oce/reference/netcdfTOC.md)
  to overview NetCDF files (issue
  [\#2232](https://github.com/dankelley/oce/issues/2232)).
- Add
  [`oceColorsCubeHelix()`](https://dankelley.github.io/oce/reference/oceColorsCubeHelix.md),
  a perceptual colour scheme used in Astronomy
- Add
  [`oceRename()`](https://dankelley.github.io/oce/reference/oceRename.md)
  to rename variables, flags and units (issue
  [\#2238](https://github.com/dankelley/oce/issues/2238)).
- Add `read.sealevel.gc2026` to read a Govt Canada format (issue
  [\#2349](https://github.com/dankelley/oce/issues/2349)).
- Add
  [`read.xbt.edf2()`](https://dankelley.github.io/oce/reference/read.xbt.edf2.md)
  to read a tab-separated file type.
- Add
  [`read.xbt.noaa2()`](https://dankelley.github.io/oce/reference/read.xbt.noaa2.md)
  to handle UBT format (issue
  [\#2289](https://github.com/dankelley/oce/issues/2289)).
- Add `Tidal Analysis` vignette (issue
  [\#2243](https://github.com/dankelley/oce/issues/2243)).
- Change `[[` to handle `"cabbeling"`, for objects holding sufficient
  information for this computation (issue
  [\#2338](https://github.com/dankelley/oce/issues/2338)).
- Change
  [`adpRdiFileTrim()`](https://dankelley.github.io/oce/reference/adpRdiFileTrim.md)
  to reorder parameters, and to accept `indices` parameter (issue
  [\#2330](https://github.com/dankelley/oce/issues/2330))
- Change `amsr[[` to handle `"metadata"` parameter (issue
  [\#2229](https://github.com/dankelley/oce/issues/2229)).
- Change
  [`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md)
  (issue [\#2270](https://github.com/dankelley/oce/issues/2270)).
- Change
  [`as.unit()`](https://dankelley.github.io/oce/reference/as.unit.md) to
  handle more units (issue
  [\#2240](https://github.com/dankelley/oce/issues/2240)).
- Change
  [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) to
  handle Argo data better (issue
  [\#2270](https://github.com/dankelley/oce/issues/2270)).
- Change
  [`binApply1D()`](https://dankelley.github.io/oce/reference/binApply1D.md)
  to
  - handle the `...` argument (issue
    [\#2265](https://github.com/dankelley/oce/issues/2265)) and
  - handle time zones better (issue
    [\#2266](https://github.com/dankelley/oce/issues/2266)).
- Change
  [`concatenate()`](https://dankelley.github.io/oce/reference/concatenate.md)
  to handle lists properly (issue
  [\#2278](https://github.com/dankelley/oce/issues/2278)).
- Change
  [`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md)
  to handle realtime data files (issue
  [\#2231](https://github.com/dankelley/oce/issues/2231)).
- Change
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md) to
  handle `decimate` as set in `~/.Rprofile` (issue
  [\#2263](https://github.com/dankelley/oce/issues/2263)).
- Change
  [`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md)
  to handle a \[coastline-class\] object as first parameter (issue
  [\#2284](https://github.com/dankelley/oce/issues/2284)).
- Change
  [`magneticField()`](https://dankelley.github.io/oce/reference/magneticField.md)
  to handle the latest (generation 14) IGRF formulation (issue
  [\#2274](https://github.com/dankelley/oce/issues/2274)).
- Change
  [`mapScalebar()`](https://dankelley.github.io/oce/reference/mapScalebar.md)
  to
  - handle `x="top"` (issue
    [\#2283](https://github.com/dankelley/oce/issues/2283)) and
  - handle `length` under 1km (issue
    [\#2287](https://github.com/dankelley/oce/issues/2287)).
- Change
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) to
  handle `las` as a parameter (issue
  [\#1707](https://github.com/dankelley/oce/issues/1707)).
- Change
  [`oceRename()`](https://dankelley.github.io/oce/reference/oceRename.md)
  to default to a built-in IOOS dictionary (issue
  [\#2311](https://github.com/dankelley/oce/issues/2311)).
- Change
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md)
  to
  - handle `grid` better (issue
    [\#2267](https://github.com/dankelley/oce/issues/2267)),
  - handle `simplify` better (issue
    [\#2277](https://github.com/dankelley/oce/issues/2277)) and
  - handle `xlab` better (issue
    [\#2285](https://github.com/dankelley/oce/issues/2285)).
- Change
  [`plot.coastline()`](https://dankelley.github.io/oce/reference/plot-coastline-method.md)
  to improve axes aesthetics (issue
  [\#2342](https://github.com/dankelley/oce/issues/2342)).
- Change
  [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md)
  to improve map margin note (issue
  [\#2343](https://github.com/dankelley/oce/issues/2343)).
- Change
  [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  to improve unit spacing (issue
  [\#2352](https://github.com/dankelley/oce/issues/2352)).
- Change
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  to handle `eos="gsw"` (issue
  [\#2256](https://github.com/dankelley/oce/issues/2256)).
- Change
  [`pwelch()`](https://dankelley.github.io/oce/reference/pwelch.md) to
  fix error if `nfft` and `spec` are provided (issue
  [\#2299](https://github.com/dankelley/oce/issues/2299)).
- Change
  [`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md)
  to handle realtime data files (issue
  [\#2230](https://github.com/dankelley/oce/issues/2230)).
- Change
  [`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md)
  to
  - handle TOC, datasets and plans better (issue
    [\#2303](https://github.com/dankelley/oce/issues/2303) and related),
  - handle bottom-track data correctly (issue
    [\#2368](https://github.com/dankelley/oce/issues/2368))
  - read temperature correctly (issue
    [\#2324](https://github.com/dankelley/oce/issues/2324)),
  - save elements of AST (etc) individually, not within a list (issue
    [\#2318](https://github.com/dankelley/oce/issues/2318))
  - scale `altimeterRawSamples` by 1/2^15 (issue
    [\#2327](https://github.com/dankelley/oce/issues/2327)).
  - set `data@distance` only if NC (number of cells) is nonzero (issue
    [\#2319](https://github.com/dankelley/oce/issues/2319)).
  - use number of raw altimeter samples from header, if it disagrees
    with value in record (issue
    [\#2326](https://github.com/dankelley/oce/issues/2326)).
- Change
  [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md)
  to
  - read data from 3-beam horizontal devices (issue
    [\#2369](https://github.com/dankelley/oce/issues/2369)).
- Change `read.adp.rdi(..., which="??")` to
  - return a data frame that includes ensemble size (issue
    [\#2329](https://github.com/dankelley/oce/issues/2329)).
- Change
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
  to
  - Handle fixed-width column names (pull request
    [\#2365](https://github.com/dankelley/oce/issues/2365), kindly
    provided by William Kumler, github user
    [@wkumler](https://github.com/wkumler)),
  - handle files that lack salinity and conductivity (issues
    [\#2279](https://github.com/dankelley/oce/issues/2279) and
    [\#2282](https://github.com/dankelley/oce/issues/2282)),
  - handle time-related header elements better (issues
    [\#2280](https://github.com/dankelley/oce/issues/2280) and
    [\#2281](https://github.com/dankelley/oce/issues/2281)), and
  - handle “new” format CNV files, at least provisionally (issue
    [\#2328](https://github.com/dankelley/oce/issues/2328)).
- Change
  [`read.ctd.aml()`](https://dankelley.github.io/oce/reference/read.ctd.aml.md)
  to handle “type 3” format (issue
  [\#2247](https://github.com/dankelley/oce/issues/2247)).
- Change
  [`read.coastline()`](https://dankelley.github.io/oce/reference/read.coastline.md)
  to handle `file` in “shapefile” format (issue
  [\#1850](https://github.com/dankelley/oce/issues/1850)).
- Change
  [`read.met()`](https://dankelley.github.io/oce/reference/read.met.md)
  to handle flags better (issue
  [\#2298](https://github.com/dankelley/oce/issues/2298)).
- Change
  [`read.netcdf()`](https://dankelley.github.io/oce/reference/read.netcdf.md)
  to
  - document flag handling better (issue
    [\#2236](https://github.com/dankelley/oce/issues/2236)),
  - handle units better and
  - permit renaming data (issue
    [\#2235](https://github.com/dankelley/oce/issues/2235)).
- Change
  [`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md)
  to
  - solve a column renaming problem (issue
    [\#2291](https://github.com/dankelley/oce/issues/2291)),
  - handle files that have empty (but not missing) `geodata` tables
    (issue [\#2336](https://github.com/dankelley/oce/issues/2336)), and
  - handle more unit formats (issue
    [\#2337](https://github.com/dankelley/oce/issues/2337)).
- Change
  [`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md)
  to disallow `method="kriging"`, because it relied on the `automap`
  package, which was removed from CRAN on 2025-06-30 (issue
  [\#2332](https://github.com/dankelley/oce/issues/2332)).
- Change `subset,amsr-method()` to handle `"pass"` parameter.
- Change `subset,ctd-method()` to handle items of unexpected length
  (issue [\#2250](https://github.com/dankelley/oce/issues/2250)).
- Change
  [`swSoundAbsorption()`](https://dankelley.github.io/oce/reference/swSoundAbsorption.md)
  to handle three more parameterizations (code and documentation
  contributed by João Resende via pull request 2345).
- Change [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md)
  to stop if there are any non-finite time values (issue
  [\#2269](https://github.com/dankelley/oce/issues/2269)).
- Change
  [`vectorShow()`](https://dankelley.github.io/oce/reference/vectorShow.md)
  to obey “digits” parameter for list arguments (issue
  [\#2313](https://github.com/dankelley/oce/issues/2313)).
- Fix bug in `read.sbe.ctd` when `btl=TRUE` that was incorrectly parsing
  files with long column names
  ([\#2365](https://github.com/dankelley/oce/issues/2365))

## oce 1.8.3 (ON CRAN)

- Add
  [`swSpiciness0()`](https://dankelley.github.io/oce/reference/swSpiciness0.md),
  [`swSpiciness1()`](https://dankelley.github.io/oce/reference/swSpiciness1.md),
  and
  [`swSpiciness2()`](https://dankelley.github.io/oce/reference/swSpiciness2.md)
  (issue [\#2188](https://github.com/dankelley/oce/issues/2188)).
- Change
  [`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)
  to use previous (6-month-old) method (issue
  [\#2199](https://github.com/dankelley/oce/issues/2199)).
- Change `[[` for many seawater properties to handle `argo` data (issues
  [\#2207](https://github.com/dankelley/oce/issues/2207) and
  [\#2208](https://github.com/dankelley/oce/issues/2208)).
- Change
  [`concatenate()`](https://dankelley.github.io/oce/reference/concatenate.md)
  to handle `data` slots that are data frames (issue
  [\#2213](https://github.com/dankelley/oce/issues/2213)).
- Change
  [`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md)
  to add the `na.rm` argument (issue
  [\#2192](https://github.com/dankelley/oce/issues/2192)).
- Change
  [`mapDirectionField()`](https://dankelley.github.io/oce/reference/mapDirectionField.md)
  to allow wind barbs (issue
  [\#2191](https://github.com/dankelley/oce/issues/2191)).
- Change
  [`mapContour()`](https://dankelley.github.io/oce/reference/mapContour.md)
  to discuss longitude conventions (issue
  [\#2218](https://github.com/dankelley/oce/issues/2218)).
- Change
  [`mapImage()`](https://dankelley.github.io/oce/reference/mapImage.md)
  to handle a user-provided gridding function (issue
  [\#2199](https://github.com/dankelley/oce/issues/2199)).
- Change
  [`mapImage()`](https://dankelley.github.io/oce/reference/mapImage.md)
  to add the `gridCoarseness` parameter (issue
  [\#2199](https://github.com/dankelley/oce/issues/2199)).
- Change
  [`oceMagic()`](https://dankelley.github.io/oce/reference/oceMagic.md)
  to close NetCDF files after checking them (issue
  [\#2209](https://github.com/dankelley/oce/issues/2209)).
- Change
  [`plot.amsr()`](https://dankelley.github.io/oce/reference/plot-amsr-method.md)
  to add the `zlab` parameter (issue
  [\#2220](https://github.com/dankelley/oce/issues/2220)).
- Change
  [`plot.argo()`](https://dankelley.github.io/oce/reference/plot-argo-method.md)
  to handle `which="sigma0 profile"` (issue
  [\#2184](https://github.com/dankelley/oce/issues/2184)).
- Change
  [`plot.sealevel()`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md)
  to add the `grid` parameter (issue
  [\#2204](https://github.com/dankelley/oce/issues/2204)).
- Change
  [`plot.sealevel()`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md)
  to improve labelling (issue
  [\#2203](https://github.com/dankelley/oce/issues/2203)).
- Change
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  to handle bottom traces better (issue
  [\#2194](https://github.com/dankelley/oce/issues/2194)).
- Change
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  to show data as points properly (issue
  [\#2195](https://github.com/dankelley/oce/issues/2195)).
- Change
  [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  to handle `xaxs` and `yaxs` when `xtype="spiciness0"` (issue
  [\#2215](https://github.com/dankelley/oce/issues/2215)).
- Change
  [`plotTaylor()`](https://dankelley.github.io/oce/reference/plotTaylor.md)
  to handle a few more parameters.
- Change
  [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md)
  to handle large (\> 2.147 GB) files (issue
  [\#2196](https://github.com/dankelley/oce/issues/2196)).
- Change
  [`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md)
  to read start/end times plus other metadata (issue
  [\#2219](https://github.com/dankelley/oce/issues/2219)).
- Change
  [`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md)
  to handle non-UTF characters in metadata (issue
  [\#2206](https://github.com/dankelley/oce/issues/2206)).
- Change
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
  to name `specc` as `specificConductance` (issue
  [\#2211](https://github.com/dankelley/oce/issues/2211)).
- Change
  [`read.ctd.ssda()`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md)
  to handle hemisphere tags in location (issue
  [\#2227](https://github.com/dankelley/oce/issues/2227)).
- Change
  [`read.met()`](https://dankelley.github.io/oce/reference/read.met.md)
  to return `data` slots that are lists (issue
  [\#2213](https://github.com/dankelley/oce/issues/2213)).
- Change
  [`resizableLabel()`](https://dankelley.github.io/oce/reference/resizableLabel.md)
  to document partial-match behaviour (issue
  [\#2197](https://github.com/dankelley/oce/issues/2197)).
- Change
  [`resizableLabel()`](https://dankelley.github.io/oce/reference/resizableLabel.md)
  to handle `item=`“N”\` (issue
  [\#2197](https://github.com/dankelley/oce/issues/2197)).
- Change [`summary()`](https://rdrr.io/r/base/summary.html) to skip time
  in the threenum table (issue
  [\#2198](https://github.com/dankelley/oce/issues/2198)).
- Change
  [`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md)
  to handle vector arguments better (issue
  [\#2178](https://github.com/dankelley/oce/issues/2178)).
- Change
  [`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md)
  to detect non-logical values of `useRefraction` (issue
  [\#2225](https://github.com/dankelley/oce/issues/2225)).
- Change some byte-level work from C to C++ to improve maintainability
  (issue [\#2201](https://github.com/dankelley/oce/issues/2201)).

## oce 1.8.2 (on CRAN)

- Add [`package?oce`](https://dankelley.github.io/oce/reference/oce.md)
  documentation (had been missing with a Roxygen2 change).
- Add
  [`read.ctd.saiv()`](https://dankelley.github.io/oce/reference/read.ctd.saiv.md)
  to read data from SAIV instruments (issue
  [\#2141](https://github.com/dankelley/oce/issues/2141)).
- Remove many dontrun examples from the docs (issue
  [\#2152](https://github.com/dankelley/oce/issues/2152)).
- Remove `trimString()`, after it had been deprecated for a long time
  (issue [\#2123](https://github.com/dankelley/oce/issues/2123)).
- Remove use of the deprecated `sp` package (issue
  [\#2154](https://github.com/dankelley/oce/issues/2154)).
- Change `amsr` dataset and functions to handle new format (issues
  [\#2124](https://github.com/dankelley/oce/issues/2124) to
  [\#2133](https://github.com/dankelley/oce/issues/2133) plus issue
  [\#2147](https://github.com/dankelley/oce/issues/2147)).
- Change
  [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) to
  handle multi-profile Argo better (issue
  [\#2173](https://github.com/dankelley/oce/issues/2173)).
- Change
  [`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md)
  to handle NOAA constituent names and frequencies (issues
  [\#2143](https://github.com/dankelley/oce/issues/2143),
  [\#2144](https://github.com/dankelley/oce/issues/2144) and
  [\#2146](https://github.com/dankelley/oce/issues/2146)).
- Change C/C++ [`sprintf()`](https://rdrr.io/r/base/sprintf.html) calls
  so all formats match arguments (issue
  [\#2172](https://github.com/dankelley/oce/issues/2172)).
- Change `computableWaterProperties` to handle a vector of variable
  names.
- Change
  [`plot.adp()`](https://dankelley.github.io/oce/reference/plot-adp-method.md)
  to obey `xlab` if x axis represents time (issue
  [\#2162](https://github.com/dankelley/oce/issues/2162)).
- Change
  [`plot.cm()`](https://dankelley.github.io/oce/reference/plot-cm-method.md)
  to obey `xlim`, `ylim`, `xaxs` and `yaxs` (issue
  [\#2121](https://github.com/dankelley/oce/issues/2121)).
- Change
  [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md) and
  [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  to accept `type="b"`.
- Change
  [`plot.windrose()`](https://dankelley.github.io/oce/reference/plot-windrose-method.md)
  for `type="fivenum"` (issue
  [\#2164](https://github.com/dankelley/oce/issues/2164)).

## oce 1.8.1 (on CRAN)

- Change
  [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) to
  handle `startTime`, `PRES`, `PSAL` and `TEMP` better.
- Change
  [`binCount1D()`](https://dankelley.github.io/oce/reference/binCount1D.md) +
  6 related functions by adding `include.lowest` parameter (issue
  [\#2113](https://github.com/dankelley/oce/issues/2113)).
- Change C++ files to avoid an error relating to include statements
  (issue [\#2119](https://github.com/dankelley/oce/issues/2119)).
- Change
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md) to
  ensure `z` (if provided) is a matrix (issue
  [\#2091](https://github.com/dankelley/oce/issues/2091)).
- Change
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  to skip the kriging example (issue
  [\#2080](https://github.com/dankelley/oce/issues/2080)).
- Change
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  to show palette triangles if necessary (issue
  [\#2083](https://github.com/dankelley/oce/issues/2083)).
- Change
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  to show data and bottom correctly (issue
  [\#2092](https://github.com/dankelley/oce/issues/2092)).
- Change
  [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md) to
  obey the `rho1000` parameter again (issue
  [\#2085](https://github.com/dankelley/oce/issues/2085)).
- Change
  [`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md)
  to translate more variable names.
- Change
  [`read.netcdf()`](https://dankelley.github.io/oce/reference/read.netcdf.md)
  to read more items.
- Change
  [`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md)
  to obey the `tz` parameter (issue
  [\#2108](https://github.com/dankelley/oce/issues/2108)).
- Change
  [`summary.adp()`](https://dankelley.github.io/oce/reference/summary-adp-method.md)
  to handle the new format for AD2CP data (issue
  [\#2087](https://github.com/dankelley/oce/issues/2087)).

## oce 1.8.0 (on CRAN)

- Add
  [`advSontekAdrFileTrim()`](https://dankelley.github.io/oce/reference/advSontekAdrFileTrim.md)
  (issue [\#1994](https://github.com/dankelley/oce/issues/1994)).
- Add
  [`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md)
  (issue [\#2027](https://github.com/dankelley/oce/issues/2027)).
- Change
  [`applyMagneticDeclination()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination.md)
  to also handle `adp` and `adv` (issue
  [\#2038](https://github.com/dankelley/oce/issues/2038)).
- Change
  [`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md)
  to handle new NOAA database (issue
  [\#2015](https://github.com/dankelley/oce/issues/2015)).
- Change `inst/extdata` by compressing some files.
- Change
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) to
  remove an infrequent low-level error (issue
  [\#2036](https://github.com/dankelley/oce/issues/2036)).
- Change
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) to
  require `projection` to be a string (issue
  [\#2076](https://github.com/dankelley/oce/issues/2076)).
- Change `numberAsPOSIXct(t, type="gps")` to handle week rollover (issue
  [\#2077](https://github.com/dankelley/oce/issues/2077)).
- Change
  [`plot.echosounder()`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md)
  to use
  [`oceColorsViridis()`](https://dankelley.github.io/oce/reference/oceColorsViridis.md)
  by default (issue
  [\#2060](https://github.com/dankelley/oce/issues/2060)).
- Change
  [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  to create xlab on vector input (issue
  [\#2047](https://github.com/dankelley/oce/issues/2047)).
- Change
  [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md) to
  compute isopycnals more accurately (issue
  [\#2046](https://github.com/dankelley/oce/issues/2046)).
- Change
  [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md) to
  handle lobo objects directly.
- Change
  [`plot.tidem()`](https://dankelley.github.io/oce/reference/plot-tidem-method.md)
  to obey `...` parameter (issue
  [\#2035](https://github.com/dankelley/oce/issues/2035)).
- Change
  [`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md),
  and structure of AD2CP objects (issue
  [\#2005](https://github.com/dankelley/oce/issues/2005)).
- Change
  [`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md)
  to read geographic information (issue
  [\#2024](https://github.com/dankelley/oce/issues/2024)).
- Change
  [`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md)
  to handle `method="kriging"` again
- Change [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md)
  (and summary method) to handle 6-hourly data (issue
  [\#2034](https://github.com/dankelley/oce/issues/2034)).
- Remove package dependence on `rgeos` and `raster` packages (issue
  [\#2028](https://github.com/dankelley/oce/issues/2028)).
- Remove `plotAD2CP()`, now superseded by generic
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method (issue
  [\#2005](https://github.com/dankelley/oce/issues/2005)).
- Remove plotting tests but retain in developer suite (issue
  [\#2073](https://github.com/dankelley/oce/issues/2073)).

## oce 1.7.10 (on CRAN)

- Fix a typo in
  [`swThermalConductivity()`](https://dankelley.github.io/oce/reference/swThermalConductivity.md)
  doc.

## oce 1.7.9

- Improve the handling of file encodings.
- Make the argo `[[` method handle spiciness.
- Improve
  [`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md)
  extensively, breaking some old behaviours.
- Add
  [`oceFileTrim()`](https://dankelley.github.io/oce/reference/oceFileTrim.md),
  [`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md)
  and
  [`adpRdiFileTrim()`](https://dankelley.github.io/oce/reference/adpRdiFileTrim.md).
- Remove `renameData()`, which had been flagged as defunct in multiple
  CRAN releases.
- Deprecate `trimString()`.

## oce 1.7.8

- Fix compiler warning of comparing signed and unsigned quantities.

## oce 1.7.7

- Add
  [`gappyIndex()`](https://dankelley.github.io/oce/reference/gappyIndex.md).
- Fix encoding problems that will occur in the upcoming version of R.

## oce 1.7.6

- Permit acoustic-Doppler files in excess of 4Gb.

## oce 1.7.5

- Remove a stray file.

## oce 1.7.4

- Change
  [`mapImage()`](https://dankelley.github.io/oce/reference/mapImage.md)
  to use `interp` instead of `akima`, as required by CRAN.
- Improve
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
  for interpreting Date in headers.
- Improve
  [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) for
  Argo data.

## oce 1.7.3

- Add
  [`read.ctd.ssda()`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md).
- Add
  [`read.argo.copernicus()`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md).
- Add
  [`sectionSort()`](https://dankelley.github.io/oce/reference/sectionSort.md)
  parameter `decreasing` (thanks to M. Renner).
- Address unused-variable warnings in C++ code.
- Address byte-order mark problem in
  [`read.met()`](https://dankelley.github.io/oce/reference/read.met.md)
  tests.
- Change most documentation hyperlinks to plain-text.
- Change
  [`read.met()`](https://dankelley.github.io/oce/reference/read.met.md)
  file encoding; remove sample data.
- Speed up some tests.

## oce 1.7.2

- Remove a link to `https://pubs.usgs.gov/pp/1395/report.pdf`, which was
  reported as unreachable during the attempted CRAN submission of
  version 1.7.1.

## oce 1.7.1

- Fix error in comparing a
  [`class()`](https://rdrr.io/r/base/class.html) result with a string,
  which was reported as an error during the attempted CRAN submission of
  version 1.7.0.

## oce 1.7.0

- Fix CRAN-check warning about 2 uninitialized variables on the
  `r-devel-linux-x86_64-debian-gcc` test system. (We thank K. Hornik,
  for an email dated 2022-03-16 notifying us about the problem.)
- Fix CRAN-check error on the `r-devel-linux-x86_6-debian-clang`,
  `r-devel-linux-x86_6-fedora-clang` and
  `r-devel-linux-x86_6-fedora-gcc` test systems, relating to a
  byte-order-mark in a test file, which is evidently treated differently
  on different systems.
- Remove a kriging example because it causes CRAN-check problems on
  `r-devel-linux-*` machines (but, curiously, no other machines or
  systems).
- Change
  [`mapTissot()`](https://dankelley.github.io/oce/reference/mapTissot.md)
  to trim shapes to earth limits.
- Alter this `NEWS.md` file so
  [`pkgdown::build_news()`](https://pkgdown.r-lib.org/reference/build_news.html)
  will notice the subsections.
- Add
  [`read.ctd.aml()`](https://dankelley.github.io/oce/reference/read.ctd.aml.md).
- Change
  [`read.met()`](https://dankelley.github.io/oce/reference/read.met.md)
  to handle a new `encoding` argument.
- Fix
  [`drawPalette()`](https://dankelley.github.io/oce/reference/drawPalette.md)
  to obey the `at` and `labels` arguments.
- Fix
  [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md)
  to recognize `which=13` for spice.

## oce 1.6.1

- Remove a journal link that has become unavailable.
- Fix “length_1” error (thanks to B. Ripley for the heads-up).

## oce 1.6.0

- Fix image-size declarations in 3 man pages (required by CRAN).
- Improve sub-class vignette.

## oce 1.5.0

- Add `[["?"]]` facility to all objects.
- Add `showNA` argument to
  [`vectorShow()`](https://dankelley.github.io/oce/reference/vectorShow.md).
- Add
  [`adpConvertRawToNumeric()`](https://dankelley.github.io/oce/reference/adpConvertRawToNumeric.md).
- Add
  [`adpFlagPastBoundary()`](https://dankelley.github.io/oce/reference/adpFlagPastBoundary.md).
- Add
  [`ctdRepair()`](https://dankelley.github.io/oce/reference/ctdRepair.md).
- Add
  [`labelWithUnit()`](https://dankelley.github.io/oce/reference/labelWithUnit.md).
- Add [`swSR()`](https://dankelley.github.io/oce/reference/swSR.md).
- Add
  [`swSstar()`](https://dankelley.github.io/oce/reference/swSstar.md).
- Change
  [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) to
  drop `other` parameter, deprecated 4y ago.
- Change
  [`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md)
  to handle more CODE and UNIT values.
- Change
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md)
  by adding `simplify` argument.
- Change
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md),
  correcting temperature label to “T” and adding many more plot types.
- Change
  [`pwelch()`](https://dankelley.github.io/oce/reference/pwelch.md),
  improving low-frequency results.
- Fix `[[` to yield ITS90 temperature for all classes, not just `ctd`.
- Fix [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md)
  handling of temperature scale of first argument.
- Fix
  [`as.section()`](https://dankelley.github.io/oce/reference/as.section.md)
  handling of list of `argo` objects as first argument.
- Fix
  [`colormap()`](https://dankelley.github.io/oce/reference/colormap.md)
  handling of `name` argument.
- Mark
  [`byteToBinary()`](https://dankelley.github.io/oce/reference/byteToBinary.md)
  as defunct (slated for removal in 1.6.0).
- Mark `renameData()` as defunct (slated for removal in 1.6.0).

## oce 1.4.0

- [`colormap()`](https://dankelley.github.io/oce/reference/colormap.md)
  uses `oceColorsViridis` as default colour scheme.
- [`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md)
  uses new NOAA server.
- [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  docs warn about problems with version 0.9-8 of sf package.
- [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  handles units of added columns.
- [`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md)
  reads more metadata.

## oce 1.3.0

- Remove a broken link (OK locally but failing in CRAN tests).
- Fix an error in the example for `setFlags.ctd`.
- Add
  [`addSpine()`](https://dankelley.github.io/oce/reference/addSpine.md)
  for defining section spines.
- Add
  [`angle2hms()`](https://dankelley.github.io/oce/reference/angle2hms.md).
- Add
  [`argoJuldToTime()`](https://dankelley.github.io/oce/reference/argoJuldToTime.md).
- Add `data(amsr)` and improve `subset.amsr()`.
- Add `data(tidalCurrent)` dataset of tidal currents from Foreman.
- Add
  [`oceAxis()`](https://dankelley.github.io/oce/reference/oceAxis.md).
- Add
  [`preferAdjusted()`](https://dankelley.github.io/oce/reference/preferAdjusted.md)
  for `argo-class` data.
- Add
  [`read.ctd.odv()`](https://dankelley.github.io/oce/reference/read.ctd.odv.md).
- Add
  [`snakeToCamel()`](https://dankelley.github.io/oce/reference/snakeToCamel.md)
  to convert variable names in e.g. Argo NetCDF files.
- Add
  [`timeToArgoJuld()`](https://dankelley.github.io/oce/reference/timeToArgoJuld.md).
- Change
  [`drawPalette()`](https://dankelley.github.io/oce/reference/drawPalette.md)
  default colour palette to
  [`oce.colorsViridis()`](https://dankelley.github.io/oce/reference/oceColorsViridis.md).
- Change
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md)
  default colour palette to
  [`oce.colorsViridis()`](https://dankelley.github.io/oce/reference/oceColorsViridis.md).
- Change
  [`plot.amsr()`](https://dankelley.github.io/oce/reference/plot-amsr-method.md)
  default colour palette to
  [`oce.colorsViridis()`](https://dankelley.github.io/oce/reference/oceColorsViridis.md).
- Change
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  default colour palette to
  [`oce.colorsViridis()`](https://dankelley.github.io/oce/reference/oceColorsViridis.md).
- Change
  [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md) to
  trim isopycnals to realistic salinities and temperatures.
- Change
  [`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md)
  and `data(argo)` to use camelCase in all metadata.
- Change user-oriented github website generator to `pkgdown`.
- Extend `[[,argo-method` to accept `"ID"` as an alternative to `"id"`.
- Extend [`as.cm()`](https://dankelley.github.io/oce/reference/as.cm.md)
  to accept `adp` and `adv` objects.
- Extend
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md),
  adding argument `logStyle`.
- Extend
  [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md)
  to handle 38kHz RDI adp files.
- Extend
  [`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md)
  and `data(argo)` by adding three more `CYCLE` variables.
- Extend
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
  to handle 4 new (Beckman) oxygen variables.
- Extend
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
  to handle `.btl` files.
- Fix `plot.coastline` error in box drawing.
- Fix `plotTS` error in auto-scaling if S and T have mixed NA status.
- Fix
  [`subset.argo()`](https://dankelley.github.io/oce/reference/subset-argo-method.md)
  error in `flags`, `location`, and `*QC` in `metadata`.
- Remove dependence on `rgdal` package, using `sf` for map projections.

## oce 1.2.0

- Address argument doc/code warning that blocked CRAN acceptance.
- Accommodate new ocedata (needed to meet new CRAN dependency rule).
- Add
  [`oceRenameData()`](https://dankelley.github.io/oce/reference/oceRenameData.md)
  and `oceRenamemetadata()`.
- Deprecate `renameData()`.
- Improve axis control for
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  and `plot.coastline`.
- Remove `addColumn()`, `ctdAddColumn()`, `ctdUpdateHeader()`,
  `findInOrdered()`, `mapMeridians()`, `mapZones()`, and
  `oce.as.POSIXlt()`, all of which have been marked as “Deprecated” for
  the past two CRAN releases.
- Extend
  [`read.met()`](https://dankelley.github.io/oce/reference/read.met.md)
  to read a new Environment Canada data format.
- Handle some upcoming rgdal/R-devel changes.
- Extend
  [`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md)
  to handle grids beyond data range.
- Extend
  [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md)
  to trim `time` if it longer than other data.
- Improve
  [`geodDist()`](https://dankelley.github.io/oce/reference/geodDist.md)
  to handle second location correctly when `alongPath=FALSE`.
- Improve vignettes.
- Fix itemized-list documentation problem pointed out by Kurt Hornik.

## oce 1.1-1

CRAN release: 2019-06-17

- Address “link-time-optimization” issues for CRAN submission.

## oce 1.1-0

- fix argument error in a .C() call
- Extend
  [`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md)
  to store ODF header as a list in `metadata`.
- Add
  [`presentTime()`](https://dankelley.github.io/oce/reference/presentTime.md).
- Add vignette on subclassing oce.
- Extend
  [`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md)
  to handle output grid better, and offer 2 new methods.
- Extend
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
  to handle more column names.
- Extend mapping code to support i386/windows.
- Extend
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md) to
  handle combined `flipy` and `ylim` arguments better.

## oce 1.0-1

CRAN release: 2018-10-04

- Renamed 0.9-24, released with OAR book publication.

## oce 0.9-24

- Extend
  [`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md)
  to store ODF header in `metadata`.
- Remove `makeSection()`, after its deprecate/defunct period ended.
- Make defunct: `addColumn()`,
  [`byteToBinary()`](https://dankelley.github.io/oce/reference/byteToBinary.md),
  `ctdAddColumn()`, `ctdUpdateHeader()`, `findInOrdered()`,
  `mapZones()`, `mapMeridians()`, `oce.as.POSIXlt()`, and `oce.magic()`
- Add
  [`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md).
- Extend
  [`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md)
  to handle multiple `NULL_VALUE`s.
- Improve
  [`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md).
- Add
  [`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md).
- Add
  [`concatenate()`](https://dankelley.github.io/oce/reference/concatenate.md).
- Add
  [`read.woa()`](https://dankelley.github.io/oce/reference/read.woa.md).
- Extend
  [`colormap()`](https://dankelley.github.io/oce/reference/colormap.md)
  to include `colfunction` in return value.

## oce 0.9-23

CRAN release: 2018-01-28

- [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md)
  permits inferred constituents
- [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md)
  follows T_TIDE phase convention
- adorn argument removed from some plot functions (after being defunct
  1+ year)
- deprecate `findInOrdered()`

## oce 0.9-22

CRAN release: 2017-08-28

- [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  can use external bathymetry
- [`lowpass()`](https://dankelley.github.io/oce/reference/lowpass.md)
  added
- [`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md)
  can isolate upcasts
- deprecate
  [`byteToBinary()`](https://dankelley.github.io/oce/reference/byteToBinary.md)
- address a CRAN NOTE about UTF-8 strings in data

## oce 0.9-21

CRAN release: 2017-03-30

- [`adpEnsembleAverage()`](https://dankelley.github.io/oce/reference/adpEnsembleAverage.md)
  added
- [`mapCoordinateSystem()`](https://dankelley.github.io/oce/reference/mapCoordinateSystem.md)
  added
- woce-exchange missing-value inference
- woce-exchange autodetection, plus read all data types
- [`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md)
  updated for data-server change
- [`download.met()`](https://dankelley.github.io/oce/reference/download.met.md)
  added
- [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) can
  specify columns within argo data
- [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md)
  handles more file types and larger RDI files
- [`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md)
  handles contradictory metadata better
- `oce.as.POSIXlt()` deprecated
- [`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
  handles units better
- address `R_registerRoutines` build-time warning
- fix memory-usage error in
  [`coastlineCut()`](https://dankelley.github.io/oce/reference/coastlineCut.md)

## oce 0.9-20

CRAN release: 2016-11-19

- remove observatory object type (changed format, seldom-used type)
- remove pangaea object type (undocumented format, seldom-used type)
- improvements for BioArgo data
- Spanish translation by Pablo Valdés
- make
  [`read.gps()`](https://dankelley.github.io/oce/reference/read.gps.md)
  handle more files
- deprecate `ctdAddColumn()` and `ctdUpdateHeader()`
- [`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md)
  added and
  [`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md)
  updated to handle new formats
- [`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md)
  added
- [`composite()`](https://dankelley.github.io/oce/reference/composite.md)
  added
- `addColumn()` marked as deprecated
- `read.cm.s4()` reads all data in file
- [`as.cm()`](https://dankelley.github.io/oce/reference/as.cm.md) added
- [`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md)
  handles Aanderaa current meter data
- [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  and
  [`mapAxis()`](https://dankelley.github.io/oce/reference/mapAxis.md)
  get cex.axis parameter
- `subset(amsr)` added; various other amsr improvements

## oce 0.9-19

CRAN release: 2016-07-08

- deprecate `adorn` argument in plot functions
- make
  [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
  obey `missingValue` (renamed)
- improve map projections
- add `renameData()`
- improve support for SBE files
- remove `is.beam()`, `is.xyz()`, `is.enu()` and `coordinate()`, since
  `x[["coordinate"]]` now permits these simply
- replace several function-style accessors with `[[` style
- fix test-suite error relating to an `rgdal` change
- add
  [`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md)
  as generic plus specifics for ctd and argo classes
- use S4 documentation,
  e.g. [`?"plot.ctd"`](https://dankelley.github.io/oce/reference/plot-ctd-method.md)
  instead of
  [`?plot.ctd`](https://dankelley.github.io/oce/reference/plot-ctd-method.md)
- add `subset(argo, "adjusted")`
- make
  [`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md)
  read all documented data and metadata fields
- add `trimString()`

## oce 0.9-18

CRAN release: 2016-02-12

- improve
  [`plot.coastline()`](https://dankelley.github.io/oce/reference/plot-coastline-method.md)
  and
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
- add support for G1SST satellite
- all objects now have metadata items for units and flags
- [`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md)
  method renamed: old A and B are new A; old C is new B
- support more channels and features of rsk files
- [`as.adp()`](https://dankelley.github.io/oce/reference/as.adp.md)
  added
- convert argo objects to sections
- `makeSection()` deprecated; use as.section() instead
- [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md)
  handles Teledyne/RDI version 23.19 bottom-track data
- [`geodXyInverse()`](https://dankelley.github.io/oce/reference/geodXyInverse.md)
  added; geod functions now spell out longitude etc
- [`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md)
  speeded up by a factor of about 30
- add colour palettes from the `cmocean` Python package by Kristen Thyng
- add [`as.oce()`](https://dankelley.github.io/oce/reference/as.oce.md)
- rename `drifter` class as `argo` to recognize what it actually handles
- add
  [`oceColorsViridis()`](https://dankelley.github.io/oce/reference/oceColorsViridis.md)
- [`interpBarnes()`](https://dankelley.github.io/oce/reference/interpBarnes.md)
  has new argument `pregrid`
- [`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)
  has new argument `flatten`
- `data(topoWorld)` now has longitude from -179.5 to 180
- [`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md)
  added
- [`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md)
  handles more data types
- [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md)
  reads more VmDas (navigational) data
- ITS-90 is now the default temperature unit
- ctd objects can have vector longitude and latitude
- logger class renamed to rsk
- `bremen` class added
- [`coastlineCut()`](https://dankelley.github.io/oce/reference/coastlineCut.md)
  added
- `rgdal` package used instead of local PROJ.4 source code
- `mapproj`-style map projections eliminated

## oce 0.9-17

CRAN release: 2015-05-22

- remove an `exit()` call in a C function

## oce 0.9-16

CRAN release: 2015-05-21

- cite PROJ.4 contributors in DESCRIPTION
- address package-build warnings; improve DESCRIPTION (thanks, BR)

## oce 0.9-15

- [`plot.echosounder()`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md)
  gets new argument `drawPalette`
- `data(landsat)` taken from ocedata (and shrunk)
- `data(nao)` and data(soi) moved to ocedata
- [`mapTissot()`](https://dankelley.github.io/oce/reference/mapTissot.md)
  added
- `read.logger()` with ctd-type data infers salinity if necessary
- [`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md)
  can have “method” as a function
- [`as.topo()`](https://dankelley.github.io/oce/reference/as.topo.md)
  can convert “bathy” objects from the marmap package
- `"+.section"` renamed sectionAddStation()
- [`oceApprox()`](https://dankelley.github.io/oce/reference/oceApprox.md)
  renamed oce.approx(), with an alias for compatibility
- [`oce.grid()`](https://dankelley.github.io/oce/reference/oce.grid.md)
  added
- `"tdr"` class renamed `"logger"`
- [`swCSTp()`](https://dankelley.github.io/oce/reference/swCSTp.md)
  added
- `swConductivity()` now
  [`swThermalConductivity()`](https://dankelley.github.io/oce/reference/swThermalConductivity.md);
  using Caldwell (1974).
- many `sw()` functions handle eos=“gsw”
- [`library(gsw)`](http://teos-10.github.io/GSW-R/) replaces `teos()`
- [`curl()`](https://dankelley.github.io/oce/reference/curl.md) added
- handle new NOAA ascii topography dataset type
- [`mapGrid()`](https://dankelley.github.io/oce/reference/mapGrid.md)
  added
- `data(soi)` moved to ocedata package
- [`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md)
  added
- [`map2lonlat()`](https://dankelley.github.io/oce/reference/map2lonlat.md)
  made more accurate
- permit `proj4` style map projections
- landsat-7 (and possibly 4 and 5) support
- landsat decimation/plotting improvements
- `drifterGrid()` added
- [`mapDirectionField()`](https://dankelley.github.io/oce/reference/mapDirectionField.md)
  added
- [`mapArrows()`](https://dankelley.github.io/oce/reference/mapArrows.md)
  added
- [`read.index()`](https://dankelley.github.io/oce/reference/read.index.md)
  added
- [`utm2lonlat()`](https://dankelley.github.io/oce/reference/utm2lonlat.md)
  and
  [`lonlat2utm()`](https://dankelley.github.io/oce/reference/lonlat2utm.md)
  added
- `data(met)` changed to time of Hurricane Juan

## oce 0.9-14

CRAN release: 2014-05-19

- fix compile-time warning
- [`colormap()`](https://dankelley.github.io/oce/reference/colormap.md)
  added, and functionality added to
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md) and
  [`drawPalette()`](https://dankelley.github.io/oce/reference/drawPalette.md)
- [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md) and
  [`drawPalette()`](https://dankelley.github.io/oce/reference/drawPalette.md)
  new arg. `axisPalette` (suggested C. Richards)
- [`drawPalette()`](https://dankelley.github.io/oce/reference/drawPalette.md)
  has new args: plot, pos, levels, and cex.axis
- [`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md)
  permits user-supplied method function
- `data(nao)` added; data(soi) updated, and names improved in latter
- landsat support added
- [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  and [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)
  get new argument `pt.bg`
- [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  grids the data if needed
- rename `sealevelHalifax` dataset as `sealevel`
- translate some axis names (Spanish, French, German and Mandarin)
- [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  now has `ytype="depth"`
- [`mapImage()`](https://dankelley.github.io/oce/reference/mapImage.md)
  and friends now demand a map exists first
- [`runlm()`](https://dankelley.github.io/oce/reference/runlm.md) added
- [`mapScalebar()`](https://dankelley.github.io/oce/reference/mapScalebar.md)
  added
- `subset.topo()` added
- [`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md)
  now handles GEBCO NetCDF files
- [`decimate()`](https://dankelley.github.io/oce/reference/decimate.md)
  now handles topo objects
- reverse oce.colorsGebco colours for water
- [`drawIsopycnals()`](https://dankelley.github.io/oce/reference/drawIsopycnals.md)
  and [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md):
  improve isopycnal labels
- [`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md)
  handles new method `unesco`
- coastline improvements (now 3 resolutions)
- [`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)
  improvements
- `read.observatory()` added
- [`read.ctd.itp()`](https://dankelley.github.io/oce/reference/read.ctd.itp.md)
  added
- [`mapImage()`](https://dankelley.github.io/oce/reference/mapImage.md)
  speeded up (60X in 1deg SST example)
- [`binApply1D()`](https://dankelley.github.io/oce/reference/binApply1D.md)
  and
  [`binApply2D()`](https://dankelley.github.io/oce/reference/binApply2D.md)
  added
- [`binCount1D()`](https://dankelley.github.io/oce/reference/binCount1D.md),
  [`binMean1D()`](https://dankelley.github.io/oce/reference/binMean1D.md),
  [`binCount2D()`](https://dankelley.github.io/oce/reference/binCount2D.md),
  and
  [`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)
  added
- [`numberAsHMS()`](https://dankelley.github.io/oce/reference/numberAsHMS.md)
  added
- `gps` data type added (provisionally)
- various functions: reorder arguments putting longitude before latitude
- [`magneticField()`](https://dankelley.github.io/oce/reference/magneticField.md)
  replaces `magneticDeclination()`
- [`ungrid()`](https://dankelley.github.io/oce/reference/ungrid.md)
  added
- [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md):
  improve zone and meridian aesthetics
- [`detrend()`](https://dankelley.github.io/oce/reference/detrend.md)
  returns list with detrend vector and coefficients
- [`decodeTime()`](https://dankelley.github.io/oce/reference/decodeTime.md)
  improvements
- [`interpBarnes()`](https://dankelley.github.io/oce/reference/interpBarnes.md)
  has new argument `trim`
- [`standardDepths()`](https://dankelley.github.io/oce/reference/standardDepths.md)
  added
- [`mapLongitudeLatitudeXY()`](https://dankelley.github.io/oce/reference/mapLongitudeLatitudeXY.md)
  added
- [`read.ctd.woce()`](https://dankelley.github.io/oce/reference/read.ctd.woce.md)
  now accepts a format used once by Arctic scientists
- [`swRrho()`](https://dankelley.github.io/oce/reference/swRrho.md)
  added
- [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  now accepts type=“o”
- [`read.adv.nortek()`](https://dankelley.github.io/oce/reference/read.adv.nortek.md):
  read salinity
- [`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md):
  fix bug for NULL columns (issue
  [\#327](https://github.com/dankelley/oce/issues/327))
- [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  now chooses best coastline file
- [`swPressure()`](https://dankelley.github.io/oce/reference/swPressure.md)
  added
- [`ctdFindProfiles()`](https://dankelley.github.io/oce/reference/ctdFindProfiles.md)
  added
- [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md): new
  arguments filename etc
- remove `oceBisect()`, which merely reproduces
  [`uniroot()`](https://rdrr.io/r/stats/uniroot.html)
- move large/specialized datasets to library(ocedata)

## oce 0.9-13

CRAN release: 2014-01-29

- prepare for an upcoming release of R-devel

## oce 0.9-12

CRAN release: 2013-07-05

- move vignette source directory

## oce 0.9-11

- [`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md)
  now handles types “sas” and “spss”
- `data(turbulence)` added
- [`plot.echosounder()`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md):
  use white for below-threshold values
- [`plot.echosounder()`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md):
  add arg `beam` to e.g. display Sv
- [`read.echosounder()`](https://dankelley.github.io/oce/reference/read.echosounder.md):
  handle dual-beam and split-beam data
- [`read.echosounder()`](https://dankelley.github.io/oce/reference/read.echosounder.md):
  decode bottom-pick
- [`swSoundAbsorption()`](https://dankelley.github.io/oce/reference/swSoundAbsorption.md)
- [`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md)
  doc improved regarding variable orientation of RDI files

## oce 0.9-10

CRAN release: 2013-05-19

- improve line breaks in some help pages (thanks, B. Ripley)

## oce 0.9-9

- [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  uses `...` graphical parameters (e.g. cex) in labelling axes
- fix recent bug in reading CNV files (issue 318)
- `plotTS(...,mar=NULL)` now avoids setting `par(mar)`

## oce 0.9-8

CRAN release: 2013-04-25

- fix memory error in teos handler (thanks, B. Ripley)

## oce 0.9-7

- make
  [`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md)
  work in R 3.0
- support for adp vmdas navigation data (coded by Clark Richards)
- [`approx3d()`](https://dankelley.github.io/oce/reference/approx3d.md):
  added
- [`drawPalette()`](https://dankelley.github.io/oce/reference/drawPalette.md):
  new arg `fullpage`
- [`plot.lobo()`](https://dankelley.github.io/oce/reference/plot-lobo-method.md):
  add arg `which`
- [`read.coastline.openstreetmap()`](https://dankelley.github.io/oce/reference/read.coastline.openstreetmap.md)
  added
- `data(levitus)` added
- [`plot.coastline()`](https://dankelley.github.io/oce/reference/plot-coastline-method.md)
  gets new arguments `projection` etc to use
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
- [`mapContour()`](https://dankelley.github.io/oce/reference/mapContour.md)
  permits first argument to be a `topo` object
- [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  gets new arguments `axes`, `bg`, `fill`, and `drawBox`

## oce 0.9-6

CRAN release: 2012-12-16

- address compilation error on Windows-64 platform
- make some long-running operations more interruptible
- [`oceApprox()`](https://dankelley.github.io/oce/reference/oceApprox.md):
  permit equal x values (issue
  [\#279](https://github.com/dankelley/oce/issues/279))

## oce 0.9-5

CRAN release: 2012-12-15

- halve package check time

## oce 0.9-4

- [`as.section()`](https://dankelley.github.io/oce/reference/as.section.md)
  added
- add `data(endeavour)`
- add map projections, with provisional functions
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  etc
- [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md):
  add argument `add`
- add crude mapping support
- [`interpBarnes()`](https://dankelley.github.io/oce/reference/interpBarnes.md):
  add args `xgl` and `ygl`
- [`read.section()`](https://dankelley.github.io/oce/reference/read.section.md):
  add `directory` argument
- [`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md):
  add `barnes` method
- [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md)
  and
  [`oce.axis.POSIXct()`](https://dankelley.github.io/oce/reference/oce.axis.POSIXct.md):
  add tformat argument
- [`read.aquadopp()`](https://dankelley.github.io/oce/reference/read.aquadopp.md)
  and cousins added (issues
  [\#253](https://github.com/dankelley/oce/issues/253) and
  [\#258](https://github.com/dankelley/oce/issues/258))
- [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
  guesses waterDepth if not supplied (issue
  [\#253](https://github.com/dankelley/oce/issues/253))
- [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md):
  extend isopycnal range (issue
  [\#252](https://github.com/dankelley/oce/issues/252))
- [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  contouring improved
- `data(oceans)` added
- `data(papa)` added
- [`fillGap()`](https://dankelley.github.io/oce/reference/fillGap.md)
  works on matrices as well as vectors
- [`read.lobo()`](https://dankelley.github.io/oce/reference/read.lobo.md)
  is more flexible on column names
- `imagep(..., axes=FALSE)`: no longer plots axis box (issue
  [\#249](https://github.com/dankelley/oce/issues/249))
- [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md):
  alter time auto-ranging method
- `oceBisect()`: increase generality of function whose root is sought
- [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md):
  clean axes after isopycnals drawn
- `teos10`: update test values (with help from PB)
- [`read.adp.nortek()`](https://dankelley.github.io/oce/reference/read.adp.nortek.md):
  handle missing `to` argument as documented
- `beamUnattenuateAdp()` renamed
  [`beamUnspreadAdp()`](https://dankelley.github.io/oce/reference/beamUnspreadAdp.md)
- [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md) and
  [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md):
  permit type=`n`
- [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  with `ytype="z"`: fix bug in y label
- improve error message if TEOS-10 is missing

## oce 0.9-3

CRAN release: 2012-09-04

- `example(riley)`: remove error in R-devel
- [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md)
  with `which=7` improvement
- [`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md):
  accept conductivity in mS/cm and S/m
- [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md):
  accept TEOS-10 values SA and CT
- make TEOS routines more resistant to unphysical values

## oce 0.9-2

CRAN release: 2012-07-29

- remove unwanted files during build process

## oce 0.9-1

- [`pwelch()`](https://dankelley.github.io/oce/reference/pwelch.md): fix
  bug in frequency
- [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md): add
  argument `regress`
- rename oce.plot.sticks() as
  [`plotSticks()`](https://dankelley.github.io/oce/reference/plotSticks.md);
  delete `stickplot()`
- use S4 style for all plot() methods
- `data(giss)` added
- `data(topo2)` added
- [`plotPolar()`](https://dankelley.github.io/oce/reference/plotPolar.md)
  added
- `oce.magic()` new name for `magic();` renamed
  [`oceMagic()`](https://dankelley.github.io/oce/reference/oceMagic.md)
  in 0.9-15
- `errorBars()` added
- [`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md)
  add a leap second for July 2012

## oce 0.8-10

CRAN release: 2012-06-12

- [`plot.topo()`](https://dankelley.github.io/oce/reference/plot-topo-method.md):
  automatic cut-point shift
- add support for Nortek aquadopp-profiler instruments
- [`oce.contour()`](https://dankelley.github.io/oce/reference/oce.contour.md)
  added
- `lisst` class added
- [`plotTaylor()`](https://dankelley.github.io/oce/reference/plotTaylor.md)
  added
- [`oce.as.raw()`](https://dankelley.github.io/oce/reference/oce.as.raw.md)
  added
- `binmap()` added

## oce 0.8-9

CRAN release: 2012-05-25

- make the newly-added TEOS-10 functions compile on Windows

## oce 0.8-8

CRAN release: 2012-05-23

- `data(cm)` added
- [`plot.coastline()`](https://dankelley.github.io/oce/reference/plot-coastline-method.md):
  improve argument `geographical`
- add
  [`prettyPosition()`](https://dankelley.github.io/oce/reference/prettyPosition.md)
- add [`grad()`](https://dankelley.github.io/oce/reference/grad.md)
- add (preliminary) support for TEOS-10, installed separately
- add
  [`integrateTrapezoid()`](https://dankelley.github.io/oce/reference/integrateTrapezoid.md)
- [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md): add
  argument teos
- add `teos()` as an interface to the TEOS-10 library
- add
  [`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md)
  plus other astronomy functions,
  e.g. [`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md)

## oce 0.8-7

CRAN release: 2012-04-05

- [`plotInset()`](https://dankelley.github.io/oce/reference/plotInset.md):
  remove `bg` and `fg` arguments
- [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md): add
  `bg` argument
- rename `dt` object and functions to `tdr`
- [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md): add
  argument `inset`
- [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md):
  fix a bug in bottom-tracking; add `soundSpeed` vector
- `data(echosounder)` created
- [`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md)
  created
- `plot(echosounder)`: new arguments `atTop` and `labelsTop`
- adp objects now also hold `percent good` for RDI instruments
- `plot.drifter()`: improve multi-panel plots
- [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md): add
  argument `missingColor`
- [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md) and
  [`drawPalette()`](https://dankelley.github.io/oce/reference/drawPalette.md):
  add arguments `labels` and `at`

## oce 0.8-6

CRAN release: 2012-03-04

- [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md):
  permit `POSIXt` type for `zlim`.

## oce 0.8-5

- reconstruct data(ctd) so plot will focus maps better
- [`plotInset()`](https://dankelley.github.io/oce/reference/plotInset.md)
  added
- [`grid()`](https://rdrr.io/r/graphics/grid.html) works for oce-based
  time axes
- `imagep(..., filledContours)`: obey `ylab` argument
- [`subset()`](https://rdrr.io/r/base/subset.html): permit subsetting
  ADP by pressure
- [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md):
  obey arguments `xaxt` and `yaxt`
- improve support for shapefile coastlines

## oce 0.8-4

CRAN release: 2011-12-19

- replace `center` argument for plotting topography and coastline with
  `clatitude` and `clongitude`
- add `echosounder` class
- [`fullFilename()`](https://dankelley.github.io/oce/reference/fullFilename.md):
  handle URLs properly
- ADV objects: add access to slow variables (e.g. `headingSlow`)

## oce 0.8-3

CRAN release: 2011-11-18

- add `met` class
- `subset.oce()` handles section objects
- `draw.section()`: permit `xtype="latitude"` or `"longitude"`
- [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md) has
  `drawPalette="space"` option

## oce 0.8-2

CRAN release: 2011-11-03

- fix bug in
  [`read.coastline()`](https://dankelley.github.io/oce/reference/read.coastline.md),
  reversing `lon` and `lat` (issue
  [\#162](https://github.com/dankelley/oce/issues/162))
- fix bug in S4 adp validity checker
- [`geodDist()`](https://dankelley.github.io/oce/reference/geodDist.md):
  add argument `alongPath`
- remove compilation warning about doc for
  [`summary.ctd()`](https://dankelley.github.io/oce/reference/summary-ctd-method.md)

## oce 0.8-1

CRAN release: 2011-10-26

- switch to S4 classes, and add new accessors and `show` functions.
- add support for RBR `rsk` files
- [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  now uses `col` if supplied

## oce 0.7-1

CRAN release: 2011-10-07

- improve
  [`oce.colorsJet()`](https://dankelley.github.io/oce/reference/oceColorsJet.md)
- improve
  [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  vertical range with missing data
- add `data(drag)`
- [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  and
  [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md):
  add `xtype="spice"`
- add `data(geosecs235)`
- [`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
  and
  [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md):
  add argument `keepNA`
- [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md): add
  arguments `other` and `missingValue`
- [`read.lobo()`](https://dankelley.github.io/oce/reference/read.lobo.md):
  tolerate more formats, including missing velocities
- add `data(schmitt)`
- add accessor `spice()`
- handle O2 and nutrients in CTD data and sections
- [`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md): use
  surface pressure as a default
- [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md): add
  arguments `inSitu` and \`referencePressure.
- [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md):
  handle section objects
- fix bugs reported in issues 150, 151, 153, 155, 156
- [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md):
  change default `drawContours` to FALSE

## oce 0.6-1

CRAN release: 2011-09-01

- add `adv` dataset
- generalize
  [`swN2()`](https://dankelley.github.io/oce/reference/swN2.md) by
  adding new arg `derivs`
- [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md)
  gets new arg `axes`
- add accessor `distance()`
- rename to
  [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md) and
  [`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md)
  to avoid S3 check warnings
- switch
  [`makeFilter()`](https://dankelley.github.io/oce/reference/makeFilter.md)
  argument `asKernel` default to TRUE

## oce 0.5-1

CRAN release: 2011-07-26

- add support for ARGO drifters
- `latitude()` and `longitude()`: new argument to repeat b`yDepth`
- `addCtdColumn()`: replace if column already exists
- add CTD accessors for lat, lon, pressure, salinity, temperature

## oce 0.4-1

CRAN release: 2011-07-23

- [`plot.sealevel()`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md):
  only show 3 panels, for clarity
- [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md):
  add argument `type`
- [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md):
  handle x, y, and z as
  [`image()`](https://rdrr.io/r/graphics/image.html) does
- [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md):
  inferred ylim matches data range within provided xlim
- [`read.adv.nortek()`](https://dankelley.github.io/oce/reference/read.adv.nortek.md):
  handle extra analog data
- add CTD accessors `longitude()`, `latitude()`,
  [`time()`](https://rdrr.io/r/stats/time.html), `elevation()`, etc

## oce 0.3-1

CRAN release: 2011-06-22

- add `read.oce.odf()`
- add `findInOrdered()`
- generalize
  [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md)
  argument list
- [`read.adv.nortek()`](https://dankelley.github.io/oce/reference/read.adv.nortek.md):
  handle burst-mode data
- add datasets `wilson`, `redfieldNP`, `redfieldNC`, `redfieldPlankton`,
  and `riley`
- change `data(ctd)` to be derived from `data(ctdRaw)`
- change to monitor=FALSE for all reading functions
- add accessor functions `heading()`, `latitude()`, `longitude()`,
  `pitch()`, `pressure()`, `processingLog()`, `roll()`, `salinity()`,
  `temperature()`, [`time()`](https://rdrr.io/r/stats/time.html), and
  `velocity()`
- rename history as `processingLog`.
- flatten data objects to be 1 level thick (for future matlab exports)
- add
  [`threenum()`](https://dankelley.github.io/oce/reference/threenum.md)
  added, and use it in all summaries (for speed)

## oce 0.2-3

- `head.adp()` added
- `tail.adp()` added
- `extract()` added

## oce 0.2-2

CRAN release: 2011-05-09

- [`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md)
  added
- new names:
  [`oceApprox()`](https://dankelley.github.io/oce/reference/oceApprox.md)
  [`integerToAscii()`](https://dankelley.github.io/oce/reference/integerToAscii.md)
  [`rangeLimit()`](https://dankelley.github.io/oce/reference/rangeLimit.md)
  `ctdRaw`
- [`topoInterpolate()`](https://dankelley.github.io/oce/reference/topoInterpolate.md)
  added
- [`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md):
  add type `argos`
- rename `beamAttenuateAdp()` as `beamUnattenuateAdp()`

## oce 0.2-1

CRAN release: 2011-04-28

- camel-case function and argument names (and the NEWS file)

## oce 0.1-83

- add
  [`binAverage()`](https://dankelley.github.io/oce/reference/binAverage.md)
- [`fillGap()`](https://dankelley.github.io/oce/reference/fillGap.md):
  add `rule` argument, analogous to same for
  [`approx()`](https://rdrr.io/r/stats/approxfun.html)
- add
  [`rescale()`](https://dankelley.github.io/oce/reference/rescale.md)
- `read.pt()`: fix timing error; use as.pt() to create return value
- add `as.pt()`
- [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md)
  uses checksums
- `number.as.POSIXct(type="gps")`: account for leap seconds

## oce 0.1-82

CRAN release: 2011-03-21

- remove warning that occurs for R 2.13.0 (alpha)
- `number.as.POSIXct()`: accept GPS times
- rename e.g. `adv.2enu()` as `to.enu.adv()`
- speed up conversion of ADP and ADV to enu by more than a factor of 10
- change RDI coordinate handling
- rename `match.bytes()` as
  [`matchBytes()`](https://dankelley.github.io/oce/reference/matchBytes.md)
- add
  [`drawPalette()`](https://dankelley.github.io/oce/reference/drawPalette.md)
- remove `matlab2POSIXt()`, now a sub-case of `number.as.POSIXct()`
- `number.as.POSIXct()`: added
- `oceBiset()`: added
- [`despike()`](https://dankelley.github.io/oce/reference/despike.md):
  add argument `action`
- [`detrend()`](https://dankelley.github.io/oce/reference/detrend.md):
  added
- [`read.adp.sontek()`](https://dankelley.github.io/oce/reference/read.adp.sontek.md):
  handle PCADP type
- [`read.adp.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adp.sontek.serial.md):
  added
- [`read.section()`](https://dankelley.github.io/oce/reference/read.section.md):
  handle WOCE quality flags for salinity
- [`retime()`](https://dankelley.github.io/oce/reference/retime.md):
  added
- [`read.adv.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adv.sontek.serial.md):
  correct error in class of `a` and `c`
- [`despike()`](https://dankelley.github.io/oce/reference/despike.md):
  use string value for argument `method`
- `logger.toc()`: fix bug in discovering files ranging over days
- [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md):
  make temperature and salinity into vectors, if not already
- `plot.TS()`: add argument `use.smoothScatter`
- [`oce.debug()`](https://dankelley.github.io/oce/reference/oceDebug.md):
  flush the console after printing a message
- `sw.theta()`: rename the method possibilities, to lower case
- in some `sw` functions, rename `pref` as `reference.pressure`
- [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md):
  fix bug in getting start.time for some time formats
- [`pwelch()`](https://dankelley.github.io/oce/reference/pwelch.md)
  added
- [`plot.sealevel()`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md):
  make y axis obey range in data subset, if xlim is given
- `adp.2enu()` added
- `coastline.world`: improve resolution by a factor of 4 or so
- [`read.ctd.woce()`](https://dankelley.github.io/oce/reference/read.ctd.woce.md):
  infer water depth as max(pressure) if not in header
- `section.smooth()`: handle misordered stations; handle missing values
  better
- [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md):
  allow strings for `which`; improve contouring
- [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md)
  and `plot.profile()`: add `use.smoothScatter` argument
- [`coriolis()`](https://dankelley.github.io/oce/reference/coriolis.md):
  improve omega value

## oce 0.1-81

CRAN release: 2010-10-18

- add
  [`despike()`](https://dankelley.github.io/oce/reference/despike.md)
- add `range.limit()`
- add `unabbreviate.time()`
- add support for bottom-tracking RDI ADCPs
- add support for interocean `s4` current meters
- add `unwrap.angle()`
- [`read.adv.nortek()`](https://dankelley.github.io/oce/reference/read.adv.nortek.md):
  detect the velocity range
- add processing.log.add(), an alternative to processing.log.append()
- [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md):
  add more plot types; which=9 for salinity, not density
- [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md):
  improve flexibility
- [`predict.tidem()`](https://dankelley.github.io/oce/reference/predict.tidem.md):
  add `newdata` argument
- `adv.2enu()` added
- `adp.xyz2enu()`: make it correct for heading.bias
- `make.filter()`: can now produce tskernel type
- improve (but temporarily limit) fill.gap
- [`plot.adp()`](https://dankelley.github.io/oce/reference/plot-adp-method.md):
  add `use.layout` argument
- [`window.oce()`](https://dankelley.github.io/oce/reference/window.oce.md)
  added
- make objects remember full filename, not just local filename (issue
  [\#8](https://github.com/dankelley/oce/issues/8))
- [`plot.tidem()`](https://dankelley.github.io/oce/reference/plot-tidem-method.md):
  remove argument `plot.type`, using `which` instead
- `read.pt()`: add arguments from, by, and to
- fix issue 57;
  [`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md)
  read the heading incorrectly
- add “rr” method to
  [`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md)
- [`oceApprox()`](https://dankelley.github.io/oce/reference/oceApprox.md)
  added
- `topoWorld` dataset added
- [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md):
  by default, make coastlines extend to box boundaries
- reformulate sun.angle in R
- fix google-code issue 56;
  [`plot.topo()`](https://dankelley.github.io/oce/reference/plot-topo-method.md)
  should accept land.z=NULL\]
- fix google-code issue 55;
  [`plot.topo()`](https://dankelley.github.io/oce/reference/plot-topo-method.md)
  could go past poles\]
- fix google-code issue 54;
  [`plot.topo()`](https://dankelley.github.io/oce/reference/plot-topo-method.md)
  was resetting par() on exit\]
- fix google-code issue 53; `interp.barnes()` gives poor error message
  for mismatched x and y
- fix google-code issue 52;
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  draws incorrect bottom shape

## oce 0.1-80

CRAN release: 2009-11-05

- add sun.angle; not tested yet; needs R reformulation
- fix google-code issue 51;
  [`summary.section()`](https://dankelley.github.io/oce/reference/summary-section-method.md)
  does not report water depth
- fix google-code issue 50; non-bug relating to sun elevation
- fix google-code issue 49; `demo(TS)` broken

## oce 0.1-79

CRAN release: 2009-10-26

- fix google-code issue 48; `oce.plot.sticks()` ignored page ratio
- fix google-code issue 47; cannot read new MEDS sea level format

## oce 0.1-78

CRAN release: 2009-10-02

- add `data(RRprofile)`
- to `plot.TS()`, add arguments `lwd.rho` and `lty.rho`.

## oce 0.1-77

CRAN release: 2009-09-13

- refactor adp code with respect to transformation matrices
- add `matrix.smooth()`
- improve labels for
  [`oce.axis.POSIXct()`](https://dankelley.github.io/oce/reference/oce.axis.POSIXct.md)
- set default for `debug` argument to `getOption("oceDebug")` in all
  functions
- `plot.profile()`: add argument `ytype`; change `type` to `xtype`
- add [`swZ()`](https://dankelley.github.io/oce/reference/swZ.md)
- add
  [`oce.smooth()`](https://dankelley.github.io/oce/reference/oceSmooth.md)
- add
  [`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md),
  etc., supporting Nortek and Sontek devices
- add
  [`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md),
  etc., supporting Nortek, RDI and Sontek devices
- add
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md)
- add [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md)
- add `bcd2integer()`
- add `matlab2POSIXt()`
- to most plot functions, add `mgp` and `mar` arguments
- `plot.pt()`: add plim and Tlim arguments.
- [`gravity()`](https://dankelley.github.io/oce/reference/gravity.md):
  give default for latitude argument.
- [`plot.sealevel()`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md):
  remove argument focus.time and add argument which.
- rename tdr functions as pt, to reflect common phrasing.
- add `geod.xy()`.
- add argument `mgp` to all plotting functions.
- use abbreviations for axis names if space is tight.
- add argument `adorn` to all
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) functions.
- add
  [`oce.colorsTwo()`](https://dankelley.github.io/oce/reference/oceColorsTwo.md),
  [`oce.colorsJet()`](https://dankelley.github.io/oce/reference/oceColorsJet.md),
  and
  [`oce.colorsPalette()`](https://dankelley.github.io/oce/reference/oceColorsPalette.md).
- add `byte2binary()`
- add “lty.grid” to all CTD plotting functions
- add “+.section”, a more convenient way to build sections from stations
- rework [`summary()`](https://rdrr.io/r/base/summary.html) for all
  existing objects
- [`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md):
  rename sampling.interval as `deltat`

## oce 0.1-76

CRAN release: 2009-03-02

- `sw.N2()`: make it tolerate NAs; adjust df to make result smoother
- add
  [`makeFilter()`](https://dankelley.github.io/oce/reference/makeFilter.md)
- add
  [`decimate()`](https://dankelley.github.io/oce/reference/decimate.md),
  which may eventually replace ctd.decimate()
- `read.tdr()`: handle 5-column data files; add tz argument
- `plot.profile()`: add types sigma+dpdt and sigma+time
- [`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md):
  add pmin parameter
- [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md):
  add Slim, Tlim, plim, lonlim, latlim args; add maps
- add coastline.sle dataset
- `plot.TS()`: fix isopycnal labels for fresh water; add args Slim, Tlim
- [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md):
  make it understand another SBE format
- add `parseLatlon()`
- [`oce.edit()`](https://dankelley.github.io/oce/reference/oceEdit.md):
  add argument `action`
- add `oce.write.`table()
- add
  [`fillGap()`](https://dankelley.github.io/oce/reference/fillGap.md)

## oce 0.1-75

CRAN release: 2009-01-06

- add `addColumn()`
- add `undriftTime(`)
- add `tdrPatm()`
- make `readTdr()` gather serial no. info; show this on `plot.pt()`
- switch to recommended version-number format
- remove `section.subset()`, replaced by
  [`subset()`](https://rdrr.io/r/base/subset.html)
- improve log items in functions that read and assemble oce objects
- add `subset.oc`e()
- add `header()`
- add argument `which` to `read.rbrdtr()`
- make `read.rbrdtr()` understand headers better
- add arguments `xtype` and `ytype` to
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
- improve accuracy of bottom drawn by
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
- add `section.smooth()`
- fix bug making `section.smooth()` fail if \<4 good data at a level
- fix bug making
  [`summary.ctd()`](https://dankelley.github.io/oce/reference/summary-ctd-method.md)
  fail for results of section.smooth()
- fix bug making `sw.dynamic.height()` choke on empty stations
- fix bug making
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  die if x is unordered
- rename rbrpt functions to tdr functions, improving generality

## oce 0.1.74

CRAN release: 2008-11-18

- add argument “src” to as.ctd()
- [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  RHS axis needs tics
- trim axis whitespace in plot.profile() and plot.TS()
- make
  [`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md)
  understand WOCE section type
- speed up
  [`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md)
- add `read.pt()`, `summary.pt()`, `plot.pt()`, and `ptTrim()`
- make `plot.TS()` isopycnal label size be same on top and right sides
- add lwd argument to `plot.profile()`
- give
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  ability to control contour levels and labels
- give
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  tics for station-location

## oce 0.1.73

CRAN release: 2008-09-14

- [`plot.topo()`](https://dankelley.github.io/oce/reference/plot-topo-method.md)
  narrows autoscale to xlim-ylim region, if provided
- add
  [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md)
  arg `textpanel`, and put the profiles in the same row
- fix bug in `plot.profile()` to let it take Slim,…, as args
- fix bug in `plot.profile()` font size
- fix bug in `plot.profile(type="S_T")` positioning
- fix bug in section plot (bottom was missing)
- fix bug in size of salinity axis label for plot.ctd()
- use pch=21 for TS plots (so data density is more visible)

## oce 0.1.72

CRAN release: 2008-06-09

- add `interp.barnes()`

## oce 0.1.71

CRAN release: 2008-04-14

- fix bug in `makeSection()` (ignored the list, if a list provided)
- add
  [`oce.edit()`](https://dankelley.github.io/oce/reference/oceEdit.md),
  later renamed
  [`oceEdit()`](https://dankelley.github.io/oce/reference/oceEdit.md)
- add topoMaritimes dataset
- add
  [`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md),
  [`plot.topo()`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
  and `summary.topo()`
- add `gebcoColors()` … renamed
  [`oce.colorsGebco()`](https://dankelley.github.io/oce/reference/oceColorsGebco.md)
  in version 0.1.77
- make
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
  check that pressures coincide

## oce 0.1.70

CRAN release: 2008-04-01

- `plot.TS()`: make isopycnal list work better for nearly-fresh water
- `trimCtd()`: improve equilibration-phase deletion
- [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md):
  handle cases without cruise information or scan column
- permit setting xlab and ylab in `plot.TS()`
- make processing log timestamps be in GMT
- add
  [`as.windrose()`](https://dankelley.github.io/oce/reference/as.windrose.md)
  and
  [`plot.windrose()`](https://dankelley.github.io/oce/reference/plot-windrose-method.md)
- add `sealevel.tuk` sea-level dataset
- add [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md),
  `predict.tide()`, `summary.tide()`, `plot.tide()`, `data(tidedata)`
- establish a uniform form for objects created by “read” and “as”
- let [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md)
  and similar functions take a ctd object as a single argument
- add
  [`swDynamicHeight()`](https://dankelley.github.io/oce/reference/swDynamicHeight.md)
- make `section.grid()` and `ctd.decimate()` extrapolate to surface
- add `summary.oce()`
- make all objects inherit from a new class “oce”
- fix google-code issue 12:
  [`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md)
  had a hard-wired filename

## oce 0.1.69

CRAN release: 2007-12-14

- modify documentation slightly
- start a migration to more uniform object structures
- rename `as.CTD()` to
  [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md), to
  make it consistent with similar functions
- add
  [`read.section()`](https://dankelley.github.io/oce/reference/read.section.md)
- add
  [`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md),
  which only grids in p at the moment
- add a03 dataset (renamed `section` in 0.9-13)
- fix SF bug 1833719: warnings from read.ctd()
- fix google-code issue 8: read.coastline() not producing data
- improve ability of trimCtd() to ignore spurious initial data
- add “connectPoints” option to plot.TS()
- add “station” item to “ctd” object
- change `as.CTD()` to produce sigma.theta instead of sigma

## oce 0.1.68

- add coastline.world; increase resolution of coastline.maritimes
- `plot.TS()` no longer rotates the RH margin isopycnal labels

## oce 0.1.67

CRAN release: 2007-10-21

- allow [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md)
  and similar take matrices as arguments for S, T, etc

## oce 0.1.66

- fix bug in `plot.TS()` that prevented it from showing some isopycnals

## oce 0.1.65

- switch to UNESCO formulation for sw potential temperature by default
- prevent warnings with C90 compiler on some machines

## oce 0.1.64

CRAN release: 2007-07-17

- add
  [`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md),
  a generic function for reading several oceanographic files
- make
  [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
  understand WOCE-exchange files
- make
  [`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md)
  understand comma-separated data from MEDS
- make
  [`plot.sealevel()`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md)
  skip spectral graphs if timeseries has NA values
- improve aesthetics of
  [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md)
- extend
  [`plot.sealevel()`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md)
  by adding focus.time argument
- add
  [`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md)
- add `oce.as.POSIXlt()`
- add
  [`latlonFormat()`](https://dankelley.github.io/oce/reference/latlonFormat.md),
  [`latFormat()`](https://dankelley.github.io/oce/reference/latFormat.md),
  and
  [`lonFormat()`](https://dankelley.github.io/oce/reference/lonFormat.md)
- add
  [`as.coastline()`](https://dankelley.github.io/oce/reference/as.coastline.md)
- add `make.section()`
- add
  [`summary.section()`](https://dankelley.github.io/oce/reference/summary-section-method.md)
- add
  [`plot.section()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
- add `data(section)`

## oce 0.1.63

CRAN release: 2007-06-02

- add
  [`swSoundSpeed()`](https://dankelley.github.io/oce/reference/swSoundSpeed.md)

## oce 0.1.62

CRAN release: 2007-05-29

- add `historyAppend()` (later named
  [`processingLogAppend()`](https://dankelley.github.io/oce/reference/processingLogAppend.md))
  and related code
- improve
  [`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md)

## oce 0.1.61

- add rudimentary read/summary/plot functions for lobo datasets

## oce 0.1.60

- add vignette; address some build-stage warning messages

## oce 0.1.59

CRAN release: 2007-05-20

- set up data files to permit latin1 encoding of some content

## oce 0.1.58

- rename all seawater-related functions to
  e.g. [`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md),
  to allow for atmospheric analogs

## oce 0.1.57

- make `as.CTD()` accept `length(p)=1` e.g. for surface plots

## oce 0.1.56

- make
  [`read.coastline()`](https://dankelley.github.io/oce/reference/read.coastline.md)
  handle S files

## oce 0.1.55

- keep
  [`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md)
  from complaining if `length(p)` equals 1

## oce 0.1.54

- add
  [`swAlpha()`](https://dankelley.github.io/oce/reference/swAlpha.md)
  and [`swBeta()`](https://dankelley.github.io/oce/reference/swBeta.md)

## oce 0.1.53

- add
  [`swAlphaOverBeta()`](https://dankelley.github.io/oce/reference/swAlphaOverBeta.md)

## oce 0.1.52

- document the use of `df` in
  [`swN2()`](https://dankelley.github.io/oce/reference/swN2.md),
  [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
  etc.

## oce 0.1.51

- permit [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md)
  (and similar) to take NAs in args (bug B32)

## oce 0.1.50

- permit [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md)
  and descendants to take scalar pressure.

## oce 0.1.49

- add a`s.CTD()`

## oce 0.1.48

- add `"UNESCO1983"` formulation to
  [`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md) as
  an alternative

## oce 0.1.47

- speed up
  [`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md) by
  moving vector work from R to C

## oce 0.1.46

- make
  [`geodDist()`](https://dankelley.github.io/oce/reference/geodDist.md)
  accept a vector for location 1 and a scalar for location 2

## oce 0.1.45

- make
  [`read.coastline()`](https://dankelley.github.io/oce/reference/read.coastline.md)
  understand matlab and Splus formats

## oce 0.1.44

- make
  [`plot.coastline()`](https://dankelley.github.io/oce/reference/plot-coastline-method.md)
  scale lat and lon correctly

## oce 0.1.43

- add
  [`read.coastline()`](https://dankelley.github.io/oce/reference/read.coastline.md),
  [`summary.coastline()`](https://dankelley.github.io/oce/reference/summary-coastline-method.md),
  and
  [`plot.coastline()`](https://dankelley.github.io/oce/reference/plot-coastline-method.md)

## oce 0.1.42

- add
  [`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md)

## oce 0.1.41

- add
  [`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md),
  `summary.sealevel()`, and
  [`plot.sealevel()`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md)

## oce 0.1.40

- add name option to `plot.ctd.scan()`

## oce 0.1.39

- add `lapse.rate()`

## oce 0.1.38

- fix B27 (header not updated for columns added during read.ctd)

## oce 0.1.37

- rename as
  [`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md)
- rename to `ctdWrite()`

## oce 0.1.36

- add
  [`write.ctd()`](https://dankelley.github.io/oce/reference/write.ctd.md)
- add `ctdAddColumn()`
- add `ctdUpdateHeader()`

## oce 0.1.35

- add `plot.ctd.scan()`

## oce 0.1.34

- make
  [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
  calculate S if missing but C, T, and p are given
- add [`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md)

## oce 0.1.33

- add `swConductivity()`

## oce 0.1.32

- rename `oceRho()` to
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md)

## oce 0.1.31

- use namespace

## oce 0.1.30

- add `oce.viscosity()`

## oce 0.1.29

- fix B23 (not done yet)
- fix B22 (not done within oce; done at system level)

## oce 0.1.28

- add pressure method to `trimCtd()`

## oce 0.1.27

- vectorize
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md) since
  it was too slow on large datasets

## oce 0.1.26

- make `demo(oce.ctd)` print data

## oce 0.1.25

- fix some documentation errors that the 2.0.0 CHECK noticed

## oce 0.1.24

- profiles have correct y-axis
- OSX port, but must first do e.g. `export PKG_LIBS="-L/sw/lib"`

## oce 0.1.23

- OSX port (you must first do `export PKG_LIBS="-L/sw/lib"`)

## oce 0.1.22

- add `trim.ctd()`

## oce 0.1.21

- permit lat/lon with the “N”-type indicator at start

## oce 0.1.20

- fix bug B16 (label the isopycnals on TS diagram)

## oce 0.1.19

- make
  [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
  handle more file types

## oce 0.1.18

- add `oceCoriolis()`, `oceDepth()`, `oceGravity()`, `oceTFreeze()`

## oce 0.1.17

- add `oceSpecificHeat()` and `oce.N2()`; fix bug B18

## oce 0.1.16

- get `data(ctd)` working

## oce 0.1.15

- add `oceSTRho()`

## oce 0.1.14

- fix bug B15 (maybe)

## oce 0.1.13

- add oceTSRho

## oce 0.1.12

- fix bugs B10, B11, and B13

## oce 0.1.11

- fix bugs B6, B7, and B12

## oce 0.1.10

- add
  [`geodDist()`](https://dankelley.github.io/oce/reference/geodDist.md).
- add
  [`plot.ctd()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
  which is rudimentary for now.
- fix bug B8.

## oce 0.1.9

- add `spice()` function

## oce 0.1.8

- permit all eos functions to handle missing values
- fix bug B4

## oce 0.1.7

- add
  [`summary.ctd()`](https://dankelley.github.io/oce/reference/summary-ctd-method.md)
  method.
- make
  [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
  return an object of type ctd.
- get tests/ working.

## oce 0.1.6

- create tests/oce.test1.R

## oce 0.1.5

- add `oceSigma()`, `oceSigmaT()`, and `oceSigmaTheta()`

## oce 0.1.4

- permit various EOS functions to handle lists
- fix bug B3

## oce 0.1.3

- add `ocerho()`

## oce 0.1.2

- fix file-path bug
- fix bug B2

## oce 0.1.1

- add `theta()`
- fix bug B1

## oce 0.1.0

- add
  [`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
