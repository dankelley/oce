# DK notes

* sf is failing to invert as expected for me, in 3 different versions.  OK for CL, though, windows sf-0.9.0
* rgdal seems ok (not tested if off-world does NA though)
* install old versions with e.g.
  `devtools::install_version("sf",version="0.9.5",repos="http://cran.us.r-project.org")`

# Test results with https://github.com/dankelley/oce/blob/EOW/sandbox/dk/eow/eow05.R



## macOS R-4.1.0 sf-0.9.5 (@dankelley)

* x and y span +-6400km, as expected
* blue dots indicate 4 problematic regions: in thin patches near 11 and 13
  o'clock, with larger patches near 5 and 8 o'clock.
* sf::sf_extSoftVersion()
          GEOS           GDAL         proj.4 GDAL_with_GEOS     USE_PROJ_H 
       "3.9.1"        "3.3.1"        "7.2.1"         "true"         "true" 

## macOS R-4.1.0 sf-1.0.0 binary from CRAN (@dankelley)

* x and y span +-6400km, as expected
* blue dots indicate 4 problematic regions: in thin patches near 11 and 13
  o'clock, and larger patches near 5 and 8 o'clock.
* sf::sf_extSoftVersion()
          GEOS           GDAL         proj.4 GDAL_with_GEOS     USE_PROJ_H           PROJ
       "3.8.1"        "3.2.1"        "7.2.1"         "true"         "true"        "7.2.1"

## macOS R-4.1.0 sf-1.0.1, built locally from CRAN tarball (@dankelley)

* x and y span +-6400km, as expected
* blue dots indicate 4 problematic regions: in thin patches near 11 and 13
  o'clock, and larger patches near 5 and 8 o'clock.
* sf::sf_extSoftVersion()
          GEOS           GDAL         proj.4 GDAL_with_GEOS     USE_PROJ_H           PROJ
       "3.9.1"        "3.3.1"        "7.2.1"         "true"         "true"        "7.2.1"

## windows-10-x64  R-4.0.2 sf-0.9.5 (@clayton33)

* x and y span +-6400km, as expected
* all red dots, indicating no uninvertible points on globe
* sf::sf_extSoftVersion()
          GEOS           GDAL         proj.4 GDAL_with_GEOS     USE_PROJ_H 
       "3.9.1"        "3.3.1"        "7.2.1"         "true"         "true" 

## ubuntu-18.04.5 R-4.1.0 sf-0.9.5 (@richardsc)

* markedly different diagram from other results
* x spans only approx +- 2200km, but y spans +-6400km. 
* all red dots, but not covering full globe so perhaps even the forward
  transform is problematic
* sf::sf_extSoftVersion()
          GEOS           GDAL         proj.4 GDAL_with_GEOS     USE_PROJ_H
       "3.8.0"        "3.0.4"        "7.0.0"         "true"         "true"

