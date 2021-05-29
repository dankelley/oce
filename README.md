# oce <img src="https://raw.githubusercontent.com/dankelley/oce/develop/oce-logo-3.png" align="right" height="95" />

<!-- badges: start -->
[![status](https://joss.theoj.org/papers/4e11074685a0c614345560332cfd27e8/status.svg)](https://joss.theoj.org/papers/4e11074685a0c614345560332cfd27e8)
[![R-CMD-check](https://github.com/dankelley/oce/workflows/R-CMD-check/badge.svg)](https://github.com/dankelley/oce/actions)
[![Codecov test
coverage](https://codecov.io/gh/dankelley/oce/branch/develop/graph/badge.svg)](https://codecov.io/gh/dankelley/oce?branch=develop)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/oce)](https://cran.r-project.org/package=oce)
![RStudio CRAN mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/oce) ![RStudio
CRAN mirror downloads](https://cranlogs.r-pkg.org/badges/last-week/oce)
![RStudio CRAN mirror
downloads](https://cranlogs.r-pkg.org/badges/last-day/oce)
[![Rdoc](https://www.rdocumentation.org/badges/version/oce)](https://www.rdocumentation.org/packages/oce)
<!-- badges: end -->

## About oce

Oce is an R package for processing oceanographic data. Its
[webpage](https://dankelley.github.io/oce/) provides details, of which
this README file is just a sketch. A book by one of the oce co-authors
provides more information on the package and its potential for
oceanographic research (Kelley, Dan E., 2018. [Oceanographic Analysis
with R](https://www.springer.com/us/book/9781493988426). New York.
Springer-Verlag ISBN 978-1-4939-8844-0).

## Installing oce

Stable versions of oce are available from CRAN, and may be installed
from within R, in the same way as other packages. However, the CRAN
version is only updated a few times a year (pursuant to policy), so many
users install the `"develop"` branch instead. This branch may be updated
several times per day, as the authors fix bugs or add features that are
motivated by day-to-day usage. This is the branch favoured by users who
need new features or who would wish to contribute to Oce development.

The easy way to install the `"develop"` branch is to execute the
following commands in R.

    remotes::install_github("dankelley/oce", ref="develop")

and most readers should also install Ocedata, with

    remotes::install_github("dankelley/ocedata", ref="master")

## Evolution of oce

Oce is emphatically an open-source system, and so the participation of
users is very important. This is why Git is used for version control of
the Oce source code, and why GitHub is the host for that code. Users are
invited to take part in the development process, by suggesting features,
by reporting bugs, or just by watching as others do such things.
Oceanography is a collaborative discipline, so it makes sense that the
evolution of Oce be similarly collaborative.

## Examples using built-in datasets

### CTD

    library(oce)
    data(ctd)
    plot(ctd, which=c(1,2,3,5), type="l", span=150)

![Sample CTD plot.](https://raw.githubusercontent.com/dankelley/oce/develop/oce-demo-1.png)


### Acoustic Doppler profiler

    library(oce)
    data(adp)
    plot(adp)

![Sample adp plot.](https://raw.githubusercontent.com/dankelley/oce/develop/oce-demo-2.png)

### Sealevel and tides

    library(oce)
    data(sealevel)
    m <- tidem(sealevel)
    par(mfrow=c(2, 1))
    plot(sealevel, which=1)
    plot(m)

![Sample sealevel plot.](https://raw.githubusercontent.com/dankelley/oce/develop/oce-demo-3.png)

### Echosounder

    library(oce)
    data(echosounder)
    plot(echosounder, which=2, drawTimeRange=TRUE, drawBottom=TRUE)

![Sample echosounder plot.](https://raw.githubusercontent.com/dankelley/oce/develop/oce-demo-4.png)

### Map

    library(oce)
    par(mar=rep(0.5, 4))
    data(endeavour, package="ocedata")
    data(coastlineWorld, package="oce")
    mapPlot(coastlineWorld, col="gray")
    mapPoints(endeavour$longitude, endeavour$latitude, pch=20, col="red")

![Sample map plot.](https://raw.githubusercontent.com/dankelley/oce/develop/oce-demo-5.png)

### Landsat image

    library(ocedata)
    library(oce)
    data(landsat)
    plot(landsat)

![Sample landsat image plot.](https://raw.githubusercontent.com/dankelley/oce/develop/oce-demo-6.png)

