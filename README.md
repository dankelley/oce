# **oce**

TravisCI test: [![Build Status](https://travis-ci.org/dankelley/oce.svg?branch=develop)](https://travis-ci.org/dankelley/oce)
[![codecov](https://codecov.io/gh/dankelley/oce/branch/develop/graphs/badge.svg)](https://codecov.io/gh/dankelley/oce)

CRAN: [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/oce)](http://cran.r-project.org/package=oce)
![RStudio CRAN mirror downloads](http://cranlogs.r-pkg.org/badges/last-month/oce)
![RStudio CRAN mirror downloads](http://cranlogs.r-pkg.org/badges/last-week/oce)
![RStudio CRAN mirror downloads](http://cranlogs.r-pkg.org/badges/last-day/oce)


``Oce`` is an R package for processing oceanographic data.  Its
[webpage](http://dankelley.github.com/oce/) provides details, of which this
README file is just a sketch.

Stable versions of the package are normally installed from within R, in the
same way as other packages.  However, this version is only updated a few times
a year (pursuant to CRAN policy), so many users will want to install the
``develop`` branch instead. This branch may be updated several times per day,
as the authors fix bugs or add features that are motivated by day-to-day
usage.  This is the branch favoured by users who need new features or would
like to contribute to Oce development.  

The easy way to install the ``develop`` branch is to execute the
following commands in R.
```splus
library(devtools)
install_github('dankelley/oce', ref='develop')
```
and most readers should also install Ocedata, with
```splus
install_github('dankelley/ocedata', ref='master')
```

Oce is emphatically an open-source system, and so the participation of users is
very important.  This is why Git is used for version control of the Oce source
code, and why GitHub is the host for that code.  All users are invited to take
part in the development process, by suggesting features, by reporting bugs, or
just by watching as others do such things.  Oceanography is a collaborative
discipline, so it makes sense that the evolution of Oce be similarly
collaborative.

