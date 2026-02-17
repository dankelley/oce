# How to build oce through a Makefile

**Preface.** This file is for developers only. It is in the
`.Rbuildignore` file, so it does not get included in the R package.

The following works well. It can help to have a target (called
`ocequick` perhaps) that skips the `CHECK` action.

``` R
OCEVSN=$(shell awk '/Version/{print($$2)}' oce/DESCRIPTION)
oce: force
    cd oce ; Rscript -e "roxygen2::roxygenise()"
    R CMD BUILD oce
    R CMD CHECK --as-cran oce_${OCEVSN}.tar.gz
    R CMD INSTALL oce_${OCEVSN}.tar.gz
```

# Diary
