This file is for developers only. It is in the ``.Rbuildignore`` file, so it
does not get included in the R package.

How to build oce through a Makefile
===================================

The following works well. It can help to have a target (called ``ocequick``
perhaps) that skips the ``CHECK`` action.

    OCEVSN=$(shell awk '/Version/{print($$2)}' oce/DESCRIPTION)
    oce: force
        cd oce ; Rscript -e "roxygen2::roxygenise()"
        R CMD BUILD oce
        R CMD CHECK --as-cran oce_${OCEVSN}.tar.gz
        R CMD INSTALL oce_${OCEVSN}.tar.gz
	
Diary
=====

2015 Feb 5
----------

CR and DK are discussing the idea of renaming some functions that disobey the
naming convention (maintaining old names as aliases); see [issue
582](https://github.com/dankelley/oce/issues/582).


