.First.lib <- function(lib, pkg)
{
    if (version$major==0 && version$minor < 62)
        stop("This version for R 0.62 or later")
    library.dynam("oce", pkg, lib)
}

.onLoad <- function(libname, pkgname)
{
    op <- options()
    opOce <- list(oceTz = "UTC",
                  ##oceMgp = c(2.5,1,0),
                  oceMgp = c(2.0, 0.7, 0),
                  oceDrawTimeRange = TRUE,
                  oceAbbreviateTimeRange = TRUE,
                  oceTimeFormat = "%Y-%m-%d %H:%M:%S",
                  ##oceFlag1 = 1,
                  teos=FALSE,
                  oceDebug = 0)
    toset <- !(names(opOce) %in% names(op))
    if (any(toset))
        options(opOce[toset])
}
