.First.lib <- function(lib, pkg)
{
    if (version$major==0 && version$minor < 62) stop("This version for R 0.62 or later")
    library.dynam("oce", pkg, lib)
}

.onLoad <- function(libname, pkgname)
{
    op <- options()
    op.oce <- list(oce.tz = "UTC") # , oce.mgp = c(3,1,0))
    toset <- !(names(op.oce) %in% names(op))
    if(any(toset)) options(op.oce[toset])
}
