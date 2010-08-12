.First.lib <- function(lib, pkg)
{
    if (version$major==0 && version$minor < 62) stop("This version for R 0.62 or later")
    library.dynam("oce", pkg, lib)
}

.onLoad <- function(libname, pkgname)
{
    op <- options()
    op.oce <- list(oce.tz = "UTC",
                   ##oce.mgp = c(2.5,1,0),
                   oce.mgp = c(2,3/4,0),
                   oce.draw.time.range = TRUE,
                   oce.debug = 0)
    toset <- !(names(op.oce) %in% names(op))
    if(any(toset)) options(op.oce[toset])
}
