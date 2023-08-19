.onLoad <- function(libname, pkgname)
{
    op <- options()
    opOce <- list(
        oceAbbreviateTimeRange=TRUE,
        oceDebug=0,
        oceDrawTimeRange=TRUE,
        oceEOS="unesco", # or "unesco"
        oceMar=c(3, 3, 2, 2),
        oceMgp=c(2.0, 0.7, 0),
        oceTimeFormat="%Y-%m-%d %H:%M:%S",
        oceTz="UTC",
        oceUnitBracket="[", # or "("
        webtide="/usr/local/WebTide")
    toset <- !(names(opOce) %in% names(op))
    if (any(toset)) {
        options(opOce[toset])
    }
}
