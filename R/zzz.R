.onLoad <- function(libname, pkgname)
{
    op <- options()
    libteos <- "/usr/local/lib/libgswteos-10.so"
    opOce <- list(oceTz = "UTC",
                  ##oceMgp = c(2.5,1,0),
                  oceMgp=c(2.0, 0.7, 0),
                  oceDrawTimeRange=TRUE,
                  oceAbbreviateTimeRange=TRUE,
                  oceTimeFormat="%Y-%m-%d %H:%M:%S",
                  ##oceFlag1 = 1,
                  eos="unesco",
                  libteos=libteos,
                  oceDebug=0)
    toset <- !(names(opOce) %in% names(op))
    if (any(toset)) {
        options(opOce[toset])
        .C("set_libteos", libteos)
    }
}
