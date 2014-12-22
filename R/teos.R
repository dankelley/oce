teosSetLibrary <- function(path)
{
    options(eos=path)
    .C("set_libteos", path)
}

teos <- function(name, a1, a2, a3, a4, lib=getOption("libteos")) # FIXME: what's max arg?
{
    warning('teos(\"', name, '\", ...) will be removed in Jan 2015; please use ', name, '(...) instead.', sep="")
    if (missing(name))
        stop("a function name must be given, and it must be in lower case letters, e.g. \"gsw_sa_from_sp\"")
    if (missing(a1))
        stop("must provide a1")
    ## FIXME: later, can count when the missing list starts, and use that to figure out which C function to call
    if (missing(a2))
        stop("must provide a2")
    if (missing(a3)) {
        args <- 2
    } else {
        if (missing(a4)) {
            args <- 3 
        } else {
            args <- 4
        }                              # FIXME: this check on the number args is inelegant
    }
    dim <- dim(a1)
    a1 <- as.vector(a1)
    n <- length(a1)
    a2 <- as.vector(a2)
    ## FIXME: should use rep() to make lengths match, perhaps (surely this depends on teos calls though,
    ## e.g. consider pref)
    if (length(a2) != n)
        stop("length(a2) must match length(a1)")
    good <- is.finite(a1) & is.finite(a2)
    if (args > 2) {
        if (length(a3) != n)
            stop("length(a3) must match length(a1)")
        a3 <- as.vector(a3)
        good <- good & is.finite(a3)
    }
    if (args > 3) {
        if (length(a4) != n)
            stop("length(a4) must match length(a1)")
        a4 <- as.vector(a4)
        good <- good & is.finite(a4)
    }
    rval <- rep(NA, n)
    ngood <- sum(good)
    if (args == 2) {
        rval[good] <- .C("gsw2a", as.character(lib), as.character(name),
                         as.integer(ngood),
                         as.double(a1[good]),
                         as.double(a2[good]),
                         rval=double(ngood), NAOK=TRUE, PACKAGE="oce")$rval
    } else if (args == 3) {
        rval[good] <- .C("gsw3a", as.character(lib), as.character(name),
                         as.integer(ngood),
                         as.double(a1[good]),
                         as.double(a2[good]),
                         as.double(a3[good]),
                         rval=double(ngood), NAOK=TRUE, PACKAGE="oce")$rval
    } else if (args == 4) {
        rval[good] <- .C("gsw4a", as.character(lib), as.character(name),
                         as.integer(ngood),
                         as.double(a1[good]),
                         as.double(a2[good]),
                         as.double(a3[good]),
                         as.double(a4[good]),
                         rval=double(ngood), NAOK=TRUE, PACKAGE="oce")$rval
    }
    rval[rval == 9e15] <- NA
    dim(rval) <- dim
    rval
}

