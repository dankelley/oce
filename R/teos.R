teosSetLibrary <- function(path)
{
    options(eos=path)
    .C("set_libteos", path)
}

teos <- function(name, a1, a2, a3, a4, a5, a6, lib=getOption("libteos")) # FIXME: what's max arg?
{
    if (missing(name)) stop("a function name must be given, and it must be in lower case letters, e.g. \"gsw_sa_from_sp\"")
    if (missing(a1)) stop("must provide a1")
    ## FIXME: later, can count when the missing list starts, and use that to figure out which C function to call
    if (missing(a2)) stop("must provide a2")
    if (missing(a3)) stop("must provide a3")
    if (missing(a4)) args <- 3 else args <- 4
    if (!missing(a5)) stop("cannot have 5 args yet")
    if (!missing(a6)) stop("cannot have 6 args yet")
    dim <- dim(a1)
    a1 <- as.vector(a1)
    n <- length(a1)
    a2 <- as.vector(a2)
    if (length(a2) != n) stop("length(a2) must match length(a1)")
    if (length(a3) != n) stop("length(a3) must match length(a1)")
    a3 <- as.vector(a3)
    ## FIXME: teos should filter on NA so the gsw routines don't return odd "missing" values
    if (args == 3) {
        rval <- .C("gsw3a", as.character(lib), as.character(name),
                   as.integer(n), as.double(a1), as.double(a2), as.double(a3), rval=double(n))$rval
    } else if (args == 4) {
        a4 <- as.vector(a4)
        if (length(a4) != n) stop("length(a4) must match length(a1)")
        rval <- .C("gsw4a", as.character(lib), as.character(name),
                   as.integer(n), as.double(a1), as.double(a2), as.double(a3), as.double(a4), rval=double(n))$rval
    }
    dim(rval) <- dim
    rval
}

