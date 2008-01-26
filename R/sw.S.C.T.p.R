sw.S.C.T.p <- function(C, t, p)
{
    dim <- dim(C)
    nC <- length(C)
    nt <- length(t)
    np <- length(p)
    if (nC != nt) stop("lengths of C and t must agree, but they are ", nC, " and ", nt, ", respectively")
    if (nC != np) stop("lengths of C and p must agree, but they are ", nC, " and ", np, ", respectively")
    rval <- .C("sw_salinity",
               as.integer(nC),
               as.double(C),
               as.double(t),
               as.double(p),
               value = double(nC),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}
