header <- function(x)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    return(x$metadata$header)
}
