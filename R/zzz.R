.First.lib <- function(lib, pkg)
{
    if (version$major==0 && version$minor < 62) stop("This version for R 0.62 or later")
    library.dynam("oce", pkg, lib)
}
