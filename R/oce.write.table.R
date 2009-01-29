oce.write.table <- function (x, file="", ...)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (!("row.names" %in% names(list(...)))) write.table(x$data, file, row.names=FALSE, ...)
    else write.table(x$data, file, ...)
}
