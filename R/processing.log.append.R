processing.log.append <- function (x, action="")
{
    x$processing.log$time <- c(x$processing.log$time, as.POSIXlt(Sys.time(),tz="GMT"))
    x$processing.log$action <- c(x$processing.log$action, action)
    x
}
