processing.log.append <- function (x, action="")
{
    x$processing.log$time <- c(x$processing.log$time, as.POSIXct(Sys.time(),tz="GMT"))
    x$processing.log$action <- c(x$processing.log$action, action)
    x
}
