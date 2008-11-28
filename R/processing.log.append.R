processing.log.append <- function (x, action="")
{
    ## FIXME was POSIXlt but problem with e.g. summary(section$data$station[[1])
    ## for something made as in OCE3001-asst2-2008
    x$processing.log$time <- c(x$processing.log$time, as.POSIXct(Sys.time(),tz="GMT"))
    x$processing.log$action <- c(x$processing.log$action, action)
    x
}
