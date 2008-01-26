processing.log.append <- function (x, action="")
{
    x$processing.log$time <- c(x$processing.log$time, Sys.time())
    x$processing.log$action <- c(x$processing.log$action, action)
    x
}
