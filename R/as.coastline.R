as.coastline <- function(latitude, longitude)
{
    n <- length(latitude)
    if (n != length(longitude))
        stop("Lengths of longitude and latitude must be equal")
    data <- data.frame(longitude=longitude, latitude=latitude)
    log.item <- list(time=c(Sys.time()), action=c("created by as.coastline()"))
    res <- list(data=data, metadata=NULL, processing.log=log.item)
    class(res) <- c("coastline", "oce")
    res
}
