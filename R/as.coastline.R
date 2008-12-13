as.coastline <- function(latitude, longitude)
{
    n <- length(latitude)
    if (n != length(longitude)) stop("Lengths of longitude and latitude must be equal")
    data <- data.frame(longitude=longitude, latitude=latitude)
    log.item <- processing.log.item(paste(deparse(match.call()), sep="", collapse=""))
    res <- list(data=data, metadata=NULL, processing.log=log.item)
    class(res) <- c("coastline", "oce")
    res
}
