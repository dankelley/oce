summary.topo <- function(object, ...)
{
    if (!inherits(object, "topo")) stop("method is only for topo objects")
    cat("ETOPO2 dataset\n")
    lat.range <- range(object$data$lat)
    cat("  Latitude  from ", lat.format(lat.range[1]), " to ", lat.format(lat.range[2]),
        " in ", length(object$data$lat), " points\n",sep="")
    lon.range <- range(object$data$lon)
    cat("  Longitude from ", lon.format(lon.range[1]), " to ", lon.format(lon.range[2]),
        " in ", length(object$data$lon), " points\n", sep="")
    z.range <- range(object$data$z)
    cat("  Altitude  from ", z.range[1], "m to ", z.range[2], "m\n",sep="")
    processing.log.summary(object)
}
