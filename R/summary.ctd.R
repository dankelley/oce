summary.ctd <- function(object, ...)
{
  	if (!inherits(object, "ctd"))
    	stop("method is only for ctd objects")
  	cat("CTD Profile\n")
	if (!is.null(object$metadata$filename.orig))      cat("  Raw file:           \"",     object$metadata$filename.orig, "\"\n",sep="")
	if (!is.null(object$metadata$system.upload.time)) cat(paste("  System upload time: ", object$metadata$system.upload.time, "\n"))
	if (!is.null(object$metadata$date))               cat(paste("  Date:               ", object$metadata$date, "\n"))
	if (!is.null(object$metadata$institute))          cat("  Institute:          ",       object$metadata$institute, "\n")
	if (!is.null(object$metadata$scientist))          cat("  Scientist:          ",       object$metadata$scientist, "\n")
	if (!is.null(object$metadata$ship))               cat("  Ship:               ",       object$metadata$ship, "\n")
	if (!is.null(object$metadata$cruise))             cat("  Cruise:             ",       object$metadata$cruise, "\n")
	if (!is.na(  object$metadata$latitude))           cat("  Location:           ",       latlon.format(object$metadata$latitude, object$metadata$longitude), "\n")
	if (!is.null(object$metadata$station))            cat("  Station:            ",       object$metadata$station, "\n")
	if (!is.null(object$metadata$start.time))         cat(paste("  Start time:         ", as.POSIXct(object$metadata$start.time), "\n"))
	if (!is.null(object$metadata$deployed)) {
  		cat("  Deployed:           ")
		print(object$date)
	}
  	#cat(" Start sec:", object$start.time, "\n")
	if (!is.null(object$metadata$recovered))          cat("  Recovered:          ", object$metadata$recovery, "\n")
	if (!is.null(object$metadata$water.depth))        cat("  Water depth:        ", object$metadata$water.depth, "\n")
  	cat("  No. of levels:      ", length(object$data$temperature),  "\n")
  	cat(sprintf("   %15s  %10s %10s %10s %10s %10s\n", "ITEM", "min", "Q1", "median", "Q3", "max"));
	names <- names(object$data)
	for (name in names) {
		f <- fivenum(object$data[[name]])
		cat(sprintf("    %15s %10.1f %10.1f %10.1f %10.1f %10.1f\n", name, f[1], f[2], f[3], f[4], f[5]))
	}
	log.summary(object)
}
 
