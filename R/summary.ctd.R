summary.ctd <- function(object, ...)
{
  	if (!inherits(object, "ctd"))
    	stop("method is only for ctd objects")
  	cat("CTD Profile\n")
	if (!is.null(object$filename.orig))      cat("  Raw file:           \"", object$filename.orig, "\"\n",sep="")
	if (!is.null(object$system.upload.time)) cat(paste("  System upload time: ", object$system.upload.time, "\n"))
	if (!is.null(object$date))               cat(paste("  Date:               ", object$date, "\n"))
	if (!is.null(object$institute))          cat("  Institute:          ", object$institute, "\n")
	if (!is.null(object$scientist))          cat("  Scientist:          ", object$scientist, "\n")
	if (!is.null(object$ship))               cat("  Ship:               ", object$ship, "\n")
	if (!is.null(object$cruise))             cat("  Cruise:             ", object$cruise, "\n")
	if (!is.na(object$latitude))             cat("  Location:           ", latlon.format(object$latitude, object$longitude), "\n")
	if (!is.null(object$station))            cat("  Station:            ", object$station, "\n")
	if (!is.null(object$start.time))         cat(paste("  Start time:         ", as.POSIXct(object$start.time), "\n"))
	if (!is.null(object$deployed)) {
  		cat("  Deployed:           ")
		print(object$date)
	}
  	#cat(" Start sec:", object$start.time, "\n")
	if (!is.null(object$recovered))          cat("  Recovered:          ", object$recovery, "\n")
	if (!is.null(object$water.depth))        cat("  Water depth:        ", object$water.depth, "\n")
  	cat("  No. of levels:      ", length(object$data$temperature),  "\n")
  	cat(sprintf("   %15s  %10s %10s %10s %10s %10s\n", "ITEM", "min", "Q1", "median", "Q3", "max"));
	names <- names(object$data)
	for (name in names) {
		f <- fivenum(object$data[[name]])
		cat(sprintf("    %15s %10.1f %10.1f %10.1f %10.1f %10.1f\n", name, f[1], f[2], f[3], f[4], f[5]))
	}
	processing.log.summary(object)
}
 
