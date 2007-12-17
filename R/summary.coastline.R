summary.coastline <- function(object, ...)
{
  	if (!inherits(object, "coastline"))
    	stop("method is only for coastline objects")
  	cat(sprintf("Coastline object contains %d points, bounded within box\n",
 		length(object$data$longitude)))
	cat(sprintf("%8.3f < longitude < %8.3f\n", 
		min(object$data$longitude, na.rm=TRUE), 
		max(object$data$longitude, na.rm=TRUE)))
	cat(sprintf("%8.3f < latitude  < %8.3f\n",
	 	min(object$data$latitude,  na.rm=TRUE), 
		max(object$data$latitude,  na.rm=TRUE)))
	log.summary(object)
}
