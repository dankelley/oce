summary.oce <- function(object, ...)
{
  	if (!inherits(object, "oce"))
    	stop("method is only for oce objects")
	cat("Data summary:\n")
	print(summary(object$data))
	cat("\nMetadata:\n")
	print(object$metadata)
	processing.log.summary(object)
	return(invisible(object))
}
