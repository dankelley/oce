summary.sealevel <- function(object, ...)
{
	if (!inherits(object, "sealevel"))
		stop("method is only for sealevel objects")
	cat(paste("Station\n"))
	cat(paste("  number:     ", object$station.number,            "\n"))
	version <- if (is.null(object$version)) "?" else object$version
	cat(paste("  version:    ", version,                          "\n"))
	cat(paste("  name:       ", object$station.name,              "\n"))
	region <- if (is.null(object$region)) "?" else object$region
	cat(paste("  region:     ", region,                           "\n"))
	cat(      "  location:  ", latlon.format(object$latitude, object$longitude), "\n")
	cat("Data\n")
	cat(paste("  number obs: ", object$n,                         "\n"))
	cat(paste("  start time: ", object$data$t[1],                 "\n"))
	cat(paste("  end time:   ", object$data$t[object$n],          "\n"))
	gmt.offset <- if (is.na(object$GMT.offset)) "?" else object$GMT.offset
	cat(paste("  GMT offset: ", gmt.offset,                       "\n"))
	fn <- fivenum(object$data$eta, na.rm=TRUE)
	cat(paste("  min:        ", fn[1],                            "\n"))
	cat(paste("  max:        ", fn[5],                            "\n"))
	cat(paste("  median:     ", fn[3],                            "\n"))
	cat(paste("  mean:       ", mean(object$data$eta,na.rm=TRUE), "\n"))
	processing.log.summary(object)
}
