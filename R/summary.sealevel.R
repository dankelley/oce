summary.sealevel <- function(object, ...)
{
	if (!inherits(object, "sealevel"))
		stop("method is only for sealevel objects")
	cat(paste("Station\n"))
	cat(paste("  number:            ", object$metadata$station.number,   "\n"))
	version <- if (is.null(object$metadata$version)) "?" else object$metadata$version
	cat(paste("  version:           ", object$metadata$station.version,   "\n"))
	cat(paste("  name:              ", object$metadata$station.name,     "\n"))
	region <- if (is.null(object$metadata$region)) "?" else object$metadata$region
	cat(paste("  region:            ", region,                           "\n"))
	cat(      "  location:          ", latlon.format(object$metadata$latitude, object$metadata$longitude), "\n")
	cat("Data\n")
	cat(paste("  number obs:        ", object$metadata$n,                "\n")) # FIXME: not used
	cat(paste("  sampling interval: ", object$metadata$sampling.interval, "hour\n"))
	cat(paste("  start time:        ", object$data$t[1],                 "\n"))
	cat(paste("  end time:          ", object$data$t[length(object$data$t)], "\n"))
	gmt.offset <- if (is.na(object$metadata$GMT.offset)) "?" else object$metadata$GMT.offset
	cat(paste("  GMT offset:        ", gmt.offset,                       "\n"))
	fn <- fivenum(object$data$eta, na.rm=TRUE)
	cat(paste("  min:               ", fn[1],                            "\n"))
	cat(paste("  max:               ", fn[5],                            "\n"))
	cat(paste("  median:            ", fn[3],                            "\n"))
	cat(paste("  mean:              ", mean(object$data$eta,na.rm=TRUE), "\n"))
	processing.log.summary(object)
}
