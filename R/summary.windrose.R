summary.windrose <- function(object, ...)
{
  	if (!inherits(object, "windrose")) stop("method is only for windrose objects")
  	cat("n=", object$metadata$n, "dtheta=", object$metadata$dtheta,"\n")
    n <- length(object$data$theta)
    cat(sprintf("%10s %10s %12s %12s %12s %12s %12s\n",
                "Angle", "Count", "Mean", "Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))
    for (i in 1:n) {
        f <- object$data$fivenum[i,]
        cat(sprintf("%10g %10g %12.4f %12.4f %12.4f %12.4f %12.4f\n",
                    object$data$theta[i], object$data$count[i], object$data$mean[i], f[1], f[2], f[3], f[4], f[5]))
    }
	processing.log.summary(object)
}

