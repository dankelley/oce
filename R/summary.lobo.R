summary.lobo <- function(object, ...)
{
  	if (!inherits(object, "lobo"))
    	stop("method is only for lobo objects")
  	cat("lobo data\n")
	tr <- range(object$time)
	cat(paste("  Time range: ", tr[1], " to ", tr[2], "\n"))
  	cat(sprintf(" %15s %12s %12s %12s %12s %12s\n", "ITEM         ", "min", "Q1", "median", "Q3", "max"));
  	f<-fivenum(object$fluorescence);    cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  fluorescence ", f[1], f[2], f[3], f[4], f[5]))
  	f<-fivenum(object$nitrate);         cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  nitrate      ", f[1], f[2], f[3], f[4], f[5]))
  	f<-fivenum(object$S);               cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  S            ", f[1], f[2], f[3], f[4], f[5]))
  	f<-fivenum(object$T);               cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  T            ", f[1], f[2], f[3], f[4], f[5]))
  	f<-fivenum(object$u);               cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  u            ", f[1], f[2], f[3], f[4], f[5]))
  	f<-fivenum(object$v);               cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  v            ", f[1], f[2], f[3], f[4], f[5]))
	processing.log.summary(object)
}
