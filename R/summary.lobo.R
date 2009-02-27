summary.lobo <- function(object, ...)
{
    if (!inherits(object, "lobo")) stop("method is only for lobo objects")
    tr <- range(object$data$time, na.rm=TRUE)
    cat(paste("Lobo data acquired over time range", tr[1], "to", tr[2], "\n"))
    cat(sprintf(" %15s %12s %12s %12s %12s %12s\n", "             ", "min", "Q1", "median", "Q3", "max"));
    f<-fivenum(object$data$fluorescence,na.rm=TRUE);    cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  fluorescence ", f[1], f[2], f[3], f[4], f[5]))
    f<-fivenum(object$data$nitrate,na.rm=TRUE);         cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  nitrate      ", f[1], f[2], f[3], f[4], f[5]))
    f<-fivenum(object$data$salinity,na.rm=TRUE);               cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  salinity     ", f[1], f[2], f[3], f[4], f[5]))
    f<-fivenum(object$data$temperature,na.rm=TRUE);               cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  temperature  ", f[1], f[2], f[3], f[4], f[5]))
    f<-fivenum(object$data$u,na.rm=TRUE);               cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  u            ", f[1], f[2], f[3], f[4], f[5]))
    f<-fivenum(object$data$v,na.rm=TRUE);               cat(sprintf(" %15s %12.3f %12.3f %12.3f %12.3f %12.3f\n", "  v            ", f[1], f[2], f[3], f[4], f[5]))
    processing.log.summary(object)
}
