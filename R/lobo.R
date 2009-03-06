plot.lobo.timeseries.TS <- function(lobo,
                                    S.col = "blue", T.col = "darkgreen", draw.legend=FALSE, ...)
{
    plot(lobo$data$time, lobo$data$salinity, type='l', ylab="", axes=FALSE, ...)
    axis(2, col.lab=S.col)
    axis.POSIXct(1, lobo$data$time)
    mtext("Salinity [PSU]", side=2, line=3, col=S.col, cex=par("cex"))
    box()
    lines(lobo$data$time, lobo$data$salinity, col=S.col, ...)
    par(new = TRUE)
    plot(lobo$data$time, lobo$data$temperature, type='l', ylab="", axes=FALSE)
    lines(lobo$data$time, lobo$data$temperature, col=T.col, ...)
    axis(4, col=T.col)
    mtext(expression(paste("Temperature [ ", degree, "C ]")), side=4, line=2.5, col=T.col, cex=par("cex"))
    if (draw.legend)
        legend("topright",c("S","T"),col=c(S.col,T.col),lwd=2)
}
plot.lobo.timeseries.uv <- function(lobo, col.u = "blue", col.v = "darkgreen", draw.legend=FALSE, ...)
{
    peak <- max(range(c(lobo$data$u,lobo$data$v),na.rm=TRUE))
    ylim <- c(-peak,peak)
    plot(lobo$data$time, lobo$data$u, ylim=ylim, type='l', axes=FALSE, col=col.u, ylab="", ...)
    box()
    lines(lobo$data$time, lobo$data$v, col=col.v, ...)
    axis.POSIXct(1, lobo$data$time)
    axis(2, col=col.u)
    axis(4, col=col.v)
    mtext("U [m/s]", side=2, line=2.5, col=col.u, cex=par("cex"))
    mtext("V [m/s]", side=4, line=2.5, col=col.v, cex=par("cex"))
    if (draw.legend)
        legend("topright",c("U","V"),col=c(col.u,col.v),lwd=2)

}
plot.lobo.timeseries.biology <- function(lobo, col.fluorescence = "blue", col.nitrate = "darkgreen", draw.legend=FALSE, ...)
{
    plot(lobo$data$time, lobo$data$fluorescence, type='l', ylab="", axes=FALSE, ...)
    axis(2, col.lab=col.fluorescence)
    axis.POSIXct(1, lobo$data$time)
    mtext("Fluorescence", side=2, line=2.5, col=col.fluorescence, cex=par("cex"))
    box()
    lines(lobo$data$time, lobo$data$fluorescence, col=col.fluorescence, ...)
    par(new = TRUE)
    plot(lobo$data$time, lobo$data$nitrate, type='l', ylab="", axes=FALSE, ...)
    lines(lobo$data$time, lobo$data$nitrate, col=col.nitrate)
    axis(4, col=col.nitrate)
    mtext("Nitrate", side=4, line=2.5, col=col.nitrate, cex=par("cex"))
    if (draw.legend)
        legend("top",c("nitrate","fluorescence"),col=c(col.nitrate,col.fluorescence),lwd=2, ...)
}

plot.lobo.TS <- function(lobo, ...)
{
    plot.TS(as.ctd(lobo$data$salinity, lobo$data$temperature, lobo$data$p), col="red", ...)
}
plot.lobo <- function(x, ...)
{
    par(mfrow=c(4,1))
    par(mar=c(2,5,1,5))
    plot.lobo.timeseries.TS(x, ...)
    par(mar=c(2,5,1,5))
    plot.lobo.timeseries.uv(x, ...)
    par(mar=c(2,5,1,5))
    plot.lobo.timeseries.biology(x, ...)
    par(mar=c(5,5,3,5))
    plot.lobo.TS(x, ...)
}
read.lobo <- function(file, cols=7, log.action) {
    header <- scan(file, what=character(), sep="\t", nlines=1, quiet=TRUE)
    d <- scan(file, what=character(), sep="\t", skip=1,  quiet=TRUE)
                                        # find columns. BUG: assumes names don't change
    col.date         <- grep("date", header)
    col.u            <- grep("current across", header)
    col.v            <- grep("current along", header)
    col.nitrate      <- grep("nitrate", header)
    col.fluorescence <- grep("fluorescence", header)
    col.salinity     <- grep("salinity", header)
    col.temperature  <- grep("temperature", header)
    if (cols == 7) {
        n <- length(d) / cols
        time         <-            d[seq(from=col.date,         by=cols, length.out = n)]
        u            <- as.numeric(d[seq(from=col.u,            by=cols, length.out = n)])
        v            <- as.numeric(d[seq(from=col.v,            by=cols, length.out = n)])
        nitrate      <- as.numeric(d[seq(from=col.nitrate,      by=cols, length.out = n)])
        fluorescence <- as.numeric(d[seq(from=col.fluorescence, by=cols, length.out = n)])
        salinity     <- as.numeric(d[seq(from=col.salinity,     by=cols, length.out = n)])
        temperature  <- as.numeric(d[seq(from=col.temperature,  by=cols, length.out = n)])
        p            <- rep(0, length(salinity))
        time <- as.POSIXlt(time)
        data <- data.frame(time=time,u=u,v=v,salinity=salinity,temperature=temperature,p=p,nitrate=nitrate,fluorescence=fluorescence)
        metadata <- list(header=header)
        if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
        log.item <- processing.log.item(log.action)
        res <- list(data=data, metadata=metadata, processing.log=log.item)
        class(res) = c("lobo", "oce")
        res
    } else {
        stop("debug: only working on one format right now")
    }
}
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
