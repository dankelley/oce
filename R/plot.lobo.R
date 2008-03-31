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
    mtext("Temperature [degC]", side=4, line=2.5, col=T.col, cex=par("cex"))
    if (draw.legend)
        legend("topright",c("S","T"),col=c(S.col,T.col),lwd=2)
}
plot.lobo.timeseries.uv <- function(lobo, col.u = "blue", col.v = "darkgreen", draw.legend=FALSE, ...)
{
    peak <- max(range(c(lobo$data$u,lobo$data$v),na.rm=TRUE))
    ylim <- c(-peak,peak)
    ylim <- c(-0.5,0.5)
    plot(lobo$data$time, lobo$data$u, ylim=ylim, type='l', axes=FALSE, col=col.u, ylab="", ...)
    box()
    lines(lobo$data$time, lobo$data$v, col=col.v, ...)
    axis.POSIXct(1, lobo$data$time)
    axis(2, col.lab=col.u)
    mtext("U [m/s]", side=2, line=2.5, col=col.u, cex=par("cex"))
    axis(4, col.lab=col.v)
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
    plot.TS(as.ctd(lobo$data$salinity, lobo$data$temperature, lobo$data$p), col="red", cex=0.75, ...)
}
plot.lobo <- function(x, ...)
{
    par(mfrow=c(4,1))
    par(mar=c(2,4,1,4))
    plot.lobo.timeseries.TS(x, ...)
    par(mar=c(2,4,1,4))
    plot.lobo.timeseries.uv(x, ...)
    par(mar=c(2,4,1,4))
    plot.lobo.timeseries.biology(x, ...)
    par(mar=c(5,4,3,4))
    plot.lobo.TS(x, ...)
}
