plot.lobo.timeseries.TS <- function(lobo,
	S.col = "blue", T.col = "darkgreen", draw.legend=FALSE, ...)
{
	plot(lobo$time, lobo$S, type='l', ylab="", axes=FALSE, ...)
	axis(2, col.lab=S.col)
	axis.POSIXct(1, lobo$time)
	mtext("Salinity [PSU]", side=2, line=3, col=S.col)
	box()
	lines(lobo$time, lobo$S, col=S.col, ...)
	par(new = TRUE)
	plot(lobo$time, lobo$T, type='l', ylab="", axes=FALSE)
	lines(lobo$time, lobo$T, col=T.col, ...)
	axis(4, col=T.col)
	mtext("Temperature [degC]", side=4, line=3, col=T.col)
	if (draw.legend)
		legend("topright",c("S","T"),col=c(S.col,T.col),lwd=2)
}
plot.lobo.timeseries.uv <- function(lobo, col.u = "blue", col.v = "darkgreen", draw.legend=FALSE, ...)
{
	peak <- max(range(c(lobo$u,lobo$v),na.rm=TRUE))
	ylim <- c(-peak,peak)
	ylim <- c(-0.5,0.5)
	plot(lobo$time, lobo$u, ylim=ylim, type='l', axes=FALSE, col=col.u, ylab="", ...)
	box()
	lines(lobo$time, lobo$v, col=col.v, ...)
	axis.POSIXct(1, lobo$time)
	axis(2, col.lab=col.u)
	mtext("U [m/s]", side=2, line=3, col=col.u)
	axis(4, col.lab=col.v)
	mtext("V [m/s]", side=4, line=3, col=col.v)
	if (draw.legend)
 		legend("topright",c("U","V"),col=c(col.u,col.v),lwd=2)
	
}
plot.lobo.timeseries.biology <- function(lobo, col.fluorescence = "blue", col.nitrate = "darkgreen", draw.legend=FALSE, ...)
{
	plot(lobo$time, lobo$fluorescence, type='l', ylab="", axes=FALSE, ...)
	axis(2, col.lab=col.fluorescence)
	axis.POSIXct(1, lobo$time)
	mtext("Fluorescence", side=2, line=3, col=col.fluorescence)
	box()
	lines(lobo$time, lobo$fluorescence, col=col.fluorescence, ...)
	par(new = TRUE)
	plot(lobo$time, lobo$nitrate, type='l', ylab="", axes=FALSE, ...)
	lines(lobo$time, lobo$nitrate, col=col.nitrate)
	axis(4, col=col.nitrate)
	mtext("Nitrate", side=4, line=3, col=col.nitrate)
	if (draw.legend)
		legend("top",c("nitrate","fluorescence"),col=c(col.nitrate,col.fluorescence),lwd=2, ...)
}

plot.lobo.TS <- function(lobo, ...)
{
	plot.TS(as.CTD(lobo$S, lobo$T, lobo$p), col="red", cex=0.5, ...)
}
plot.lobo <- function(x, ...)
{
	split.screen(matrix(c(0,1,3/4,1,  0,1,2/4,3/4,  0,1,1/4,2/4,  1/2,1,0,1/4),byrow=TRUE,ncol=4))
	par(mar=c(2,4,1,4))
	screen(1)
	par(mar=c(2,4,1,4))
	plot.lobo.timeseries.TS(x, ...)
	screen(2)
	par(mar=c(2,4,1,4))
	plot.lobo.timeseries.uv(x, ...)
	screen(3)
	par(mar=c(2,4,1,4))
	plot.lobo.timeseries.biology(x, ...)
	screen(4)
	par(mar=c(4,4,1,4))
	plot.lobo.TS(x, ...)
	close.screen(all = TRUE)
}


