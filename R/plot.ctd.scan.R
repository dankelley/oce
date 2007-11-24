plot.ctd.scan <- function(x,
	name = "scan",
	S.col = "darkgreen",
	T.col = "darkred", 
	p.col = "blue", ...)
{
	if (!inherits(x, "ctd")) 
        stop("method is only for ctd objects")
    oldpar <- par(no.readonly = TRUE)
	par(mar=c(4,4,1,4)) # bot left top right
	par(mfrow=c(2,1))  
	xx <- x$data[[name]];
	xxlen <- length(xx)
	if (xxlen < 1) 
		stop(paste("this ctd has no data column named '", name, "'",sep=""))
	if (xxlen != length(x$data$pressure))
		stop(paste("length mismatch.  '", name, "' has length ", xxlen, 
		" but pressure has length ", length(x$data$pressure),sep=""))
    plot(x$data[[name]], x$data$pressure, 
		xlab=name, ylab="Pressure [dbar]", 
		type="l", axes=FALSE)
	box()
	grid(col="brown")
	axis(1)
	axis(2,col=p.col, col.axis=p.col, col.lab = p.col)

	par(mar=c(4,4,1,4)) # bot left top right
	Slen <- length(x$data$salinity)
	Tlen <- length(x$data$temperature)
	if (Slen != Tlen)
		stop(paste("length mismatch.  'salinity' has length ", Slen, 
		" but 'temperature' has length ", Tlen, sep=""))
	plot(x$data[[name]], x$data$temperature, xlab="Scan", ylab="", type="l",
		col = T.col, axes=FALSE)
	axis(1)
	axis(2,col=T.col, col.axis = T.col, col.lab = T.col)
	box()
	grid(NULL, NA, col="brown")
    mtext("Temperature [degC]", side = 2, line = 2, col = T.col)
	#
	par(new=TRUE) # overplot
    plot(x$data[[name]], x$data$salinity, xlab="", ylab="", col=S.col, type="l", axes=FALSE)
    mtext("Salinity [PSU]", side = 4, line = 2, col = S.col)
	axis(4,col=S.col, col.axis = S.col, col.lab = S.col)
	par(oldpar)
}
