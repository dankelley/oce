plot.tide <- function(x, label.if=NULL, style=c("spikes", "staircase"), log="", ...)
{
	data(tide.constituents)
	draw.constituent <- function(frequency=0.0805114007,label="M2",col="blue",side=1, adj=NULL)
	{
		i <- which(
		abline(v=frequency, col=col, lty="dotted")
		if (is.null(adj))
			mtext(label, side=side, at=frequency, col=col, cex=0.8)
		else
			mtext(label, side=side, at=frequency, col=col, cex=0.8, adj=adj)
	}
	draw.constituents <- function(type="standard", label.if=NULL, col="blue")
	{
		if (type == "standard") {
			#draw.constituent(0.0416666721, "S1", side=3)
			draw.constituent(0.1610228, "SA", side=3)
			draw.constituent(0.0417807462, "K1", side=3)
			draw.constituent(0.0387306544, "O1", side=1)
			draw.constituent(0.0805114007, "M2", side=3, adj=1)
			draw.constituent(0.0833333333, "S2", side=1, adj=0)
			draw.constituent(0.1610228, "M4", side=3)
		} else {
			if (is.null(label.if)) label.if <- amplitude[order(amplitude, decreasing=TRUE)[3]]
			for (i in 1:nc) {
				if (amplitude[i] >= label.if) {
					abline(v=frequency[i], col=col, lty="dotted")
					mtext(name[i], side=3, at=frequency[i], col=col)
				}
			}
		}
	}
	if (!inherits(x, "tide")) stop("method is only for tidal analysis objects")
	frequency <- x$frequency[-1] # trim z0
	amplitude <- x$amplitude[-1]
	name      <- x$name[-1]
	nc <- length(frequency)
	style <- match.arg(style)
	if (style == "spikes") {
    	plot(frequency, amplitude, col="white", xlab="Frequency [ cph ]", ylab="Amplitude [ m ]", log=log)
		segments(frequency, 0, frequency, amplitude)
		draw.constituents()
	} else if (style == "staircase") {
		plot(frequency, cumsum(amplitude), type='l', xlab="Frequency [ cph ]", ylab="Amplitude [ m ]", log=log)
		draw.constituents()
	} else {
		stop("unknown style ", style)
	}
}
