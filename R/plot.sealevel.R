plot.sealevel <- function(x, focus.time=NULL, ...)
{
	# tidal constituents (in cpd):
	# http://www.soest.hawaii.edu/oceanography/dluther/HOME/Tables/Kaw.htm
	if (!is.null(focus.time)) {
		focus.time <- as.POSIXct(focus.time)
		focus <- (focus.time[1] <= x$data$t) & (x$data$t <= focus.time[2])
		eta.m <- x$data$eta
		MSL <- mean(eta.m, na.rm=TRUE)
		eta <- (eta.m[focus] - MSL)
		plot(as.POSIXct(x$data$t)[focus], eta, type='l',ylab="Sealevel Anomaly [m]")
		abline(h=0,col="darkgreen")
		mtext(side=4,text=sprintf("%.2f m", MSL),at=0,col="darkgreen")
	} else {
	    oldpar <- par(no.readonly = TRUE)
		eg.days <- 28
		stop <- 24 * eg.days
		eta.m <- x$data$eta
		MSL <- mean(eta.m, na.rm=TRUE)
		tmp <- (pretty(max(eta.m-MSL,na.rm=TRUE)-min(eta.m-MSL,na.rm=TRUE))/2)[2]
		ylim <- c(-tmp,tmp)
		# Whole timeseries
		n <- length(x$data$eta) # do not trust value in metadata
		from <- as.POSIXlt(x$data$t[1])
		from$mday <- 1
		from$hour <- from$min <- from$sec <- 0
		to <- as.POSIXlt(x$data$t[n])
		to$mday <- 28
		to$hour <- to$min <- to$sec <- 0
		at.t <- seq(from=from, to=to, by="month")
		num.NA <- sum(is.na(x$data$eta))
		if (num.NA) {
			warning("time series contains ", num.NA, " missing data, so no spectra will be drawn")
			par(mfrow=c(2,1))
			par(mar=c(4,5,3,1)+0.1)
			par(cex=1)
		} else {
			par(mfrow=c(4,1))
			par(mar=c(2,5,2,1)+0.1)
			par(cex=1)
		}
		plot(as.POSIXlt(x$data$t)[1:n], eta.m-MSL,
#			xlab="",ylab="Sealevel Anomaly [m]",type='l',ylim=ylim,
			xlab="",ylab=expression(paste(eta-eta[0], "  [m]")), type='l',ylim=ylim,
			lwd=0.5, axes=FALSE)	
		axis(1, at.t, format(at.t, "%b %d"), cex=0.7)  # small font to get all 12 month names
		yax <- axis(2)
		box()
		abline(h=yax, col="darkgray", lty="dotted")
		abline(v=at.t, col="darkgray", lty="dotted")
		abline(h=0,col="darkgreen")
		mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen")
		title <- paste("Station ",
				x$metadata$station.number, " (",
				x$metadata$station.name,   ") ",
				x$metadata$region,         "",
				" ", latlon.format(x$metadata$latitude, x$metadata$longitude),
				if (!is.na(x$metadata$year)) paste(" (", x$metadata$year, ")") else "",
				sep="")
		mtext(side=3, title, line=0.5)
		# First bit
		if (num.NA)
			par(mar=c(3,5,0,1)+0.1)
		else
			par(mar=c(2,5,1,1)+0.1)
		from <- as.POSIXlt(x$data$t[1])
		from$hour <- from$min <- from$sec <- 0
		to <- from + 28 * 86400 # 28 days
		at.week <- seq(from=from, to=to, by="week")
		at.day  <- seq(from=from, to=to, by="day")
		tmp <- (pretty(max(eta.m[1:stop]-MSL,na.rm=TRUE)-min(eta.m[1:stop]-MSL,na.rm=TRUE))/2)[2]
		ylim <- c(-tmp,tmp)
		plot(x$data$t[1:stop], eta.m[1:stop] - MSL,
#			xlab="",ylab="Sealevel Anomaly [m]",type='l',ylim=ylim, axes=FALSE)
			xlab="",ylab=expression(paste(eta-eta[0], "  [m]")), type='l',ylim=ylim, axes=FALSE)
		axis(1, at.week, labels=format(at.week, "%b %d"))
		yax <- axis(2)
		abline(h=yax, col="lightgray", lty="dotted")
		box()
		abline(v=at.week, col="darkgray", lty="dotted")
		abline(v=at.day, col="lightgray", lty="dotted")
		abline(h=0,col="darkgreen")
		mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen")
		# 
		# Draw spectra, if series has no NA, so that spectrum is easy to construct
		if (!num.NA) {
			freq <-1/ as.numeric(difftime(x$data$t[2], x$data$t[1], units="hours"))
			cat("freq:",freq,"\n")
			Eta <- ts(eta.m,start=1,frequency=freq)
			#s<-spectrum(Eta-mean(Eta),spans=c(5,5),xlim=c(0,0.1),plot=FALSE,log="y") 
			#s<-spectrum(Eta-mean(Eta),xlim=c(0,0.1),plot=FALSE,log="y",demean=TRUE) 
			s <- spectrum(Eta-mean(Eta),spans=3,plot=FALSE,log="y",demean=TRUE,detrend=TRUE) 
			par(mar=c(2,5,1,1)+0.1)
			plot(s$freq,s$spec,xlim=c(0,0.1),
				xlab="",ylab=expression(paste(Gamma^2, "   [",m^2/cph,"]")),
				type='l',log="y")
			grid()#col="lightgray",lty="dashed")
			draw.constituent <- function(frequency=0.0805114007,label="M2",col="darkred",side=1)
			{
				abline(v=frequency, col=col)
				mtext(label, side=side, at=frequency, col=col,cex=0.8)
			}
			draw.constituents <- function()
			{
				draw.constituent(0.0387306544, "O1", side=1)
				#draw.constituent(0.0416666721, "S1", side=3)
				draw.constituent(0.0417807462, "K1", side=3)
	    		draw.constituent(0.0789992488, "N2", side=1)
				draw.constituent(0.0805114007, "M2", side=3)
				draw.constituent(0.0833333333, "S2", side=1)
			}
			draw.constituents()
			n <- x$n
			n.cum.spec <- length(s$spec)
			cum.spec <- sqrt(cumsum(s$spec) / n.cum.spec)
			e<-eta.m-mean(eta.m)
			par(mar=c(4,5,1,1)+0.1)
			plot(s$freq,cum.spec,
				xlab="Frequency [ cph ]",
				ylab=expression(paste(integral(Gamma,0,f)," df [m]")),
				type='l',xlim=c(0,0.1))
			grid()
			draw.constituents()
		}
		par(oldpar)
	}
}
