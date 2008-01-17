fit.tide <- function(sl, constituents="standard", rc=1)
{
	debug <- !TRUE
	if (!inherits(sl, "sealevel")) stop("method is only for sealevel objects")
	tc <- tide.constituents()
                                        # The [-1] below trims Z0 (since R handles intercepts by itself)
	if (length(constituents) == 1 && constituents == "standard") {
		name      <- tc$name[tc$standard][-1]
		frequency <- tc$frequency[tc$standard][-1]
		compare   <- tc$compare[tc$standard][-1]
	} else {
		iZ0 <- which(constituents == "Z0")
		name <- constituents
		if (length(iZ0)) name <- name[-iZ0]
		nc <- length(name)
		frequency <- vector("numeric", nc)
		compare   <- vector("numeric", nc)
		for (i in 1:nc) {
			ic <- which(tc$name == constituents[i])
			if (!length(ic)) stop("there is no tidal constituent named \"", constituents[i], "\"")
			frequency[i] <- tc$frequency[ic]
			compare[i] <- tc$compare[ic]
		}
	}
	nc <- length(frequency)
                                        # Check Rayleigh criterion
	interval <- as.numeric(difftime(max(sl$data$t,na.rm=TRUE),min(sl$data$t,na.rm=TRUE),units="hours"))
                                        #cat("interval:",interval,"\n")
	drop.term <- NULL
	for (i in 1:nc) {
		cc <- which(tc$name == compare[i])
		cannot.fit <- (interval * abs(frequency[i]-tc$frequency[cc])) < rc
                                        #cat("compare", name[i], "with", compare[i],":", cannot.fit,"\n")
		if (cannot.fit)	drop.term <- c(drop.term, i)
	}
                                        #cat("DROP:",drop.term,"\n")
	if (length(drop.term) > 0) {
		cat("Record is too short to fit for constituents:", name[drop.term],"\n")
		frequency <- frequency[-drop.term]
		name      <- name[-drop.term]
		compare   <- compare[-drop.term]
	}
	nc <- length(frequency)
	nt <- length(sl$data$eta)
	x <- array(dim=c(nt, 2 * nc))
	x[,1] <- rep(1, nt)
	hour2pi <- (as.numeric((sl$data$t - sl$data$t[1]))) / 3600 * (2 * pi)
	for (i in 1:nc) {
		omega.t <- frequency[i] * hour2pi
		x[,2*i-1] <- sin(omega.t)
		x[,2*i  ] <- cos(omega.t)
	}
	name2 <- matrix(rbind(paste(name,"_S",sep=""), paste(name,"_C",sep="")), nrow=(length(name)), ncol=2)
	dim(name2) <- c(2 * length(name), 1)
	colnames(x) <- name2
	eta <- sl$data$eta
	model <- lm(eta ~ x, na.action=na.exclude)
	coef  <- model$coefficients
	p.all <- summary(model)$coefficients[,4]
	amplitude <- phase <- p <-vector("numeric", length=1+nc)
                                        # FIXME: decide whether to use Z0 or do mean/detrend as T_TIDE; it
                                        # affects the loop indexing, something I've had mixed up before :-(
	amplitude[1] <- coef[1]
	phase[1] <- 0
	p[1] <- p.all[1]
	for (i in 2:(nc+1)) {
		is <- 2 * (i - 1)
		ic <- 2 * (i - 1) + 1
		s <- coef[is]
		c <- coef[ic]
		if (debug) cat("i=",i, "gives s=",s,"and c=",c,"\n")
		amplitude[i] <- sqrt(s^2 + c^2)
		phase[i]     <- atan2(c, s) # atan2(y,x) ... made into degrees later
		p[i]         <- 0.5 * (p.all[is] + p.all[ic])
	}
	if (debug) cat("coef:", coef, "\n")
	phase <- phase * 180 / pi
	rval <- list(model=model,
                 name=c("Z0", name),
                 frequency=c(0,frequency),
                 amplitude=amplitude,
                 phase=phase,
                 p=p)
	class(rval) <- "tide"
	rval
}
