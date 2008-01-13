fit.tide <- function(sl, constituents="standard", rc=1)
{
	warning("rc is not being used")
	if (!inherits(sl, "sealevel")) stop("method is only for sealevel objects")
	tc <- tide.constituents()
	# The [-1] below trims Z0 (since R handles intercepts by itself)
	if (length(constituents) == 1 && constituents == "standard") {
		name      <- tc$name[tc$standard][-1]
		frequency <- tc$frequency[tc$standard][-1]
	} else {
		iZ0 <- which(constituents == "Z0")
		name <- constituents
		if (length(iZ0)) name <- name[-iZ0]
		nc <- length(name)
		frequency <- vector("numeric", nc)
		for (i in 1:nc) {
			ic <- which(tc$name == constituents[i])
			if (!length(ic)) stop("there is no tidal constituent named \"", constituents[i], "\"")
			frequency[i] <- tc$frequency[ic]
		}
	}
	nc <- length(frequency)
	nt <- length(sl$data$eta)
	x <- array(dim=c(nt, 2 * nc))
	x[,1] <- rep(1, nt)
	hour <- (as.numeric((sl$data$t - sl$data$t[1]))) / 3600
	for (i in 1:nc) {
		omega.t <- 2 * pi *frequency[i] * hour
		x[,2*i-1] <- sin(omega.t)
		x[,2*i  ] <- cos(omega.t)
	}
	name2 <- matrix(rbind(paste(name,"_S",sep=""), paste(name,"_C",sep="")), nrow=(length(name)), ncol=2)
	dim(name2) <- c(2 * length(name), 1) 
	colnames(x) <- name2
	eta <- sl$data$eta
	model <- lm(eta ~ x)
	coef  <- model$coefficients
	p.all <- summary(model)$coefficients[,4]
	amplitude <- phase <- p <-vector("numeric", length=1+nc)
	amplitude[1] <- coef[1]
	phase[1] <- 0
	p[1] <- p.all[1]
	for (i in 2:(nc+1)) {
		is <- 2 * (i - 1)
		ic <- 2 * (i - 1) + 1
		s <- coef[is]
		c <- coef[ic]
		amplitude[i] <- sqrt(s^2 + c^2)
		phase[i]     <- atan2(c, s)
		p[i]         <- 0.5 * (p.all[is] + p.all[ic])
	}
	rval <- list(model=model, name=c("Z0", name), 
		frequency=c(0,frequency), amplitude=amplitude,
		phase=phase, p=p)
	class(rval) <- "tide"
	rval
}
