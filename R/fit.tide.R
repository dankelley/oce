fit.tide <- function(sl, constituents, rc=1)
{
	debug <- !TRUE
	if (!inherits(sl, "sealevel")) stop("method is only for sealevel objects")
	tc <- tide.constituents()
    ntc <- length(tc$name)
    name <- frequency <- compare <- NULL
    indices <- NULL
    if (missing(constituents)) {
		name      <- tc$name[tc$standard][-1]
		frequency <- tc$frequency[tc$standard][-1]
		compare   <- tc$compare[tc$standard][-1]
        indices <- c(indices, seq(1:ntc)[tc$standard])
    } else {
        nconst <- length(constituents)
        for (i in 1:nconst) {
            if (debug) cat("[", constituents[i], "]\n",sep="")
            if (constituents[i] == "standard") { # must be first!
                if (i != 1) stop("\"standard\" must occur first in constituents list")
                name      <- tc$name[tc$standard][-1]
                frequency <- tc$frequency[tc$standard][-1]
                compare   <- tc$compare[tc$standard][-1]
                indices <- c(indices, seq(1:ntc)[tc$standard])
                ##cat("INDICES:", indices, "\n")
            } else {
                if (substr(constituents[i], 1, 1) == "-") {
                    cc <- substr(constituents[i], 2, nchar(constituents[i]))
                    delete <- which(tc$name == cc)
                    if (length(delete) == 1) {
                        indices <- indices[indices != delete]
                    } else {
                        stop("cannot delete constituent '", cc, "' from the list because it is not there")
                    }
                } else {
                    add <- which(tc$name == constituents[i])
                    if (length(add) == 1) {
                        if (0 == sum(indices == add)) indices <- c(indices, add) # avoid duplicates
                    } else {
                        stop("cannot add constituent '", constituents[i], "' because it is not known; see ?tide.constituents")
                    }
                }
            }
            if (debug) cat("<<", tc$name[indices], ">>\n")
        }
    }
    indices <- indices[order(indices)]
    tc2 <- list(name=tc$name[indices], frequency=tc$frequency[indices], compare=tc$compare[indices])

    ##print(data.frame(tc2))

    iZ0 <- which(tc2$name == "Z0")      # Remove Z0
    name <- tc2$name
    if (length(iZ0)) name <- name[-iZ0]
    nc <- length(name)
    frequency <- vector("numeric", nc)
    compare   <- vector("numeric", nc)
    for (i in 1:nc) {                   # Build up based on constituent names
        ic <- which(tc$name == name[i])
        if (!length(ic)) stop("there is no tidal constituent named \"", name[i], "\"")
        frequency[i] <- tc$frequency[ic]
        compare[i] <- tc$compare[ic]
    }
    ##cat("A:\n")
    ##print(data.frame(name,frequency,compare))

    nc <- length(frequency)
                                        # Check Rayleigh criterion
	interval <- as.numeric(difftime(max(sl$data$t,na.rm=TRUE),min(sl$data$t,na.rm=TRUE),units="hours"))
    ##cat("interval:",interval,"\n")
	drop.term <- NULL
	for (i in 1:nc) {
		cc <- which(tc2$name == compare[i])
        if (length(cc)) {
            cannot.fit <- (interval * abs(frequency[i]-tc2$frequency[cc])) < rc
            ##cat("compare", name[i], "with", compare[i],":", cannot.fit,"\n")
            if (cannot.fit)	drop.term <- c(drop.term, i)
        }
	}
    ##cat("DROP:",drop.term,"\n")
	if (length(drop.term) > 0) {
		cat("Record is too short to fit for constituents:", name[drop.term],"\n")
		frequency <- frequency[-drop.term]
		name      <- name[-drop.term]
		compare   <- compare[-drop.term]
	}

    ##cat("FITTING TO\n")
    ##print(data.frame(name,frequency))

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
