# Source= http://sam.ucsd.edu/sio210/propseawater/ppsw_fortran/ppsw.f
# check value: cpsw = 3849.500 j/(kg deg. c) for s = 40 (ipss-78),
sw.specific.heat <- function(S, t=NULL, p=NULL)
{
	if (inherits(S, "ctd")) {
		t <- S$data$temperature
		p <- S$data$pressure
		S <- S$data$salinity # note: this destroys the ctd object
	}
	dim <- dim(S)
  	nS <- length(S)
  	nt <- length(t)
  	np <- length(p)
  	if (nS != nt)
    	stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
  	if (nS != np)
    	stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
  	for (i in 1:nS) {
    	this.CP <- .Fortran("ocecp", as.double(S[i]), 
				as.double(t[i]), as.double(p[i]),
                CP = double(1), PACKAGE = "oce")$CP
    	if (i == 1)
			rval <- this.CP
		else
			rval <- c(rval, this.CP)
  	}
	dim(rval) <- dim
	rval
}
