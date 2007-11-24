read.lobo <- function(file, cols=7) {
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
		S            <- as.numeric(d[seq(from=col.salinity,     by=cols, length.out = n)])
		T            <- as.numeric(d[seq(from=col.temperature,  by=cols, length.out = n)])
		p            <- rep(0, length(S))
		time <- as.POSIXlt(time)
		processing.log <- list(time=c(Sys.time()), 
			action=c(paste("created by read.lobo(\"",file,"\", cols=",cols,")",sep="")))
		res <- list(processing.log=processing.log,
			time=time, u=u, v=v, nitrate=nitrate, fluorescence=fluorescence, S=S, T=T, p=p, header=header, data=d)
		class(res) = "lobo"
		res
	} else {
		stop("debug: only working on one format right now")
	}
}