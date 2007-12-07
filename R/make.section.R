make.section <- function(...)
{
	UseMethod("make.section")
}
make.section.ctd <- function(...)
{
	make.section.list(list(...))
}
make.section.list <- function(...)
{
	args <- ...
	demand.equal.pressures <- FALSE
	num.stations <- length(args)
	action <- c("created by make.section() with CTD stations originating from the following file names: ")
	# put together the log entry, and also scan for oddness
	for (i in 1:num.stations) {
		#print(deparse(args[[i]]))
		if (i == num.stations)
			action <- paste(action, " and ", sep="")
		action <- paste(action, "'", args[[i]]$filename, "'", sep="")
		if (i < num.stations - 1)
			action <- paste(action, ",", sep="")
		if (demand.equal.pressures && i > 1) {
			if (length(args[[i]]$data$pressure) != length(args[[1]]$data$pressure))
				stop("stations 1 and ", i, " have a different number of depths")
			if (any(args[[i]]$data$pressure != args[[1]]$data$pressure))
				stop("stations 1 and ", i, " have unequal depth levels")
		}
	}
	action <- paste(action, ".", sep="")
	processing.log <- list(time=c(Sys.time()), action=action)
	n<-0
	stations <- vector("list", n)
	for (i in 1:num.stations) {
		stations[[i]] <- args[[i]] # BUG: why not just copy?
	}
	# NOTE: should add something in the header??
	res <- list(processing.log=processing.log, stations=stations)
  	class(res) <- "section"
	res
}
