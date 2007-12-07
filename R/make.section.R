make.section <- function(item, ...)
{
	if (class(item) == "ctd") {
		extra.args <- list(...)
		if (length(extra.args) < 1) {
			stop("cannot make a section from one station")
		}
		num.stations <- 1 + length(extra.args)
		stations <- vector("list", num.stations)
		stations[[1]] <- item
		for (i in 2:num.stations) {
			stations[[i]] <- extra.args[[i-1]]
			summary(extra.args[i-1])
			#cat("dood i=",i,"\n")
		}
		#cat("CASE 1\n")
	} else if (class(item) == "list") {
		args <- list(...)
		if (length(args) < 2) {
			stop("cannot make a section from one station")
		}
		num.stations <- length(args)
		stations <- vector("list", num.stations)
		for (i in 1:num.stations) {
			stations[[i]] <- args[[i]]
		}
		#cat("CASE 2\n")
	} else {
		stop("first argument must be of class \"ctd\" or a \"list\"")
	}
	#cat("num.stations = ", num.stations, "\n")
	action <- "created by make.section()"
	processing.log <- list(time=c(Sys.time()), action=action)
	res <- list(processing.log=processing.log, stations=stations)
  	class(res) <- "section"
	res
}
