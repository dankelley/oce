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
		lon <- vector("numeric", num.stations)
		lat <- vector("numeric", num.stations)
		lat[1] <- extra.args[[1]]$latitude
		lon[1] <- extra.args[[1]]$longitude
		for (i in 2:num.stations) {
			lat[i] <- extra.args[[i-1]]$latitude
			lon[i] <- extra.args[[i-1]]$longitude
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
		lon <- vector("numeric", num.stations)
		lat <- vector("numeric", num.stations)
		for (i in 1:num.stations) {
			lat[i] <- args[[i]]$latitude
			lon[i] <- args[[i]]$longitude
			stations[[i]] <- args[[i]]
		}
		#cat("CASE 2\n")
	} else {
		stop("first argument must be of class \"ctd\" or a \"list\"")
	}
	#cat("num.stations = ", num.stations, "\n")
	action <- "created by make.section()"
	processing.log <- list(time=c(Sys.time()), action=action)
	res <- list(header="", section.id="",
		latitude=lat, longitude=lon, stations = stations,
		processing.log = processing.log)
  	class(res) <- "section"
	res
}
