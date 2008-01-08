magic <- function(file)
{
	if (is.character(file)) {
		file <- file(file, "r")
		on.exit(close(file))
  	}
  	if (!inherits(file, "connection")) {
    	stop("argument `file' must be a character string or connection")
	}
	if (!isOpen(file)) {
    	open(file, "r")
    	on.exit(close(file))
  	}
	line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
	pushBack(line, file)
	if (substr(line, 1, 3) == "CTD")
	 	return("ctd.woce")
	if ("* Sea-Bird" == substr(line, 1, 10))
	 		return("ctd.seabird")
	if ("# -b" == substr(line, 1, 4))
		return("coastline")
    if ("# Station_Name," == substr(line, 1, 15))
		return("sealevel")
	#275A Halifax            Canada              1920 44400N 063350W 0000 3 00000R MM
	if (0 < regexpr("^[0-9][0-9][0-9][A-Z] ", line))
		return("sealevel")
	return("unknown")
}

read.oce <- function(file, ...)
{
	# if (is.character(file)) {
	#    	file <- file(file, "r")
	#     	on.exit(close(file))
	#   	}
	#   	if (!inherits(file, "connection")) {
	#     	stop("argument `file' must be a character string or connection")
	# }
	# if (!isOpen(file)) {
	#     	open(file, "r")
	#     	on.exit(close(file))
	#   	}
	type <- magic(file)
	if (type == "ctd.woce")
		return(read.ctd(file, ...))
	if (type == "ctd.seabird")
		return(read.ctd(file, ...))
	if (type == "coastline")
		return(read.coastline(file, type="mapgen", ...))
	if (type == "sealevel")
		return(read.sealevel(file, ...))
	stop("unknown file type")
}
