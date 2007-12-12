read.section <- function(file, section.id, debug=FALSE)
{
	if (is.character(file)) {
		filename <- file
		file <- file(file, "r")
		on.exit(close(file))
	}
	if (!inherits(file, "connection")) {
		stop("argument `file' must be a character string or connection")
	}
	if (!isOpen(file)) {
		filename <- "(connection)"
		open(file, "r")
		on.exit(close(file))
	}
	# Skip header
	lines <- readLines(file)
	if ("BOTTLE" != substr(lines[1], 1, 6))
		stop("only type \"BOTTLE\" understood, but got header line\n", lines[1],"\n")
	lines
	n <- length(lines)
	header <- lines[1]
	for (l in (2:n)) {
		if (debug) cat(lines[l],"\n")
		if ("#" != substr(lines[l], 1, 1)) {
			header <- c(header, lines[l])
			break
		}
	}
	header.length <- l + 1
	ccc <- textConnection(lines[header.length - 1])
	var.names <- scan(ccc, sep=",", what="", quiet=TRUE)
	close(ccc)
	ccc <- textConnection(lines[header.length])
	var.units <- scan(ccc, sep=",", what="", quiet=TRUE)
	close(ccc)
	if (length(var.units) != length(var.names)) stop("length mismatch in variable lists")
	header <- lines[1:header.length]
#	cat(length(var.names), "\n")
#	cat(var.names, "\n")
#	cat(length(var.units), "\n")
#	cat(var.units, "\n")
	nd <- n - header.length - 1
	nv <- length(var.names)
	data <- array(dim=c(nd, nv - 2))
	stn.id <- vector("character", nd)
	section.id <- ""
	for (l in ((header.length + 1):(n-1))) { # last line is END_DATA
#		cat(length(strsplit(lines[l], split=",")[[1]]), "\n")
#		cat(lines[l], "\n")
		contents <- strsplit(lines[l], split=",")[[1]]
		if (missing(section.id) && l == (header.length + 1))
			section.id <- sub(" *","", contents[2])
		stn.id[l - header.length] <- sub(" *","", contents[3])
		data[l - header.length,] <- contents[3:nv]
		# FIXME: maybe should just scan this thing; it might work better anyway
	}
	p <- as.numeric(data[,which(var.names=="CTDPRS") - 2])
	T <- as.numeric(data[,which(var.names=="CTDTMP") - 2])
	S <- as.numeric(data[,which(var.names=="CTDSAL") - 2])
	latitude  <- as.numeric(data[,which(var.names=="LATITUDE") - 2])
	longitude <- as.numeric(data[,which(var.names=="LONGITUDE") - 2])
	station <- data[,which(var.names=="STNNBR") - 2]
	station.list <- unique(station)
	num.stations <- length(station.list)
	stations <- vector("list", num.stations)
    for (i in 1:num.stations) {
		select <- which(station == station.list[i])
		this.station <- as.ctd(S=S[select], T=T[select], p=p[select],
			latitude=latitude[select[1]], 
			longitude=longitude[select[1]],
			station=stn.id[select[1]],
			filename=filename)
		if (debug) cat("station at ", latitude[select[1]], "N and ", longitude[select[1]], "W\n")
        stations[[i]] <- this.station
	}
#	rval <- list(names=var.names, units=var.units, expcode=expcode, sect.id=sect.id, data=data,
#		p=p, t=t, S=S, latitude=latitude,longitude=longitude, stations=stations)
   	action <- paste("created by read.section(file=\"", filename, "\", debug=",debug, ")",sep="")
    processing.log <- list(time = c(Sys.time()), action = action)
    res <- list(processing.log = processing.log, section.id=section.id, stations = stations)
    class(res) <- "section"
	res
}
