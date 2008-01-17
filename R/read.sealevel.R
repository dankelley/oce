read.sealevel <- function(file, debug=FALSE)
{
                                        # Read sea-level data in format described at ftp://ilikai.soest.hawaii.edu/rqds/hourly.fmt
	filename <- file
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
	first.line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
	header <- first.line
	pushBack(first.line, file)
	station.number <- NA
	station.version <- NA
	station.name <- NULL
	region <- NULL
	year <- NA
	latitude <- NA
	longitude <- NA
	GMT.offset <- NA
	decimation.method <- NA
	reference.offset <- NA
	reference.code <- NA
	if (substr(first.line, 1, 12) == "Station_Name") { # type 2
		if(debug) cat("File is of format 1 (e.g. as in MEDS archives)\n")
                                        # Station_Name,HALIFAX
                                        # Station_Number,490
                                        # Latitude_Decimal_Degrees,44.666667
                                        # Longitude_Decimal_Degrees,63.583333
                                        # Datum,CD
                                        # Time_Zone,AST
                                        # SLEV=Observed Water Level
                                        # Obs_date,SLEV
                                        # 01/01/2001 12:00 AM,1.82,
		header.length <- 8
		header <- scan(file, character(), n=header.length, quiet=TRUE)
		station.name   <- strsplit(header[1], ",")[[1]][2]
		station.number <- as.numeric(strsplit(header[2], ",")[[1]][2])
		latitude       <- as.numeric(strsplit(header[3], ",")[[1]][2])
		longitude      <- as.numeric(strsplit(header[4], ",")[[1]][2])
                                        # get GMT offset
		tz             <- strsplit(header[6], ",")[[1]][2]
		GMT.offset     <- GMT.offset.from.tz(tz)
		x <- read.csv(file, skip=header.length, header=FALSE)
		eta <- as.numeric(x$V2)
		t <- as.POSIXct(strptime(as.character(x$V1), "%d/%m/%Y %I:%M %p"))
	} else { # type 1
		if(debug) cat("File is of type 2 (e.g. as in the Hawaii archives)\n")
		d <- readLines(file)
		n <- length(d)
		header <- d[1]
		station.number    <- substr(header,  1,  3)
		station.version   <- substr(header,  4,  4)
		station.name      <- substr(header,  6, 23)
		station.name      <- sub("[ ]*$","",station.name)
		region            <- substr(header, 25, 43)
		region            <- sub("[ ]*$","",region)
		year              <- substr(header, 45, 48)
		latitude.str      <- substr(header, 50, 55) #degrees,minutes,tenths,hemisphere
		latitude <- as.numeric(substr(latitude.str,   1, 2)) + (as.numeric(substr(latitude.str,  3, 5)))/600
		if (tolower(substr(latitude.str,  6, 6)) == "s") latitude <- -latitude
		longitude.str     <- substr(header, 57, 63) #degrees,minutes,tenths,hemisphere
		longitude <- as.numeric(substr(longitude.str, 1, 3)) + (as.numeric(substr(longitude.str, 4, 6)))/600
		if (tolower(substr(longitude.str, 7, 7)) == "w") longitude <- -longitude
		GMT.offset        <- substr(header, 65, 68) #hours,tenths (East is +ve)
		if (debug) cat("GMT.offset:", GMT.offset, "\n")
		decimation.method <- substr(header, 70, 70) #1=filtered 2=average 3=spot readings 4=other
		reference.offset  <- substr(header, 72, 76) # add to values
		reference.code    <- substr(header, 77, 77) # add to values
		units             <- substr(header, 79, 80)
		if (tolower(units) != "mm") stop("require units to be 'mm' or 'MM', not '", units, "'")
		eta <- array(NA, 12*(n-1))
		first.twelve.hours  <- 3600 * (0:11)
		second.twelve.hours <- 3600 * (12:23)
		twelve <- seq(1, 12, 1)
		for (i in 2:n) {
			sp <- strsplit(d[i],"[ ]+")[[1]]
			target.index <- 12 * (i-2) + twelve
			eta[target.index] <- as.numeric(sp[4:15])
			day.portion <- as.numeric(substr(sp[3], 9, 9))
			if (i == 2) {
				start.day <- as.POSIXct(strptime(paste(substr(sp[3],1,8),"00:00:00"), "%Y%m%d"))
			} else {
				if (day.portion == 1) {
					if (last.day.portion != 2)
						stop("non-alternating day portions on data line ", i)
				} else if (day.portion == 2) {
					if (last.day.portion != 1)
						stop("non-alternating day portions on data line ", i)
				} else {
					stop("day portion is ", day.portion, " but must be 1 or 2, on data line", i)
				}
			}
			last.day.portion <- day.portion
		}
		t <- as.POSIXct(start.day + 3600 * (seq(0, 12*(n-1)-1)))
		eta[eta==9999] <- NA
		if (tolower(units) == "mm") {
			eta <- eta / 1000
		} else {
			stop("require units to be MM")
		}
	}
	num.missing <- sum(is.na(eta))
	if (debug) {
		cat("t summary:");print(summary(t))
		cat("eta summary:");print(summary(eta))
	}
	if (num.missing > 0) warning("there are ", num.missing, " missing points in this timeseries, at indices ", paste(which(is.na(eta)), ""))
	data <- data.frame(t=t, eta=eta)
	metadata <- list(
                     header=header,
                     year=year,
                     station.number=station.number,
                     station.version=station.version,
                     station.name=station.name,
                     region=region,
                     latitude=latitude,
                     longitude=longitude,
                     GMT.offset=GMT.offset,
                     decimation.method=decimation.method,
                     reference.offset=reference.offset,
                     reference.code=reference.code,
                     units=NA,
                     n=length(t),
                     sampling.interval=as.numeric(difftime(t[2], t[1], units="hours")))
	log.item <- list(time=c(Sys.time()), action=c(paste("created by read.sealevel(\"",filename,"\")",sep="")))
	rval <- list(data=data, metadata=metadata, processing.log=log.item)
	class(rval) <- c("sealevel", "oce")
	rval
}
