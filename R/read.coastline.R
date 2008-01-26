read.coastline <- function(file,type=c("R","S","mapgen"),debug=FALSE)
{
    type <- match.arg(type)
    log.item <- list(time=c(Sys.time()),
                     action=c(paste("created by read.coastline(\"",file,"\", type=",type,")",sep="")))
    if (type == "R" || type == "S") {
                                        #
                                        # e.g. data from http://rimmer.ngdc.noaa.gov/coast/
                                        # e.g. "~/data/Coastline/wcl_1_5000000.dat")
        if (is.character(file)) {
            file <- file(file, "r")
            on.exit(close(file))
        }
        if (!inherits(file, "connection")) stop("'file' must be a character string or connection")
        if (!isOpen(file)) {
            open(file, "r")
            on.exit(close(file))
        }
        data <- read.table(file, header=FALSE, col.names=c("longitude","latitude"))
        res <- list(data=data, metadata=NULL, processing.log=log.item)
    } else if (type == "mapgen") {
        header <- scan(file, what=character(0), nlines=1, quiet=TRUE);
        if (debug) {
            cat("method is mapgen\n")
            cat("header ")
            cat(header)
        }
        separator <- NULL
                                        # mapgen    # -b
                                        # matlab	nan nan
                                        # Splus     NA NA
                                        # mapgen...
                                        #	1
                                        #	...
                                        #	END
                                        #	2
                                        #   ...
                                        #   END
        if (all.equal(header, c("#","-b"))) {
            lonlat <- scan(file,what=double(0),na.strings=c("#","-b"), quiet=TRUE)
        } else {
            if (all.equal(header, c("nan","nan"))) {
                lonlat <- scan(file,what=double(0),na.strings=c("nan","nan"), quiet=TRUE)
            } else {
                if (all.equal(header, c("NA","NA"))) {
                    lonlat <- scan(file,what=double(0), quiet=TRUE)
                } else {
                    stop(cat("Unknown file type; the unrecognized header line is '",header,"'\n",sep=" "))
                }
            }
        }
        lonlat <- matrix(lonlat, ncol=2,byrow=TRUE)
        data <- data.frame(longitude=lonlat[,1], latitude=lonlat[,2])
        res <- list(data=data, metadata=NULL, processing.log=log.item)
    } else {
        stop("unknown method.  Should be \"R\", \"S\", or \"mapgen\"")
    }
    class(res) <- c("coastline", "oce")
    return(res)
}
