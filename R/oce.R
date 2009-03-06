oce.as.POSIXlt <- function (x, tz = "")
{
    fromchar <- function(x) {
        xx <- x[1]
        if (is.na(xx)) {
            j <- 1
            while (is.na(xx) && (j <- j + 1) <= length(x)) xx <- x[j]
            if (is.na(xx))
                f <- "%Y-%m-%d"
        }
        if (is.na(xx) ||
                                        # additions ...
			((nchar(xx) == 8) && !is.na(strptime(xx, f <- "%Y%m%d"))) || # 20020823
 			!is.na(strptime(xx, f <- "%B %d %Y %H:%M:%OS")) || # Aug 23 2002 or August 23 2002
 			!is.na(strptime(xx, f <- "%Y %B %d %H:%M:%OS")) || # 2002 Aug 23
 			!is.na(strptime(xx, f <- "%d %B %Y %H:%M:%OS")) || # 23 Aug 2002
                                        # ... and now back to the standard
 			!is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%OS")) ||
            !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M:%OS")) ||
            !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M")) ||
 			!is.na(strptime(xx, f <- "%Y/%m/%d %H:%M")) ||
			!is.na(strptime(xx, f <- "%Y-%m-%d")) ||
            !is.na(strptime(xx, f <- "%Y/%m/%d"))) {
            res <- strptime(x, f)
            if (nchar(tz))
                attr(res, "tzone") <- tz
            return(res)
        }
        stop("The string \"", x, "\" is not in a known date format")
    }
    if (inherits(x, "POSIXlt"))
        return(x)
    if (inherits(x, "Date"))
        return(.Internal(Date2POSIXlt(x)))
    tzone <- attr(x, "tzone")
    if (inherits(x, "date") || inherits(x, "dates"))
        x <- as.POSIXct(x)
    if (is.character(x))
        return(fromchar(unclass(x)))
    if (is.factor(x))
        return(fromchar(as.character(x)))
    if (is.logical(x) && all(is.na(x)))
        x <- as.POSIXct.default(x)
    if (!inherits(x, "POSIXct")) stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"", deparse(substitute(x))))
    if (missing(tz) && !is.null(tzone))
        tz <- tzone[1]
    .Internal(as.POSIXlt(x, tz))
}
oce.edit <- function(x, item, value, action, reason="not specified", person="not specified")
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (!missing(item)) {
        if (missing(value)) stop("must supply a 'value' for this 'item'")
        if (!(item %in% names(x$metadata))) stop("no item named '", item, "' in object's  metadata")
        x$metadata[item] <- value
    } else if (!missing(action)) {
        eval(parse(text=action))        # FIXME: should check if it worked
    } else {
        stop("must supply either an 'item' plus a 'value', or an 'action'")
    }
    processing.log.append(x, paste(deparse(match.call()), sep="", collapse=""))
}
oce.write.table <- function (x, file="", ...)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (!("row.names" %in% names(list(...)))) write.table(x$data, file, row.names=FALSE, ...)
    else write.table(x$data, file, ...)
}
subset.oce <- function (x, subset, indices=NULL, ...)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (inherits(x, "section")) {
        if (!is.null(indices)) {        # select a portion of the stations
            n <- length(indices)
            station <- vector("list", n)
            stn <- vector("character", n)
            lon <- vector("numeric", n)
            lat <- vector("numeric", n)
            for (i in 1:n) {
                ii <- indices[i]
                stn[i] <- x$metadata$station.id[ii]
                lat[i] <- x$metadata$latitude[ii]
                lon[i] <- x$metadata$longitude[ii]
                station[[i]] <- x$data$station[[ii]]
            }
            data <- list(station=station)
            metadata <- list(header=x$header,section.id=x$section.id,station.id=stn,latitude=lat,longitude=lon)
            rval <- list(data=data, metadata=metadata, processing.log=x$processing.log)
            class(rval) <- c("section", "oce")
        } else {                        # subset within the stations
            rval <- x
            n <- length(x$data$station)
            r <- eval(substitute(subset), x$data$station[[1]]$data, parent.frame())
            for (i in 1:n) {
                rval$data$station[[i]]$data <- x$data$station[[i]]$data[r,]
            }
        }
        rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    } else {
        r <- eval(substitute(subset), x$data, parent.frame())
        r <- r & !is.na(r)
        rval <- x
        rval$data <- x$data[r,]
        rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    }
    rval
}
summary.oce <- function(object, ...)
{
    if (!inherits(object, "oce")) stop("method is only for oce objects")
    cat("Data summary:\n")
    print(summary(object$data))
    cat("\nMetadata:\n")
    print(object$metadata)
    processing.log.summary(object)
    return(invisible(object))
}
