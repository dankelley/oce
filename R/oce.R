## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' oce: A Package for Oceanographic Analysis
#'
#' The oce package provides functions for working with
#' Oceanographic data, for calculations that are specific
#' to Oceanography, and for producing graphics that
#' match the conventions of the field.
#'
#' @section Specialized functions:
#' A key function is \code{\link{read.oce}}, which will attempt
#' to read Oceanographic data in raw format. This uses
#' \code{\link{oceMagic}} to try to detect the file type,
#' based on the file name and contents. If it proves impossible
#' to detect the type, users should next try a more specialized
#' function, e.g. \code{\link{read.ctd}} for CTD files, or
#' \code{\link{read.ctd.sbe}} for Teledyne-Seabird files.
#'
#' @section Generic methods:
#' A list of the generic methods in oce is provided by
#' `methods(class="oce")`; a few that are used frequently
#' are as follows.
#' \describe{
#' \item{[[}{Find the value of an item in the object's
#'     \code{metadata} or \code{data} slot. If the item does
#'     not exist, but can be calculated from the other items,
#'     then the calculated value is returned. As an example of the
#'     latter, consider the built-in \code{ctd} dataset, which does
#'     not contain potential temperature, "\code{theta}". Using
#'     \code{ctd[["theta"]]} therefore causes \code{\link{swTheta}}
#'     to be called, to calculate \code{theta}.
#'     See \link{[[,oce-method} or type \code{?"[[,oce-method"}
#'     to learn more.}
#' \item{[[<-}{Alters the named item in the object's \code{metadata} or
#'     \code{data} slot.  If the item does not exist, it is created.
#'     See \link{[[<-,oce-method} or type \code{?"[[<-,oce-method"}
#'     to learn more.}
#' \item{summary}{Displays some information about the object named as an
#'     argument, including a few elements from its \code{metadata} slot
#'     and some statistics of the contents of its \code{data} slot.
#'     See \link{summary,oce-method} or type \code{?"summary,oce-method"}
#'     to learn more.}
#' \item{subset}{Takes a subset of an oce object.
#'     See \link{subset,oce-method} or type \code{?"subset,oce-method"}
#'     to learn more.}
#' }
#'
#'
#' @section Oceanographic data types handled:
#' Over a dozen specialized data types are handled by oce,
#' with generic plots and summaries for each, along with
#' the specialized functions needed for typical Oceanographic
#' analysis.
#'
#' @section Oce object structure:
#' See \code{\link{oce-class}} for a summary of the class structure
#' and links to documentation for the many subclasses of
#' oce objects, each aligned with a class of instrument or
#' or type of dataset.
#'
#' @docType package
#' @name oce
NULL


#' Deprecated and Defunct Elements of package \sQuote{oce}
#'
#' Certain functions and function arguments are still provided for
#' compatibility with older versions of \sQuote{oce}, but will be removed soon.
#' The \sQuote{oce} scheme for removing functions is similar to that used by
#' \sQuote{Bioconductor}: items are marked as "deprecated" in one release, marked as
#' "defunct" in the next, and removed in the next after that. This goal is to provide a
#' gentle migration path for users who keep their packages reasonably
#' up-to-date.
#'
#' Several \sQuote{oce} functions are marked "deprecated" in the present
#' release of oce. Please use the replacement functions as listed below.
#' The next CRAN release of \sQuote{oce} will designate these functions as
#' "defunct".
#'
#' \tabular{lll}{
#' \strong{Deprecated}       \tab \strong{Replacement}            \tab \strong{Notes}\cr
#' \code{findInOrdered(x,f)} \tab \code{\link{findInterval}(f,x)} \tab Deprecated 2017-09-07\cr
#' \code{mapZones}           \tab \code{\link{mapGrid}}           \tab Deprecated 2016-02-13\cr
#' \code{mapMeridians}       \tab \code{\link{mapGrid}}           \tab Deprecated 2016-02-13\cr
#' \code{addColumn}          \tab \code{\link{oceSetData}}        \tab Deprecated 2016-08-01\cr
#' \code{oce.magic}          \tab \code{\link{oceMagic}}          \tab Deprecated 2016-09-01\cr
#' \code{ctdAddColumn}       \tab \code{\link{oceSetData}}        \tab Deprecated 2016-11-11\cr
#' \code{ctdUpdateHeader}    \tab -                               \tab Deprecated 2016-11-11\cr
#' \code{oce.as.POSIXlt}     \tab \code{\link[lubridate]{parse_date_time}} \tab Deprecated 2016-12-17\cr
#' }
#'
#' The following are marked "defunct", so calling them in the
#' the present version produces an error message that hints at a replacement
#' function. Once a function is marked "defunct" on one CRAN release, it will
#' be slated for outright deletion in a subsequent release.
#'
#'\tabular{lll}{
#'\strong{Defunct}   \tab \strong{Replacement}     \tab \strong{Notes}\cr
#'\code{makeSection} \tab \code{\link{as.section}} \tab Improve utility and name sensibility\cr
#'\code{columns}     \tab \code{\link{read.ctd}}   \tab Unnecessary; never worked\cr
#'}
#'
#' The following were removed in recent years.
#'
#'\tabular{lll}{
#'\strong{Function}  \tab \strong{Replacement}     \tab \strong{Notes}\cr
#' FILL IN           \tab FILL IN                  \tab FILL IN
#'}
#'
#' Several \sQuote{oce} function arguments are considered "deprecated", which
#' means they will be marked "defunct" in the next CRAN release. These are normally
#' listed in the help page for the function in question. A few that may be
#' of general interest are also listed below.
#'
#' \itemize{
#'
#' \item The \code{endian} argument of \code{\link{byteToBinary}} will be removed sometime
#' in the year 2017, and should be set to \code{"big"} in the meantime.
#'
#' \item The \code{parameters} argument of \code{\link{plot,ctd-method}}
#' was deprecated on 2016-12-30.  It was once used by
#' \code{\link{plot,coastline-method}} but has been ignored by that
#' function since February 2016.
#'
#' \item The \code{orientation} argument of \code{\link{plot,ctd-method}}
#' was deprecated on 2016-12-30.  It was once used by
#' \code{\link{plot,coastline-method}} but has been ignored by that
#' function since February 2016.
#'
#' }
#'
#' Several \sQuote{oce} function arguments are considered "defunct", which
#' means they will be removed in the next CRAN release. They are as follows.
#'
#' \itemize{
#'
#' \item The \code{date} argument of \code{\link{as.ctd}}
#' was discovered to have been unused in early 2016. Since
#' the \code{startTime} actually fills its role, \code{date}
#' was considered to be deprecated in June 2016.
#'
#' \item The \code{quality} flag of \code{\link{as.ctd}} was
#' marked as deprecated in March 2016.
#'
#' \item The \code{fill} argument of \code{\link{mapPlot}} was confusing
#' to users, so it was designated as deprecated in June 2016.
#' (The confusion stemmed from subtle differences between
#' \code{\link{plot}} and \code{\link{polygon}}, and the problem is that
#' \code{\link{mapPlot}} can use either of these functions, according
#' to whether coastlines are to be filled.)
#' The functionality is preserved, in the \code{col} argument.
#'
#' }
#'
#' @aliases oce-defunct
#' @name oce-deprecated
#'
#' @seealso The \sQuote{Bioconductor} scheme for removing functions is
#' described at
#' \url{https://www.bioconductor.org/developers/how-to/deprecation/} and it is
#' extended here to function arguments.
NULL

#' Coerce Something Into an Oce Object
#'
#' @details
#' This function is limited and not intended for common use.
#' In most circumstances, users should employ a function such
#' as \code{\link{as.ctd}} to construct specialized oce sub-classes.
#'
#' \code{as.oce} creates an oce object from data contained within its
#' first argument, which may be a list, a data frame, or an object
#' of \code{\link{oce-class}}.  (In the last case, \code{x} is
#' simply returned, without modification.)
#'
#' If \code{x} is a list containing items named \code{longitude} and
#' \code{latitude}, then \code{\link{as.coastline}} is called (with
#' the specified \dots value) to create a coastline object.
#'
#' If \code{x} is a list created by \code{read_odf} from the (as
#' yet unreleased) ODF package developed by the Bedford Institute of
#' Oceanography, then \code{\link{ODF2oce}} is called (with
#' no arguments other than the first) to calculate a return value.
#' If the sub-class inference made by \code{\link{ODF2oce}} is
#' incorrect, users should call that function directly, specifying
#' a value for its \code{coerce} argument.
#'
#' If \code{x} has not been created by \code{read_odf}, then the names
#' of the items it contains are examined, and used to try to infer
#' the proper return value.  There
#' are only a few cases (although more may be added if there is
#' sufficient user demand). The cases are as follows.
#' \itemize{
#'
#' \item If \code{x} contains items named \code{temperature},
#' \code{pressure} and either \code{salinity} or \code{conductivity},
#' then an object of type \code{\link{ctd-class}} will be returned.
#'
#' \item If \code{x} contains columns named \code{longitude} and \code{latitude},
#' but no other columns, then an object of class \code{\link{coastline-class}}
#' is returned.
#' }
#'
#' @param x an item containing data. This may be data frame, list, or an oce object.
#' @param \dots optional extra arguments, passed to conversion functions
#' \code{\link{as.coastline}} or \code{\link{ODF2oce}}, if these are used.
#'
#' @return \code{as.oce} returns an object inheriting from \code{\link{oce-class}}.
as.oce <- function(x, ...)
{
    if (inherits(x, "oce")) {
        names <- names(x)
        return(if ("EVENT_HEADER" %in% names) ODF2oce(x) else x)
    }
    if (!is.list(x) && !is.data.frame(x))
        stop("x must be a list, a data frame, or an oce object")
    names <- names(x)
    if ("temperature" %in% names && "pressure" %in% names && "salinity" %in% names) {
        ## Assume it's a CTD
        res <- as.ctd(salinity=x$salinity, temperature=x$temperature, pressure=x$pressure)
        ## Add other columns
        for (name in names) {
            if (name != "temperature" && name != "pressure" && name != "salinity") {
                res <- oceSetData(res, name=name, value=x[[name]])
            }
        }
        return(res)
    }
    if ("longitude" %in% names && "latitude" %in% names && length(names) == 2) {
        ## Assume it's a coastline
        return(as.coastline(longitude=x$longitude, latitude=x$latitude))
    }
    ## Not sure what it is, so make a generic oce object. We
    ## have no way to guess the unit.
    res <- new("oce")
    for (name in names)
        res <- oceSetData(res, name=name, value=x[[name]])
    res
}


#' Replace the Heading for One Instrument With That of Another
#'
#' Replace the heading angles in one oce object with that from another,
#' possibly with a constant adjustment.
#'
#' @param b object holding data from an instrument whose heading is bad, but
#' whose other data are good.
#' @param g object holding data from an instrument whose heading is good, and
#' should be interpolated to the time base of \code{b}.
#' @param add an angle, in degrees, to be added to the heading.
#' @return A copy of \code{b}, but with \code{b$data$heading} replaced with
#' heading angles that result from linear interpolation of the headings in
#' \code{g}, and then adding the angle \code{add}.
#' @author Dan Kelley
useHeading <- function(b, g, add=0)
{
    if (!"heading" %in% names(b@data))
        stop("'b' does not have any heading data (in b@data$heading)")
    if (!"time" %in% names(b@data))
        stop("'b' does not have any time data (in b@data$time)")
    if (!"heading" %in% names(g@data))
        stop("'g' does not have any heading data (in g@data$heading)")
    if (!"time" %in% names(g@data))
        stop("'g' does not have any time data (in g@data$time)")
    res <- b
    t0 <- as.numeric(g@data$time[1])
    if (is.na(t0))
        stop("need first element of g@data$time to be non-NA")
    b.t <- as.numeric(b@data$time) - t0 # FIXME: what if heading in tsSlow?
    g.t <- as.numeric(g@data$time) - t0 # FIXME: what if heading in tsSlow?
    res@data$heading <- approx(x=g.t, y=g@data$heading, xout=b.t)$y + add
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}



#' Window an Oce Object by Time or Distance
#'
#' Windows \code{x} on either time or distance, depending on the value of
#' \code{which}.  In each case, values of \code{start} and \code{end} may be
#' integers, to indicate a portion of the time or distance range.  If
#' \code{which} is \code{"time"}, then the \code{start} and \code{end} values
#' may also be provided as POSIX times, or character strings indicating times
#' (in time zone given by the value of \code{getOption("oceTz")}).
#' Note that \code{\link{subset}} may be more useful than this function.
#'
#' @param x an \code{oce} object.
#' @param start the start time (or distance) of the time (or space) region of
#' interest.  This may be a single value or a vector.
#' @param end the end time (or distance) of the time (or space) region of
#' interest.  This may be a single value or a vector.
#' @param frequency not permitted yet.
#' @param deltat not permitted yet
#' @param extend not permitted yet
#' @param which string containing the name of the quantity on which sampling is
#' done.  Possibilities are \code{"time"}, which applies the windowing on the
#' \code{time} entry of the \code{data} slot, and \code{"distance"}, for the
#' \code{distance} entry (for those objects, such as \code{adp}, that have this
#' entry).
#' @param indexReturn boolean flag indicating whether to return a list of the
#' "kept" indices for the \code{time} entry of the \code{data} slot, as well as
#' the \code{timeSlow} entry, if there is one..  Either of these lists will be
#' \code{NULL}, if the object lacks the relevant items.
#' @param debug a flag that turns on debugging.
#' @param \dots ignored
#' @return Normally, this is new \code{oce} object.  However, if
#' \code{indexReturn=TRUE}, the return value is two-element list containing
#' items named \code{index} and \code{indexSlow}, which are the indices for the
#' \code{time} entry of the \code{data} slot (and the \code{timeSlow}, if it
#' exists).
#' @author Dan Kelley
#' @seealso \code{\link{subset}} provides more flexible selection of subsets.
#' @examples
#' library(oce)
#' data(adp)
#' plot(adp)
#' early <- window(adp, start="2008-06-26 00:00:00", end="2008-06-26 12:00:00")
#' plot(early)
#' bottom <- window(adp, start=0, end=20, which="distance")
#' plot(bottom)
window.oce <- function(x, start=NULL, end=NULL, frequency=NULL, deltat=NULL, extend=FALSE,
                       which=c("time", "distance"), indexReturn=FALSE,
                       debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "window.oce(...,start=",
             paste(format(start), collapse=","), ",end=",
             paste(format(end), collapse=","),
             ",indexReturn=", indexReturn, ",...) {\n", unindent=1)
    if (extend)
        stop("cannot handle extend=TRUE yet")
    if (!is.null(frequency))
        stop("cannot handle frequency yet")
    if (!is.null(deltat))
        stop("cannot handle deltat yet")
    if (is.null(start))
        stop("must provide start")
    if (is.null(end))
        stop("must provide end")
    oceDebug(debug, "class of (x) is: ", paste(class(x), collapse=","), "\n")
    res <- x
    which <- match.arg(which)
    nstart <- length(start)
    if (nstart != length(end))
        stop("lengths of 'start' and 'end' must match")
    if (which == "time") {
        oceDebug(debug, "windowing by time\n")
        if (!("time" %in% names(x@data))) {
            warning("oce object has no @data$time vector, so window is returning it unaltered")
            return(x)
        }
        if (is.character(start))
            start <- as.POSIXct(start, tz=getOption("oceTz"))
        if (is.character(end))
            end <- as.POSIXct(end, tz=getOption("oceTz"))
        oceDebug(debug, "tz of start:", attr(start, "tzone"), "\n")
        oceDebug(debug, "tz of end:", attr(end, "tzone"), "\n")
        oceDebug(debug, "tz of data$time:", attr(res@data$time, "tzone"), "\n")
        nstart <- length(start)
        ntime <- length(x@data$time)
        keep <- rep(FALSE, ntime)
        haveSlow <- "timeSlow" %in% names(x@data)
        keepSlow <- if (haveSlow) rep(FALSE, length(x@data$timeSlow)) else NULL
        for (w in 1:nstart) {
            keep <- keep | (start[w] <= res@data$time & res@data$time <= end[w])
            if (haveSlow)
                keepSlow <- keepSlow | (start[w] <= res@data$timeSlow & res@data$timeSlow <= end[w])
            oceDebug(debug, "data window (start=", format(start[w]), ", end=", format(end[w]), ") retains", sum(keep)/ntime*100, "percent\n")
        }
        if (indexReturn) {
            res <- list(index=keep, indexSlow=keepSlow)
            return(res)
        } else {
            for (name in names(res@data)) {
                if ("distance" == name)
                    next
                if (length(grep("^time", name)) || is.vector(res@data[[name]])) {
                    if (1 == length(agrep("Slow$", name))) {
                        oceDebug(debug, "windowing 'slow' variable data$", name, "\n", sep="")
                        res@data[[name]] <- x@data[[name]][keepSlow]
                    } else {
                        oceDebug(debug, "windowing data@", name, "\n", sep="")
                        res@data[[name]] <- x@data[[name]][keep]
                    }
                } else if (is.matrix(res@data[[name]])) {
                    oceDebug(debug, "windowing data@", name, ", which is a matrix\n", sep="")
                    res@data[[name]] <- x@data[[name]][keep, ]
                } else if (is.array(res@data[[name]])) {
                    oceDebug(debug, "windowing data@", name, ", which is an array\n", sep="")
                    res@data[[name]] <- x@data[[name]][keep, , , drop=FALSE]
                }
            }
        }
    } else if (which == "distance") {
        if (!inherits(x, "adp")) {
            warning("window(..., which=\"distance\") only works for objects of class adp")
            return(x)
        }
        if (!("distance" %in% names(x@data))) {
            warning("oce object has no @data$s$distance vector, so window is returning it unaltered")
            return(x)
        }
        oceDebug(debug, "windowing an ADP object by distance\n")
        ## FIXME: make it work on sections, on CTD, etc.
        keep <- start <= res@data$distance & res@data$distance < end
        res@data$distance <- x@data$distanc[keep]
        for (name in names(res@data)) {
            if (is.array(res@data[[name]]) && 3 == length(dim(x@data[[name]]))){
                res@data[[name]] <- res@data[[name]][, keep, ]
            }
        }
    } else {
        stop("unknown value of which \"", which, "\"") # cannot get here
    }
    ## sync up some metadata that might have been altered
    if (inherits(x, "adp")) {
        res@metadata$numberOfSamples <- dim(res@data$v)[1]
        res@metadata$numberOfCells <- dim(res@data$v)[2]
    } else if (inherits(x, "adv")) {
        res@metadata$numberOfSamples <- dim(res@data$v)[1]
    }
    oceDebug(debug, "} # window.oce()\n", unindent=1)
    res
}


#' Extract The Start of an Oce Object
#'
#' @templateVar headOrTail head
#' @template head_or_tail
#'
#' @param x An \code{oce} object.
#' @param n Number of elements to extract.
#' @param ... ignored
#' @seealso \code{\link{tail.oce}}, which yields the end of an \code{oce} object.
#' @author Dan Kelley
head.oce <- function(x, n=6L, ...)
{
    res <- x
    if (inherits(x, "adp") || inherits(x, "adv")) {
        numberOfProfiles <- dim(x@data$v)[1]
        look <- if (n < 0) seq.int(max(1, (1 + numberOfProfiles + n)), numberOfProfiles)
            else seq.int(1, min(n, numberOfProfiles))
        for (name in names(x@data)) {
            if ("distance" == name)
                next
            if (is.vector(x@data[[name]])) {
                res@data[[name]] <- x@data[[name]][look]
            } else if (is.matrix(x@data[[name]])) {
                res@data[[name]] <- x@data[[name]][look, ]
            } else if (is.array(x@data[[name]])) {
                res@data[[name]] <- x@data[[name]][look, , ]
            } else {
                res@data[[name]] <- x@data[[name]][look] # for reasons unknown, 'time' is not a vector
            }
        }
        res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        res
    } else if (inherits(x, "ctd")) {
        for (name in names(x@data)) {
            res@data[[name]] <- head(x@data[[name]], n)
        }
    } else if (inherits(x, "section")) {
        res@metadata$stationId <- head(x@metadata$stationId, n)
        res@metadata$longitude <- head(x@metadata$longitude, n)
        res@metadata$latitude <- head(x@metadata$latitude, n)
        res@metadata$time <- head(x@metadata$time, n)
        res@data$station <- head(x@data$station, n)
    } else if (inherits(x, "topo")) {
        ## using head() to determine the indices, because then the 
        ## z matrix indexing will work for both +ve and -ve n.
        ilon <- head(seq_along(x@data$longitude), n)
        ilat <- head(seq_along(x@data$latitude), n)
        res@data$longitude <- x@data$longitude[ilon]
        res@data$latitude <- x@data$latitude[ilat]
        res@data$z <- x@data$z[ilon, ilat]
    } else if (inherits(x, "landsat")) {
        warning("head.oce() cannot handle landsat, so returning it unaltered\n")
    } else if (inherits(x, "amsr")) {
        warning("head.oce() cannot handle amsr, so returning it unaltered\n")
    } else {
        ## FIXME: probably this will fail on many classes.
        for (name in names(x@data)) {
            if (is.vector(x@data[[name]]) && !is.list(x@data[[name]])) {
                res@data[[name]] <- tail(x@data[[name]], n)
            } else {
                warning("ignoring '", name, "' because it is not a vector\n")
            }
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Extract the End of an Oce Object
#'
#' @templateVar headOrTail tail
#' @template head_or_tail
#'
#' @inheritParams head.oce
#' @seealso \code{\link{head.oce}}, which yields the start of an \code{oce} object.
#' @author Dan Kelley
tail.oce <- function(x, n=6L, ...)
{
    res <- x
    if (inherits(x, "adp")) {
        numberOfProfiles <- dim(x@data$v)[1]
        look <- if (n < 0) seq.int(1, min(numberOfProfiles, numberOfProfiles + n))
            else seq.int(max(1, (1 + numberOfProfiles - n)), numberOfProfiles)
        for (name in names(x@data)) {
            if (is.vector(x@data[[name]])) {
                res@data[[name]] <- x@data[[name]][look]
            } else if (is.matrix(x@data[[name]])) {
                res@data[[name]] <- x@data[[name]][look, ]
            } else if (is.array(x@data[[name]])) {
                res@data[[name]] <- x@data[[name]][look, , ]
            } else {
                res@data[[name]] <- x@data[[name]][look] # for reasons unknown, 'time' is not a vector
            }
        }
     } else if (inherits(x, "ctd")) {
        for (name in names(x@data)) {
            res@data[[name]] <- tail(x@data[[name]], n)
        }
    } else if (inherits(x, "section")) {
        res@metadata$stationId <- tail(x@metadata$stationId, n)
        res@metadata$longitude <- tail(x@metadata$longitude, n)
        res@metadata$latitude <- tail(x@metadata$latitude, n)
        res@metadata$time <- tail(x@metadata$time, n)
        res@data$station <- tail(x@data$station, n)
    } else if (inherits(x, "topo")) {
        ## using head() to determine the indices, because then the 
        ## z matrix indexing will work for both +ve and -ve n.
        ilon <- tail(seq_along(x@data$longitude), n)
        ilat <- tail(seq_along(x@data$latitude), n)
        res@data$longitude <- x@data$longitude[ilon]
        res@data$latitude <- x@data$latitude[ilat]
        res@data$z <- x@data$z[ilon, ilat]
    } else if (inherits(x, "landsat")) {
        warning("tail.oce() cannot handle landsat, so returning it unaltered\n")
    } else if (inherits(x, "amsr")) {
        warning("tail.oce() cannot handle amsr, so returning it unaltered\n")
    } else {
        ## FIXME: probably this will fail on many classes.
        for (name in names(x@data)) {
            if (is.vector(x@data[[name]]) && !is.list(x@data[[name]])) {
                res@data[[name]] <- tail(x@data[[name]], n)
            } else {
                warning("ignoring '", name, "' because it is not a vector\n")
            }
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Draw a Polar Plot
#'
#' Creates a crude polar plot.
#'
#' @param r radii of points to plot.
#' @param theta angles of points to plot, in degrees.
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @param \dots optional arguments passed to the lower-level plotting
#' functions.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' r <- rnorm(50, mean=2, sd=0.1)
#' theta <- runif(50, 0, 360)
#' plotPolar(r, theta)
plotPolar <- function(r, theta, debug=getOption("oceDebug"), ...)
{

    oceDebug(debug, "plotPolar(...)\n", unindent=1)
    if (missing(r)) stop("must supply 'r'")
    if (missing(theta)) stop("must supply 'theta'")
    thetaRad <- theta * atan2(1, 1) / 45
    x <- r * cos(thetaRad)
    y <- r * sin(thetaRad)
    R <- 1.2 * max(r, na.rm=TRUE)
    ##Rpretty <- pretty(c(0, R))
    plot.new()
    plot.window(c(-R, R), c(-R, R), asp=1)
    points(x, y, ...)
    xa <- axis(1, pos=0)
    abline(v=0)
    th <- seq(0, 2 * atan2(1, 1) * 4, length.out=100)
    for (radius in xa[xa>0]) {
        lines(radius * cos(th), radius * sin(th))
    }
    abline(h=0)
    abline(v=0)
    oceDebug(debug, "} # plotPolar()\n", unindent=1)
}


#' Interpolate 1D Data with UNESCO or Reiniger-Ross Algorithm
#'
#' Interpolate one-dimensional data using schemes that permit curvature but
#' tends minimize extrema that are not well-indicated by the data.
#'
#' Setting \code{method="rr"} yields the weighted-parabola algorithm of
#' Reiniger and Ross (1968).  For procedure is as follows.  First, the
#' interpolant for any \code{xout} value that is outside the range of \code{x}
#' is set to NA.  Next, linear interpolation is used for any \code{xout} value
#' that has only one smaller neighboring \code{x} value, or one larger
#' neighboring value.  For all other values of \code{xout}, the 4 neighboring
#' points \code{x} are sought, two smaller and two larger.  Then two parabolas
#' are determined, one from the two smaller points plus the nearest larger
#' point, and the other from the nearest smaller point and the two larger
#' points.  A weighted sum of these two parabolas provides the interpolated
#' value.  Note that, in the notation of Reiniger and Ross (1968), this
#' algorithm uses \code{m}=2 and \code{n}=1.  (A future version of this routine
#' might provide the ability to modify these values.)
#'
#' Setting \code{method="unesco"} yields the method that is used by the U.S.
#' National Oceanographic Data Center. It is described in pages 48-50 of
#' reference 2; reference 3 presumably contains the same information but it is
#' not as easily accessible.  The method works as follows.
#'
#' \itemize{
#'
#' \item If there are data above 5m depth, then the surface value is taken to
#' equal to the shallowest recorded value.
#'
#' \item Distance bounds are put on the four neighboring points, and the
#' Reiniger-Ross method is used for interpolated points with sufficiently four
#' close neighbors.  The bounds are described in table 15 of reference 2 only
#' for so-called standard depths; in the present instance they are transformed
#' to the following rules.  Inner neighbors must be within 5m for data above
#' 10m, 50m above 250m 100m above 900m, 200m above 2000m, or within 1000m
#' otherwise.  Outer neighbors must be within 200m above 500m, 400m above
#' 1300m, or 1000m otherwise.  If two or more points meet these criteria,
#' Lagrangian interpolation is used.  If not, \code{NA} is used as the
#' interpolant.
#'
#' }
#'
#' After these rules are applied, the interpolated value is compared with the
#' values immediately above and below it, and if it is outside the range,
#' simple linear interpolation is used.
#'
#' @param x the independent variable (z or p, usually).
#' @param y the dependent variable.
#' @param xout the values of the independent variable at which interpolation is
#' to be done.
#' @param method method to use.  See \dQuote{Details}.
#' @return A vector of interpolated values, corresponding to the \code{xout}
#' values and equal in number.
#' @author Dan Kelley
#' @references
#'
#' \enumerate{
#'
#' \item R.F. Reiniger and C.K. Ross, 1968.  A method of interpolation with
#' application to oceanographic data.  \emph{Deep Sea Research}, \bold{15},
#' 185-193.
#'
#' \item Daphne R. Johnson, Tim P. Boyer, Hernan E. Garcia, Ricardo A.
#' Locarnini, Olga K. Baranova, and Melissa M. Zweng, 2011. World Ocean
#' Database 2009 Documentation.  NODC Internal report 20.  Ocean Climate
#' Laboratory, National Oceanographic Data Center.  Silver Spring, Maryland.
#'
#' \item UNESCO, 1991. Processing of oceanographic station data, 138 pp.,
#' Imprimerie des Presses Universitaires de France, United Nations Educational,
#' Scientific and Cultural Organization, France.
#'
#' }
#' @aliases oce.approx
#' @examples
#'
#' library(oce)
#' if (require(ocedata)) {
#'     data(RRprofile)
#'     zz <- seq(0, 2000, 2)
#'     plot(RRprofile$temperature, RRprofile$depth, ylim=c(500, 0), xlim=c(2, 11))
#'     ## Contrast two methods
#'     a1 <- oce.approx(RRprofile$depth, RRprofile$temperature, zz, "rr")
#'     a2 <- oce.approx(RRprofile$depth, RRprofile$temperature, zz, "unesco")
#'     lines(a1, zz)
#'     lines(a2, zz, col='red')
#'     legend("bottomright",lwd=1,col=1:2, legend=c("rr","unesco"),cex=3/4)
#' }
oceApprox <- function(x, y, xout, method=c("rr", "unesco"))
{
    method <- match.arg(method)
    if (missing(x))
        stop("must supply x")
    if (missing(y))
        stop("must supply y")
    lx <- length(x)
    ly <- length(y)
    if (lx != ly)
        stop("length of x (", lx, ") and y (", ly, ") must agree")
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]
    y <- y[ok]
    o <- order(x)
    x <- x[o]
    y <- y[o]
    keep <- c(TRUE, 0 != diff(x))
    x <- x[keep]
    y <- y[keep]
    if (missing(xout))
        xout <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=lx)
    else
        if (any(is.na(xout)))
            stop("must not have any NA values in xout")
    .Call("oce_approx", x=x, y=y, xout=xout,
          method=pmatch(method, c("unesco", "rr")))
}
oce.approx <- oceApprox


#' Draw a Stick Plot
#'
#' The arrows are drawn with directions on the graph that match the directions
#' indicated by the \code{u} and \code{v} components. The arrow size is set
#' relative to the units of the \code{y} axis, according to the value of
#' \code{yscale}, which has the unit of \code{v} divided by the unit of
#' \code{y}.
#' The interpretation of diagrams produced by \code{plotSticks} can be
#' difficult, owing to overlap in the arrows.  For this reason, it is often
#' a good idea to smooth \code{u} and \code{v} before using this function.
#'
#' @param x x coordinates of stick origins.
#' @param y y coordinates of stick origins.  If not supplied, 0 will be used;
#' if length is less than that of x, the first number is repeated and the rest
#' are ignored.
#' @param u x component of stick length.
#' @param v y component of stick length.
#' @param yscale scale from u and v to y (see \dQuote{Description}).
#' @param add boolean, set \code{TRUE} to add to an existing plot.
#' @param length value to be provided to \code{\link{arrows}}; here, we set a
#' default that is smaller than normally used, because these plots tend to be
#' crowded in oceanographic applications.
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also
#' for \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' @param mar value to be used with \code{\link{par}("mar")}.
#' @param xlab,ylab labels for the plot axes. The default is not to label them.
#' @param col colour of sticks, in either numerical or character format. This is
#' made to have length matching that of \code{x} by a call to \code{\link{rep}},
#' which can be handy in e.g. colourizing a velocity field by day.
#' @param \dots graphical parameters passed down to \code{\link{arrows}}.  It
#' is common, for example, to use smaller arrow heads than \code{\link{arrows}}
#' uses; see \dQuote{Examples}.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#'
#' # Flow from a point source
#' n <- 16
#' x <- rep(0, n)
#' y <- rep(0, n)
#' theta <- seq(0, 2*pi, length.out=n)
#' u <- sin(theta)
#' v <- cos(theta)
#' plotSticks(x, y, u, v, xlim=c(-2, 2), ylim=c(-2, 2))
#' rm(n, x, y, theta, u, v)
#'
#' # Oceanographic example
#' data(met)
#' t <- met[["time"]]
#' u <- met[["u"]]
#' v <- met[["v"]]
#' p <- met[["pressure"]]
#' oce.plot.ts(t, p)
#' plotSticks(t, 99, u, v, yscale=25, add=TRUE)
plotSticks <- function(x, y, u, v, yscale=1, add=FALSE, length=1/20,
                       mgp=getOption("oceMgp"),
                       mar=c(mgp[1]+1, mgp[1]+1, 1, 1+par("cex")),
                       xlab="", ylab="", col=1, ...)
{
    pin <- par("pin")
    page.ratio <- pin[2]/pin[1]
    if (missing(x))
        stop("must supply x")
    nx <- length(x)
    if (missing(y))
        y <- rep(0, nx)
    if (length(y) < nx)
        y <- rep(y[1], nx)
    if (missing(u))
        stop("must supply u")
    if (missing(v))
        stop("must supply v")
    n <- length(x)
    if (length(y) != n)
        stop("lengths of x and y must match, but they are ", n, " and ", length(y))
    if (length(u) != n)
        stop("lengths of x and u must match, but they are ", n, " and ", length(u))
    if (length(v) != n)
        stop("lenghts of x and v must match, but they are ", n, " and ", length(v))
    col <- rep(col, length.out=n)
    par(mar=mar, mgp=mgp)
    if (!add)
        plot(range(x), range(y), type='n', xlab=xlab, ylab=ylab, ...)
    usr <- par("usr")
    yrxr <- (usr[4] - usr[3]) / (usr[2] - usr[1])
    warn <- options("warn")$warn # FIXME: fails to quieten arrows()
    options(warn=0)
    ok <- !is.na(u) & !is.na(v) & (u^2+v^2) > 0
    arrows(as.numeric(x[ok]),
           y[ok],
           (as.numeric(x[ok]) + u[ok] / yscale / yrxr * page.ratio),
           (y[ok] + v[ok] / yscale),
           length=length, col=col[ok],
           ...)
    options(warn=warn)
}

#' Add a Grid to an Existing Oce Plot
#'
#' @details
#' For plots not created by oce functions, or for missing \code{xat} and \code{yat},
#' this is the same as a call to \code{\link{grid}} with missing \code{nx} and
#' \code{ny}. However, if \code{xat} is the return value from certain oce functions,
#' a more sophisticated grid is constructed. The problem with \code{\link{grid}} is
#' that it cannot handle axes with non-uniform grids, e.g. those with time axes
#' that span months of differing lengths.
#'
#' As of early February 2015, \code{oce.grid} handles \code{xat} produced as the
#' return value from the following functions: \code{\link{imagep}} and
#' \code{\link{oce.plot.ts}}, \code{\link{plot,adp-method}},
#' \code{\link{plot,echosounder-method}}, and \code{\link{plotTS}}.
#' It makes no sense to try to use \code{oce.grid} for multipanel oce plots,
#' e.g. the default plot from \code{\link{plot,adp-method}}.
#'
#' @examples
#' library(oce)
#' i <- imagep(volcano)
#' oce.grid(i, lwd=2)
#'
#' data(sealevel)
#' i <- oce.plot.ts(sealevel[["time"]], sealevel[["elevation"]])
#' oce.grid(i, col='red')
#'
#' data(ctd)
#' i <- plotTS(ctd)
#' oce.grid(i, col='red')
#'
#' data(adp)
#' i <- plot(adp, which=1)
#' oce.grid(i, col='gray', lty=1)
#'
#' data(echosounder)
#' i <- plot(echosounder)
#' oce.grid(i, col='pink', lty=1)
#'
#' @param xat either a list of x values at which to draw the grid, or the return value from an oce plotting function
#' @param yat a list of y values at which to plot the grid (ignored if \code{gx} was a return value from an oce plotting function)
#' @param col colour of grid lines (see \code{\link{par}})
#' @param lty type for grid lines (see \code{\link{par}})
#' @param lwd width for grid lines (see \code{\link{par}})
oce.grid <- function(xat, yat, col="lightgray", lty="dotted", lwd=par("lwd"))
{
    if (missing(xat) && missing(yat)) {
        grid(col=col, lty=lty, lwd=lwd)
    } else {
        if (is.list(xat)) {
            ## following over-rides the args
            yat <- xat$yat
            xat <- xat$xat
        }
        if (!missing(xat)) abline(v=xat, col=col, lty=lty, lwd=lwd)
        if (!missing(yat)) abline(h=yat, col=col, lty=lty, lwd=lwd)
    }
}


#' Oce Variant of plot.ts
#'
#' Plot a time-series, obeying the timezone and possibly drawing the range in
#' the top-left margin.
#'
#' @details
#' Depending on the version of R, the standard \code{\link{plot}} and
#' \code{\link{plot.ts}} routines will not obey the time zone of the data.
#' This routine gets around that problem.  It can also plot the time range in
#' the top-left margin, if desired; this string includes the timezone, to
#' remove any possible confusion.
#' The time axis is drawn with \code{\link{oce.axis.POSIXct}}.
#'
#' @param x the times of observations.
#' @param y the observations.
#' @param type plot type, \code{"l"} for lines, \code{"p"} for points.
#' @param xlim optional limit for x axis.  This has an additional effect,
#' beyond that for conventional R functions: it effectively windows the data,
#' so that autoscaling will yield limits for y that make sense within the
#' window.
#' @param ylim optional limit for y axis.
#' @param drawTimeRange an optional indication of whether/how to draw a time range,
#' in the top-left margin of the plot; see \code{\link{oce.axis.POSIXct}} for details.
#'
#' @param fill boolean, set \code{TRUE} to fill the curve to zero (which it
#' does incorrectly if there are missing values in \code{y}).
#' @param xlab name for x axis; defaults to \code{""}.
#' @param ylab name for y axis; defaults to the plotted item.
#' @param xaxs control x axis ending; see \code{\link{par}("xaxs")}.
#' @param yaxs control y axis ending; see \code{\link{par}("yaxs")}.
#' @param cex size of labels on axes; see \code{\link[graphics]{par}}("cex").
#' @param cex.axis see \code{\link[graphics]{par}}("cex.axis").
#' @param cex.main see \code{\link[graphics]{par}}("cex.main").
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also
#' for \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' @param mar value to be used with \code{\link{par}("mar")} to set margins.
#' The default value uses significantly tighter margins than is the norm in R,
#' which gives more space for the data.  However, in doing this, the existing
#' \code{par("mar")} value is ignored, which contradicts values that may have
#' been set by a previous call to \code{\link{drawPalette}}.  To get plot with
#' a palette, first call \code{\link{drawPalette}}, then call
#' \code{oce.plot.ts} with \code{mar=par("mar")}.
#' @param main title of plot.
#' @param despike boolean flag that can turn on despiking with
#' \code{\link{despike}}.
#' @param axes boolean, set to \code{TRUE} to get axes plotted
#' @param tformat optional format for labels on the time axis
#' @param marginsAsImage boolean indicating whether to set the right-hand
#' margin to the width normally taken by an image drawn with
#' \code{\link{imagep}}.
#' @param grid if \code{TRUE}, a grid will be drawn for each panel.  (This
#' argument is needed, because calling \code{\link{grid}} after doing a
#' sequence of plots will not result in useful results for the individual
#' panels.
#' @param grid.col colour of grid
#' @param grid.lty line type of grid
#' @param grid.lwd line width of grid
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @param \dots graphical parameters passed down to \code{\link{plot}}.
#' @return A list is silently returned, containing \code{xat} and \code{yat},
#' values that can be used by \code{\link{oce.grid}} to add a grid to the plot.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' t0 <- as.POSIXct("2008-01-01", tz="UTC")
#' t <- seq(t0, length.out=48, by="30 min")
#' y <- sin(as.numeric(t - t0) * 2 * pi / (12 * 3600))
#' oce.plot.ts(t, y, type='l', xaxs='i')
oce.plot.ts <- function(x, y, type="l", xlim, ylim, xlab, ylab,
                        drawTimeRange, fill=FALSE,
                        xaxs=par("xaxs"), yaxs=par("yaxs"),
                        cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                        mgp=getOption("oceMgp"),
                        mar=c(mgp[1]+if (nchar(xlab)>0) 1.5 else 1, mgp[1]+1.5, mgp[2]+1, mgp[2]+3/4),
                        main="",
                        despike=FALSE,
                        axes=TRUE, tformat,
                        marginsAsImage=FALSE,
                        grid=FALSE, grid.col="darkgray", grid.lty="dotted", grid.lwd=1,
                        debug=getOption("oceDebug"),
                        ...)
{
    if (is.function(x))
        stop("x cannot be a function")
    if ("adorn" %in% names(list(...)))
        warning("the 'adorn' argument was removed in November 2017")
    if (missing(xlab))
        xlab <- ""
    if (missing(ylab))
        ylab  <- deparse(substitute(y))
    if (missing(drawTimeRange))
        drawTimeRange <- getOption("oceDrawTimeRange", TRUE)
    ##ocex <- par("cex")
    #par(cex=cex)
    debug <- min(debug, 4)
    oceDebug(debug, "oce.plot.ts(..., debug=", debug, ", type=\"", type, "\", \n", sep="", unindent=1)
    oceDebug(debug, "  mar=c(", paste(mar, collapse=", "), "),\n", sep="")
    oceDebug(debug, "  mgp=c(", paste(mgp, collapse=", "), "),\n", sep="")
    oceDebug(debug, "  ...) {\n", sep="")
    oceDebug(debug, "length(x)", length(x), "; length(y)", length(y), "\n")
    oceDebug(debug, "cex=", cex, " cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
    oceDebug(debug, "mar=c(", paste(mar, collapse=","), ")\n")
    oceDebug(debug, "marginsAsImage=", marginsAsImage, ")\n")
    oceDebug(debug, "x has timezone", attr(x[1], "tzone"), "\n")
    pc <- paletteCalculations(maidiff=rep(0, 4))
    par(mgp=mgp, mar=mar)
    args <- list(...)
    xlimGiven <- !missing(xlim)
    if (xlimGiven) {
        if (2 != length(xlim))
            stop("'xlim' must be of length 2")
        if (xlim[2] <= xlim[1])
            stop("the elements of xlim must be in order")
        ends <- .Call("trim_ts", as.numeric(x), as.numeric(xlim), as.numeric(0.04))
        x <- x[seq.int(ends$from, ends$to)]
        y <- y[seq.int(ends$from, ends$to)]
    }
    if (length(y) == 1)
        y <- rep(y, length(x))
    if (despike)
        y <- despike(y)
    if (marginsAsImage) {
        ## FIXME: obey their mar?
        the.mai <- pc$mai0
        the.mai <- clipmin(the.mai, 0)         # just in case
        oceDebug(debug, "the.mai=", the.mai, "\n")

        par(mai=the.mai, cex=cex)
        drawPalette(mai=rep(0, 4))
    }
    xrange <- range(x, na.rm=TRUE)
    yrange <- range(y, na.rm=TRUE)
    if (!is.finite(yrange[1])) {
        plot(xrange, c(0, 1), axes=FALSE, xaxs=xaxs, yaxs=yaxs,
             xlim=if (xlimGiven) xlim else xrange,
             xlab=xlab, ylab=ylab, type='n')
        oce.axis.POSIXct(1, drawTimeRange=FALSE)
        box()
        mtext("bad data", side=3, line=-1, cex=cex)
        warning("no valid data for '", ylab, "'", sep="")
        oceDebug(debug, "} # oce.plot.ts()\n", unindent=1)
        return()
     } else {
        if (fill) {
            xx <- c(x[1], x, x[length(x)])
            yy <- c(0, y, 0)
            plot(x, y, axes=FALSE, xaxs=xaxs, yaxs=yaxs,
                 xlim=if (xlimGiven) xlim else range(x, na.rm=TRUE),
                 xlab=xlab, ylab=ylab,
                 type=type, cex=cex, ...)
            fillcol <- if ("col" %in% names(args)) args$col else "lightgray" # FIXME: should be a formal argument
            do.call(polygon, list(x=xx, y=yy, col=fillcol))
        } else {
            plot(x, y, axes=FALSE, xaxs=xaxs, yaxs=yaxs,
                 xlim=if (missing(xlim)) NULL else xlim,
                 ylim=if (missing(ylim)) NULL else ylim,
                 xlab=xlab, ylab=ylab,
                 type=type, cex=cex, ...)
        }
        xat <- NULL
        yat <- NULL
        if (axes) {
            xaxt <- list(...)["xaxt"]
            drawxaxis <- !is.null(xaxt) && xaxt != 'n'
            yaxt <- list(...)["yaxt"]
            drawyaxis <- !is.null(yaxt) && yaxt != 'n'
            if (drawxaxis) {
                xlabs <- oce.axis.POSIXct(1, x=x, drawTimeRange=drawTimeRange, main=main,
                                          mgp=mgp,
                                          xlim=if (missing(xlim)) range(x) else xlim,
                                          cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                          tformat=tformat,
                                          debug=debug-1)
                xat <- xlabs
                oceDebug(debug, "drawing x axis; set xat=c(", paste(xat, collapse=","), ")", "\n", sep="")
            }
            if (grid) {
                lwd <- par("lwd")
                if (drawxaxis)
                    abline(v=xlabs, col="lightgray", lty="dotted", lwd=lwd)
                yaxp <- par("yaxp")
                abline(h=seq(yaxp[1], yaxp[2], length.out=1+yaxp[3]),
                       col="lightgray", lty="dotted", lwd=lwd)
            }
            box()
            ##cat("cex.axis=",cex.axis,"; par('cex.axis') is", par('cex.axis'), "; par('cex') is", par('cex'), "\n")
            if (drawyaxis)
                axis(2, cex.axis=cex.axis, cex=cex.axis)
            yat <- axis(4, labels=FALSE)
        }
        if (grid)
            grid(col=grid.col, lty=grid.lty, lwd=grid.lwd)
        oceDebug(debug, "} # oce.plot.ts()\n", unindent=1)
        invisible(list(xat=xat, yat=yat))
    }
}


#' Oce Variant of as.POSIXlt [deprecated]
#'
#' \strong{WARNING:} This function will be removed soon; see \link{oce-deprecated}.
#' It was realized in December of 2016 that this function was not used within
#' oce, and also that \code{\link[lubridate]{parse_date_time}} in the
#' \CRANpkg{lubridate} package was superior and therefore a better choice for
#' \dQuote{oce} users.
#'
#' @param x a date, as for \code{as.POSIXlt}, but also including forms in which
#' the month name appears.
#' @param tz the timezone, as for \code{as.POSIXlt}
#' @return A POSIXlt object.
#' @author Dan Kelley
#' @family functions that will be removed soon
oce.as.POSIXlt <- function (x, tz = "")
{
    .Deprecated("lubridate::parse_date_time",
                msg="oce.as.POSIXlt() will be removed soon; see ?'oce-deprecated'.")
    fromchar <- function(x)
    {
        xx <- x[1]
        if (is.na(xx)) {
            j <- 1
            while (is.na(xx) && (j <- j + 1) <= length(x)) xx <- x[j]
            if (is.na(xx))
                f <- "%Y-%m-%d"
        }
        ## year day hhmm
        tokens <- strsplit(xx, " +")[[1]]
        if (length(tokens) == 3 && nchar(tokens[3]) == 4) {
            ## the nchar check skips [year month day]
            return(strptime(x, format="%Y %j %H%M"))
        }
        if (is.na(xx) ||
                                        # additions ...
            ( (nchar(xx) == 8) && !is.na(strptime(xx, f <- "%Y%m%d")) ) || # 20020823
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
        warning("The string \"", x, "\" is not in a known date format")
        return(NA)
    }
    if (inherits(x, "POSIXlt"))
        return(x)
    if (inherits(x, "Date"))
        return(as.POSIXlt(x))
    tzone <- attr(x, "tzone")
    if (inherits(x, "date") || inherits(x, "dates"))
        x <- as.POSIXct(x)
    if (is.character(x))
        return(fromchar(unclass(x)))
    if (is.factor(x))
        return(fromchar(as.character(x)))
    if (is.logical(x) && all(is.na(x)))
        x <- as.POSIXct.default(x)
    if (!inherits(x, "POSIXct"))
        stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"", deparse(substitute(x))))
    if (missing(tz) && !is.null(tzone))
        tz <- tzone[1]
    as.POSIXlt(x, tz)
}


#' Edit an Oce Object
#'
#' Edit an element of an oce object, inserting a note in the processing
#' log of the returned object.
#'
#' There are several ways to use this function.
#'
#'\itemize{
#' \item Case 1. If both an \code{item} and \code{value} are supplied, then
#' either the object's metadata or data slot may be altered. There are
#' two ways in which this can be done.
#'
#' \itemize{
#'
#' \item Case 1A. If the \code{item} string does not contain an
#' \code{@} character, then the \code{metadata} slot is examined
#' for an entry named \code{item}, and that is modified if so.
#' Alternatively, if \code{item} is found in \code{metadata}, then
#' that value is modified. However, if \code{item} is not found in
#' either \code{metadata} or \code{data}, then an error is reported
#' (see 1B for how to add something that does not yet exist).
#' 
#' \item Case 1B. If the \code{item} string contains
#' the \code{@} character, then the text to the left of that character
#' must be either \code{"metadata"} or \code{"data"}, and it names the slot
#' in which the change is done. In contrast with case 1A, this will
#' \emph{create} a new item, if it is not already in existence. 
#'
#' }
#'
#' \item Case 2. If \code{item} and \code{value} are not supplied, then \code{action} must
#' be supplied.  This is a character string specifying some action to be
#' performed on the object, e.g. a manipulation of a column.  The action must
#' refer to the object as \code{x}; see Examples.
#'
#'}
#'
#' In any case, a log entry is stored in the object, to document the change.
#' Indeed, this is the main benefit to using this function, instead of altering
#' the object directly.  The log entry will be most useful if it contains a
#' brief note on the \code{reason} for the change, and the name of the
#' \code{person} doing the work.
#'
#' @aliases oce.edit
#' @param x an \code{oce} object.  The exact action of \code{oceEdit} depends
#' on the \code{\link{class}} of \code{x}.
#' @param item if supplied, a character string naming an item in the object's
#' \code{metadata} or \code{data} slot, the former being checked first.
#' An exception is if \code{item} starts with \code{"data@"} or
#' \code{"metadata@"}, in which case the named slot is updated with a changed
#' value of the contents of \code{item} after the \code{@} character.
#' @param value new value for \code{item}, if both supplied.
#' @param action optional character string containing R code to carry out some
#' action on the object.
#' @param reason character string giving the reason for the change.
#' @param person character string giving the name of person making the change.
#' @param debug an integer that specifies a level of debugging, with 0 or less
#' indicating no debugging, and 1 or more indicating debugging.
#' @return An object of \code{\link[base]{class}} \code{"oce"}, altered
#' appropriately, and with a log item indicating the nature of the alteration.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' data(ctd)
#' ctd2 <- oceEdit(ctd, item="latitude", value=47.8879,
#'                reason="illustration", person="Dan Kelley")
#' ctd3 <- oceEdit(ctd,action="x@@data$pressure<-x@@data$pressure-1")
oceEdit <- function(x, item, value, action, reason="", person="",
                     debug=getOption("oceDebug"))
{
    oceDebug(debug, "oceEdit() {\n", unindent=1)
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    if (missing(item) && missing(value) && missing(action)) {
        x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        oceDebug(debug, "} # oceEdit()\n", unindent=1)
        return(x)
    }
    slot <- NULL
    if (!missing(item)) {
        if (missing(value))
            stop("must supply a value")
        ##oceDebug(debug, "ORIG item='", item, "'\n", sep="")
        ## Split out the slotname, if any.
        if (length(grep("@", item))) {
            slot <- gsub("@.*$", "", item)
            if (slot != "metadata" && slot != "data")
                stop("slot must be 'metadata' or 'data'")
            item <- gsub("^.*@", "", item)
        }
        ##oceDebug(debug, "LATER slot='", slot, "' and item='", item, "'\n", sep="")
        if (missing(value))
            stop("must supply a 'value' for this 'item'")
        if (inherits(x, "adv")) {
            oceDebug(debug, "object is an ADV\n")
            hpr <- 0 < length(grep("heading|pitch|roll", item))
            if (hpr) {
                ## FIXME: I think this violates the 1A rule on creating new data,
                ## FIXME: but I am retaining this since it's years old.
                ## FIXME: why are adp and adv handled differently, anyway? Is
                ## FIXME: this a fast/slow variable issue?
                x@data[[item]] <- value
            } else {
                if (!is.null(slot)) {
                    slot(x, slot)[[item]] <- value
                } else if (item %in% names(x@metadata)) {
                    x@metadata[[item]] <- value
                } else if (item %in% names(x@data)) {
                    x@data[[item]] <- value
                } else {
                    stop("nothing named '", item, "' in object's metadata or data")
                }
            }
        } else if (inherits(x, "adp")) {
            oceDebug(debug, "object is an ADP\n")
            hpr <- 0 < length(grep("heading|pitch|roll", item))
            if (hpr) {
                ## FIXME: I think this violates the 1A rule on creating new data,
                ## FIXME: but I am retaining this since it's years old.
                ## FIXME: why are adp and adv handled differently, anyway? Is
                ## FIXME: this a fast/slow variable issue?
                x@data[[item]] <- value
            } else {
                if (!is.null(slot)) {
                    slot(x, slot)[[item]] <- value
                } else if (item %in% names(x@metadata)) {
                    x@metadata[[item]] <- value
                } else if (item %in% names(x@data)) {
                    x@data[[item]] <- value
                } else {
                    stop("nothing named '", item, "' in object's metadata or data")
                }
            }
        } else if ("instrumentType" %in% names(x@metadata) && x@metadata$instrumentType == "aquadopp-hr") {
            ## FIXME: what if S4?
            oceDebug(debug, "About to try editing AQUADOPP ...\n")
            hpr <- 0 < length(grep("heading|pitch|roll", item)) # FIXME: possibly aquadopp should have tsSlow
            x@data[[item]] <- value
            if (hpr) {
                x@data[[item]] <- value
            } else {
                if (!is.null(slot)) {
                    slot(x, slot)[[item]] <- value
                } else if (item %in% names(x@metadata)) {
                    x@metadata[[item]] <- value
                } else if (item %in% names(x@data)) {
                    x@data[[item]] <- value
                } else {
                    stop("nothing named '", item, "' in object's metadata or data")
                }
            }
            oceDebug(debug, "...AQUADOPP edited\n")
        } else {
            oceDebug(debug, "general object; item='", item, "'; slot='", slot, "'\n", sep="")
            if (!is.null(slot)) {
                slot(x, slot)[[item]] <- value
            } else if (item %in% names(x@metadata)) {
                x@metadata[[item]] <- value
            } else if (item %in% names(x@data)) {
                x@data[[item]] <- value
            } else {
                stop("nothing named '", item, "' in object's metadata or data")
            }
        }
    } else if (!missing(action)) {
        warning("the 'action' method may not work -- this needs testing!")
        eval(parse(text=action))        # FIXME: should check if it worked
    } else {
        stop("must supply either an 'item' plus a 'value', or an 'action'")
    }
    x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # oceEdit()\n", unindent=1)
    x
}
oce.edit <- oceEdit


#' Write the Data Portion of Object to a File
#'
#' The output has a line containing the names of the columns in \code{x$data},
#' each enclosed in double quotes.  After that line are lines for the data
#' themselves.  The default is to separate data items by a single space
#' character, but this can be altered by using a \code{sep} argument in the
#' \code{...} list (see \code{\link[utils]{write.table}}).
#'
#' This function is little more than a thin wrapper around
#' \code{\link[utils]{write.table}}, the only difference being that row names
#' are omitted here, making for a file format that is more conventional in
#' Oceanography.
#'
#' @param x an \code{oce} object that contains a \code{data} table.
#' @param file file name, as passed to \code{\link[utils]{write.table}}.  Use
#' \code{""} to get a listing in the terminal window.
#' @param ... optional arguments passed to \code{\link[utils]{write.table}}.
#' @return The value of \code{\link[utils]{write.table}} is returned.
#' @author Dan Kelley
#' @seealso \code{\link[utils]{write.table}}, which does the actual work.
oce.write.table <- function (x, file="", ...)
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    if (!("row.names" %in% names(list(...))))
        write.table(x@data, file, row.names=FALSE, ...)
    else
        write.table(x@data, file, ...)
}


#' Standard Oceanographic Depths
#'
#' This returns so-called standard depths 0m, 10m, etc. below the sea surface.
#'
#' @return A vector of depths, c(0, 10, ...).
#' @author Dan Kelley
standardDepths <- function()
{
    c(0,   10,   20,   30,   50,   75,  100,  125,  150,  200,
      250,  300,  400,  500,  600,  700,  800,  900, 1000, 1100,
      1200, 1300, 1400, 1500, 1750, 2000, 2500, 3000, 3500, 4000,
      4500, 5000, 5500)
}

#' Find the Type of an Oceanographic Data File
#'
#' \code{oceMagic} tries to infer the file type, based on the data
#' within the file, the file name, or a combination of the two.
#'
#' \code{oceMagic} was previously called \code{oce.magic}, but that
#' alias will be removed towards the end of the year 2016; see
#' \link{oce-deprecated}.
#'
#' @aliases oceMagic oce.magic
#' @param file a connection or a character string giving the name of the file
#' to be checked.
#' @param debug an integer, set non-zero to turn on debugging.  Higher values
#' indicate more debugging.
#' @return A character string indicating the file type, or \code{"unknown"}, if
#' the type cannot be determined. If the result contains \code{"/"} characters,
#' these separate a list describing the file type, with the first element being
#' the general type, the second element being the manufacturer, and the third
#' element being the manufacturer's name for the instrument. For example,
#' \code{"adp/nortek/aquadopp"} indicates a acoustic-doppler profiler made by
#' NorTek, of the model type called AquaDopp.
#' @author Dan Kelley
#' @seealso This is used mainly by \code{\link{read.oce}}.
oceMagic <- function(file, debug=getOption("oceDebug"))
{
    filename <- file
    oceDebug(debug, paste("oceMagic(file=\"", filename, "\") {\n", sep=""), unindent=1)
    isdir<- file.info(file)$isdir
    if (is.finite(isdir) && isdir) {
        tst <- file.info(paste(file, "/", file, "_MTL.txt", sep=""))$isdir
        if (!is.na(tst) && !tst)
            return("landsat")
        stop("please supply a file name, not a directory name")
    }
    if (is.character(file)) {
        oceDebug(debug, "checking filename to see if it matches known patterns\n")
        if (length(grep(".asc$", filename))) {
            someLines <- readLines(file, encoding="UTF-8", n=1)
            if (42 == length(strsplit(someLines[1], ' ')[[1]]))
                return("lisst")
        }
        if (length(grep(".adr$", filename))) {
            oceDebug(debug, "file names ends in .adr, so this is an adv/sontek/adr file.\n")
            return("adv/sontek/adr")
        }
        if (length(grep(".rsk$", filename))) {
            oceDebug(debug, "file names ends with \".rsk\", so this is an RBR/rsk file.\n")
            return("RBR/rsk")
        }
        if (length(grep(".s4a.", filename))) {
            oceDebug(debug, "file names contains \".s4a.\", so this is an interocean S4 file.\n")
            return("interocean/s4")
        }
        if (length(grep(".ODF$", filename, ignore.case=TRUE))) {
            ## in BIO files, the data type seems to be on line 14.  Read more, for safety.
            someLines <- readLines(file, n=100, encoding="UTF-8")
            dt <- grep("DATA_TYPE=", someLines)
            if (length(dt) < 1)
                stop("cannot infer type of ODF file")
            subtype <- gsub("[',]", "", tolower(strsplit(someLines[dt[1]], "=")[[1]][2]))
            subtype <- gsub("^\\s*", "", subtype)
            subtype <- gsub("\\s*$", "", subtype)
            res <- paste(subtype, "odf", sep="/")
            oceDebug(debug, "file type:", res, "\n")
            return(res)
        }
        if (length(grep(".WCT$", filename, ignore.case=TRUE))) {
            ## old-style WOCE
            return("ctd/woce/other") # e.g. http://cchdo.ucsd.edu/data/onetime/atlantic/a01/a01e/a01ect.zip
        }
        if (length(grep(".nc$", filename, ignore.case=TRUE))) {
            ## argo?
            if (requireNamespace("ncdf4", quietly=TRUE)) {
                if (substr(filename, 1, 5) == "http:") {
                    stop("cannot open netcdf files over the web; try doing as follows\n    download.file(\"",
                         filename, "\", \"", gsub(".*/", "", filename), "\")")
                }
                ## NOTE: need to name ncdf4 package because otherwise R checks give warnings.
                f <- ncdf4::nc_open(filename)
                if ("DATA_TYPE" %in% names(f$var)) {
                    if (grep("argo", ncdf4::ncvar_get(f, "DATA_TYPE"), ignore.case=TRUE)) return("argo")
                    else return("netcdf")
                } else if ("data_type" %in% names(f$var)) {
                    if (grep("argo", ncdf4::ncvar_get(f, "data_type"), ignore.case=TRUE)) return("argo")
                    else return("netcdf")
                }
            } else {
                stop('must install.packages("ncdf4") to read a netCDF file')
            }
        }
        if (length(grep(".osm.xml$", filename, ignore.case=TRUE))) {
            ## openstreetmap
            return("openstreetmap")
        }
        if (length(grep(".osm$", filename, ignore.case=TRUE))) {
            ## openstreetmap
            return("openstreetmap")
        }
        if (length(grep(".gpx$", filename, ignore.case=TRUE))) {
            ## gpx (e.g. Garmin GPS)
            return("gpx")
        }
        if (length(grep(".csv$", filename, ignore.case=TRUE))) {
            someLines <- readLines(filename, 30)
            if (1 == length(grep("^WMO Identifier", someLines, useBytes=TRUE))) {
                return("met") # FIXME: may be other things too ...
            } else if (1 == length(grep("^Station_Name,", someLines, useBytes=TRUE))) {
                return("sealevel")
            } else if (1 == length(grep("^CTD,", someLines, useBytes=TRUE))) {
                return("ctd/woce/exchange")
            } else if (1 == length(grep("^BOTTLE,", someLines, useBytes=TRUE))) {
                return("section")
            } else {
                return("unknown")
            }
        }
        file <- file(file, "r")
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file))
        open(file, "r")
    ## Grab text at start of file.
    lines <- readLines(file, n=2, skipNul=TRUE)
    line <- lines[1]
    line2 <- lines[2]
    oceDebug(debug, "first line of file: ", line, "\n", sep="")
    oceDebug(debug, "second line of file: ", line2, "\n", sep="")
    close(file)
    file <- file(filename, "rb")
    bytes <- readBin(file, what="raw", n=4)
    oceDebug(debug, paste("first two bytes in file: 0x", bytes[1], " and 0x", bytes[2], "\n", sep=""))
    on.exit(close(file))
    ##read.index()  ## check for an ocean index file e.g.
    ##read.index()  # https://www.esrl.noaa.gov/psd/data/correlation/ao.data
    ##read.index()  tokens <- scan(text=line, what='integer', n=2, quiet=TRUE)
    ##read.index()  if (2 == length(tokens)) {
    ##read.index()      tokens2 <- scan(text=line2, what='integer', quiet=TRUE)
    ##read.index()      if (tokens[1] == tokens2[1])
    ##read.index()          return("index")
    ##read.index()  }
    if (bytes[1] == 0x00 && bytes[2] == 0x00 && bytes[3] == 0x27 && bytes[4] == 0x0a) {
        oceDebug(debug, "this is a shapefile; see e.g. http://en.wikipedia.org/wiki/Shapefile\n  }\n")
        return("shapefile")
    }
    if (bytes[3] == 0xff && bytes[4] == 0xff) {
        oceDebug(debug, "this is a biosonics echosounder file")
        return("echosounder")
    }
    if (bytes[1] == 0x10 && bytes[2] == 0x02) {
        ## 'ADPManual v710.pdf' p83
        if (96 == readBin(bytes[3:4], "integer", n=1, size=2, endian="little"))
            oceDebug(debug, "this is adp/sontek (4 byte match)\n  }\n")
        else
            oceDebug(debug, "this is adp/sontek (2 byte match, but bytes 3 and 4 should become integer 96)\n  }\n")
        return("adp/sontek")
    }
    if (bytes[1] == 0x7f && bytes[2] == 0x7f) {
        oceDebug(debug, "this is adp/rdi\n  }\n")
        return("adp/rdi")
    }
    if (bytes[1] == 0xa5 && bytes[2] == 0x05) {
        ## NorTek files require deeper inspection.  Here, SIG stands for "System Integrator Guide",
        ## Dated Jue 2008 (Nortek Doc No PS100-0101-0608)
        seek(file, 0)
        oceDebug(debug, "This is probably a nortek file of some sort.  Reading further to see for sure ...\n")
        hardware.configuration <- readBin(file, what="raw", n=48) # FIXME: this hard-wiring is repeated elsewhere
        if (hardware.configuration[1] != 0xa5 || hardware.configuration[2] != 0x05) return("unknown")
        oceDebug(debug, "hardware.configuration[1:2]", hardware.configuration[1:2], "(expect 0xa5 0x05)\n")
        head.configuration <- readBin(file, what="raw", n=224)
        oceDebug(debug, "head.configuration[1:2]", head.configuration[1:2], "(expect 0xa5 0x04)\n")
        if (head.configuration[1] != 0xa5 || head.configuration[2] != 0x04) return("unknown")
        user.configuration <- readBin(file, what="raw", n=512)
        oceDebug(debug, "user.configuration[1:2]", user.configuration[1:2], "(expect 0xa5 0x00)\n")
        if (user.configuration[1] != 0xa5 || user.configuration[2] != 0x00) return("unknown")
        nextTwoBytes <- readBin(file, what="raw", n=2)
        oceDebug(debug, "nextTwoBytes:", paste("0x", nextTwoBytes[1], sep=''),
                 paste("0x", nextTwoBytes[2], sep=''),
                 "(e.g. 0x5 0x12 is adv/nortek/vector)\n")
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x12) {
            oceDebug(debug, "these two bytes imply this is adv/nortek/vector\n")
            return("adv/nortek/vector")
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x01) {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudopp (see system-integrator-manual_jan2011.pdf Table 5.2)\n")
            return("adp/nortek/aquadopp")
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x21)  {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudoppProfiler\n")
            return("adp/nortek/aquadoppProfiler") # p37 SIG
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x2a)  {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudoppHR\n")
            return("adp/nortek/aquadoppHR") # p38 SIG
        }
        stop("some sort of nortek ... two bytes are 0x", nextTwoBytes[1], " and 0x", nextTwoBytes[2], " but cannot figure out what the type is")
        ##} else if (as.integer(bytes[1]) == 81) {
        ##    warning("possibly this file is a sontek ADV (first byte is 81)")
        ##} else if (as.integer(bytes[1]) == 83) {
        ##    warning("possibly this file is a sontek ADV (first byte is 83)")
        ##} else if (as.integer(bytes[1]) == 87) {
        ##    warning("possibly this file is a sontek ADV (first byte is 87)")
    }
    if (bytes[1] == 0xa5 && bytes[4] == 0x10) {
        return("adp/nortek/ad2cp")
    }
    if (bytes[1] == 0x9b && bytes[2] == 0x00) {
        warning(paste("Possibly this is an RDI CTD file. Oce cannot read such files yet, because\n",
                      " the author has not located file-format documents.  If you get such documents\n",
                      " from RDI, please send them to dan.kelley@dal.ca so the format can be added."))
        return("possibly RDI CTD")
    }
    ## if (substr(line, 1, 3) == "CTD")) {
    if (1 == length(grep("^CTD", line, useBytes=TRUE))) {
        oceDebug(debug, "this is ctd/woce/exchange\n")
        return("ctd/woce/exchange")
    }
    if (1 == length(grep("^EXPOCODE", line, useBytes=TRUE))) {
        oceDebug(debug, "this is ctd/woce/other\n")
        return("ctd/woce/other")
    }
    if (1 == length(grep("^\\s*ODF_HEADER", line, useBytes=TRUE))){
        oceDebug(debug, "this is an ODF file\n")
        return("odf")
    }
    if (1 == length(grep("^\\* Sea-Bird", line, useBytes=TRUE))) {
        oceDebug(debug, "this is ctd/sbe/19\n")
        return("ctd/sbe/19")
    }
    ## if ("%ITP" == substr(line, 1, 4)) {
    if (1 == length(grep("^%ITP", line, useBytes=TRUE))) {
        oceDebug(debug, "this is ice-tethered profile\n")
        return("ctd/itp")
    }
    ##if ("# -b" == substr(line, 1, 4)) {
    if (1 == length(grep("^# -b", line, useBytes=TRUE))) {
        oceDebug(debug, "this is coastline\n")
        return("coastline")
    }
    ## if ("# Station_Name," == substr(line, 1, 15)) {
    if (1 == length(grep("^# Station_Name,", line, useBytes=TRUE))) {
        oceDebug(debug, "this is sealevel\n")
        return("sealevel")
    }
    ##if ("Station_Name," == substr(line, 1, 13)) {
    if (1 == length(grep("^Station_Name,", line, useBytes=TRUE))) {
        oceDebug(debug, "this is sealevel\n")
        return("sealevel")
    }
    if (1 == length(grep("^[0-9][0-9][0-9][A-Z] ", line, useBytes=TRUE))) {
        oceDebug(debug, "this is sealevel\n")
        return("sealevel")
    }
    if (1 == length(grep("^NCOLS[ ]*[0-9]*[ ]*$", line, useBytes=TRUE, ignore.case=TRUE))) {
        oceDebug(debug, "this is topo\n")
        return("topo")
    }
    ##if ("RBR TDR" == substr(line, 1, 7))  { ## FIXME: obsolete; to be removed Fall 2015
    if (1 == length(grep("^RBR TDR", line, useBytes=TRUE))) {
        ## FIXME: obsolete; to be removed Fall 2015
        oceDebug(debug, "this is RBR/dat\n")
        return("RBR/dat")
    }
    ## if ("Model=" == substr(line, 1, 6))  {
    if (1 == length(grep("^Model=", line, useBytes=TRUE))) {
        oceDebug(debug, "this is RBR/txt\n")
        return("RBR/txt")
    }
    ## if ("BOTTLE"  == substr(line, 1, 6))  {
    if (1 == length(grep("^BOTTLE", line, useBytes=TRUE)))  {
        oceDebug(debug, "this is section\n")
        return("section")
    }
    ## if (length(grep("^//SDN_parameter_mapping", line, useBytes=TRUE)) ||
    ##     length(grep("^//SDN_parameter_mapping", line2, useBytes=TRUE))) {
    ##     oceDebug(debug, "this is ODV\n")
    ##     return("ctd/odv")
    ## }
    oceDebug(debug, "this is unknown\n")
    return("unknown")
}
oce.magic <- oceMagic




#' Read an Oceanographic Data File
#'
#' Read an oceanographic data file, auto-discovering the file type from the
#' first line of the file.
#' This function tries to infer the file type from the first line, using
#' \code{\link{oceMagic}}.  If it can be discovered, then an
#' instrument-specific file reading function is called, with the \code{file}
#' and with any additional arguments being supplied.
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param ... arguments to be handed to whichever instrument-specific reading
#' function is selected, based on the header.
#' @return An object of \code{\link{oce-class}} that is
#' specialized to the data type, e.g. \code{\link{ctd-class}},
#' if the data file contains \code{ctd} data.
#' @author Dan Kelley
#' @seealso The file type is determined by \code{\link{oceMagic}}.  If the file
#' type can be determined, then one of the following is called:
#' \code{\link{read.ctd}}, \code{\link{read.coastline}}
#' \code{\link{read.lobo}}, \code{\link{read.rsk}},
#' \code{\link{read.sealevel}}, etc.
#' @examples
#'
#' library(oce)
#' x <- read.oce(system.file("extdata", "ctd.cnv", package="oce"))
#' plot(x) # summary with TS and profiles
#' plotTS(x) # just the TS
read.oce <- function(file, ...)
{
    type <- oceMagic(file)
    debug <- if ("debug" %in% names(list(...))) list(...)$debug else 0
    oceDebug(debug,
             "read.oce(\"", as.character(file), "\", ...) inferred type=\"", type, "\"\n",
             sep="", unindent=1)
    processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ## read.index if (type == "index")
    ## read.index     return(read.index(file))
    if (type == "shapefile")
        return(read.coastline.shapefile(file, processingLog=processingLog, ...))
    if (type == "openstreetmap")
        return(read.coastline.openstreetmap(file, processingLog=processingLog, ...))
    if (type == "echosounder")
        return(read.echosounder(file, processingLog=processingLog, ...))
    if (type == "adp/rdi")
        return(read.adp.rdi(file, processingLog=processingLog, ...))
    if (type == "adp/sontek")
        return(read.adp.sontek(file, processingLog=processingLog, ...)) # FIXME is pcadcp different?
    if (type == "adp/nortek/aquadopp")
        return(read.aquadopp(file, processingLog=processingLog, ...))
    if (type == "adp/nortek/aquadoppProfiler")
        return(read.aquadoppProfiler(file, processingLog=processingLog, ...))
    if (type == "adp/nortek/aquadoppHR")
        return(read.aquadoppHR(file, processingLog=processingLog, ...))
    if (type == "adp/nortek/ad2cp")
        return(read.ad2cp(file, processingLog=processingLog, ...))
    if (type == "adv/nortek/vector")
        return(read.adv.nortek(file, processingLog=processingLog, ...))
    if (type == "adv/sontek/adr")
        return(read.adv.sontek.adr(file, processingLog=processingLog, ...))
    ## FIXME need adv/sontek (non adr)
    if (type == "interocean/s4")
        return(read.cm.s4(file, processingLog=processingLog, ...))
    if (type == "ctd/sbe/19")
        return(read.ctd.sbe(file, processingLog=processingLog, ...))
    if (type == "ctd/woce/exchange")
        return(read.ctd.woce(file, processingLog=processingLog, ...))
    if (type == "ctd/woce/other")
        return(read.ctd.woce.other(file, processingLog=processingLog, ...))
    if (type == "ctd/odf" || type == "mctd/odf" || type == "mvctd/odf")
        return(read.ctd.odf(file, processingLog=processingLog, ...))
    if (length(grep("/odf$", type)))
        return(read.odf(file, debug=debug))
    if (type == "mtg/odf") {
        ## FIXME: document this data type
        ## Moored tide gauge: returns a data frame.
        fromHeader <- function(key)
        {
            i <- grep(key, lines)
            if (length(i) < 1)
                ""
            else
                gsub("\\s*$", "", gsub("^\\s*", "", gsub("'", "", gsub(",", "", strsplit(lines[i[1]], "=")[[1]][2]))))
        }
        lines <- readLines(file, encoding="UTF-8")
        nlines <- length(lines)
        headerEnd <- grep("-- DATA --", lines)
        if (1 != length(headerEnd))
            stop("found zero or multiple '-- DATA --' (end of header) lines in a mtg/odf file")
        ##header <- lines[1:headerEnd]
        data <- lines[seq.int(headerEnd+1, nlines)]
        d <- read.table(text=data, header=FALSE, col.names=c("time", "temperature", "ptotal", "psea", "depth"))
        d$time <- strptime(d$time, "%d-%B-%Y %H:%M:%S", tz="UTC") # guess on timezone
        missing_value <- -99.0 # FIXME: it's different for each column
        d[d==missing_value] <- NA
        attr(d, "scientist") <- fromHeader("CHIEF_SCIENTIST")
        attr(d, "latitude") <- as.numeric(fromHeader("INITIAL_LATITUDE"))
        attr(d, "longitude") <- as.numeric(fromHeader("INITIAL_LONGITUDE"))
        attr(d, "cruise_name") <- fromHeader("CRUISE_NAME")
        attr(d, "cruise_description") <- fromHeader("CRUISE_DESCRIPTION")
        attr(d, "inst_type") <- fromHeader("INST_TYPE")
        attr(d, "model") <- fromHeader("MODEL")
        attr(d, "serial_number") <- fromHeader("SERIAL_NUMBER")
        attr(d, "missing_value") <- missing_value
        warning("Missing-value code for mtg/odf is hard-wired to -99, which will likely be wrong in other files")
        warning("The format of mtg/odf objects is likely to change throughout April, 2015")
        return(d)
    }
    ## if (type == "ctd/odv")
    ##     return(read.ctd.odv(file, processingLog=processingLog, ...))
    if (type == "ctd/itp")
        return(read.ctd.itp(file, processingLog=processingLog, ...))
    if (type == "gpx")
        return(read.gps(file, type="gpx", processingLog=processingLog, ...))
    if (type == "coastline")
        return(read.coastline(file, type="mapgen", processingLog=processingLog, ...))
    if (type == "argo")
        return(read.argo(file, ...))
    if (type == "lisst")
        return(read.lisst(file))
    if (type == "sealevel")
        return(read.sealevel(file, processingLog=processingLog, ...))
    if (type == "topo")
        return(read.topo(file))
    if (type == "RBR/dat") # FIXME: obsolete; to be removed by Fall 2015
        return(read.rsk(file, processingLog=processingLog, ...))
    if (type == "RBR/rsk")
        return(read.rsk(file, processingLog=processingLog, ...))
    if (type == "RBR/txt")
        return(read.rsk(file, processingLog=processingLog, type='txt', ...))
    if (type == "section")
        return(read.section(file, processingLog=processingLog, ...))
    if (type == "ctd/woce/other")
        return(read.ctd.woce.other(file, processingLog=processingLog, ...))
    if (type == "landsat")
        return(read.landsat(file, ...))
    if (type == "netcdf")
        return(read.netcdf(file, ...))
    if (type == "met")
        return(read.met(file, ...))
    if (type == "odf")
        return(read.odf(file, ...))
    stop("unknown file type \"", type, "\"")
}

#' Read a NetCDF File
#'
#' @details
#' Read a netcdf file, trying to interpret its contents sensibly.
#'
#' It is important to note that this is a preliminary version of
#' this function, and much about it may change without notice.
#' Indeed, it may be removed entirely.
#'
#' Below are some features that may be changed.
#'
#' 1. The names of data items are not changed from those in the netcdf
#' file on the assumption that this will offer the least surprise to
#' the user.
#'
#' 2. An attempt is made to find some common metadata from global
#' attributes in the netcdf file. These attributes include
#' \code{Longitude}, \code{Latitude}, \code{Ship} and \code{Cruise}.
#' Before they are stored in the metadata, they are converted to
#' lower case, since that is the oce convention.
#'
#' @param file the name of a file
#' @param ... unused
#'
#' @return
#' An object of \code{\link{oce-class}}.
read.netcdf <- function(file, ...)
{
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop('must install.packages("ncdf4") to read netcdf data')
    f <- ncdf4::nc_open(file)
    res <- new("oce")
    names <- names(f$var)
    data <- list()

    for (name in names) {
        ## message("name=", name)
        if (1 == length(grep("^history_", name)))
            next
        ## if (name == "history_institution" || name == "history_step" || name == "history_software"
        ##     || name == "history_software_release" || name == "history_reference" || name == "history_date"
        ##     || name == "history_action" || name == "history_parameter" || name == "history_start_pres"
        ##     || name == "history_stop_pres" || name == "history_previous_value"
        ##     || name == "history_qctest")
        ##     next
        item <- ncdf4::ncvar_get(f, name)
        if (is.array(item) && 1 == length(dim(item))) # 1D array converted to 1col matrix
            item <- matrix(item)
        data[[name]] <- item
        if (name=="TIME") {
            u <- ncdf4::ncatt_get(f, name, "units")$value
            if ("seconds since 1970-01-01 UTC" == u) {
                data[[name]] <- numberAsPOSIXct(item)
            } else {
                warning("time unit is not understood, so it remains simply numeric")
            }
        }
    }
    res@data <- data
    ## Try to get some global attributes.
    ## Inelegantly permit first letter lower-case or upper-case
    if (ncdf4::ncatt_get(f, 0, "Longitude")$hasatt)
        res@metadata$longitude <- ncdf4::ncatt_get(f, 0, "Longitude")$value
    if (ncdf4::ncatt_get(f, 0, "longitude")$hasatt)
        res@metadata$longitude <- ncdf4::ncatt_get(f, 0, "longitude")$value
    if (ncdf4::ncatt_get(f, 0, "Latitude")$hasatt)
        res@metadata$latitude <- ncdf4::ncatt_get(f, 0, "Latitude")$value
    if (ncdf4::ncatt_get(f, 0, "latitude")$hasatt)
        res@metadata$latitude <- ncdf4::ncatt_get(f, 0, "latitude")$value
    if (ncdf4::ncatt_get(f, 0, "Station")$hasatt)
        res@metadata$station <- ncdf4::ncatt_get(f, 0, "Station")$value
    if (ncdf4::ncatt_get(f, 0, "station")$hasatt)
        res@metadata$station <- ncdf4::ncatt_get(f, 0, "station")$value
    if (ncdf4::ncatt_get(f, 0, "Ship")$hasatt)
        res@metadata$ship <- ncdf4::ncatt_get(f, 0, "Ship")$value
    if (ncdf4::ncatt_get(f, 0, "ship")$hasatt)
        res@metadata$ship <- ncdf4::ncatt_get(f, 0, "ship")$value
    if (ncdf4::ncatt_get(f, 0, "Cruise")$hasatt)
        res@metadata$cruise <- ncdf4::ncatt_get(f, 0, "Cruise")$value
    if (ncdf4::ncatt_get(f, 0, "cruise")$hasatt)
        res@metadata$cruise <- ncdf4::ncatt_get(f, 0, "cruise")$value
    res@processingLog <- processingLogAppend(res@processingLog,
                                              paste("read.netcdf(\"", file, "\")", sep=""))
    res
}


#' Create a Palette of Colours
#'
#' Create a palette of colours.
#'
#' \code{oce.colorsPalette} provides a variety of pre-defined palettes.
#' \code{which}=1 yields the ColorBrewer diverging red/blue scheme while
#' \code{which}=2 yields the ColorBrewer diverging RYB scheme [1].
#'
#' A family of nine-colour schemes is as follows: \code{which="jet"} (or
#' \code{which="9A"} or \code{which=9.01} for the Jet scheme; \code{which="9B"}
#' or \code{which=9.02} for a scheme similar to Jet but omitting the green, and
#' somewhat desaturating the yellow and cyan.
#'
#' \code{\link{oce.colorsGebco}} provides palettes that mimic the GEBCO atlas colours,
#' with shades of blue for water and of brown for land.  The blue values go
#' from dark to light, and the brown ones from light to dark; in this way,
#' topographic images have light values near sea-level, and get darker in
#' either deeper water or higher terrain.
#'
#' \code{oce.colorsJet} provides a palette similar to the Matlab \dQuote{jet}
#' palette.
#'
#' \code{oce.colorsTwo} provides a two-tone palette that fades to white at
#' central values.
#'
#' \code{oce.colorsViridis} provides a matplotlib (python) colour scheme that
#' became the standard in 2015; see [2]. This is a blue-yellow transition that
#' is designed to reproduce well in black-and-white, and also to be
#' interpretable by those with certain forms of colour blindness [3, 4, 5].
#'
#' \code{oce.colorsCDOM}, \code{oce.colorsChlorophyll},
#' \code{oce.colorsDensity}, \code{oce.colorsFreesurface},
#' \code{oce.colorsOxygen}, \code{oce.colorspAR}, \code{oce.colorsPhase},
#' \code{oce.colorsSalinity}, \code{oce.colorsTemperature},
#' \code{oce.colorsTurbidity}, \code{oce.colorsVelocity} and
#' \code{oce.colorsVorticity} are based on RGB values set up by Kristen M.
#' Thyng for her Python package named \code{cmcolor} [7].
#'
#' @aliases oce.colors oceColors oce.colorsJet oceColorsJet
#' oce.colorsTwo oceColorsTwo oce.colorsPalette oceColorsPalette
#' oce.colors9A oceColors9A oce.colors9B oceColors9B oce.colorsViridis
#' oceColorsViridis oce.colorsCDOM oce.colorsChlorophyll oce.colorsDensity
#' oce.colorsFreesurface oce.colorsOxygen oce.colorsPAR oce.colorsPhase
#' oce.colorsSalinity oce.colorsTemperature oce.colorsTurbidity
#' oce.colorsVelocity oce.colorsVorticity oceColorsCDOM oceColorsChlorophyll
#' oceColorsDensity oceColorsFreesurface oceColorsOxygen oceColorsPAR
#' oceColorsPhase oceColorsSalinity oceColorsTemperature oceColorsTurbidity
#' oceColorsVelocity oceColorsVorticity
#' @param n the number of colours (\eqn{\ge 1}{>=1}) to be in the palette.
#' @param low the hue, in [0, 1], for the low end of a \code{oce.colorsTwo}
#' scale.
#' @param high the hue, in [0, 1], for the high end of a \code{oce.colorsTwo}
#' scale.
#' @param smax the maximum saturation, in [0, 1], for the colours of
#' \code{oce.colorsTwo}.
#' @param alpha the alpha value, in [0, 1], for the colours of
#' \code{oce.colorsTwo}.
#' @author Dan Kelley
#' @references [1] Color Brewer. \url{http://colorbrewer2.org/}
#'
#' [2] A blog item on the Viridis (and related) matplotlib colour scales is at
#' \url{http://bids.github.io/colormap/}.
#'
#' [3] Light, A., and P. J. Bartlein, 2004. The End of the Rainbow? Color
#' Schemes for Improved Data Graphics. \emph{Eos Trans. AGU}, 85(40),
#' doi:10.1029/2004EO400002.
#'
#' [4] Martin Jakobsson, Ron Macnab, and Members of the Editorial Board, IBCAO.
#' Selective comparisons of GEBCO (1979) and IBCAO (2000) maps.
#' \samp{https://www.ngdc.noaa.gov/mgg/bathymetry/arctic/ibcao_gebco_comp.html}.
#'
#' [5] Stephenson, David B., 2005. Comment on ``Color schemes for improved data
#' graphics,'' by A. Light and P. J. Bartlein. \emph{Eos Trans. AGU}, 86(20).
#'
#' [6] The Geography department at the University of Oregon has good resources
#' on colour schemes; see e.g.
#' \code{http://geography.uoregon.edu/datagraphics/color_scales.htm} (This URL
#' worked prior to December 8, 2015, but was found to fail on that date; it is
#' included here in case users want to search for themselves.)
#'
#' [7] The \code{cmocean} Python package, written by Kristen M Thyng, is
#' available at \url{https://github.com/kthyng/cmocean}.
#' @examples
#'
#' library(oce)
#' opar <- par(no.readonly = TRUE)
#' # 1. Show a few palettes
#' x <- array(1:1000, dim=c(1, 1000))
#' par(mfrow=c(1, 5), mar=c(1, 3, 3, 1))
#' image(x, col=oce.colorsTwo(200), main="oce.colorsTwo")
#' image(x, col=oce.colorsJet(200), main="oce.colorsJet")
#' image(x, col=oce.colorsGebco(200), main="oce.colorsGebco")
#' image(x, col=oce.colorsPalette(200), main="oce.colorsPalette")
#' image(x, col=oce.colorsViridis(200), main="oce.colorsViridis")
#'
#' # 4. Kristen M Thyng's 'cmocean' colours, specialised for oceanography.
#' par(mfrow=c(3, 4), mar=c(1, 3, 3, 1))
#' image(x, col=oce.colorsCDOM(200), main="oce.colorsCDOM")
#' image(x, col=oce.colorsChlorophyll(200), main="oce.colorsChlorophyll")
#' image(x, col=oce.colorsDensity(200), main="oce.colorsDensity")
#' image(x, col=oce.colorsFreesurface(200), main="oce.colorsFreesurface")
#' image(x, col=oce.colorsOxygen(200), main="oce.colorsOxygen")
#' image(x, col=oce.colorsPAR(200), main="oce.colorsPAR")
#' image(x, col=oce.colorsPhase(200), main="oce.colorsPhase")
#' image(x, col=oce.colorsSalinity(200), main="oce.colorsSalinity")
#' image(x, col=oce.colorsTemperature(200), main="oce.colorsTemperature")
#' image(x, col=oce.colorsTurbidity(200), main="oce.colorsTurbidity")
#' image(x, col=oce.colorsVelocity(200), main="oce.colorsVelocity")
#' image(x, col=oce.colorsVorticity(200), main="oce.colorsVorticity")
#'
#' # 3. Acoustic-Doppler profiler data; note that plot,adp-method() puts makes
#' # zlim be symmetric about zero velocity.
#' par(mfrow=c(1, 1))
#' data(adp)
#' plot(adp, which='u1')
#'
#' # 4. Contrast Jet with Viridis, using standard Volcano dataset;
#' # try printing the results in black and white.
#' par(mfrow=c(2, 1))
#' imagep(volcano, col=oce.colorsJet)
#' imagep(volcano, col=oce.colorsViridis)
#' @family things related to colors
oce.colorsTwo <- function (n, low=2/3, high=0, smax=1, alpha = 1)
{
    ## code borrows heavily from cm.color()
    if ( (n <- as.integer(n[1])) > 0 ) {
        even.n <- n%%2 == 0
        k <- n%/%2
        l1 <- k + 1 - even.n
        l2 <- n - k + even.n
        c(if (l1 > 0) hsv(h = low,
                          s = seq.int(smax, ifelse(even.n, 0.5/k, 0), length.out = l1),
                          v = 1, alpha = alpha),
          if (l2 > 1) hsv(h = high,
                          s = seq.int(0, smax, length.out = l2)[-1],
                          v = 1, alpha = alpha))
    }
    else character(0)
}
oceColorsTwo <- oce.colorsTwo

#' Gebco Colors
#' @aliases oceColorsGebco oce.colors.gebco
#' @param n Number of colors to return
#' @param region String indicating application region, one of \code{"water"}, \code{"land"},
#' or \code{"both"}.
#' @param type String indicating the purpose, one of \code{"fill"} or \code{"line"}.
#' @family things related to colors
oce.colorsGebco <- function(n=9, region=c("water", "land", "both"), type=c("fill", "line"))
{
    region <- match.arg(region)
    type <- match.arg(type)
    if (type == "fill") {
        ## generate land colors by e.g. rgb(t(col2rgb(land[5])-1*c(10, 4, 10))/255)
        land <- c("#FBC784", "#F1C37A", "#E6B670", "#DCA865", "#D19A5C",
                  "#C79652", "#BD9248", "#B38E3E", "#A98A34")
        water <- rev(c("#E1FCF7", "#BFF2EC", "#A0E8E4", "#83DEDE", "#68CDD4",
                       "#4FBBC9", "#38A7BF", "#2292B5", "#0F7CAB"))
    } else {
        land <- c("#FBC784", "#F1C37A", "#E6B670", "#DCA865", "#D19A5C",
                  "#C79652", "#BD9248", "#B38E3E", "#A98A34")
        water <- rev(c("#A4FCE3", "#72EFE9", "#4FE3ED", "#47DCF2", "#46D7F6",
                       "#3FC0DF", "#3FC0DF", "#3BB7D3", "#36A5C3"))#,"#3194B4",
                       #"#2A7CA4","#205081","#16255E","#100C2F"))
    }
    if (region == "water") {
        rgb.list <- col2rgb(water) / 255
        l <- length(water)
        r <- approx(1:l, rgb.list[1, 1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2, 1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3, 1:l], xout=seq(1, l, length.out=n))$y
    } else if (region == "land") {
        rgb.list <- col2rgb(land) / 255
        l <- length(land)
        r <- approx(1:l, rgb.list[1, 1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2, 1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3, 1:l], xout=seq(1, l, length.out=n))$y
    } else {
        ## both
        rgb.list <- col2rgb(c(water, land)) / 255
        l <- length(land) + length(water)
        r <- approx(1:l, rgb.list[1, 1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2, 1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3, 1:l], xout=seq(1, l, length.out=n))$y
    }
    rgb(r, g, b)
}
oceColorsGebco <- oce.colorsGebco


oce.colorsCLOSURE <- function(colorname) {
    function(n) {
        data("colors", package="oce", envir=environment())
        col <- get("colors")[[colorname]]
        if (missing(n) || n <= 0) colorRampPalette(col) else colorRampPalette(col)(n)
    }
}

## Viridis is python matplotlib default colormap, as of mid/late 2015.
oceColorsViridis <- oce.colorsViridis <- oce.colorsCLOSURE("viridis")
## The next are from the cmocean colour schemes, as of 2015-10-01.
oceColorsCDOM <- oce.colorsCDOM <- oce.colorsCLOSURE("cdom")
oceColorsChlorophyll <- oce.colorsChlorophyll <- oce.colorsCLOSURE("chlorophyll")
oceColorsDensity <- oce.colorsDensity <- oce.colorsCLOSURE("density")
oceColorsFreesurface <- oce.colorsFreesurface <- oce.colorsCLOSURE("freesurface")
oceColorsOxygen <- oce.colorsOxygen <- oce.colorsCLOSURE("oxygen")
oceColorsPAR <- oce.colorsPAR <- oce.colorsCLOSURE("par")
oceColorsPhase <- oce.colorsPhase <- oce.colorsCLOSURE("phase")
oceColorsSalinity <- oce.colorsSalinity <- oce.colorsCLOSURE("salinity")
oceColorsTemperature <- oce.colorsTemperature <- oce.colorsCLOSURE("temperature")
oceColorsTurbidity <- oce.colorsTurbidity <- oce.colorsCLOSURE("turbidity")
oceColorsVelocity <- oce.colorsVelocity <- oce.colorsCLOSURE("velocity")
oceColorsVorticity <- oce.colorsVorticity <- oce.colorsCLOSURE("vorticity")


## Simulation of Matlab Jet Colors
oce.colorsJet <- function(n)
{
    if (missing(n) || n <= 0)
        colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    else {
        colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(n)
    }
}
oceColorsJet <- oce.colorsJet

oce.colors9A <- function(n)
{
    oce.colorsJet(n)
}
oceColors9A <- oce.colors9A

oce.colors9B <- function(n)
{
    if (missing(n) || n <= 0)
        colorRampPalette(c("#00007F", "blue", "#007FFF", "#22e4e7",
                           "white", "#ffe45e", "#FF7F00", "red", "#7F0000"))
    else {
        colorRampPalette(c("#00007F", "blue", "#007FFF", "#22e4e7",
                           "white", "#ffe45e", "#FF7F00", "red", "#7F0000"))(n)
    }
}
oceColors9B <- oce.colors9B


oce.colorsPalette <- function(n, which=1)
{
    if ( (n <- as.integer(n[1])) > 0 ) {
        if (which == 1) {
            ## Started with http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer.html
            ## RdBu 11 divisions
            ## and then smoothed the components with smooth.spline(...,df=6)
            rgb <- matrix(c(
                            103, 000, 026,
                            178, 024, 046,
                            214, 096, 072,
                            244, 165, 136,
                            253, 219, 195,
                            247, 247, 247,
                            209, 229, 238,
                            146, 197, 226,
                            067, 147, 184,
                            033, 102, 179,
                            005, 048,  97), ncol=3, byrow=TRUE) / 255
            m <- dim(rgb)[1]
            i <- 1:m
            xout <- seq(1, m, length.out=n)
            rev(rgb(red=approx(i, rgb[, 1], xout, rule=1)$y,
                    green=approx(i, rgb[, 2], xout, rule=1)$y,
                    blue=approx(i, rgb[, 3], xout, rule=1)$y,
                    alpha=1))
        } else if (which == 2) {
            ## http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer.html
            m <- 11                         # number of classes
            r <- c(165, 215, 244, 253, 254, 255, 224, 171, 116,  69,  49) / 255
            g <- c(  0,  48, 109, 174, 224, 255, 243, 217, 173, 117,  54) / 255
            b <- c( 38,  39,  67,  97, 144, 191, 248, 233, 209, 180, 149) / 255
            i <- 1:m
            xout <- seq(1, m, length.out=n)
            rev(rgb(approx(i, r, xout, rule=1)$y,
                    approx(i, g, xout, rule=1)$y,
                    approx(i, b, xout, rule=1)$y))
        } else if (which == 9.01 || which == "9A" || which == "jet") {
            ## jet, also known as 9A or 9.01
            oce.colorsJet(n)
        } else if (which == 9.02 || which == "9B") {
            oce.colors9B(n)
        } else stop("unknown which")
    }
    else character(0)
}
oceColorsPalette <- oce.colorsPalette


#' Oce Version of axis.POSIXct
#'
#' A specialized variant of \code{\link{axis.POSIXct}} that produces
#' results with less ambiguity in axis labels.
#'
#' The tick marks are set automatically based on examination of the time range on
#' the axis. The scheme was devised by constructing test cases with a typical plot
#' size and font size, and over a wide range of time scales. In some categories,
#' both small tick marks are interspersed between large ones.
#'
#' The user may set the format of axis numbers with the \code{tformat} argument.
#' If this is not supplied, the format is set based on the time span of the axis:
#'
#' \itemize{
#'
#' \item If this time span is less than a minute, the time axis labels are in
#' seconds (fractional seconds, if the interval is less than 2 seconds), with
#' leading zeros on small integers. (Fractional seconds are enabled with a trick:
#' the usual R format \code{"\%S"} is supplemented with a new format e.g.
#' \code{"\%.2S"}, meaning to use two digits after the decimal.)
#'
#' \item If the time span exceeds a minute but is less than 1.5 days, the label
#' format is \code{"\%H:\%M:\%S"}.
#'
#' \item If the time span exceeds 1.5 days but is less than 1 year, the format is
#' \code{"\%b \%d"} (e.g. Jul 15) and, again, the tick marks are set up for several
#' subcategories.
#'
#' \item If the time span exceeds a year, the format is \code{"\%Y"}, i.e. the year
#' is displayed with 4 digits.
#'
#' }
#'
#' It should be noted that this scheme differs from the R approach in several
#' ways. First, R writes day names for some time ranges, in a convention that is
#' seldom seen in the literature. Second, R will write nn:mm for both HH:MM and
#' MM:SS, an ambiguity that might confuse readers. Third, the use of both large
#' and small tick marks is not something that R does.
#'
#' Bear in mind that \code{tformat} may be set to alter the number format, but
#' that the tick mark scheme cannot (presently) be controlled.
#'
#' @param side as for \code{\link{axis.POSIXct}}.
#' @param x as for \code{\link{axis.POSIXct}}.
#' @param at as for \code{\link{axis.POSIXct}}.
#' @param tformat as \code{format} for \code{\link{axis.POSIXct}} for now, but
#' may eventually have new features for multiline labels, e.g. day on one line
#' and month on another.
#'
#' @param labels as for \code{\link{axis.POSIXct}}.
#'
#' @param drawTimeRange Optional indication of whether/how to draw the time range
#' in the margin on the side of the the plot opposite the time axis. If this is
#' not supplied, it defaults to the value returned by
#' \code{\link{getOption}("oceDrawTimeRange")}, and if that option is not set,
#' it defaults to \code{TRUE}. No time range is drawn if \code{drawTimeRange} is \code{FALSE}.
#' If it is \code{TRUE}, the range will be shown. This range refers to
#' range of the x axis (not the data). The format of the elements of that range is set by
#' \code{\link{getOption}("oceTimeFormat")} (or with the default value
#' of an empty string, if this option has not been set). The timezone will
#' be indicated if the time range is under a week.  For preliminary work, it makes
#' sense to use \code{drawTimeRange=TRUE}, but for published work it can be better
#' to drop this label and indicate something about the time in the figure caption.
#'
#' @param drawFrequency boolean, \code{TRUE} to show the frequency of sampling
#' in the data
#'
#' @param abbreviateTimeRange boolean, \code{TRUE} to abbreviate the second
#' number in the time range, e.g. dropping the year if it is the same in the
#' first number.
#' @param cex size of labels on axes; see \code{\link[graphics]{par}}("cex").
#' @param cex.axis see \code{\link[graphics]{par}}("cex.axis").
#' @param cex.main see \code{\link[graphics]{par}}("cex.main").
#' @param mar value for \code{par(mar)} for axis
#' @param mgp value for \code{par(mgp)} for axis
#' @param main title of plot
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @param \dots as for \code{\link{axis.POSIXct}}.
#' @return A vector of times corresponding to axis ticks is returned silently.
#' @author Dan Kelley
#' @seealso This is used mainly by \code{\link{oce.plot.ts}}.
oce.axis.POSIXct <- function (side, x, at, tformat, labels = TRUE,
                              drawTimeRange,
                              abbreviateTimeRange=FALSE, drawFrequency=FALSE,
                              cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                              mar=par("mar"),
                              mgp=par("mgp"),
                              main="",
                              debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "oce.axis.POSIXct(...,debug=", debug, ",...) {\n", sep="", unindent=1)
    oceDebug(debug, "mar=", mar, "\n")
    oceDebug(debug, "mgp=", mgp, "\n")
    oceDebug(debug, "cex=", cex, " cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
    oceDebug(debug, vectorShow(x, "x"))
    tformatGiven <- !missing(tformat)
    if (missing(drawTimeRange))
        drawTimeRange <- getOption("oceDrawTimeRange")
    ## This was written because axis.POSIXt in R version 2.8.x did not obey the
    ## time zone in the data.  (Version 2.9.0 obeys the time zone.)
    if (missing(x))
        x <- numberAsPOSIXct(par('usr')[1:2])
    dots <- list(...)
    if ("xlim" %in% names(dots)) {
        ok <- dots$xlim[1] <= x & x <= dots$xlim[2]
        x <- x[ok]
    }
    mat <- missing(at) || is.null(at)
    if (!mat) x <- as.POSIXct(at) else x <- as.POSIXct(x)
    range <- par("usr")[if (side%%2) 1:2 else 3:4]
    d <- range[2] - range[1]            # time span, in seconds
    z <- c(range, x[is.finite(x)])
    attr(z, "tzone") <- attr(x, "tzone") # need this because c() makes it local time zone (!)
    rr <- range + as.POSIXct("2000-01-20") - as.numeric(as.POSIXct("2000-01-20"))
    attr(rr, "tzone") <- attr(x, "tzone")
    oceDebug(debug, "range=",
              format.POSIXct(rr[1], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC to ",
              format.POSIXct(rr[2], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC\n")
    z.sub <- NULL # unlabelled tics may be set in some time ranges, e.g. hours, for few-day plots
    oceDebug(debug, "d=", d, " (time range)\n")
    if (d <= 2) {
        oceDebug(debug, "Time range is under 2 sec\n")
        ## The time rounding will fail for very small time intervals;
        ## a wider range can be added easily.
        t.start <- rr[1]
        t.end <- rr[2]
        span <- as.numeric(t.end) - as.numeric(t.start)
        if (     span > 1     ) round <- 0.5
        else if (span > 0.1   ) round <- 0.05
        else if (span > 0.01  ) round <- 0.005
        else if (span > 0.001 ) round <- 0.0005
        else if (span > 0.0001) round <- 0.00005
        else round <- 0.00001
        t0 <- trunc(t.start, "sec")
        t.start <- t0 + round * floor( (as.numeric(t.start) - as.numeric(t0)) / round )
        t.end <- t0 + round * floor( (as.numeric(t.end) - as.numeric(t0)) / round )
        z <- seq(t.start, t.end, by=round)
        oceDebug(debug, vectorShow(z, "TIME RANGE is under 2 seconds; z="))
        ## BOOKMARK 1A
        if (missing(tformat)) {
            tformat <- "%.1S" # NOTE: this .1 is interpreted at BOOKMARK 1B
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60) {
        oceDebug(debug, "Time range is between 2 sec and 1 min\n")
        t.start <- trunc(rr[1]-1, "secs")
        t.end <- trunc(rr[2]+1, "secs")
        z <- seq(t.start, t.end, by="1 sec")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%S"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60 * 3) {
        oceDebug(debug, "Time range is between 1 min and 3 min\n")
        t.start <- trunc(rr[1]-60, "mins")
        t.end <- trunc(rr[2]+60, "mins")
        z <- seq(t.start, t.end, by="10 sec")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%M:%S"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60 * 30) {
        oceDebug(debug, "Time range is between 3 min and 30 min\n")
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="min")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%M:%S"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60 * 60) {
        oceDebug(debug, "Time range is between 30 min and 1 hour\n")
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%M:%S"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60 * 60 * 2) {
        oceDebug(debug, "Time range is between 1 and 2 hours\n")
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%H:%M:%S"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60 * 60 * 6) {
        oceDebug(debug, "Time range is between 2 and 6 hours\n")
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 3600, "hour")
        z <- seq(t.start, t.end, by="30 min")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%H:%M:%S"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60 * 60 * 24 * 1.5) {
        oceDebug(debug, "Time range is between 6 hours and 1.5 days\n")
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 86400, "hour")
        z <- seq(t.start, t.end, by="2 hour")
        z.sub <- seq(t.start, t.end, by="hour")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%H:%M:%S"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60 * 60 * 24 * 5) {
        oceDebug(debug, "Time range is between 1.5 and 5 days\n")
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        z.sub <- seq(t.start, t.end, by="6 hour")
        oceDebug(debug, vectorShow(z))
        oceDebug(debug, vectorShow(z.sub))
        if (missing(tformat)) {
            tformat <- "%b %d"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60 * 60 * 24 * 14) {
        oceDebug(debug, "Time range is between 4 days and 2 weeks\n")
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        z.sub <- seq(t.start, t.end, by="12 hour")
        oceDebug(debug, vectorShow(z))
        oceDebug(debug, vectorShow(z.sub))
        if (missing(tformat)) {
            tformat <- "%b %d"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 60 * 60 * 24 * 31) {
        oceDebug(debug, "Time range is between 2 weeks and 1 month (defined as 31 days)\n")
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="week")
        z.sub <- seq(t.start, t.end, by="day")
        oceDebug(debug, vectorShow(z))
        oceDebug(debug, vectorShow(z.sub))
        if (missing(tformat)) {
            tformat <- "%b %d"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d < 60 * 60 * 24 * 31 * 2) {
        oceDebug(debug, "Time range is between 1 and 2 months (defined as 31 days)\n")
        t.start <- trunc(rr[1], "days")
        t.end <- trunc(rr[2] + 86400, "days")
        z <- seq(t.start, t.end, by="week") # big ticks
        z.sub <- seq(t.start, t.end, by="day") # small ticks
        oceDebug(debug, vectorShow(z))
        oceDebug(debug, vectorShow(z.sub))
        if (missing(tformat)) {
            tformat <- "%b %d"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d < 60 * 60 * 24 * 31 * 4) {
        oceDebug(debug, "Time range is between 2 and 4 months (defined as 31 days)\n")
        t.start <- trunc(rr[1], "days")
        t.end <- trunc(rr[2] + 86400, "days")
        z <- seq(t.start, t.end, by="week")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%b %d"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d < 1.1 * 60 * 60 * 24 * 365) {
        oceDebug(debug, "Time range is between 4 months and 1 year\n")
        rrl <- as.POSIXlt(rr)
        rrl[1]$mday <- 1
        rrl[2] <- rrl[2] + 31 * 86400
        rrl[2]$mday <- 1
        t.start <- trunc(rrl[1], "day")
        t.end <- trunc(rrl[2] + 86400, "day")
        z <- seq(t.start, t.end, by="month")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%b %d"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d < 3.1 * 60 * 60 * 24 * 365) {
        oceDebug(debug, "Time range is between 1 and 3 years\n")
        rrl <- as.POSIXlt(rr)
        rrl[1]$mday <- 1
        rrl[2] <- rrl[2] + 31 * 86400
        rrl[2]$mday <- 1
        t.start <- trunc(rrl[1], "days")
        t.end <- trunc(rrl[2], "days")
        z <- seq(t.start, t.end, by="3 month")
        oceDebug(debug, vectorShow(z))
        z.sub <- seq(t.start, t.end, by="month") # small ticks
        oceDebug(debug, vectorShow(z.sub))
        if (missing(tformat)) {
            tformat <- "%Y %b"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else {
        oceDebug(debug, "Time range is longer than 3 years\n")
        class(z) <- c("POSIXt", "POSIXct")
        tz <- attr(x, "tzone")
        attr(z, "tzone") <- tz
        zz <- unclass(as.POSIXlt(z, tz=tz))
        zz$mday <- zz$wday <- zz$yday <- 1
        zz$isdst <- -1
        zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year)
        M <- length(zz$year)
        zz <- lapply(zz, function(x) rep(x, length.out = M))
        class(zz) <- c("POSIXt", "POSIXlt")
        z <- as.POSIXct(zz, tz=tz)
        attr(z, "tzone") <- attr(x, "tzone")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%Y"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
        oceDebug(debug, vectorShow(z, "z="))
    }
    if (!mat)
        z <- x[is.finite(x)]
    ##
    ## FIXME: I was twiddling the numbers, to get more labels, but xaxs="r" fixes that.
    twiddle <- 0.04 * diff(as.numeric(range))  # FIXME: do I need this anymore?
    oceDebug(debug, "range=",
              format.POSIXct(rr[1], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC to ",
              format.POSIXct(rr[2], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC\n")
    keep <- range[1] <= (z + twiddle) & (z - twiddle) <= range[2]
    ##oceDebug(debug, vectorShow(keep, "keep"))
    oceDebug(debug, vectorShow(z, "z before keep"))
    z <- z[keep]
    oceDebug(debug, vectorShow(z, "z after keep"))
    if (!is.logical(labels)) {
        labels <- labels[keep]
    } else if (labels[1]) {
        if (length(grep("[0-9]+S.*", tformat))) {
            ## BOOKMARK 1B a special trick to get fractional seconds (cf BOOKMARK 1A)
            old <- options("digits.secs")$digits.secs
            n <- as.numeric(gsub("^%.*\\.([0-9]*)S.*", "\\1", tformat))
            options(digits.secs=n)
            labels <- format(z) # "2016-01-01 hh:mm:ss.digits"
            labels <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:", "", labels)
            options(digits.secs=old)
        } else {
            labels <- format(z, format=tformat)
        }
    } else if (!labels[1]) {
        labels <- rep("", length(z))
    }
    oceDebug(debug, vectorShow(labels, n=-1))
    oceDebug(debug, vectorShow(format(z), n=-1))
    oceDebug(debug, vectorShow(z, n=-1))
    if (drawTimeRange) {
        time.range <- par("usr")[1:2]   # axis, not data
        class(time.range) <- c("POSIXt", "POSIXct")
        attr(time.range, "tzone") <- attr(x, "tzone")[1]
        time.range <-  as.POSIXlt(time.range)
        ## time.range.data <- range(x, na.rm=TRUE)
        ## what was this for?# time.range[1] <- max(time.range[1], time.range.data[1], na.rm=TRUE)
        ## what was this for?# time.range[2] <- min(time.range[2], time.range.data[2], na.rm=TRUE)
        if (!is.null(getOption("oceTimeFormat"))) {
            tr1 <- format(time.range[1], getOption("oceTimeFormat"))
            tr2 <- format(time.range[2], getOption("oceTimeFormat"))
        } else {
            tr1 <- format(time.range[1])
            tr2 <- format(time.range[2])
        }
        if (abbreviateTimeRange) {
            if (time.range[1]$year == time.range[2]$year) {
                tr2 <- substr(tr2, 6, nchar(tr2)) # remove the "YYYY-"
                if (time.range[1]$mon == time.range[2]$mon) {
                    tr2 <- substr(tr2, 4, nchar(tr2)) # remove the "MM-"
                    if (time.range[1]$mday == time.range[2]$mday) {
                        tr2 <- substr(tr2, 4, nchar(tr2)) # remove the "DD-"
                    }
                }
            }
            time.range <- as.POSIXct(time.range)
        }
        deltat <- mean(diff(as.numeric(x)), na.rm=TRUE)
        ## only show timezone if hours are shown
        oceDebug(debug, "time.range[1]:", format(time.range[1]), "\n")
        oceDebug(debug, "round(time.range[1], 'days'):", format(round(time.range[1], 'days')), "\n")
        oceDebug(debug, "time.range[2]:", format(time.range[2]), "\n")
        oceDebug(debug, "round(time.range[2], 'days'):", format(round(time.range[2], 'days')), "\n")
        ## The below is not fool-proof, depending on how xlim might have been supplied; see
        ##    https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14449
        if (diff(as.numeric(time.range)) > 7*86400) {
            label <- paste(tr1, tr2, sep=" to ")
        } else {
            label <- paste(tr1, attr(time.range[1], "tzone")[1], " to ", tr2,  attr(time.range[2], "tzone")[1], sep="")
        }
        if (drawFrequency && is.finite(1/deltat))
            label <- paste(label, "@", sprintf("%.4g Hz", 1/deltat), sep=" ")
        oceDebug(debug, "label=", label, "\n")
        mtext(label, side=if (side==1) 3 else 1, cex=0.9*cex.axis*par('cex'), adj=0)
        oceDebug(debug, "cex.axis=", cex.axis, "; par('cex')=", par('cex'), "\n")
    }
    if (nchar(main) > 0) {
        mtext(main, side=if (side==1) 3 else 1, cex=cex.axis*par('cex'), adj=1)
    }
    oceDebug(debug, vectorShow(z, "z="))
    if (length(z.sub) > 0) {
        axis(side, at = z.sub, line=0, labels = FALSE, tcl=-0.25)
        oceDebug(debug, vectorShow(z.sub, "z.sub="))
    }
    oceDebug(debug, vectorShow(labels, "labels="))
    ##ocex <- par('cex')
    ocex.axis <- par('cex.axis')
    ocex.main <- par('cex.main')
    omgp <- par('mgp')
    par(cex.axis=cex.axis, cex.main=cex.main, mgp=mgp, tcl=-0.5)
    ##axis(side, at=z, line=0, labels=labels, cex=cex, cex.axis=cex.axis, cex.main=cex.main, mar=mar, mgp=mgp)

    ## If the user did gave tformat, shorten the strings for aesthetic reasons.
    if (!tformatGiven) {
        oceDebug(debug, "axis labels before shortenTimeString(): '", paste(labels, "', '"), "'\n")
        labels <- shortenTimeString(labels, debug=debug-1)
        oceDebug(debug, "axis labels after shortenTimeString(): '", paste(labels, "', '"), "'\n")
    }
    axis(side, at=z, line=0, labels=labels, mgp=mgp, cex.main=cex.main, cex.axis=cex.axis, ...)
    par(cex.axis=ocex.axis, cex.main=ocex.main, mgp=omgp)
    oceDebug(debug, "} # oce.axis.ts()\n", unindent=1)
    zzz <- as.numeric(z)
    if (1 < length(zzz)) {
        xaxp <- c(min(zzz, na.rm=TRUE), max(zzz, na.rm=TRUE), -1+length(zzz))
        par(xaxp=xaxp)
    }
    invisible(z)                       # FIXME: or z.sub?
}


#' Convert a Numeric Time to Hour, Minute, and Second
#'
#' @param t a vector of factors or character strings, in the format 1200 for
#' 12:00, 0900 for 09:00, etc.
#' @param default value to be used for the returned hour, minute and second if
#' there is something wrong with the input value (e.g. its length exceeds 4
#' characters, or it contains non-numeric characters)
#' @return A list containing \code{hour}, \code{minute}, and \code{second}, the
#' last of which is always zero.
#' @author Dan Kelley
#' @examples
#'
#' t <- c("0900", "1234")
#' numberAsHMS(t)
#'
#' @family things related to time
numberAsHMS <- function(t, default=0)
{
    if ("factor" == class(t))
        t <- as.character(t)
    if (!is.character(t))
        stop("can only handle strings or factors")
    nc <- nchar(t)
    t[nc == 0] <- "0000"
    nc <- nchar(t)
    t[nc==1] <- paste("000", t[nc==1], sep="")
    t[nc==2] <- paste("00", t[nc==2], sep="")
    t[nc==3] <- paste("0", t[nc==3], sep="")
    nc <- nchar(t)
    warn <- options('warn')$warn
    options(warn=-1)
    try({
        hour <- as.numeric(substr(t, 1, 2))
        minute <- as.numeric(substr(t, 3, 4))
        second <- rep(0, length(hour))
    }, silent=TRUE)
    options(warn=warn)
    bad <- nc > 4 | grepl('[^[0-9]]*', t)
    hour[bad] <- default
    minute[bad] <- default
    second[bad] <- default
    list(hour=hour, minute=minute, second=second)
}


#' Convert a Numeric Time to a POSIXct Time
#'
#' There are many varieties, according to the value of \code{type} as defined
#' in \sQuote{Details}.
#'
#' \itemize{
#'
#' \item \code{"unix"} employs Unix times, measured in seconds since the start
#' of the year 1970.
#'
#' \item \code{"matlab"} employs Matlab times, measured in days since what
#' MathWorks [1] calls ``January 0, 0000'' (i.e.  \code{ISOdatetime(0, 1, 1, 0,
#' 0, 0)} in R notation).
#'
#' \item \code{"gps"} employs the GPS convention. For this, \code{t} is a
#' two-column matrix, with the first column being the the GPS "week"
#' (referenced to 1999-08-22) and the second being the GPS "second" (i.e. the
#' second within the week). Since the GPS satellites do not handle leap
#' seconds, the R-defined \code{.leap.seconds} is used for corrections.
#'
#' \item \code{"argo"} employs Argo times, measured in days since the start of
#' the year 1900.
#'
#' \item \code{"ncep1"} employs NCEP times, measured in hours since the start
#' of the year 1800.
#'
#' \item \code{"ncep2"} employs NCEP times, measured in days since the start of
#' the year 1. (Note that, for reasons that are unknown at this time, a simple
#' R expression of this definition is out by two days compared with the UDUNITS
#' library, which is used by NCEP. Therefore, a two-day offset is applied. See
#' [2, 3].)
#'
#' \item \code{"sas"} employs SAS times, indicated by \code{type="sas"}, have
#' origin at the start of 1960.
#'
#' \item \code{"spss"} employs SPSS times, in seconds after 1582-10-14.
#'
#' \item \code{"yearday"} employs a convention in which \code{t} is a
#' two-column matrix, with the first column being the year, and the second the
#' yearday (starting at 1 for the first second of January 1, to match the
#' convention used by Sea-Bird CTD software).
#'
#' }
#'
#' @param t an integer corresponding to a time, in a way that depends on
#' \code{type}.
#' @param type the type of time (see \dQuote{Details}).
#' @param tz a string indicating the time zone, used only for unix and matlab
#' times, since GPS times are always referenced to the UTC timezone.
#' @return A \code{\link{POSIXct}} time vector.
#' @author Dan Kelley
#' @seealso \code{\link{numberAsHMS}}
#' @references [1] Matlab times:
#' \url{http://www.mathworks.com/help/matlab/ref/datenum.html}
#'
#' [2] NCEP times: \url{https://www.esrl.noaa.gov/psd/data/gridded/faq.html#3}
#'
#' [3] problem with NCEP times:
#' \url{https://github.com/dankelley/oce/issues/738}
#' @examples
#'
#' numberAsPOSIXct(0)                     # unix time 0
#' numberAsPOSIXct(1, type="matlab")      # matlab time 1
#' numberAsPOSIXct(cbind(566, 345615), type="gps") # Canada Day, zero hour UTC
#' numberAsPOSIXct(cbind(2013, 0), type="yearday") # start of 2013
#'
#' @family things related to time
numberAsPOSIXct <- function(t, type=c("unix", "matlab", "gps", "argo",
                                      "ncep1", "ncep2",
                                      "sas", "spss", "yearday"), tz="UTC")
{
    type <- match.arg(type)
    if (type == "unix") {
        tref <- as.POSIXct("2000-01-01", tz=tz) # arbitrary
        return(tref + as.numeric(t) - as.numeric(tref))
    } else if (type == "matlab") {
        ## R won't take a day "0", so subtract one
        return(as.POSIXct(ISOdatetime(0000, 01, 01, 0, 0, 0, tz=tz) + 86400 * (t - 1)))
    } else if (type == "yearday") {
        if (2 != ncol(t))
            stop("'t' must have two columns, one for year, the other for yearday")
        return(ISOdatetime(t[, 1], 1, 1, 0, 0, 0, tz=tz) + 1 + t[, 2] * 24 * 3600)
    } else if (type == "argo") {
        return(t * 86400 + as.POSIXct("1900-01-01 00:00:00", tz="UTC"))
    } else if (type == "ncep1") {
        ## hours since the start of 1800
        return(t * 3600 + as.POSIXct("1800-01-01 00:00:00", tz="UTC"))
    } else if (type == "ncep2") {
        ## days since 1-1-1 00:00:0.0 (supposedly, but offset to match a test case; see
        resOriginal <- t * 86400 + as.POSIXct("0001-01-01 00:00:00", tz="UTC")
        return(resOriginal - 2 * 86400) # kludge for ht of https://github.com/dankelley/oce/issues/738
    } else if (type == "gps") {
        if (!is.matrix(t) || dim(t)[2] != 2)
            stop("for GPS times, 't' must be a two-column matrix, with first col the week, second the second")

        ## Account for leap seconds since the GPS start time in 1980 (for the present week wraparound grouping).
        ##20171014 See http://en.wikipedia.org/wiki/Leap_second and other sources for a list.  Updates can happen
        ##20171014 # on June 30 and December 31 of any given year.  The information below was last updated
        ##20171014 # in January, 2017.
        ##20171014 # leapsOLD <- as.POSIXct(strptime(c("1981-07-01", "1982-07-01", "1983-07-01", "1985-07-01", "1987-12-31",
        ##20171014 #                                   "1989-12-31", "1990-12-31", "1992-07-01", "1993-07-01", "1994-07-01",
        ##20171014 #                                   "1995-12-31", "1997-07-01", "1998-12-31", "2005-12-31", "2008-12-31",
        ##20171014 #                                   "2012-07-01", "2015-07-01", "2016-12-31"),
        ##20171014 #                                 format="%Y-%m-%d", tz="UTC"))
        ##20171014 message("leapsOLD ", paste(leapsOLD, collapse=" "))
        leaps <- as.POSIXlt(.leap.seconds, tz="UTC")
        ##20171014 message("leaps A ", paste(leaps, collapse=" "))
        leaps <- leaps[leaps > as.POSIXlt("1980-01-01 00:00:00", tz="UTC")]
        ##20171014 message("leaps B ", paste(leaps, collapse=" "))
        leaps <- leaps[leaps > as.POSIXlt("1980-01-01 00:00:00", tz="UTC")]
        ##20171014 message("leaps C ", paste(leaps, collapse=" "))
        t <- as.POSIXct("1999-08-22 00:00:00", tz="UTC") + 86400*7*t[, 1] + t[, 2]
        ##>message("initially, t=", paste(t, collapse=" "))
        for (l in 1:length(leaps)) {
            t <- t - ifelse(t >= leaps[l], 1, 0)
            ##20171014 message("l=", l, ", leaps[l]=", leaps[l],
            ##20171014         ", t=", paste(t, collapse=" "), ", t>=leaps[l] ", t>=leaps[l])
        }
        ##20171014 print(leapsOLD - leaps) # mostly 0 but a few one-day shifts; I trust .leap.seconds more
    } else if (type == "spss") {
        t <- as.POSIXct(t, origin="1582-10-14", tz=tz)
    } else if (type == "sas") {
        t <- as.POSIXct(t, origin="1960-01-01", tz=tz)
    } else {
        stop("type must be \"unix\", \"matlab\" or \"GPS\"")
    }
    t
}


#' Plot an Inset Diagram
#'
#' Adds an inset diagram to an existing plot.  Note that if the inset is a map
#' or coastline, it will be necessary to supply \code{inset=TRUE} to prevent
#' the inset diagram from occupying the whole device width.  After
#' \code{plotInset()} has been called, any further plotting will take place
#' within the inset, so it is essential to finish a plot before drawing an
#' inset.
#'
#' @param xleft location of left-hand of the inset diagram, in the existing
#' plot units.  (PROVISIONAL FEATURE: this may also be \code{"bottomleft"}, to
#' put the inset there.  Eventually, other positions may be added.)
#' @param ybottom location of bottom side of the inset diagram, in the existing
#' plot units.
#' @param xright location of right-hand side of the inset diagram, in the
#' existing plot units.
#' @param ytop location of top side of the inset diagram, in the existing plot
#' units.
#' @param expr An expression that draws the inset plot.  This may be a single
#' plot command, or a sequence of commands enclosed in curly braces.
#' @param mar margins, in line heights, to be used at the four sides of the
#' inset diagram.  (This is often helpful to save space.)
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' ## power law in linear and log form
#' x <- 1:10
#' y <- x^2
#' plot(x, y, log='xy',type='l')
#' plotInset(3, 1, 10, 8,
#'           expr=plot(x,y,type='l',cex.axis=3/4,mgp=c(3/2, 1/2, 0)),
#'           mar=c(2.5, 2.5, 1, 1))
#'
#' ## CTD data with location
#' data(ctd)
#' plot(ctd, which="TS")
#' plotInset(29.9, 2.7, 31, 10,
#'           expr=plot(ctd, which='map',
#'           coastline="coastlineWorld",
#'           span=5000, mar=NULL, cex.axis=3/4))
plotInset <- function(xleft, ybottom, xright, ytop, expr,
                      mar=c(2, 2, 1, 1),
                      debug=getOption("oceDebug"))
{
    opar <- par(no.readonly=TRUE)
    mai <- par('mai')                  # bottom left top right
    ##omfg <- par('mfg')                 # original mfg
    xLog <- par('xlog')
    yLog <- par('ylog')
    usr <- par('usr')                  # xmin xmax ymin ymax
    fin <- par('fin') # figure width height
    x2in <- function(x) {
        if (xLog)
            mai[2] + (log10(x) - usr[1]) * (fin[1]-mai[2]-mai[4]) / (usr[2]-usr[1])
        else
            mai[2] + (x-usr[1]) * (fin[1]-mai[2]-mai[4]) / (usr[2]-usr[1])
    }
    y2in <- function(y) {
        if (yLog)
            mai[1] + (log10(y) - usr[3]) * (fin[2]-mai[1]-mai[3]) / (usr[4]-usr[3])
        else
            mai[1] + (y-usr[3]) * (fin[2]-mai[1]-mai[3]) / (usr[4]-usr[3])
    }

    if (is.character(xleft)) {
        if (xleft != "bottomleft")
            stop("only named position is \"bottomleft\"")
        f1 <- 0.02
        f2 <- 1/3
        if (xLog) {
            stop("cannot handle xlog yet")
        } else {
            xleft <- usr[1] + f1 * (usr[2] - usr[1])
            xright <- usr[1] + f2 * (usr[2] - usr[1])
        }
        if (yLog) {
            stop("cannot handle ylog yet")
        } else {
            ybottom <- usr[3] + f1 * (usr[4] - usr[3])
            ytop <- usr[3] + f2 * (usr[4] - usr[3])
        }
    } else {
        oceDebug(debug, "plotInset(xleft=", xleft, ", ybottom=", ybottom,
                 ", xright=", xright, ", ytop=", ytop,
                 ",  ...) {\n",
                 sep="", unindent=1)
    }
    oceDebug(debug, "par('mfg')=", par('mfg'), "\n")
    oceDebug(debug, "par('mai')=", par('mai'), '\n')
    oceDebug(debug, "par('usr')=", par('usr'), '\n')
    oceDebug(debug, "par('fin')=", fin, "(figure width and height)\n")
    nmai <- c(y2in(ybottom), x2in(xleft), fin[2]-y2in(ytop), fin[1]-x2in(xright))
    oceDebug(debug, "nmai:", nmai, "\n")
    if (any(nmai < 0)) {
        warning("part of the inset is off the page")
    }
    nmai[nmai<0] <- 0
    if (nmai[1] < 0) nmai[1] <- fin[1]
    if (nmai[2] < 0) nmai[2] <- fin[1]
    if (nmai[3] > fin[2] - 0.2) nmai[3] <- fin[2] - 0.2
    if (nmai[4] > fin[1] - 0.2) nmai[4] <- fin[1] - 0.2
    oceDebug(debug, "nmai:", nmai, "(after trimming negatives)\n")
    ##mfg2 <- par('mfg')
    par(new=TRUE, mai=nmai)
    thismar <- par('mar')
    par(mar=thismar+mar)
    if (debug > 1) {
        cat("\n\nBEFORE expr, PAR IS:\n");
        str(par())
    }
    mfg <- par('mfg')
    oceDebug(debug, "BEFORE expr, mfg=", mfg, "\n")
    ## Draw the inset plot (or perform any action, actually)
    expr
    if (mfg[1] == mfg[3] && mfg[2] == mfg[4]) {
        oceDebug(debug, "setting new=FALSE; mfg=", mfg, "\n")
        par(new=FALSE)
    } else {
        oceDebug(debug, "setting new=TRUE; mfg=", mfg, "\n")
        par(new=TRUE)
    }
    ## Reset some things that could have been set in the inset, and
    ## then adjust 'new' appropriately.
    par(usr=opar$usr, mai=opar$mai, cex=opar$cex, lwd=opar$lwd, lty=opar$lty, bg=opar$bg)
    oceDebug(debug, "} # plotInset()\n", unindent=1)
    invisible()
}


#' Oce Version of as.POSIXct
#'
#' @details
#' Each format in \code{timeFormats} is used in turn as the \code{format}
#' argument to \code{\link{as.POSIXct}}, and the first that produces a
#' non-\code{NA} result is used.  If \code{timeFormats} is missing, the
#' following formats are tried, in the stated order:
#'
#' \itemize{
#'
#' \item \code{"\%b \%d \%Y \%H:\%M:\%S"} (e.g. \code{"Jul 1 2013 01:02:03"})
#'
#' \item \code{"\%b \%d \%Y"} (e.g. \code{"Jul 1 2013"})
#'
#' \item \code{"\%B \%d \%Y \%H:\%M:\%S"} (e.g. \code{"July 1 2013 01:02:03"})
#'
#' \item \code{"\%B \%d \%Y"} (e.g. \code{"July 1 2013"})
#'
#' \item \code{"\%d \%b \%Y \%H:\%M:\%S"} (e.g. \code{"1 Jul 2013 01:02:03"})
#'
#' \item \code{"\%d \%b \%Y"} (e.g. \code{"1 Jul 2013"})
#'
#' \item \code{"\%d \%B \%Y \%H:\%M:\%S"} (e.g. \code{"1 July 2013 01:02:03"})
#'
#' \item \code{"\%d \%B \%Y"} (e.g. \code{"1 July 2013"})
#'
#' \item \code{"\%Y-\%m-\%d \%H:\%M:\%S"} (e.g.  \code{"2013-07-01 01:02:03"})
#'
#' \item \code{"\%Y-\%m-\%d"} (e.g. \code{"2013-07-01"})
#'
#' \item \code{"\%Y-\%b-\%d \%H:\%M:\%S"} (e.g.  \code{"2013-July-01 01:02:03"})
#'
#' \item \code{"\%Y-\%b-\%d"} (e.g.  \code{"2013-Jul-01"})
#'
#' \item \code{"\%Y-\%B-\%d \%H:\%M:\%S"} (e.g. \code{"2013-July-01 01:02:03"})
#'
#' \item \code{"\%Y-\%B-\%d"} (e.g. \code{"2013-July-01"})
#'
#' \item \code{"\%d-\%b-\%Y \%H:\%M:\%S"} (e.g.  \code{"01-Jul-2013 01:02:03"})
#'
#' \item \code{"\%d-\%b-\%Y"} (e.g. \code{"01-Jul-2013"})
#'
#' \item \code{"\%d-\%B-\%Y \%H:\%M:\%S"} (e.g. \code{"01-July-2013 01:02:03"})
#'
#' \item \code{"\%d-\%B-\%Y"} (e.g. \code{"01-July-2013"})
#'
#' \item \code{"\%Y/\%b/\%d \%H:\%M:\%S"} (e.g. \code{"2013/Jul/01 01:02:03"})
#'
#' \item \code{"\%Y/\%b/\%d"} (e.g. \code{"2013/Jul/01"})
#'
#' \item \code{"\%Y/\%B/\%d \%H:\%M:\%S"} (e.g. \code{"2013/July/01 01:02:03"})
#'
#' \item \code{"\%Y/\%B/\%d"} (e.g. \code{"2013/July/01"})
#'
#' \item \code{"\%Y/\%m/\%d \%H:\%M:\%S"} (e.g. \code{"2013/07/01 01:02:03"})
#'
#' \item \code{"\%Y/\%m/\%d"} (e.g. \code{"2013/07/01"})
#'
#' }
#'
#' @param time Character string with an indication of the time.
#' @param timeFormats Optional vector of time formats to use, as for \code{\link{as.POSIXct}}.
#' @param tz Time zone.
#' @return A time as returned by \code{\link{as.POSIXct}}.
#' @author Dan Kelley
#' @examples
#' decodeTime("July 1 2013 01:02:03")
#' decodeTime("Jul 1 2013 01:02:03")
#' decodeTime("1 July 2013 01:02:03")
#' decodeTime("1 Jul 2013 01:02:03")
#' decodeTime("2013-07-01 01:02:03")
#' decodeTime("2013/07/01 01:02:03")
#' decodeTime("2013/07/01")
#' @family functions relating to time
decodeTime <- function(time, timeFormats, tz="UTC")
{
    if (missing(timeFormats))
        timeFormats <- c("%b %d %Y %H:%M:%S", "%b %d %Y", # Jul 1 2013
                         "%B %d %Y %H:%M:%S", "%B %d %Y", # July 1 2013
                         "%d %b %Y %H:%M:%S", "%d %b %Y", # 1 Jul 2013
                         "%d %B %Y %H:%M:%S", "%d %B %Y", # 1 July 2013
                         "%Y-%m-%d %H:%M:%S", "%Y-%m-%d", # 2013-07-01
                         "%Y-%b-%d %H:%M:%S", "%Y-%b-%d", # 2013-Jul-01
                         "%Y-%B-%d %H:%M:%S", "%Y-%B-%d", # 2013-July-01
                         "%d-%b-%Y %H:%M:%S", "%d-%b-%Y", # 01-Jul-2013
                         "%d-%B-%Y %H:%M:%S", "%d-%B-%Y", # 01-July-2013
                         "%Y/%m/%d %H:%M:%S", "%Y/%m/%d", # 2013/07/01
                         "%Y/%b/%d %H:%M:%S", "%Y/%b/%d", # 2013/Jul/01
                         "%Y/%B/%d %H:%M:%S", "%Y/%B/%d", # 2013/July/01
                         "%Y/%m/%d %H:%M:%S", "%Y/%m/%d") # 2013/07/01
    ## FIXME: permit time to be a vector
    res <- NA
    for (format in timeFormats) {
        ##cat("TRYING FORMAT:", format, "\n")
        if (!is.na(res <-  as.POSIXct(time, format=format, tz=tz))) {
            break
        }
    }
    res
}


#' Draw a Direction Field
#'
#' The direction field is indicated variously, depending on the value of
#' \code{type}:\itemize{
#' \item For \code{type=1}, each indicator is drawn with a symbol, according to the
#' value of \code{pch} (either supplied globally, or as an element of the
#' \code{...} list) and of size \code{cex}, and colour \code{col}.   Then, a
#' line segment is drawn for each, and for this \code{lwd} and \code{col} may
#' be set globally or in the \code{...} list.
#' \item For \code{type=2}, the points are not drawn, but arrows are drawn instead
#' of the line segments.  Again, \code{lwd} and \code{col} control the type of
#' the line.
#' }
#'
#' @param x,y coordinates at which velocities are specified. The
#'     length of \code{x} and \code{y} depends on the form of \code{u}
#'     and \code{v} (vectors or matrices).
#' @param u,v velocity components in the x and y directions. Can be
#'     either vectors with the same length as \code{x, y}, or
#'     matrices, of dimension \code{length(x)} by \code{length(y)}.
#' @param scalex,scaley scale to be used for the velocity arrows.
#'     Exactly one of these must be specified.  Arrows that have
#'     \code{u^2+v^2=1} will have length \code{scalex} along the x
#'     axis, or \code{scaley} along the y axis, according to which
#'     argument is given.
#' @param skip either an integer, or a two-element vector indicating
#'     the number of points to skip when plotting arrows (for the
#'     matrix \code{u, v} case). If a single value, the same
#'     \code{skip} is applied to both the \code{x} and \code{y}
#'     directions. If a two-element vector, specifies different values
#'     for the \code{x} and \code{y} directions.
#' @param length indication of \strong{width} of arrowheads. The
#'     somewhat confusing name of this argument is a consequence of
#'     the fact that it is passed to \code{\link{arrows}} for drawing
#'     arrows.  Note that the present default is smaller than the
#'     default used by \code{\link{arrows}}.
#' @param add if \code{TRUE}, the arrows are added to an existing
#'     plot; otherwise, a new plot is started by calling
#'     \code{\link{plot}} with \code{x}, \code{y} and \code{type="n"}.
#'     In other words, the plot will be very basic. In most cases, the
#'     user will probably want to draw a diagram first, and \code{add}
#'     the direction field later.
#' @param type indication of the style of arrow-like indication of the
#'     direction.
#' @param col colour of line segments or arrows
#' @param pch,cex plot character and expansion factor, used for
#'     \code{type=1}
#' @param lwd,lty line width and type, used for \code{type=2}
#' @param xlab,ylab \code{x} and \code{y} axis labels
#' @param debug debugging value; set to a positive integer to get
#'     debugging information.
#' @param ... other arguments to be passed to plotting functions
#'     (e.g. axis labels, etc).
#'
#' @return None.
#'
#' @examples
#' library(oce)
#' plot(c(-1.5, 1.5), c(-1.5, 1.5), xlab="", ylab="", type='n')
#' drawDirectionField(x=rep(0, 2), y=rep(0, 2), u=c(1, 1), v=c(1, -1), scalex=0.5, add=TRUE)
#' plot(c(-1.5, 1.5), c(-1.5, 1.5), xlab="", ylab="", type='n')
#' drawDirectionField(x=rep(0, 2), y=rep(0, 2), u=c(1, 1), v=c(1, -1), scalex=0.5, add=TRUE,
#'                    type=2)
#'
#' ## 2D example
#' x <- seq(-2, 2, 0.1)
#' y <- x
#' xx <- expand.grid(x, y)[,1]
#' yy <- expand.grid(x, y)[,2]
#' z <- matrix(xx*exp(-xx^2 -yy^2), nrow=length(x))
#' gz <- grad(z, x, y)
#' drawDirectionField(x, y, gz$gx, gz$gy, scalex=0.5, type=2, len=0.02)
#' oceContour(x, y, z, add=TRUE)
#' @author Dan Kelley and Clark Richards
drawDirectionField <- function(x, y, u, v, scalex, scaley, skip, length=0.05, add=FALSE,
                               type=1, col=par("fg"), pch=1, cex=par("cex"),
                               lwd=par("lwd"), lty=par("lty"),
                               xlab='', ylab='',
                               debug=getOption("oceDebug"),
                               ...)
{
    oceDebug(debug, "drawDirectionField(...) {\n", unindent=1)
    if (missing(x) || missing(y) || missing(u) || missing(v))
        stop("must supply x, y, u, and v")
    if ( (missing(scalex) && missing(scaley)) || (!missing(scalex) && !missing(scaley)) )
        stop("either 'scalex' or 'scaley' must be specified (but not both)")
    if (is.matrix(u) && !is.matrix(v))
        stop("if 'u' is a matrix, then 'v' must also be a matrix")
    if (is.matrix(v) && !is.matrix(u))
        stop("if 'v' is a matrix, then 'u' must also be a matrix")
    if (is.matrix(u) && is.matrix(v)) {
        oceDebug(debug, "u, v are matrices")
        if ( (dim(u)[1] != dim(v)[1]) & (dim(u)[2] != dim(v)[2]) )
            stop("dimensions of u and v must match")
        dim <- dim(u)
        if (length(x) != dim[1] | length(y) != dim[2])
            stop("lengths of x, y must match dimensions of u, v")
        if (missing(skip)) {
            skip <- c(1, 1)
        } else {
            if (length(skip) == 1)
                skip <- rep(skip, 2)
        }
        nx <- length(x)
        ny <- length(y)
        ix <- seq(1, nx, skip[1])
        iy <- seq(1, ny, skip[2])
        xx <- expand.grid(x[ix], y[iy])[,1]
        yy <- expand.grid(x[ix], y[iy])[,2]
        uu <- as.vector(u[ix, iy])
        vv <- as.vector(v[ix, iy])
    } else {
        if (length(x) != length(y))
            stop("lengths of x and y must match")
        if (length(x) != length(u))
            stop("lengths of x and u must match")
        if (length(x) != length(v))
            stop("lengths of x and v must match")
        xx <- x
        yy <- y
        uu <- u
        vv <- v
    }
    if (!add) {
        plot(xx, yy, type='n', xlab=xlab, ylab=ylab, ...)
    }
    usr <- par('usr')
    pin <- par('pin')
    ##mai <- par('mai')
    xPerInch <- diff(usr[1:2]) / pin[1]
    yPerInch <- diff(usr[3:4]) / pin[2]
    oceDebug(debug, 'pin=', pin, 'usr=', usr, 'xPerInch=', xPerInch, 'yPerInch=', yPerInch, '\n')
    if (missing(scaley)) {
        oceDebug(debug, "scaling for x\n")
        uPerX <- 1 / scalex
        vPerY <- uPerX * xPerInch / yPerInch
    } else {
        vPerY <- 1 / scaley
        uPerX <- vPerY * yPerInch / xPerInch
        oceDebug(debug, "scaling for y\n")
    }
    oceDebug(debug, 'uPerX=', uPerX, '\n')
    oceDebug(debug, 'vPerY=', vPerY, '\n')
    len <- sqrt( (u/uPerX)^2 + (v/vPerY)^2 )
    ok <- len > 0.0
    xx <- xx[ok]
    yy <- yy[ok]
    uu <- uu[ok]
    vv <- vv[ok]
    if (type == 1) {
        points(xx, yy, col=col, pch=pch, cex=cex)
        segments(x0=xx, y0=yy, x1=xx+uu/uPerX, y1=yy+vv/vPerY, col=col, lwd=lwd, lty=lty)
    } else if (type == 2) {
        arrows(x0=xx, y0=yy, x1=xx+uu/uPerX, y1=yy+vv/vPerY, length=length, col=col, lwd=lwd, lty=lty)
    } else {
        stop("unknown value of type ", type)
    }
    oceDebug(debug, "} # drawDirectionField\n", unindent=1)
}


#' Oce Variant of contour
#'
#' This provides something analagous to \code{\link{contour}}, but with the
#' ability to flip x and y.
#' Setting \code{revy=TRUE} can be helpful if the \code{y} data represent
#' pressure or depth below the surface.
#'
#' @aliases oce.contour oceContour
#' @param x values for x grid.
#' @param y values for y grid.
#' @param z matrix for values to be contoured.  The first dimension of \code{z}
#' must equal the number of items in \code{x}, etc.
#' @param revx set to \code{TRUE} to reverse the order in which the labels on
#' the x axis are drawn
#' @param revy set to \code{TRUE} to reverse the order in which the labels on
#' the y axis are drawn
#' @param add logical value indicating whether the contours should be added to
#' a pre-existing plot.
#' @param tformat time format; if not supplied, a reasonable choice will be
#' made by \code{\link{oce.axis.POSIXct}}, which draws time axes.
#' @param drawTimeRange logical, only used if the \code{x} axis is a time.  If
#' \code{TRUE}, then an indication of the time range of the data (not the axis)
#' is indicated at the top-left margin of the graph.  This is useful because
#' the labels on time axes only indicate hours if the range is less than a day,
#' etc.
#' @param debug a flag that turns on debugging; set to 1 to information about
#' the processing.
#' @param \dots optional arguments passed to plotting functions.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' data(topoWorld)
#' ## coastline now, and in last glacial maximum
#' lon <- topoWorld[["longitude"]]
#' lat <- topoWorld[["latitude"]]
#' z <- topoWorld[["z"]]
#' oce.contour(lon, lat, z, levels=0, drawlabels=FALSE)
#' oce.contour(lon, lat, z, levels=-130, drawlabels=FALSE, col='blue', add=TRUE)
oce.contour <- function(x, y, z, revx=FALSE, revy=FALSE, add=FALSE,
                       tformat, drawTimeRange=getOption("oceDrawTimeRange"),
                       debug=getOption("oceDebug"), ...)
{
    ##dots <- list(...)
    ##dotsNames <- names(dots)
    mustReverseX <- any(0 > diff(order(x)))
    mustReverseY <- any(0 > diff(order(y)))
    oceDebug(debug, "mustReverseX:", mustReverseX, '\n')
    oceDebug(debug, "mustReverseY:", mustReverseY, '\n')

    ## perhaps get (x,y,z) from x, etc., trying to emulate contour()
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            } else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        } else stop("no 'z' matrix specified")
    } else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    zdim <- dim(z)


    ## store local values for the tricky cases of reversing axes etc
    xx <- x
    yy <- y
    zz <- z
    if (mustReverseX) {
        xx <- rev(xx)
        zz <- zz[seq.int(zdim[1], 1), ]
    }
    if (mustReverseY) {
        yy <- rev(yy)
        zz <- zz[, seq.int(zdim[2], 1)]
    }
    if (add) {
        contour(xx, yy, zz, add=TRUE, ...)
    } else {
        if (revx) {
            xx <- rev(xx)
        }
        if (revy) {
            yy <- rev(yy)
        }
        contour(xx, yy, zz, axes=FALSE, ...)
        ## see src/library/graphics/R/contour.R
        xaxp <- par('xaxp')
        xat <- seq(xaxp[1], xaxp[2], length.out=xaxp[3])
        xlabels <- format(xat)
        yaxp <- par('yaxp')
        yat <- seq(yaxp[1], yaxp[2], length.out=yaxp[3])
        ylabels <- format(yat)
        xIsTime <- inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")

        if (revx) {
            if (xIsTime) {
                oce.axis.POSIXct(side=1, x=x,
                                 drawTimeRange=drawTimeRange,
                                 #mar=mar, mgp=mgp,
                                 tformat=tformat, debug=debug-1)
            } else {
                Axis(xx, side=1, at=xat, labels=rev(xlabels))
            }
        } else {
            if (xIsTime) {
                oce.axis.POSIXct(side=1, x=x,
                                 drawTimeRange=drawTimeRange,
                                 #mar=mar, mgp=mgp,
                                 tformat=tformat, debug=debug-1)
            } else {
                Axis(xx, side=1)
            }
        }
        if (revy) {
            Axis(yy, side=2, at=yat, labels=rev(ylabels))
        } else {
            Axis(yy, side=2)
        }
        box()
    }
}
oceContour <- oce.contour
