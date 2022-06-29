# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' oce: A Package for Oceanographic Analysis.
#'
#' @description
#' The oce package provides functions for working with
#' Oceanographic data, for calculations that are specific
#' to Oceanography, and for producing graphics that
#' match the conventions of the field.
#'
#' @details
#'
#' Over a dozen specialized data types are handled by oce,
#' with generic plots and summaries for each, along with
#' the specialized functions needed for typical Oceanographic
#' analysis.
#'
#' See [oce-class] for a summary of the class structure
#' and links to documentation for the many subclasses of
#' oce objects, each aligned with a class of instrument or
#' or type of dataset.  For a more task-oriented approach,
#' see the several vignettes that are provided with oce,
#' and a book
#' (Kelley, Dan E. Oceanographic Analysis with R. New York: Springer-Verlag, 2018.
#' https://link.springer.com/book/10.1007/978-1-4939-8844-0) written
#' by one of the oce co-authors.
#'
#' @section Specialized Functions:
#' A key function is [read.oce()], which will attempt
#' to read Oceanographic data in raw format. This uses
#' [oceMagic()] to try to detect the file type,
#' based on the file name and contents. If this detection
#' is not possible, users will need to go beyond [read.oce()],
#' using a more specialized function, e.g. [read.ctd()] for CTD files,
#' [read.ctd.sbe()] for Teledyne-Seabird files, etc.
#'
#' @section Generic Methods:
#' A list of the generic methods in oce is provided by
#' [methods]`(class="oce")`; a few that are used frequently
#' are as follows.
#' * `[[` Finds the value of an item in the object's
#'    `metadata` or `data` slot. If the item does
#'    not exist, but can be calculated from the other items,
#'    then the calculated value is returned. As an example of the
#'    latter, consider the built-in `ctd` dataset, which does
#'    not contain potential temperature, "`theta`". Using
#'    `ctd[["theta"]]` therefore causes [swTheta()]
#'    to be called, to calculate `theta`.
#'    See \link{[[,oce-method} or type `?"[[,oce-method"`
#'    to learn more about general functioning, or a specialized
#'    method like \link{[[,ctd-method} for CTD data, etc.
#' * `[[<-` Alters the named item in the object's `metadata` or
#'    `data` slot.  If the item does not exist, it is created.
#'    See \link{[[<-,oce-method} or type `?"[[<-,oce-method"`
#'    to learn more about the general methodology, or a specialized
#'    method like \link{[[<-,ctd-method} for CTD data, etc.
#' * `summary()` Displays some information about the object named as an
#'    argument, including a few elements from its `metadata` slot
#'    and some statistics of the contents of its `data` slot.
#'    See \link{summary,oce-method} or type `?"summary,oce-method"`
#'    to learn more about general functioning, or a specialized
#'    method like \link{summary,ctd-method} for CTD data, etc.
#' * `subset()` Takes a subset of an oce object.
#'    See \link{subset,oce-method} or type `?"subset,oce-method"`
#'    to learn more about general functioning, or a specialized
#'    method like \link{subset,ctd-method} for CTD data, etc.
#'
#' @docType package
#'
#' @name oce
NULL


##################################################################
## DEVELOPER NOTE: Update this whenever function status changes. #
##################################################################
#' Deprecated and Defunct Elements of the oce package
#'
#' Certain functions and function arguments are still provided for
#' compatibility with older versions of \CRANpkg{oce}, but will be removed soon.
#' The \CRANpkg{oce} scheme for removing functions is similar to that used by
#' `Bioconductor`: items are marked as "deprecated" in one release, marked as
#' "defunct" in the next, and removed in the next after that. This goal is to provide a
#' gentle migration path for users who keep their packages reasonably
#' up-to-date.
#'
#' The following are marked "deprecated" in the present CRAN release of \CRANpkg{oce}.
#' Please use the replacement functions as listed below. The upcoming CRAN
#' release of \CRANpkg{oce} will mark these as "defunct", which is the
#' last step before outright removal.
#'
#' \tabular{lllll}{
#' **Deprecated**             \tab **Replacement**   \tab **Deprecated**  \tab **Defunct** \tab **Removed** \cr
## `byteToBinary(x,"endian")` \tab [rawToBits()]     \tab 1.1-1           \tab 1.1-3       \tab 1.1-4       \cr
#' `renameData()`             \tab [oceRenameData()] \tab 1.1-2           \tab 1.1-3       \tab 1.1-4       \cr
#' }
#'
#' The following are marked "defunct", so calling them in the
#' the present version produces an error message that hints at a replacement
#' function. Once a function is marked "defunct" on one CRAN release, it will
#' be slated for outright deletion in a subsequent release.
#'
#'\tabular{lll}{
#' **Defunct**         \tab **Replacement**                \tab **Version**\cr
#' `renameData()`      \tab [oceRenameData()]              \tab 1.5        \cr
#' `byteToBinary()`    \tab [rawToBits()]                  \tab 1.5        \cr
#'}
#'
#' The following functions were removed after having been marked as "deprecated"
#' in at least one CRAN release, and possibly as "defunct" in at least
#' one CRAN release.  (The version number in the table is the first
#' version to lack the named function.)
#'
#'\tabular{lll}{
#' **Function**        \tab **Replacement**                \tab **Version**\cr
#' `addColumn()`       \tab [oceSetData()]                 \tab 1.1-2      \cr
#' `ctdAddColumn()`    \tab [oceSetData()]                 \tab 1.1-2      \cr
#' `ctdUpdateHeader()` \tab [oceSetMetadata()]             \tab 1.1-2      \cr
#' `findInOrdered()`   \tab [findInterval()]               \tab 1.1-2      \cr
#' `makeSection()`     \tab [as.section()]                 \tab 0.9.24     \cr
#' `mapMeridians()`    \tab [mapGrid()]                    \tab 1.1-2      \cr
#' `mapZones()`        \tab [mapGrid()]                    \tab 1.1-2      \cr
#' `oce.as.POSIXlt()`  \tab [lubridate::parse_date_time()] \tab 1.1-2      \cr
#'}
#'
#' Several \CRANpkg{oce} function arguments are considered "deprecated", which
#' means they will be marked "defunct" in the next CRAN release. These are normally
#' listed in the help page for the function in question. A few that may be
#' of general interest are also listed below.
#'
#' * The `adorn` argument was still being checked for (in the dots argument)
#' until 2020 August 11.
#'
#' * The `eos` argument of [swN2()] was removed on 2019
#' April 11; for details, see the \dQuote{Deprecation Notation} section
#' of the documentation for [swN2()].
#'
#' * The `endian` argument of [byteToBinary()] will be removed sometime
#' in the year 2017, and should be set to `"big"` in the meantime.
#'
#' * The `parameters` argument of [plot,ctd-method()]
#' was deprecated on 2016-12-30.  It was once used by
#' [plot,coastline-method()] but has been ignored by that
#' function since February 2016.
#'
#' * The `orientation` argument of [plot,ctd-method()]
#' was deprecated on 2016-12-30.  It was once used by
#' [plot,coastline-method()] but has been ignored by that
#' function since February 2016.
#'
#' Several \sQuote{oce} function arguments are considered "defunct", which
#' means they will be removed in the next CRAN release. They are as follows.
#'
#' * The `fill` argument of [mapPlot()] was confusing
#' to users, so it was designated as deprecated in June 2016.
#' (The confusion stemmed from subtle differences between
#' [plot()] and [polygon()], and the problem is that
#' [mapPlot()] can use either of these functions, according
#' to whether coastlines are to be filled.)
#' The functionality is preserved, in the `col` argument.
#'
#' @aliases oce-defunct
#'
#' @name oce-deprecated
#'
#' @seealso The \sQuote{Bioconductor} scheme for removing functions is
#' described at
#' `https://www.bioconductor.org/developers/how-to/deprecation/` and it is
#' extended here to function arguments.
NULL

#' Coerce Something Into an Oce Object
#'
#' @details
#' This function is limited and not intended for common use.
#' In most circumstances, users should employ a function such
#' as [as.ctd()] to construct specialized oce sub-classes.
#'
#' `as.oce` creates an oce object from data contained within its
#' first argument, which may be a list, a data frame, or an object
#' of [oce-class].  (In the last case, `x` is
#' simply returned, without modification.)
#'
#' If `x` is a list containing items named `longitude` and
#' `latitude`, then [as.coastline()] is called (with
#' the specified \dots value) to create a coastline object.
#'
#' If `x` is a list created by `read_odf()` from the (as
#' yet unreleased) ODF package developed by the Bedford Institute of
#' Oceanography, then [ODF2oce()] is called (with
#' no arguments other than the first) to calculate a return value.
#' If the sub-class inference made by [ODF2oce()] is
#' incorrect, users should call that function directly, specifying
#' a value for its `coerce` argument.
#'
#' If `x` has not been created by `read_odf()`, then the names
#' of the items it contains are examined, and used to try to infer
#' the proper return value.  There
#' are only a few cases (although more may be added if there is
#' sufficient user demand). The cases are as follows.
#'
#' * If `x` contains items named `temperature`,
#' `pressure` and either `salinity` or `conductivity`,
#' then an object of type [ctd-class] will be returned.
#'
#' * If `x` contains columns named `longitude` and `latitude`,
#' but no other columns, then an object of class [coastline-class]
#' is returned.
#'
#' @param x an item containing data. This may be data frame, list, or an oce object.
#'
#' @param \dots optional extra arguments, passed to conversion functions
#' [as.coastline()] or [ODF2oce()], if these are used.
#'
#' @return An [oce-class] object.
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
        # Assume it's a CTD
        res <- as.ctd(salinity=x$salinity, temperature=x$temperature, pressure=x$pressure)
        # Add other columns
        for (name in names) {
            if (name != "temperature" && name != "pressure" && name != "salinity") {
                res <- oceSetData(res, name=name, value=x[[name]])
            }
        }
        return(res)
    }
    if ("longitude" %in% names && "latitude" %in% names && length(names) == 2) {
        # Assume it's a coastline
        return(as.coastline(longitude=x$longitude, latitude=x$latitude))
    }
    # Not sure what it is, so make a generic oce object. We
    # have no way to guess the unit.
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
#'
#' @param g object holding data from an instrument whose heading is good, and
#' should be interpolated to the time base of `b`.
#'
#' @param add an angle, in degrees, to be added to the heading.
#'
#' @return A copy of `b`, but with `b$data$heading` replaced with
#' heading angles that result from linear interpolation of the headings in
#' `g`, and then adding the angle `add`.
#'
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
#' Windows `x` on either time or distance, depending on the value of
#' `which`.  In each case, values of `start` and `end` may be
#' integers, to indicate a portion of the time or distance range.  If
#' `which` is `"time"`, then the `start` and `end` values
#' may also be provided as POSIX times, or character strings indicating times
#' (in time zone given by the value of `getOption("oceTz")`).
#' Note that [subset()] may be more useful than this function.
#'
#' @param x an [oce-class] object.
#'
#' @param start the start time (or distance) of the time (or space) region of
#' interest.  This may be a single value or a vector.
#'
#' @param end the end time (or distance) of the time (or space) region of
#' interest.  This may be a single value or a vector.
#'
#' @param frequency not permitted yet.
#'
#' @param deltat not permitted yet
#'
#' @param extend not permitted yet
#'
#' @param which string containing the name of the quantity on which sampling is
#' done.  Possibilities are `"time"`, which applies the windowing on the
#' `time` entry of the `data` slot, and `"distance"`, for the
#' `distance` entry (for those objects, such as `adp`, that have this
#' entry).
#'
#' @param indexReturn boolean flag indicating whether to return a list of the
#' "kept" indices for the `time` entry of the `data` slot, as well as
#' the `timeSlow` entry, if there is one..  Either of these lists will be
#' `NULL`, if the object lacks the relevant items.
#'
#' @param debug a flag that turns on debugging.
#'
#' @param \dots ignored
#'
#' @return Normally, this is new `oce` object.  However, if
#' `indexReturn=TRUE`, the return value is two-element list containing
#' items named `index` and `indexSlow`, which are the indices for the
#' `time` entry of the `data` slot (and the `timeSlow`, if it
#' exists).
#'
#' @author Dan Kelley
#'
#' @seealso [subset()] provides more flexible selection of subsets.
#'
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
        # FIXME: make it work on sections, on CTD, etc.
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
    # sync up some metadata that might have been altered
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
#' @param x an [oce-class] object.
#'
#' @param n Number of elements to extract, as for [head()].
#'
#' @param ... ignored
#'
#' @seealso [tail.oce()], which yields the end of an `oce` object.
#'
#' @author Dan Kelley
head.oce <- function(x, n=6L, ...)
    headOrTail(x=x, n=n, headTail=head, ...)

headOrTail <- function(x, n=6L, headTail=head, ...)
{
    # Using headTail (a) lets us handle both head.oce and
    # tail.oce and also handles both positive and negative n.
    res <- x
    if (inherits(x, "adp") || inherits(x, "adv")) {
        look <- headTail(seq_len(dim(x@data$v)[1]), n)
        for (name in names(x@data)) {
            if ("distance" == name)
                next
            if (name == "time") {
                res@data[[name]] <- headTail(x@data[[name]], n)
            } else if (is.vector(x@data[[name]])) {
                res@data[[name]] <- x@data[[name]][look]
            } else if (is.matrix(x@data[[name]])) {
                res@data[[name]] <- x@data[[name]][look, ]
            } else if (is.array(x@data[[name]])) {
                res@data[[name]] <- x@data[[name]][look, , ]
            } else {
                res@data[[name]] <- x@data[[name]][look] # for reasons unknown, 'time' is not a vector
            }
        }
    } else if (inherits(x, "amsr")) {
        looklon <- headTail(seq_along(x@metadata$longitude), n)
        looklat <- headTail(seq_along(x@metadata$latitude), n)
        res@metadata$longitude <- x@metadata$longitude[looklon]
        res@metadata$latitude <- x@metadata$latitude[looklat]
        for (n in names(res@data)) {
            if (is.matrix(res@data[[n]]))
                res@data[[n]] <- x@data[[n]][looklon, looklat]
        }
    } else if (inherits(x, "argo")) {
        for (name in names(x@metadata)) {
            if (name %in% c("direction", "juldQc", "positionQc")) {
                # select characters in a string
                look <- headTail(seq_len(nchar(x@metadata[[name]])), n)
                res@metadata[[name]] <- substr(x@metadata[[name]], look[1], tail(look, 1))
            } else if (name == "flags") {
                look <- headTail(seq_len(dim(x@metadata$flags[[1]])[2]), n)
                for (fname in names(x@metadata$flags)) {
                    res@metadata$flags[[fname]] <- x@metadata$flags[[fname]][, look]
                }
            } else if (is.vector(x@metadata[[name]])) {
                res@metadata[[name]] <- headTail(x@metadata[[name]], n)
            } else if (is.matrix(x@metadata[[name]])) {
                look <- headTail(seq_len(dim(x@metadata[[name]])[2]), n)
                res@metadata[[name]] <- x@metadata[[name]][, look]
            #} else {
            #    warning("ignoring metadata item: '", name, "'")
            }
        }
        for (name in names(x@data)) {
            if (is.vector(x@data[[name]])) {
                res@data[[name]] <- headTail(x@data[[name]], n)
            } else if (is.matrix(x@data[[name]])) {
                look <- headTail(seq_len(dim(x@data[[name]])[2]), n)
                res@data[[name]] <- x@data[[name]][, look]
            } else if (name == "time") {
                # for reasons unknown, time is not a vector
                res@data[[name]] <- headTail(x@data[[name]], n)
            } else {
                warning("ignoring data item: '", name, "'")
            }
        }
    } else if (inherits(x, "ctd")) {
        for (name in names(x@data))
            res@data[[name]] <- headTail(x@data[[name]], n)
        for (fname in names(x@metadata$flags))
            res@metadata$flags[[fname]] <- headTail(x@metadata$flags[[fname]], n)
    } else if (inherits(x, "coastline")) {
        for (name in c("longitude", "latitude"))
            res@data[[name]] <- headTail(x@data[[name]], n)
    } else if (inherits(x, "echosounder")) {
        look <- headTail(seq_along(x@data$latitude), n=n)
        for (name in c("longitude", "latitude", "time"))
            res@data[[name]] <- x@data[[name]][look]
        res@data$a <- x@data$a[look, ]
        # FIXME: decide whether the 'Slow' variables should be altered
    } else if (inherits(x, "g1sst")) {
        # not in the test suite, because the files are too big, but
        # looks fine manually on the following test file
        # f <- dc.g1sst(2015, 10, 14, -66, -60, 41, 46)
        # d <- read.g1sst(f)
        looklon <- headTail(seq_along(x@metadata$longitude), n)
        looklat <- headTail(seq_along(x@metadata$latitude), n)
        res@metadata$longitude <- x@metadata$longitude[looklon]
        res@metadata$latitude <- x@metadata$latitude[looklat]
        res@data$SST <- x@data$SST[looklon, looklat]
    } else if (inherits(x, "lisst")) {
        look <- headTail(seq_along(x@data[[1]]), n=n)
        for (name in names(x@data))
            res@data[[name]] <- x@data[[name]][look]
    } else if (inherits(x, "met")) {
        look <- headTail(seq_along(x@data$temperature), n=n)
        for (name in names(x@data))
            res@data[[name]] <- x@data[[name]][look]
    } else if (inherits(x, "section")) {
        look <- headTail(seq_along(x@metadata$latitude), n=n)
        for (name in c("stationId", "longitude", "latitude", "time"))
            res@metadata[[name]] <- x@metadata[[name]][look]
        res@data$station <- x@data$station[look]
    } else if (inherits(x, "topo")) {
        looklon <- headTail(seq_along(x@data$longitude), n)
        looklat <- headTail(seq_along(x@data$latitude), n)
        res@data$longitude <- x@data$longitude[looklon]
        res@data$latitude <- x@data$latitude[looklat]
        res@data$z <- x@data$z[looklon, looklat]
    } else if (inherits(x, "landsat")) {
        # Actually, handling this would not be too hard, but there are some
        # interlocked metadata elements that require some thought.
        warning("head.oce() and tail.oce() cannot handle landsat, so returning it unaltered\n")
    } else {
        # The following general code works on the following objects (and is tested
        # for them, in tests/testthat/test_oce.R)
        #
        #   cm
        #   ladp
        #   lobo
        #   rsk
        #   sealevel
        for (name in names(x@data)) {
            # For reasons I don't understand, the 'time' items are not vectors.
            if ((is.vector(x@data[[name]]) && !is.list(x@data[[name]])) || name=="time") {
                res@data[[name]] <- headTail(x@data[[name]], n)
            } else if (is.data.frame(x@data[[name]])) {
                res@data[[name]] <- headTail(x@data[[name]], n)
            } else if (is.null(x@data[[name]])) {
            } else {
                warning("ignoring '", name, "' because it is not a vector\n")
            }
        }
        for (fname in names(x@metadata$flags)) {
            if (is.list(x@metadata$flags[[fname]])) {
                #> message("x@metadata$flags$", fname, " is a list")
                for (fname2 in names(x@metadata$flags[[fname]]))
                    res@metadata$flags[[fname]][[fname2]] <- headTail(x@metadata$flags[[fname]][[fname2]], n)
            } else {
                res@metadata$flags[[fname]] <- headTail(x@metadata$flags[[fname]], n)
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
#' @param x an [oce-class] object.
#'
#' @param n Number of elements to extract, as for [tail()].
#'
#' @param ... ignored
#'
#' @seealso [head.oce()], which yields the start of an `oce` object.
#'
#' @author Dan Kelley
tail.oce <- function(x, n=6L, ...)
    headOrTail(x=x, n=n, headTail=tail, ...)


#' Draw a Polar Plot
#'
#' Creates a crude polar plot.
#'
#' @param r radii of points to plot.
#'
#' @param theta angles of points to plot, in degrees.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots optional arguments passed to the lower-level plotting
#' functions.
#'
#' @author Dan Kelley
#'
#' @examples
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
    #Rpretty <- pretty(c(0, R))
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
#' Setting `method="rr"` yields the weighted-parabola algorithm of
#' Reiniger and Ross (1968).  For procedure is as follows.  First, the
#' interpolant for any `xout` value that is outside the range of `x`
#' is set to NA.  Next, linear interpolation is used for any `xout` value
#' that has only one smaller neighboring `x` value, or one larger
#' neighboring value.  For all other values of `xout`, the 4 neighboring
#' points `x` are sought, two smaller and two larger.  Then two parabolas
#' are determined, one from the two smaller points plus the nearest larger
#' point, and the other from the nearest smaller point and the two larger
#' points.  A weighted sum of these two parabolas provides the interpolated
#' value.  Note that, in the notation of Reiniger and Ross (1968), this
#' algorithm uses `m`=2 and `n`=1.  (A future version of this routine
#' might provide the ability to modify these values.)
#'
#' Setting `method="unesco"` yields the method that is used by the U.S.
#' National Oceanographic Data Center. It is described in pages 48-50 of
#' reference 2; reference 3 presumably contains the same information but it is
#' not as easily accessible.  The method works as follows.
#'
#' * If there are data above 5m depth, then the surface value is taken to
#' equal to the shallowest recorded value.
#'
#' * Distance bounds are put on the four neighboring points, and the
#' Reiniger-Ross method is used for interpolated points with sufficiently four
#' close neighbors.  The bounds are described in table 15 of reference 2 only
#' for so-called standard depths; in the present instance they are transformed
#' to the following rules.  Inner neighbors must be within 5m for data above
#' 10m, 50m above 250m 100m above 900m, 200m above 2000m, or within 1000m
#' otherwise.  Outer neighbors must be within 200m above 500m, 400m above
#' 1300m, or 1000m otherwise.  If two or more points meet these criteria,
#' Lagrangian interpolation is used.  If not, `NA` is used as the
#' interpolant.
#'
#' After these rules are applied, the interpolated value is compared with the
#' values immediately above and below it, and if it is outside the range,
#' simple linear interpolation is used.
#'
#' @param x the independent variable (z or p, usually).
#'
#' @param y the dependent variable.
#'
#' @param xout the values of the independent variable at which interpolation is
#' to be done.
#'
#' @param method method to use.  See \dQuote{Details}.
#'
#' @return A vector of interpolated values, corresponding to the `xout`
#' values and equal in number.
#'
#' @author Dan Kelley
#'
#' @references
#' 1.  R.F. Reiniger and C.K. Ross, 1968.  A method of interpolation with
#' application to oceanographic data.  *Deep Sea Research*, **15**,
#' 185-193.
#'
#' 2. Daphne R. Johnson, Tim P. Boyer, Hernan E. Garcia, Ricardo A.
#' Locarnini, Olga K. Baranova, and Melissa M. Zweng, 2011. World Ocean
#' Database 2009 Documentation.  NODC Internal report 20.  Ocean Climate
#' Laboratory, National Oceanographic Data Center.  Silver Spring, Maryland.
#'
#' 3. UNESCO, 1991. Processing of oceanographic station data, 138 pp.,
#' Imprimerie des Presses Universitaires de France, United Nations Educational,
#' Scientific and Cultural Organization, France.
#'
#' @aliases oce.approx
#'
#' @examples
#' library(oce)
#' if (require(ocedata)) {
#'     data(RRprofile)
#'     zz <- seq(0, 2000, 2)
#'     plot(RRprofile$temperature, RRprofile$depth, ylim=c(500, 0), xlim=c(2, 11))
#'     # Contrast two methods
#'     a1 <- oce.approx(RRprofile$depth, RRprofile$temperature, zz, "rr")
#'     a2 <- oce.approx(RRprofile$depth, RRprofile$temperature, zz, "unesco")
#'     lines(a1, zz)
#'     lines(a2, zz, col='red')
#'     legend("bottomright",lwd=1,col=1:2, legend=c("rr","unesco"),cex=3/4)
#'}
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
    do_oceApprox(x=x, y=y, xout=xout, method=pmatch(method, c("unesco", "rr")))
}
oce.approx <- oceApprox


#' Draw a Stick Plot
#'
#' The arrows are drawn with directions on the graph that match the directions
#' indicated by the `u` and `v` components. The arrow size is set
#' relative to the units of the `y` axis, according to the value of
#' `yscale`, which has the unit of `v` divided by the unit of
#' `y`.
#' The interpretation of diagrams produced by `plotSticks` can be
#' difficult, owing to overlap in the arrows.  For this reason, it is often
#' a good idea to smooth `u` and `v` before using this function.
#'
#' @param x x coordinates of stick origins.
#'
#' @param y y coordinates of stick origins.  If not supplied, 0 will be used;
#' if length is less than that of x, the first number is repeated and the rest
#' are ignored.
#'
#' @param u x component of stick length.
#'
#' @param v y component of stick length.
#'
#' @param yscale scale from u and v to y (see \dQuote{Description}).
#'
#' @param add boolean, set `TRUE` to add to an existing plot.
#'
#' @param length value to be provided to [arrows()]; here, we set a
#' default that is smaller than normally used, because these plots tend to be
#' crowded in oceanographic applications.
#'
#' @param mgp 3-element numerical vector to use for [`par`]`("mgp")`. Note
#' that the default `mar` is computed from the `mgp` value.
#' The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar value to be used with [`par`]`("mar")`.
#'
#' @param xlab,ylab labels for the plot axes. The default is not to label them.
#'
#' @param col color of sticks, in either numerical or character format. This is
#' made to have length matching that of `x` by a call to [rep()],
#' which can be handy in e.g. colorizing a velocity field by day.
#'
#' @param \dots graphical parameters passed down to [arrows()].  It
#' is common, for example, to use smaller arrow heads than [arrows()]
#' uses; see \dQuote{Examples}.
#'
#' @author Dan Kelley
#'
#' @examples
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
#' For plots not created by oce functions, or for missing `xat` and `yat`,
#' this is the same as a call to [grid()] with missing `nx` and
#' `ny`. However, if `xat` is the return value from certain oce functions,
#' a more sophisticated grid is constructed. The problem with [grid()] is
#' that it cannot handle axes with non-uniform grids, e.g. those with time axes
#' that span months of differing lengths.
#'
#' As of early February 2015, `oce.grid` handles `xat` produced as the
#' return value from the following functions: [imagep()] and
#' [oce.plot.ts()], [plot,adp-method()],
#' [plot,echosounder-method()], and [plotTS()].
#' It makes no sense to try to use `oce.grid` for multipanel oce plots,
#' e.g. the default plot from [plot,adp-method()].
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
#'
#' @param yat a list of y values at which to plot the grid (ignored if `gx` was a return value from an oce plotting function)
#'
#' @param col color of grid lines (see [par()])
#'
#' @param lty type for grid lines (see [par()])
#'
#' @param lwd width for grid lines (see [par()])
oce.grid <- function(xat, yat, col="lightgray", lty="dotted", lwd=par("lwd"))
{
    if (missing(xat) && missing(yat)) {
        grid(col=col, lty=lty, lwd=lwd)
    } else {
        if (is.list(xat)) {
            # following over-rides the args
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
#' Depending on the version of R, the standard [plot()] and
#' [plot.ts()] routines will not obey the time zone of the data.
#' This routine gets around that problem.  It can also plot the time range in
#' the top-left margin, if desired; this string includes the timezone, to
#' remove any possible confusion.
#' The time axis is drawn with [oce.axis.POSIXct()].
#'
#' @param x the times of observations.  If this is not a [POSIXt] object, then an
#' attempt is made to convert it to one using [as.POSIXct()].
#'
#' @param y the observations.
#'
#' @param type plot type, `"l"` for lines, `"p"` for points.
#'
#' @param xlim optional limit for x axis.  This has an additional effect,
#' beyond that for conventional R functions: it effectively windows the data,
#' so that autoscaling will yield limits for y that make sense within the
#' window.
#'
#' @param ylim optional limit for y axis.
#'
#' @param log a character value that must be either empty (the default) for linear
#' `y` axis, or `"y"` for logarithmic `y` axis.  (Unlike
#' [plot.default()] etc., `oce.plot.ts` does not permit
#' logarithmic time, or `x` axis.)
#'
#' @param logStyle a character value that indicates how to draw the y axis, if
#' `log="y"`.  If it is `"r"` (the default) then the conventional R style is used,
#' in which a logarithmic transform connects y values to position on the "page"
#' of the plot device, so that tics will be nonlinearly spaced, but not
#' organized by integral powers of 10.  However, if it is `"decade"`, then
#' the style will be that used in the scientific literature, in which large
#' tick marks are used for integral powers of 10, with smaller tick marks
#' at integral multiples of those powers, and with labels that use exponential
#' format for values above 100 or below 0.01.  The value of `logStyle` is passed
#' to [oceAxis()], which draws the axis.
#'
#' @param drawTimeRange an optional indication of whether/how to draw a time range,
#' in the top-left margin of the plot; see [oce.axis.POSIXct()] for details.
#'
#' @param simplify an integer value that indicates
#' whether to speed up `type="l"` plots by replacing the data
#' with minimum and maximum values within a subsampled time mesh.
#' This can speed up plots of large datasets (e.g. by factor 20 for 10^7 points),
#' sometimes with minor changes in appearance.
#' This procedure is skipped if `simplify` is `NA` or
#' if the number of visible data points is less than 5 times `simplify`.
#' Otherwise, `oce.plot.ts` creates `simplify` intervals ranging across
#' the visible time range. Intervals with under 2 finite
#' `y` data are ignored. In the rest, `y` values
#' are replaced with their range, and `x` values are replaced
#' with the repeated midpoint time. Thus, each retained sub-interval
#' has exactly 2 data points.
#' A warning is printed if this replacement is done.
#' The default value of `simplify` means that cases with
#' under 2560 visible points are plotted conventionally.
#'
#' @param fill boolean, set `TRUE` to fill the curve to zero (which it
#' does incorrectly if there are missing values in `y`).
#' @param col The colours for points (if `type=="p"`) or lines (if `type=="l"`).
#' For the `type="p"` case,
#' if there are fewer `col` values than there are `x` values, then the `col` values
#' are recycled in the standard fashion.
#' For the `type="l"` case, the line is plotted in the first colour specified.
#'
#' @param pch character code, used if `type=="p"`.
#' If there are fewer `pch` values than there are `x` values, then the `pch` values
#' are recycled in the standard fashion.
#' See [points()] for the possible values for `pch`.
#'
#' @param cex numeric character expansion factor for points on plots, ignored unless
#' `type` is `"p"`.  This may be a single number, applied to all points, or
#' a vector of numbers to be applied to the points in sequence.  If there are
#' fewer `pch` values than there are `x` values, then the `pch` values are recycled
#' in the standard fashion. See [par()] for more on `cex`.
#'
#' @param cex.axis,cex.lab,cex.main numeric character expansion factors for axis numbers,
#' axis names and plot titles; see [par()].
#'
#' @param flipy Logical, with `TRUE` indicating that the graph
#' should have the y axis reversed, i.e. with smaller values at
#' the bottom of the page.
#'
#' @param xlab name for x axis; defaults to `""`.
#'
#' @param ylab name for y axis; defaults to the plotted item.
#'
#' @param xaxs control x axis ending; see [`par`]`("xaxs")`.
#'
#' @param yaxs control y axis ending; see [`par`]`("yaxs")`.
#'
#' @param mgp 3-element numerical vector to use for `par(mgp)`, and also
#' for `par(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar value to be used with [`par`]`("mar")` to set margins.
#' The default value uses significantly tighter margins than is the norm in R,
#' which gives more space for the data.  However, in doing this, the existing
#' `par("mar")` value is ignored, which contradicts values that may have
#' been set by a previous call to [drawPalette()].  To get plot with
#' a palette, first call [drawPalette()], then call
#' `oce.plot.ts` with `mar=par("mar")`.
#'
#' @param main title of plot.
#'
#' @param despike boolean flag that can turn on despiking with
#' [despike()].
#'
#' @param axes boolean, set to `TRUE` to get axes plotted
#'
#' @param tformat optional format for labels on the time axis
#'
#' @param marginsAsImage boolean indicating whether to set the right-hand
#' margin to the width normally taken by an image drawn with
#' [imagep()].
#'
#' @param grid if `TRUE`, a grid will be drawn for each panel.  (This
#' argument is needed, because calling [grid()] after doing a
#' sequence of plots will not result in useful results for the individual
#' panels.
#'
#' @param grid.col color of grid
#'
#' @param grid.lty line type of grid
#'
#' @param grid.lwd line width of grid
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots graphical parameters passed down to [plot()].
#'
#' @return A list is silently returned, containing `xat` and `yat`,
#' values that can be used by [oce.grid()] to add a grid to the plot.
#'
#' @author Dan Kelley and Clark Richards
#'
#' @examples
#' library(oce)
#' t0 <- as.POSIXct("2008-01-01", tz="UTC")
#' t <- seq(t0, length.out=48, by="30 min")
#' y <- sin(as.numeric(t - t0) * 2 * pi / (12 * 3600))
#' oce.plot.ts(t, y, type='l', xaxs='i')
#' # Show how col, pch and cex get recycled
#' oce.plot.ts(t, y, type='p', xaxs='i',
#'             col=1:3, pch=c(rep(1, 6), rep(20, 6)), cex=sqrt(1:6))
#' # Trimming x; note the narrowing of the y view
#' oce.plot.ts(t, y, type='p', xlim=c(t[6], t[12]))
#' # Flip the y axis
#' oce.plot.ts(t, y, flipy=TRUE)
oce.plot.ts <- function(x, y, type="l", xlim, ylim, log="", logStyle="r", flipy=FALSE, xlab, ylab,
                        drawTimeRange, simplify=2560, fill=FALSE, col=par("col"), pch=par("pch"),
                        cex=par("cex"), cex.axis=par("cex.axis"), cex.lab=par("cex.lab"), cex.main=par("cex.main"),
                        xaxs=par("xaxs"), yaxs=par("yaxs"),
                        mgp=getOption("oceMgp"),
                        mar=c(mgp[1]+if (nchar(xlab)>0) 1.5 else 1, mgp[1]+1.5, mgp[2]+1, mgp[2]+3/4),
                        main="",
                        despike=FALSE,
                        axes=TRUE, tformat,
                        marginsAsImage=FALSE,
                        grid=FALSE, grid.col="lightgray", grid.lty="dotted", grid.lwd=par("lwd"),
                        debug=getOption("oceDebug"),
                        ...)
{
    if (is.function(x))
        stop("x cannot be a function")
    if (!inherits(x, "POSIXt"))
        x <- as.POSIXct(x)
    if (missing(xlab))
        xlab <- ""
    if (missing(ylab))
        ylab  <- deparse(substitute(expr=y, env=environment()))
    if (missing(drawTimeRange))
        drawTimeRange <- getOption("oceDrawTimeRange", TRUE)
    #ocex <- par("cex")
    #par(cex=cex)
    debug <- min(debug, 4)
    oceDebug(debug, "oce.plot.ts(...,debug=", debug,",",
             argShow(type),
             argShow(flipy),
             argShow(log), "\n", sep="", unindent=1, style="bold")
    oceDebug(debug,
             "          ",
             argShow(mar),
             argShow(mgp), "\n", sep="", style="bold")
    oceDebug(debug,
             "          ",
             argShow(cex),
             argShow(cex.axis),
             argShow(cex.lab),
             "...) {\n", sep="", style="bold")
    if (!is.logical(flipy))
        stop("flipy must be TRUE or FALSE")
    if (!log %in% c("", "y"))
        stop("log must be either an empty string or \"y\", not \"", log, "\" as given")
    if (!is.na(simplify) && (!is.numeric(simplify) || simplify <= 0))
        stop("simplify must be NA or a positive number, but it is ", simplify)

    #oceDebug(debug, "length(x)", length(x), "; length(y)", length(y), "\n")
    #oceDebug(debug, "marginsAsImage=", marginsAsImage, ")\n")
    oceDebug(debug, "x has timezone", attr(x[1], "tzone"), "\n")
    # Repeat col, pch and cex to the right length, for possible trimming later.
    drawingPoints <- type == "p" || type == "o" || type == "b"
    if (drawingPoints) {
        # BOOKMARK 1a: alter 1b if more added or subtracted here
        nx <- length(x)
        col <- rep(col, length.out=nx)
        pch <- rep(pch, length.out=nx)
        cex <- rep(cex, length.out=nx)
        oceDebug(debug, "made col, pch and cex of length ", nx, " to match length(x)\n")
    }

    pc <- paletteCalculations(maidiff=rep(0, 4))
    oceDebug(debug, as.character(dput(pc)), "\n", style="red")
    par(mgp=mgp, mar=mar)
    args <- list(...)
    xlimGiven <- !missing(xlim)
    # Trim data to visible window (to improve speed and to facilitate 'simplify' calculation)
    if (xlimGiven) {
        if (2 != length(xlim))
            stop("'xlim' must be of length 2, but it is of length ", length(xlim))
        if (xlim[2] <= xlim[1])
            stop("the elements of xlim must be in order, but they are ",
                 format(xlim[1]), " and ", format(xlim[2]), ", respectively")
        # Comment-out next line for issue 1508, since trim_ts
        # fails if times are NA.
        # ends <- trim_ts(x, xlim, 0.04)
        dx  <- diff(as.numeric(xlim))
        keep <- (xlim[1] - dx*0.04) < x & x < (xlim[2] + dx*0.04)
        x <- x[keep]
        y <- y[keep]
        if (drawingPoints) {
            # BOOKMARK 1b: alter 1a if more added or subtracted here
            col <- col[keep]
            pch <- pch[keep]
            cex <- cex[keep]
        }
    }
    if (length(y) == 1)
        y <- rep(y, length(x))
    if (despike)
        y <- despike(y)
    if (marginsAsImage) {
        # FIXME: obey their mar?
        the.mai <- pc$mai0
        the.mai <- clipmin(the.mai, 0)         # just in case
        oceDebug(debug, "the.mai=", vectorShow(the.mai))
        par(mai=the.mai)
        drawPalette(mai=rep(0, 4))
    }
    # Find data ranges. Note that x is a time, so it's either going to be NA or
    # sensible; thus the na.rm argument to range() is suitable for trimming bad
    # values.  However, for y, we emulate plot(), by trimming (and warning).
    if ("y" %in% log) {
        yBAD <- (!is.finite(y)) | y <= 0.0
        nyBAD <- sum(yBAD)
        if (nyBAD > 0L) {
            warning(nyBAD, " y value <= 0 omitted from logarithmic oce.plot.ts\n")
            x <- x[!yBAD]
            y <- y[!yBAD]
        }
    }
    # Handle 'simplify' argument
    nx <- length(x)
    if (type == "l" && is.numeric(simplify) & nx > (5L*simplify)) {
        warning("simplifying a large dataset; set simplify=NA to see raw data\n")
        xgrid <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=simplify)
        df <- data.frame(x, y)
        # Tests N=1e8 suggest split(X,findInterval()) is 2X faster than split(X,cut())
        #>>> dfSplit <- split(df, cut(df$x, xgrid))
        dfSplit <- split(df, as.factor(findInterval(df$x, xgrid)))
        # Compute within the sub-intervals, inserting NAs when no data there
        tz <- attr(x, "tzone")         # cause gridded x to inherit timezone from original x
        x <- rep(unname(sapply(dfSplit, function(DF) if (length(DF$x) > 2) mean(DF$x, na.rm=TRUE) else NA)), each=2)
        x <- numberAsPOSIXct(x, tz=tz)
        ymin <- unname(sapply(dfSplit, function(DF) if (length(DF$y) > 2) min(DF$y, na.rm=TRUE) else NA))
        ymax <- unname(sapply(dfSplit, function(DF) if (length(DF$y) > 2) max(DF$y, na.rm=TRUE) else NA))
        y <- as.vector(rbind(ymin, ymax))
        # Remove any segments for which min and max could not be computed
        bad <- !is.finite(y)
        x <- x[!bad]
        y <- y[!bad]
    }
    xrange <- range(x, na.rm=TRUE)
    yrange <- range(y, finite=TRUE)
    maybeflip <- function(y) if (flipy) rev(sort(y)) else y
    if (!is.finite(yrange[1])) {
        plot(xrange, c(0, 1), axes=FALSE, xaxs=xaxs, yaxs=yaxs,
             xlim=if (xlimGiven) xlim else xrange,
             ylim=if (missing(ylim)) maybeflip(range(y, na.rm=TRUE)) else maybeflip(ylim),
             xlab=xlab, ylab=ylab, type='n', log=log, col=col, pch=pch, cex=cex)
        oce.axis.POSIXct(1, drawTimeRange=FALSE)
        box()
        mtext("bad data", side=3, line=-1, cex=cex)
        warning("no valid data for '", ylab, "'", sep="")
        oceDebug(debug, "} # oce.plot.ts()\n", unindent=1, style="bold")
        return()
    } else {
        if (fill) {
            xx <- c(x[1], x, x[length(x)])
            yy <- c(0, y, 0)
            plot(x, y, axes=FALSE, xaxs=xaxs, yaxs=yaxs,
                 xlim=if (xlimGiven) xlim else range(x, na.rm=TRUE),
                 ylim=if (missing(ylim)) maybeflip(range(y, na.rm=TRUE)) else maybeflip(ylim),
                 xlab="", ylab="",
                 type=type, col=col, cex=cex, pch=pch, log=log, ...)
            mtext(xlab, side=1, cex=cex.lab*par("cex"), line=mgp[1])
            mtext(ylab, side=2, cex=cex.lab*par("cex"), line=mgp[1])
            fillcol <- if ("col" %in% names(args)) args$col else "lightgray" # FIXME: should this be a formal argument?
            do.call(polygon, list(x=xx, y=yy, col=fillcol))
        } else {
            plot(x, y, axes=FALSE, xaxs=xaxs, yaxs=yaxs,
                 xlim=if (missing(xlim)) NULL else xlim,
                 ylim=if (missing(ylim)) maybeflip(range(y, na.rm=TRUE)) else maybeflip(ylim),
                 xlab="", ylab="",
                 type=type, col=col, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, pch=pch, log=log, ...)
            #mtext(paste("TEST: xlab at mgp[1]", xlab), side=1, cex=cex.lab*par('cex'), line=mgp[1])
            mtext(ylab, side=2, cex=cex.lab*par('cex'), line=mgp[1])
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
                                          cex.axis=cex.axis, cex.main=cex.main, cex.lab=cex.lab,
                                          tformat=tformat,
                                          debug=debug-1)
                xat <- xlabs
                oceDebug(debug, "drawing x axis; set xat=c(", paste(xat, collapse=","), ")", "\n", sep="")
            }
            if (FALSE && grid) { # FIXME: don't w do this below? Why here also?
                lwd <- par("lwd")
                if (drawxaxis)
                    abline(v=xlabs, col="lightgray", lty="dotted", lwd=lwd)
                yaxp <- par("yaxp")
                if (log == "y") {
                    abline(h=axTicks(2), col="lightgray", lty="dotted", lwd=lwd)
                } else {
                    abline(h=seq(yaxp[1], yaxp[2], length.out=1+yaxp[3]),
                           col="lightgray", lty="dotted", lwd=lwd)
                }
            }
            box()
            #cat("cex.axis=",cex.axis,"; par('cex.axis') is", par('cex.axis'), "; par('cex') is", par('cex'), "\n")
            if (drawyaxis) {
                if (log != "y" || logStyle == "r") {
                    axis(2, cex.axis=cex.axis, cex=cex.axis)
                    yat <- axis(4, labels=FALSE)
                } else if (log == "y" && logStyle == "decade") {
                    yat <- oceAxis(2, logStyle=logStyle, cex.axis=cex.axis, cex=cex.axis)
                    oceAxis(4, logStyle=logStyle, labels=FALSE)
                } else if (log == "y") {
                    stop("if log=\"y\", then logStyle must be \"r\" or \"decade\", not \"", logStyle, "\"")
                }
            }
        }
        if (grid) {
            if (log == "y") {
                at <- axTicks(2)
                lat <- log10(at)
                powerOfTen <- abs(lat - round(lat)) < 1e-4
                    abline(h=if (any(powerOfTen)) at[powerOfTen] else at,
                           col=grid.col, lty=grid.lty, lwd=grid.lwd)
            } else {
                abline(h=axTicks(2), col=grid.col, lty=grid.lty, lwd=grid.lwd)
            }
            abline(v=axTicks(1), col=grid.col, lty=grid.lty, lwd=grid.lwd)
        }
        oceDebug(debug, "} # oce.plot.ts()\n", unindent=1, style="bold")
        invisible(list(xat=xat, yat=yat))
    }
}                                      # oce.plot.ts()



#' Edit an Oce Object
#'
#' Edit an element of an oce object, inserting a note in the processing
#' log of the returned object.
#'
#' There are several ways to use this function.
#'
#' 1. If both an `item` and `value` are supplied, then
#' either the object's metadata or data slot may be altered. There are
#' two ways in which this can be done.
#'
#'    * Case 1A. If the `item` string does not contain an
#'    `@` character, then the `metadata` slot is examined
#'    for an entry named `item`, and that is modified if so.
#'    Alternatively, if `item` is found in `metadata`, then
#'    that value is modified. However, if `item` is not found in
#'    either `metadata` or `data`, then an error is reported
#'    (see 1B for how to add something that does not yet exist).
#'
#'    * Case 1B. If the `item` string contains
#'    the `@` character, then the text to the left of that character
#'    must be either `"metadata"` or `"data"`, and it names the slot
#'    in which the change is done. In contrast with case 1A, this will
#'    *create* a new item, if it is not already in existence.
#'
#'
#' 2. If `item` and `value` are not supplied, then `action` must
#' be supplied.  This is a character string specifying some action to be
#' performed on the object, e.g. a manipulation of a column.  The action must
#' refer to the object as `x`; see Examples.
#'
#' In any case, a log entry is stored in the object, to document the change.
#' Indeed, this is the main benefit to using this function, instead of altering
#' the object directly.  The log entry will be most useful if it contains a
#' brief note on the `reason` for the change, and the name of the
#' `person` doing the work.
#'
#' @aliases oce.edit
#'
#' @param x an [oce-class] object.  The exact action of [oceEdit()] depends
#' on the sub-class of `x`.
#'
#' @param item if supplied, a character string naming an item in the object's
#' `metadata` or `data` slot, the former being checked first.
#' An exception is if `item` starts with `"data@"` or
#' `"metadata@"`, in which case the named slot is updated with a changed
#' value of the contents of `item` after the `@` character.
#'
#' @param value new value for `item`, if both supplied.
#'
#' @param action optional character string containing R code to carry out some
#' action on the object.
#'
#' @param reason character string giving the reason for the change.
#'
#' @param person character string giving the name of person making the change.
#'
#' @param debug an integer that specifies a level of debugging, with 0 or less
#' indicating no debugging, and 1 or more indicating debugging.
#'
#' @return A [oce-class] object, altered
#' appropriately, and with a log item indicating the nature of the alteration.
#'
#' @author Dan Kelley
#'
#' @examples
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
        #oceDebug(debug, "ORIG item='", item, "'\n", sep="")
        # Split out the slotname, if any.
        if (length(grep("@", item))) {
            slot <- gsub("@.*$", "", item)
            if (slot != "metadata" && slot != "data")
                stop("slot must be 'metadata' or 'data'")
            item <- gsub("^.*@", "", item)
        }
        #oceDebug(debug, "LATER slot='", slot, "' and item='", item, "'\n", sep="")
        if (missing(value))
            stop("must supply a 'value' for this 'item'")
        if (inherits(x, "adv")) {
            oceDebug(debug, "object is an ADV\n")
            hpr <- 0 < length(grep("heading|pitch|roll", item))
            if (hpr) {
                # FIXME: I think this violates the 1A rule on creating new data,
                # FIXME: but I am retaining this since it's years old.
                # FIXME: why are adp and adv handled differently, anyway? Is
                # FIXME: this a fast/slow variable issue?
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
                # FIXME: I think this violates the 1A rule on creating new data,
                # FIXME: but I am retaining this since it's years old.
                # FIXME: why are adp and adv handled differently, anyway? Is
                # FIXME: this a fast/slow variable issue?
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
            # FIXME: what if S4?
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
#' The output has a line containing the names of the columns in `x$data`,
#' each enclosed in double quotes.  After that line are lines for the data
#' themselves.  The default is to separate data items by a single space
#' character, but this can be altered by using a `sep` argument in the
#' `...` list; see [utils::write.table()].
#'
#' This function is little more than a thin wrapper around
#' [utils::write.table()], the only difference being that row names
#' are omitted here, making for a file format that is more conventional in
#' Oceanography.
#'
#' @param x an [oce-class] object.
#'
#' @param file file name, as passed to [utils::write.table()].  Use
#' `""` to get a listing in the terminal window.
#'
#' @param ... optional arguments passed to [utils::write.table()].
#'
#' @return The value returned by [utils::write.table()].
#'
#' @author Dan Kelley
#'
#' @seealso `[utils::write.table()], which does the actual work.
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
#' This returns a vector of numbers that build upon the shorter lists
#' provided in Chapter 10 of reference 1 and the more modern World
#' Ocean Atlases (e.g. reference 2).
#' With the default call,
#' i.e. with `n=0`, the result is
#' \code{c(0, 10, 20, 30, 40, 50, 75, 100, 125, 150, 200, 250,
#' seq(300, 1500, by=100), 1750, seq(2000, 10000, by=500))}.
#' For higher values of `n`, progressively more and more values
#' are added between each pair in this sequence.
#' See the documentation for
#' [sectionGrid()] for how `standardDepths` can be used
#' in gridding data for section plots.
#'
#' @param n Integer specifying the number of subdivisions to insert between
#' each of the stated levels. For exmple, setting `n=1` puts a 5m level
#' between the 0 and 10m levels, and `n=2` puts 3.33 and 6.66 between
#' 0 and 10m.
#'
#' @return A vector of depths that are more closely spaced for small values,
#' i.e. a finer grid near the ocean surface.
#'
#' @examples
#' depth  <- standardDepths()
#' depth1 <- standardDepths(1)
#' plot(depth, depth)
#' points(depth1, depth1, col=2, pch=20, cex=1/2)
#'
#' @author Dan Kelley
#'
#' @references
#' 1. Sverdrup, H U, Martin W Johnson, and Richard H Fleming. The Oceans,
#' Their Physics, Chemistry, and General Biology. New York: Prentice-Hall, 1942.
## `https://ark.cdlib.org/ark:/13030/kt167nb66r`
## next worked well most of the time, but I got a failure on 2021-08-07 and
## I just don't see the point in retaining a link that will *ever* fail, given
## the possible consequence in terms of CRAN.
#' \code{https://publishing.cdlib.org/ucpressebooks/view?docId=kt167nb66r}
#'
#' 2.Locarnini, R. A., A. V. Mishonov, J. I. Antonov, T. P. Boyer,
#' H. E. Garcia, O. K. Baranova, M. M. Zweng, D. R. Johnson, and
#' S. Levitus. \dQuote{World Ocean Atlas 2009 Temperature.}
#' US Government printing Office, 2010.
standardDepths <- function(n=0)
{
    d <- c(0, 10, 20, 30, 40, 50, 75, 100, 125, 150, 200, 250,
           seq(300, 1500, by=100), 1750, seq(2000, 10000, by=500))
    n <- as.integer(n)
    if (n < 0)
        stop("cannot have negative n")
    if (n == 0)
        return(d)
    ul <- cbind(head(d, -1), tail(d, -1))
    res <- NULL
    for (i in seq_len(nrow(ul)))
        res <- c(res, approx(c(0, 1), ul[i,], seq(0, 1, by=1/(n+1)))$y)
    unique(res) # remove duplicates from one 'l' being the next 'u'
}

#' Find the Type of an Oceanographic Data File
#'
#' `oceMagic` tries to infer the file type, based on the data
#' within the file, the file name, or a combination of the two.
#'
#' `oceMagic` was previously called `oce.magic`, but that
#' alias was removed in version 0.9.24; see [oce-defunct].
#'
#' @param file a connection or a character string giving the name of the file
#' to be checked.
#'
#' @template encodingTemplate
#'
#' @param debug an integer, set non-zero to turn on debugging.  Higher values
#' indicate more debugging.
#'
#' @return A character string indicating the file type, or `"unknown"`, if
#' the type cannot be determined. If the result contains `"/"` characters,
#' these separate a list describing the file type, with the first element being
#' the general type, the second element being the manufacturer, and the third
#' element being the manufacturer's name for the instrument. For example,
#' `"adp/nortek/aquadopp"` indicates a acoustic-doppler profiler made by
#' NorTek, of the model type called Aquadopp.
#'
#' @author Dan Kelley
#'
#' @seealso This is used mainly by [read.oce()].
oceMagic <- function(file, encoding="latin1", debug=getOption("oceDebug"))
{
    filename <- file
    oceDebug(debug, paste("oceMagic(file=\"", filename, "\") {\n", sep=""), unindent=1, style="bold", sep="")
    isdir<- file.info(file)$isdir
    if (is.finite(isdir) && isdir) {
        tst <- file.info(paste(file, "/", file, "_MTL.txt", sep=""))$isdir
        if (!is.na(tst) && !tst) {
            oceDebug(debug, "} # oceMagic returning landsat\n", unindent=1)
            return("landsat")
        } else {
            stop("please supply a file name, not a directory name")
        }
    }
    if (is.character(file)) {
        oceDebug(debug, "'file' is a character value\n")
        if (grepl(".asc$", filename)) {
            someLines <- readLines(file, encoding=encoding, n=1)
            if (42 == length(strsplit(someLines[1], ' ')[[1]])) {
                oceDebug(debug, "} # oceMagic returning lisst\n", unindent=1, style="bold")
                return("lisst")
            }
        }
        if (grepl(".adr$", filename)) {
            oceDebug(debug, "file names ends in .adr, so this is an adv/sontek/adr file.\n")
            oceDebug(debug, "} # oceMagic returning adv/sontek/adr\n", unindent=1)
            return("adv/sontek/adr")
        }
        if (grepl(".rsk$", filename)) {
            oceDebug(debug, "file names ends with \".rsk\", so this is an RBR/rsk file.\n")
            oceDebug(debug, "} # oceMagic returning RBR/rsk\n", unindent=1)
            return("RBR/rsk")
        }
        if (grepl(".s4a.", filename)) {
            oceDebug(debug, "file names contains \".s4a.\", so this is an interocean S4 file.\n")
            oceDebug(debug, "} # oceMagic returning interocean/s4\n", unindent=1)
            return("interocean/s4")
        }
        if (grepl(".ODF$", filename, ignore.case=TRUE)) {
            # in BIO files, the data type seems to be on line 14.  Read more, for safety.
            lines <- readLines(file, encoding=encoding)
            dt <- grep("DATA_TYPE[ \t]*=", lines)
            if (length(dt) < 1)
                stop("cannot infer type of ODF file")
            subtype <- gsub("[',]", "", tolower(strsplit(lines[dt[1]], "=")[[1]][2]))
            subtype <- gsub("^\\s*", "", subtype)
            subtype <- gsub("\\s*$", "", subtype)
            res <- paste(subtype, "odf", sep="/")
            oceDebug(debug, paste0("} # oceMagic() returning ", res, "\n"), unindent=1, style="bold")
            return(res)
        }
        if (grepl(".WCT$", filename, ignore.case=TRUE)) {
            # old-style WOCE
            oceDebug(debug, "} # oceMagic returning ctd/woce/other\n", unindent=1)
            return("ctd/woce/other") # e.g. http://cchdo.ucsd.edu/data/onetime/atlantic/a01/a01e/a01ect.zip
        }
        if (grepl(".nc$", filename, ignore.case=TRUE)) {
            # argo or netcdf?
            if (requireNamespace("ncdf4", quietly=TRUE)) {
                if (substr(filename, 1, 5) == "http:") {
                    stop("cannot open netcdf files over the web; try doing as follows\n    download.file(\"",
                         filename, "\", \"", gsub(".*/", "", filename), "\")")
                }
                # NOTE: need to name ncdf4 package because otherwise R checks give warnings.
                f <- ncdf4::nc_open(filename)
                if ("DATA_TYPE" %in% names(f$var)) {
                    if (grepl("argo", ncdf4::ncvar_get(f, "DATA_TYPE"), ignore.case=TRUE))  {
                        oceDebug(debug, "} # oceMagic returning argo (upper-case style)\n", unindent=1)
                        return("argo")
                    } else {
                        oceDebug(debug, "} # oceMagic returning netcdf (upper-case style)\n", unindent=1)
                        return("netcdf")
                    }
                } else if ("data_type" %in% names(f$var)) {
                    if (grepl("argo", ncdf4::ncvar_get(f, "data_type"), ignore.case=TRUE)) {
                        oceDebug(debug, "} # oceMagic returning argo (lower-case style)\n", unindent=1)
                        return("argo")
                    } else {
                        oceDebug(debug, "} # oceMagic returning netcdf (lower-case style)\n", unindent=1)
                        return("netcdf")
                    }
                }
            } else {
                stop('must install.packages("ncdf4") to read a NetCDF file')
            }
        }
        if (grepl(".xml$", filename, ignore.case=TRUE)) {
            firstLine <- readLines(filename, 1, encoding=encoding)
            if (grepl(".weather.gc.ca", firstLine)) {
                oceDebug(debug, "} # oceMagic returning met/xml2\n", unindent=1)
                return("met/xml2")
            }
        }
        if (grepl(".osm.xml$", filename, ignore.case=TRUE)) {
            oceDebug(debug, "} # oceMagic returning openstreetmap (xml style)\n", unindent=1)
            return("openstreetmap")
        }
        if (grepl(".osm$", filename, ignore.case=TRUE)) {
            oceDebug(debug, "} # oceMagic returning openstreetmap (non xml style)\n", unindent=1, style="bold")
            return("openstreetmap")
        }
        if (grepl(".gpx$", filename, ignore.case=TRUE)) {
            oceDebug(debug, "} # oceMagic returning gpx (e.g. Garmin GPS data)\n", unindent=1, style="bold")
            return("gpx")
        }
        if (grepl(".csv$", filename, ignore.case=TRUE)) {
            someLines <- readLines(filename, 30, encoding="UTF-8-BOM")
            #print(someLines[1])
            if (grepl("^SSDA Sea & Sun Technology", someLines[1], useBytes=TRUE)) {
                return("ctd/ssda")
            } else if (1L == length(grep('^.*"WMO Identifier",', someLines))) {
                oceDebug(debug, "} # oceMagic returning met/csv1\n", unindent=1, style="bold")
                return("met/csv1") # FIXME: may be other things too ...
            } else if (grepl('^.*Longitude.*Latitude.*Station Name.*Climate ID.*Dew Point', someLines[1])) {
                oceDebug(debug, "} # oceMagic returning met/csv2 or met/csv3\n", unindent=1, style="bold")
                if (grepl("Time \\(LST\\)", someLines[1], useBytes=TRUE)) {
                    oceDebug(debug, "} # oceMagic returning met/csv2\n", unindent=1, style="bold")
                    return("met/csv3" )
                } else {
                    oceDebug(debug, "} # oceMagic returning met/csv3\n", unindent=1, style="bold")
                    return("met/csv2")
                }
            } else if (length(grep("^Station_Name,", someLines, useBytes=TRUE))) {
                oceDebug(debug, "} # oceMagic returning sealevel\n", unindent=1, style="bold")
                return("sealevel")
            } else if (1L == length(grep("^CTD,", someLines, useBytes=TRUE))) {
                oceDebug(debug, "} # oceMagic returning ctd/woce/exchange\n", unindent=1, style="bold")
                return("ctd/woce/exchange")
            } else if (1L == length(grep("^BOTTLE,", someLines, useBytes=TRUE))) {
                oceDebug(debug, "} # oceMagic returning section\n", unindent=1, style="bold")
                return("section")
            } else {
                return("unknown")
            }
        }
        if (grepl(".edf$", filename, ignore.case=TRUE)) {
            oceDebug(debug, "} # oceMagic returning xbt/edf\n", unindent=1, style="bold")
            return("xbt/edf")
        }
        file <- file(file, "r")
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    oceDebug(debug, "'file' is a connection\n")
    if (!isOpen(file))
        open(file, "r")
    # Grab text at start of file.
    lines <- readLines(file, n=2, skipNul=TRUE, encoding=encoding)
    line <- lines[1]
    line2 <- lines[2]
    oceDebug(debug, "first line of file: ", line, "\n", sep="")
    oceDebug(debug, "second line of file: ", line2, "\n", sep="")
    close(file)
    file <- file(filename, "rb")
    bytes <- readBin(file, what="raw", n=4)
    oceDebug(debug, paste("first two bytes in file: 0x", bytes[1], " and 0x", bytes[2], "\n", sep=""))
    on.exit(close(file))
    if (bytes[1] == 0x00 && bytes[2] == 0x00 && bytes[3] == 0x27 && bytes[4] == 0x0a) {
        oceDebug(debug, "this is a shapefile; see e.g. http://en.wikipedia.org/wiki/Shapefile\n  }\n")
        oceDebug(debug, "} # oceMagic returning shapefile\n", unindent=1, style="bold")
        return("shapefile")
    }
    if (bytes[3] == 0xff && bytes[4] == 0xff) {
        oceDebug(debug, "} # oceMagic returning echosounder\n", unindent=1, style="bold")
        return("echosounder")
    }
    if (bytes[1] == 0x10 && bytes[2] == 0x02) {
        # 'ADPManual v710.pdf' p83
        if (96 == readBin(bytes[3:4], "integer", n=1, size=2, endian="little"))
            oceDebug(debug, "this is adp/sontek (4 byte match)\n  }\n")
        else
            oceDebug(debug, "this is adp/sontek (2 byte match, but bytes 3 and 4 should become integer 96)\n  }\n")
        oceDebug(debug, "} # oceMagic returning adp/sontek\n", unindent=1, style="bold")
        return("adp/sontek")
    }
    if (bytes[1] == 0x7f && bytes[2] == 0x7f) {
        oceDebug(debug, "} # oceMagic returning adp/rdi\n", unindent=1, style="bold")
        return("adp/rdi")
    }
    if (bytes[1] == 0xa5 && bytes[2] == 0x05) {
        # NorTek files require deeper inspection.  Here, SIG stands for "System Integrator Guide",
        # Dated Jue 2008 (Nortek Doc No PS100-0101-0608)
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
            oceDebug(debug, "} # oceMagic returning adv/nortek/vector\n", unindent=1, style="bold")
            return("adv/nortek/vector")
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x01) {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudopp (see system-integrator-manual_jan2011.pdf Table 5.2)\n")
            oceDebug(debug, "} # oceMagic returning adp/nortek/aquadopp\n", unindent=1, style="bold")
            return("adp/nortek/aquadopp")
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x81) {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudopp (see N3015-023-Integrators-Guide-Classic_1220.pdf page 30)\n")
            oceDebug(debug, "} # oceMagic returning adp/nortek/aquadoppPlusMagnetometer\n", unindent=1, style="bold")
            return("adp/nortek/aquadoppPlusMagnetometer")
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x21)  {
            oceDebug(debug, "} # oceMagic returning adp/nortek/aquadoppProfiler\n", unindent=1, style="bold")
            return("adp/nortek/aquadoppProfiler") # p37 SIG
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x2a)  {
            oceDebug(debug, "} # oceMagic returning adp/nortek/aquadoppHR\n", unindent=1, style="bold")
            return("adp/nortek/aquadoppHR") # p38 SIG
        }
        stop("some sort of nortek ... two bytes are 0x", nextTwoBytes[1], " and 0x", nextTwoBytes[2], " but cannot figure out what the type is")
    }
    if (bytes[1] == 0xa5 && bytes[4] == 0x10) {
        oceDebug(debug, "} # oceMagic returning adp/nortek/ad2cp\n", unindent=1)
        return("adp/nortek/ad2cp")
    }
    if (bytes[1] == 0x9b && bytes[2] == 0x00) {
        warning(paste("Possibly this is an RDI CTD file. Oce cannot read such files yet, because\n",
                      " the author has not located file-format documents.  If you get such documents\n",
                      " from RDI, please send them to dan.kelley@dal.ca so the format can be added."))
        return("possibly RDI CTD")
    }
    if (1 == length(grep("^CTD", line, useBytes=TRUE))) {
        oceDebug(debug, "} # oceMagic returning ctd/woce/exchange\n", unindent=1, style="bold")
        return("ctd/woce/exchange")
    }
    if (1 == length(grep("^EXPOCODE", line, useBytes=TRUE))) {
        oceDebug(debug, "} # oceMagic returning ctd/woce/other\n", unindent=1, style="bold")
        return("ctd/woce/other")
    }
    if (1 == length(grep("^\\s*ODF_HEADER", line, useBytes=TRUE))){
        oceDebug(debug, "} # oceMagic returning odf\n", unindent=1, style="bold")
        return("odf")
    }
    if (grepl("^\\* Sea-Bird SBE", line, useBytes=TRUE) ||
        grepl("^\\* Viking Buoy CTD file", line, useBytes=TRUE)) {
        oceDebug(debug, "} # oceMagic returning ctd/sbe\n", unindent=1, style="bold")
        return("ctd/sbe")
    }

    if (1 == length(grep("^%ITP", line, useBytes=TRUE))) {
        oceDebug(debug, "} # oceMagic returning ctd/itp\n", unindent=1, style="bold")
        return("ctd/itp")
    }
    if (1 == length(grep("^# -b", line, useBytes=TRUE))) {
        oceDebug(debug, "} # oceMagic returning coastline\n", unindent=1, style="bold")
        return("coastline")
    }
    if (1 == length(grep("^# Station_Name,", line, useBytes=TRUE))) {
        oceDebug(debug, "} # oceMagic returning sealevel\n", unindent=1, style="bold")
        return("sealevel")
    }
    if (1 == length(grep("^Station_Name,", line, useBytes=TRUE))) {
        oceDebug(debug, "} # oceMagic returning sealevel\n", unindent=1, style="bold")
        return("sealevel")
    }
    if (1 == length(grep("^[0-9][0-9][0-9][A-Z] ", line, useBytes=TRUE))) {
        oceDebug(debug, "} # oceMagic returning sealevel\n", unindent=1, style="bold")
        return("sealevel")
    }
    if (1 == length(grep("^NCOLS[ ]*[0-9]*[ ]*$", line, useBytes=TRUE, ignore.case=TRUE))) {
        oceDebug(debug, "} # oceMagic returning topo\n", unindent=1, style="bold")
        return("topo")
    }
    if (1 == length(grep("^RBR TDR", line, useBytes=TRUE))) {
        # FIXME: obsolete; to be removed Fall 2015
        oceDebug(debug, "} # oceMagic returning RBR/dat\n", unindent=1, style="bold")
        return("RBR/dat")
    }
    if (1 == length(grep("^Model=", line, useBytes=TRUE))) {
        oceDebug(debug, "} # oceMagic returning RBR/txt\n", unindent=1, style="bold")
        return("RBR/txt")
    }
    if (1 == length(grep("^BOTTLE", line, useBytes=TRUE)))  {
        oceDebug(debug, "} # oceMagic returning section\n", unindent=1, style="bold")
        return("section")
    }
    oceDebug(debug, "this is unknown\n")
    return("unknown")
}



#' Read an Oceanographic Data File
#'
#' Read an oceanographic data file, auto-discovering the file type from the
#' first line of the file.
#' This function tries to infer the file type from the first line, using
#' [oceMagic()].  If it can be discovered, then an
#' instrument-specific file reading function is called, with the `file`
#' and with any additional arguments being supplied.
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#'
#' @param ... arguments to be handed to whichever instrument-specific reading
#' function is selected, based on the header.
#'
#' @param encoding a character string giving the file encoding.  This defaults
#' to `"latin1`", which seems to work for files available to the authors, but
#' be aware that a different setting may be required for files that contain
#' unusual accents or characters.  (Try `"UTF-8"` if the default produces
#' errors.) Note that `encoding` is ignored in binary files, and also
#' in some text-based files, as well.
#'
#' @return An [oce-class] object of that is
#' specialized to the data type, e.g. [ctd-class],
#' if the data file contains `ctd` data.
#'
#' @author Dan Kelley
#'
#' @seealso The file type is determined by [oceMagic()].  If the file
#' type can be determined, then one of the following is called:
#' [read.ctd()], [read.coastline()]
#' [read.lobo()], [read.rsk()],
#' [read.sealevel()], etc.
#'
#' @examples
#' library(oce)
#' x <- read.oce(system.file("extdata", "ctd.cnv", package="oce"))
#' plot(x) # summary with TS and profiles
#' plotTS(x) # just the TS
read.oce <- function(file, ..., encoding="latin1")
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    dots <- list(...)
    debug <- if ("debug" %in% names(dots)) as.integer(dots$debug) else 0L
    debug <- max(0L, debug)
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file) && "http://" != substr(file, 1, 7) && !file.exists(file))
        stop("In read.oce() : cannot open '", file, "' because there is no such file or directory", call.=FALSE)
    if (is.character(file) && "http://" != substr(file, 1, 7) && 0 == file.info(file)$size)
        stop("empty file")
    type <- oceMagic(file, debug=debug-1)
    oceDebug(debug, "read.oce(\"", as.character(file), "\") {\n", sep="", unindent=1, style="bold")
    if (is.character(file) && "http://" != substr(file, 1, 7) && 0 == file.info(file)$size)
        stop("empty file")
    #> OLD: deparse is unhelpful if "file" is a variable in the calling code
    #> OLD: processingLog <- paste(deparse(match.call()), sep="", collapse="")
    processingLog <- paste('read.oce("', file, '"', ifelse(length(list(...)), ", ...)", ")"), sep="")

    # read.index if (type == "index")
    # read.index     return(read.index(file))
    if (type == "shapefile") {
        res <- read.coastline.shapefile(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "openstreetmap") {
        res <- read.coastline.openstreetmap(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "echosounder") {
        res <- read.echosounder(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "adp/rdi") {
        res <- read.adp.rdi(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "adp/sontek") {
        res <- read.adp.sontek(file, encoding=encoding, processingLog=processingLog, ...) # FIXME is pcadcp different?
    } else if (type == "adp/nortek/aquadopp") {
        res <- read.aquadopp(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "adp/nortek/aquadoppPlusMagnetometer") {
        res <- read.aquadopp(file, type="aquadoppPlusMagnetometer", encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "adp/nortek/aquadoppProfiler") {
        res <- read.aquadoppProfiler(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "adp/nortek/aquadoppHR") {
        res <- read.aquadoppHR(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "adp/nortek/ad2cp") {
        res <- read.adp.ad2cp(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "adv/nortek/vector") {
        res <- read.adv.nortek(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "adv/sontek/adr") {
        res <- read.adv.sontek.adr(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "aml/txt") {
        res <- read.ctd.aml(file, encoding=encoding, processingLog=processingLog, ...)
    # FIXME need adv/sontek (non adr)
    } else if (type == "interocean/s4") {
        res <- read.cm.s4(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "ctd/sbe") {
        res <- read.ctd.sbe(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "ctd/woce/exchange") {
        res <- read.ctd.woce(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "ctd/woce/other") {
        res <- read.ctd.woce.other(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "ctd/odf" || type == "mctd/odf" || type == "mvctd/odf") {
        res <- read.ctd.odf(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (length(grep(".odf$", type))) {
        res <- read.odf(file, encoding=encoding, ...)
    } else if (type == "mtg/odf") {
        # FIXME: document this data type
        # Moored tide gauge: returns a data frame.
        fromHeader <- function(key)
        {
            i <- grep(key, lines)
            if (length(i) < 1)
                ""
            else
                gsub("\\s*$", "", gsub("^\\s*", "", gsub("'", "", gsub(",", "", strsplit(lines[i[1]], "=")[[1]][2]))))
        }
        lines <- readLines(file, encoding=encoding)
        nlines <- length(lines)
        headerEnd <- grep("-- DATA --", lines)
        if (1 != length(headerEnd))
            stop("found zero or multiple '-- DATA --' (end of header) lines in a mtg/odf file")
        #header <- lines[1:headerEnd]
        data <- lines[seq.int(headerEnd+1, nlines)]
        res <- read.table(text=data, header=FALSE, col.names=c("time", "temperature", "ptotal", "psea", "depth"))
        res$time <- strptime(res$time, "%d-%B-%Y %H:%M:%S", tz="UTC") # guess on timezone
        missing_value <- -99.0 # FIXME: it's different for each column
        res[res==missing_value] <- NA
        attr(res, "scientist") <- fromHeader("CHIEF_SCIENTIST")
        attr(res, "latitude") <- as.numeric(fromHeader("INITIAL_LATITUDE"))
        attr(res, "longitude") <- as.numeric(fromHeader("INITIAL_LONGITUDE"))
        attr(res, "cruise_name") <- fromHeader("CRUISE_NAME")
        attr(res, "cruise_description") <- fromHeader("CRUISE_DESCRIPTION")
        attr(res, "inst_type") <- fromHeader("INST_TYPE")
        attr(res, "model") <- fromHeader("MODEL")
        attr(res, "serial_number") <- fromHeader("SERIAL_NUMBER")
        attr(res, "missing_value") <- missing_value
        warning("Missing-value code for mtg/odf is hard-wired to -99, which will likely be wrong in other files")
        warning("The format of mtg/odf objects is likely to change throughout April, 2015")
    # if (type == "ctd/odv")
    #     return(read.ctd.odv(file, processingLog=processingLog, ...))
    } else if (type == "ctd/itp") {
        res <- read.ctd.itp(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "ctd/ssda") {
        res <- read.ctd.ssda(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "gpx") {
        res <- read.gps(file, type="gpx", encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "coastline") {
        res <- read.coastline(file, type="mapgen", encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "argo") {
        res <- read.argo(file, encoding=encoding, ...)
    } else if (type == "lisst") {
        res <- read.lisst(file, encoding=encoding)
    } else if (type == "sealevel") {
        res <- read.sealevel(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "topo") {
        res <- read.topo(file)
    } else if (type == "RBR/dat") { # FIXME: obsolete; to be removed by Fall 2015
        res <- read.rsk(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "RBR/rsk") {
        res <- read.rsk(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "RBR/txt") {
        res <- read.rsk(file, encoding=encoding, processingLog=processingLog, type='txt', ...)
    } else if (type == "section") {
        res <- read.section(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "ctd/woce/other") {
        res <- read.ctd.woce.other(file, encoding=encoding, processingLog=processingLog, ...)
    } else if (type == "landsat") {
        res <- read.landsat(file, encoding=encoding, ...)
    } else if (type == "netcdf") {
        res <- read.netcdf(file, encoding=encoding, ...)
    } else if (type == "met/csv1") {
        res <- read.met(file, type="csv1", encoding=encoding, ...)
    } else if (type == "met/csv2") {
        res <- read.met(file, type="csv2", encoding=encoding, ...)
    } else if (type == "met/xml2") {
        res <- read.met(file, type="xml2", encoding=encoding, ...)
    } else if (type == "odf") {
        res <- read.odf(file, encoding=encoding, ...)
    } else if (type == "xbt/edf") {
        res <- read.xbt.edf(file, encoding=encoding, ...)
    } else {
        stop("unknown file type \"", type, "\"")
    }
    oceDebug(debug, "} # read.oce()\n", unindent=1, sep="", style="bold")
    res
}

#' Read a NetCDF File
#'
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
#' `Longitude`, `Latitude`, `Ship` and `Cruise`.
#' Before they are stored in the metadata, they are converted to
#' lower case, since that is the oce convention.
#'
#' @param file the name of a file
#' @template encodingIgnoredTemplate
#' @param ... ignored
#'
#' @return
#' An [oce-class] object.
read.netcdf <- function(file, encoding, ...)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop('must install.packages("ncdf4") to read netcdf data')
    f <- ncdf4::nc_open(file)
    res <- new("oce")
    names <- names(f$var)
    data <- list()

    for (name in names) {
        # message("name=", name)
        if (1 == length(grep("^history_", name)))
            next
        # if (name == "history_institution" || name == "history_step" || name == "history_software"
        #     || name == "history_software_release" || name == "history_reference" || name == "history_date"
        #     || name == "history_action" || name == "history_parameter" || name == "history_start_pres"
        #     || name == "history_stop_pres" || name == "history_previous_value"
        #     || name == "history_qctest")
        #     next
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
    # Try to get some global attributes.
    # Inelegantly permit first letter lower-case or upper-case
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


#' Draw an axis, possibly with decade-style logarithmic scaling
#'
#' @param logStyle a character value that indicates how to draw the y axis, if
#' `log="y"`.  If it is `"r"` (the default) then the conventional R style is used,
#' in which a logarithmic transform connects y values to position on the "page"
#' of the plot device, so that tics will be nonlinearly spaced, but not
#' organized by integral powers of 10.  However, if it is `"decade"`, then
#' the style will be that used in the scientific literature, in which large
#' tick marks are used for integral powers of 10, with smaller tick marks
#' at integral multiples of those powers, and with labels that use exponential
#' format for values above 100 or below 0.01.
#' @param side an integer specifying which axis to draw, with 1 for bottom axis, 2 for left axis,
#' 3 for top axis, and 4 for right axis (as with [axis()]).
#' @param labels either a vector of character values used for labels or a logical value indicating
#' whether to draw such labels.  The first form only works if the coordinate is not logarithmic,
#' and if `logStyle` is `"r"`.
#' @param \dots other graphical parameters, passed to [axis()].
#'
#' @return Numerical values at which tick marks were drawn (or would have been drawn, if `labels`
#' specified to draw them).
#'
#' @examples
#' library(oce)
#' Ra <- 10^seq(4, 10, 0.1)
#' Nu <- 0.085 * Ra^(1/3)
#' plot(Ra, Nu, log="xy", axes=FALSE)
#' box()
#' oceAxis(1, logStyle="decade")
#' oceAxis(2, logStyle="decade")
#'
#' @author Dan Kelley
oceAxis <- function(side, labels=TRUE, logStyle="r", ...)
{
    if (missing(side))
        stop("in oceAxis() :\n  argument \"side\" is missing, with no default", call.=FALSE)
    if (length(side) != 1)
        stop("in oceAxis() :\n  argument \"side\" must be a single number", call.=FALSE)
    if (!(side %in% 1:4))
        stop("in oceAxis() :\n  argument \"side\" must be 1, 2, 3 or 4", call.=FALSE)
    if (!(logStyle %in% c("r", "decade")))
        stop("logStyle must be \"r\" or \"decade\", not \"", logStyle, "\"")
    if (logStyle == "r") {
        return(invisible(axis(side=side, labels=labels, ...)))
    } else {
        # use decade axis if previous plot() call made this coordinate be logarithmic
        if (((side %in% c(1,3)) && par("xlog")) || ((side %in% c(2,4)) && par("ylog"))) {
            usr <- if (side %in% c(1, 3)) par("usr")[1:2] else par("usr")[3:4]
            lowerDecade <- floor(usr[1])
            upperDecade <- floor(1 + usr[2])
            smallTickAt <- NULL
            bigTickAt <- NULL
            bigTickLabel <- NULL
            for (bigTick in lowerDecade:upperDecade) {
                bigTickAt <- c(bigTickAt, bigTick)
                bigTickLabel <- c(bigTickLabel,
                                  if (bigTick < -1L || bigTick > 1L)
                                      substitute(10^A, list(A=bigTick)) else 10^bigTick)

                smallTickAt <- c(smallTickAt, -1 + bigTick + log10(2:9))
                smallTickAt <- c(smallTickAt,      bigTick + log10(2:9))
            }
            bigTickInWindow <- usr[1] <= bigTickAt & bigTickAt <= usr[2]
            bigTickAt <- bigTickAt[bigTickInWindow]
            bigTickLabel <- as.expression(bigTickLabel[bigTickInWindow])
            smallTickInWindow <- usr[1] <= smallTickAt & smallTickAt <= usr[2]
            smallTickAt <- smallTickAt[smallTickInWindow]
            rval <- axis(side=side, at=10^bigTickAt, labels=if(labels) bigTickLabel else FALSE)
            rug(side=side, x=10^smallTickAt, tcl=0.5*par("tcl"), lwd=par("lwd"))
            return(invisible(rval))
        } else {
            return(invisible(axis(side=side, labels=labels, ...)))
        }
    }
}


#' Create two-color palette
#'
#' Create colors ranging between two specified limits, with white
#' in the middle.
#'
#' @aliases oceColorsTwo oce.colorsTwo
#'
#' @param n number of colors to generate.
#'
#' @param low,high numerical values (in range 0 to 1) specifying the hue
#' for the low and high ends of the color scale.
#'
#' @param smax numerical value (in range 0 to 1) for the color saturation.
#'
#' @param alpha numerical value (in ragne 0 to 1) for the alpha (transparency)
#' of the colors.
#'
#' @examples
#' library(oce)
#' imagep(volcano-mean(range(volcano)), col=oceColorsTwo(128),
#'        zlim="symmetric", zlab="oceColorsTwo")
#' @family things related to colors
oceColorsTwo <- function (n, low=2/3, high=0, smax=1, alpha = 1)
{
    # code borrows heavily from cm.color()
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
oce.colorsTwo <- oceColorsTwo

#' Create colors in a Gebco-like scheme
#'
#' The colours were determined by examination of paper
#' charts printed during the GEBCO Fifth Edition era.
#' The hues range from dark blue to light blue, then
#' from light brown to dark brown.  If used to show
#' topography in scheme centred on z=0, this means that
#' near-coastal regions are light in tone, with darker
#' colours representing both mountains and the deep sea.
#'
#' @aliases oceColorsGebco oce.colorsGebco
#'
#' @param n Number of colors to return
#'
#' @param region String indicating application region,
#' one of `"water"`, `"land"`, or `"both"`.
#'
#' @param type String indicating the purpose, one of `"fill"` or `"line"`.
#'
#' @param debug a flag that turns on debugging.
#'
#' @examples
#' library(oce)
#' imagep(volcano, col=oceColorsGebco(128, region="both"))
#'
#' @family things related to colors
oceColorsGebco <- function(n=9, region=c("water", "land", "both"), type=c("fill", "line"), debug=getOption("oceDebug"))
{
    oceDebug(debug, "oceColorsGebco(n=", n, ", region=\"", region, "\", type=\"", type, "\", debug=", debug, ")\n", sep="", unindent=1)
    region <- match.arg(region)
    type <- match.arg(type)

    land <- c("#FEF1E0", "#FDE3C1", "#FBC784", "#F1C37A", "#E6B670", "#DCA865",
              "#D19A5C", "#C79652", "#BD9248", "#B38E3E")
    water <- c("#0F7CAB", "#2292B5", "#38A7BF", "#4FBBC9", "#68CDD4", "#83DEDE",
               "#A0E8E4", "#BFF2EC", "#E1FCF7", "#F0FDFB")
    if (type == "fill") {
        # generate land colors by e.g. rgb(t(col2rgb(land[5])-1*c(10, 4, 10))/255)
        # until 2020-12-14 land <- c("#FBC784", "#F1C37A", "#E6B670", "#DCA865", "#D19A5C",
        # until 2020-12-14           "#C79652", "#BD9248", "#B38E3E", "#A98A34")
        # until 2020-12-14 water <- rev(c("#E1FCF7", "#BFF2EC", "#A0E8E4", "#83DEDE", "#68CDD4",
        # until 2020-12-14                "#4FBBC9", "#38A7BF", "#2292B5", "#0F7CAB"))
        #land <- c("#FEF1E0", "#FDE3C1", "#FBC784", "#F1C37A", "#E6B670", "#DCA865",
        #          "#D19A5C", "#C79652", "#BD9248", "#B38E3E")
        #water <- c("#0F7CAB", "#2292B5", "#38A7BF", "#4FBBC9", "#68CDD4", "#83DEDE",
        #           "#A0E8E4", "#BFF2EC", "#E1FCF7", "#F0FDFB")
        land <- c("#FFF0DF", "#FFE9D0", "#FFE2C1", "#FDD6A6", "#FBC98A", "#F7C580", "#F2C37B", "#EDBE76", "#E8B872", "#E3B26D",
                  "#DEAB67", "#D9A563", "#D49E5E", "#CF995A", "#CA9755", "#C59550", "#C1934C", "#BC9147", "#B78F42", "#B38E3E")
        water <- c("#0F7CAB", "#1886AF", "#2090B4", "#2B9AB9", "#35A4BD", "#40AEC2", "#4BB7C7", "#56C0CC", "#62C9D1", "#6FD1D6",
                   "#7BD9DB", "#89E0DF", "#96E4E2", "#A4E9E5", "#B3EEE9", "#C2F3ED", "#D2F7F2", "#E2FCF7", "#EBFDF9", "#F5FEFC")
    } else {
        oceDebug(debug, "type='line'\n")
        land <- c("#FBC784", "#F1C37A", "#E6B670", "#DCA865", "#D19A5C",
                  "#C79652", "#BD9248", "#B38E3E", "#A98A34")
        water <- rev(c("#A4FCE3", "#72EFE9", "#4FE3ED", "#47DCF2", "#46D7F6",
                       "#3FC0DF", "#3FC0DF", "#3BB7D3", "#36A5C3"))
    }
    if (region == "water") {
        rgb.list <- col2rgb(water) / 255
        l <- length(water)
        r <- approx(1:l, rgb.list[1, 1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2, 1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3, 1:l], xout=seq(1, l, length.out=n))$y
        res <- rgb(r, g, b)
    } else if (region == "land") {
        rgb.list <- col2rgb(land) / 255
        l <- length(land)
        r <- approx(1:l, rgb.list[1, 1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2, 1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3, 1:l], xout=seq(1, l, length.out=n))$y
        res <- rgb(r, g, b)
    } else {
        # both
        # See https://github.com/dankelley/oce/discussions/1756#discussioncomment-204754 for
        # a discussion of adding some white 'ink' between the water and the land.
        #? rgb.list <- col2rgb(c(water, "#FFFFFF", "#FFFFFF", land)) / 255
        #? rgb.list <- col2rgb(c(water, "#FFFFFF", land)) / 255
        #20201214> rgb.list <- col2rgb(c(water, "#FFFFFF", land)) / 255
        #20201214> l <- ncol(rgb.list)
        #20201214> r <- approx(1:l, rgb.list[1, 1:l], xout=seq(1, l, length.out=n))$y
        #20201214> g <- approx(1:l, rgb.list[2, 1:l], xout=seq(1, l, length.out=n))$y
        #20201214> b <- approx(1:l, rgb.list[3, 1:l], xout=seq(1, l, length.out=n))$y
        # I find it very difficult to see a difference between 'rgb' and 'Lab' spaces, and between
        # 'linear' and 'spline' interpolations.
        cr <- colorRamp(c(water, "#FFFFFF", land), bias=1, space="rgb", interpolate="spline")(seq(0, 1, length.out=n))
        res <- rgb(cr, maxColorValue=255)
    }
    oceDebug(debug, "} # oceColorsGebco()", sep="", unindent=1)
    res
}
oce.colorsGebco <- oceColorsGebco

#' Create color functions
#'
#' This function generates other functions that are used to specify colors.
#' It is used within oce to create [oceColorsTemperature()]
#' and its many cousins. Users may also find it helpful, for creating
#' custom color schemes (see \dQuote{Examples}).
#'
#' @param spec Specification of the color scheme. This may be a
#' character string, in which case it must be the name of an item stored
#' in `data(ocecolors)`, or either a 3-column data frame or
#' matrix, in which case the columns specify red, green and blue values
#' (in range from 0 to 1).
#'
#' @examples
#'\dontrun{
#' # Update oxygen color scheme to latest matplotlib value.
#' library(oce)
#' oxy <- "https://raw.githubusercontent.com/matplotlib/cmocean/master/cmocean/rgb/oxy-rgb.txt"
#' oxyrgb <- read.table(oxy, header=FALSE)
#' oceColorsOxygenUpdated <- oceColorsClosure(oxyrgb)
#' par(mfrow=c(1, 2))
#' m <- matrix(1:256)
#' imagep(m, col=oceColorsOxygen, zlab="oxygen")
#' imagep(m, col=oceColorsOxygenUpdated, zlab="oxygenUpdated")
#'}
#' @family things related to colors
oceColorsClosure <- function(spec) {
    function(n) {
        if (is.character(spec)) {
            data("ocecolors", package="oce", envir=environment())
            col <- get("ocecolors")[[spec]]
        } else if (is.data.frame(spec) || is.matrix(spec)) {
            col <- rgb(spec[,1], spec[,2], spec[,3])
        } else {
            stop("oceColorsClosure(): first arg must be character, data frame, or 3-column matrix", call.=FALSE)
        }
        if (missing(n) || n <= 0) colorRampPalette(col) else colorRampPalette(col)(n)
    }
}

#' Create colors similar to the google turbo scheme
#'
#' This uses the coefficients published (with Apache license) by google,
#' as described by Mikhailo (2019).
#'
#' @aliases oce.colorsTurbo oceColorsTurbo
#'
#' @param n number of colors to create.
#'
#' @references
#' Mikhailo, Anton.
#' \dQuote{Turbo, An Improved Rainbow Colormap for Visualization.}
#' Google AI (blog), August 20, 2019.
#' `https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html`
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' imagep(volcano, col=oceColorsTurbo(128),
#'        zlab="oceColorsTurbo")
#'
#' @template colourBlindnessTemplate
#' @family things related to colors
oceColorsTurbo <- oce.colorsTurbo <- oceColorsClosure("turbo")


#' Create colors similar to the matlab Viridis scheme
#'
#' This is patterned on a \proglang{matlab}/\proglang{python} scheme that blends
#' from yellow to blue in a way that is designed to reproduce well
#' in black-and-white, and to be interpretable by those with
#' certain forms of color blindness.  See the references for
#' notes about issues of colour blindness in computer graphics.
#' An alternative
#' to [oceColorsViridis] is provided in the \CRANpkg{viridis} package, as illustrated
#' in Example 2.
#'
#' @aliases oce.colorsViridis oceColorsViridis
#'
#' @param n number of colors to create.
#'
#' @template colourBlindnessTemplate
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' # Example 1: oceColorsViridis
#' imagep(volcano, col=oceColorsViridis(128),
#'        zlab="oceColorsViridis")
#' # Example 2: viridis::viridis
#'\dontrun{
#' imagep(volcano, col=viridis::viridis,
#'        zlab="viridis::viridis")}
#'
#' @family things related to colors
#'
#' @template colourBlindnessTemplate
oceColorsViridis <- oce.colorsViridis <- oceColorsClosure("viridis")

#' @templateVar colorItem CDOM
#' @templateVar colorItemUC CDOM
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsCDOM <- oce.colorsCDOM <- oceColorsClosure("cdom")

#' @templateVar colorItem chlorophyll
#' @templateVar colorItemUC Chlorophyll
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsChlorophyll <- oce.colorsChlorophyll <- oceColorsClosure("chlorophyll")

#' @templateVar colorItem density
#' @templateVar colorItemUC Density
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsDensity <- oce.colorsDensity <- oceColorsClosure("density")

#' @templateVar colorItem freesurface
#' @templateVar colorItemUC Freesurface
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsFreesurface <- oce.colorsFreesurface <- oceColorsClosure("freesurface")

#' @templateVar colorItem oxygen
#' @templateVar colorItemUC Oxygen
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsOxygen <- oce.colorsOxygen <- oceColorsClosure("oxygen")

#' @templateVar colorItem PAR
#' @templateVar colorItemUC PAR
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsPAR <- oce.colorsPAR <- oceColorsClosure("par")

#' @templateVar colorItem phase
#' @templateVar colorItemUC Phase
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsPhase <- oce.colorsPhase <- oceColorsClosure("phase")

#' @templateVar colorItem salinity
#' @templateVar colorItemUC Salinity
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsSalinity <- oce.colorsSalinity <- oceColorsClosure("salinity")

#' @templateVar colorItem temperature
#' @templateVar colorItemUC Temperature
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsTemperature <- oce.colorsTemperature <- oceColorsClosure("temperature")

#' @templateVar colorItem turbidity
#' @templateVar colorItemUC Turbidity
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsTurbidity <- oce.colorsTurbidity <- oceColorsClosure("turbidity")

#' @templateVar colorItem velocity
#' @templateVar colorItemUC Velocity
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsVelocity <- oce.colorsVelocity <- oceColorsClosure("velocity")

#' @templateVar colorItem vorticity
#' @templateVar colorItemUC Vorticity
#' @template cmcolorTemplate
#'
#' @template colourBlindnessTemplate
oceColorsVorticity <- oce.colorsVorticity <- oceColorsClosure("vorticity")


#' Create colors similar to the Matlab Jet scheme
#' @aliases oceColorsJet oce.colorsJet oceColors9A oce.colors9A
#' @param n number of colors
#' @examples
#' library(oce)
#' imagep(volcano, col=oceColorsJet, zlab="oceColorsJet")
#'
#' @template colourBlindnessTemplate
#' @family things related to colors
oceColorsJet <- function(n)
{
    if (missing(n) || n <= 0)
        colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    else {
        colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(n)
    }
}
oce.colors9A <- oceColors9A <- oce.colorsJet <- oceColorsJet

#' Create colors in a red-yellow-blue color scheme
#'
#' The results are similar to those of [oceColorsJet()], but
#' with white hues in the centre, rather than green ones. The scheme
#' may be useful in displaying signed quantities, and thus is somewhat
#' analogous to [oceColorsTwo()], except that they (average)
#' eye may be more able to distinguish colors with `oceColors9B`.
#'
#' @aliases oceColors9B oce.colors9B
#'
#' @param n number of colors
#'
#' @examples
#' library(oce)
#' imagep(volcano, col=oceColors9B(128),
#'        zlab="oceColors9B")
#'
#' @template colourBlindnessTemplate
#' @family things related to colors
oceColors9B <- function(n)
{
    if (missing(n) || n <= 0)
        colorRampPalette(c("#00007F", "blue", "#007FFF", "#22e4e7",
                           "white", "#ffe45e", "#FF7F00", "red", "#7F0000"))
    else {
        colorRampPalette(c("#00007F", "blue", "#007FFF", "#22e4e7",
                           "white", "#ffe45e", "#FF7F00", "red", "#7F0000"))(n)
    }
}
oce.colors9B <- oceColors9B

#' Create a vector of colors
#'
#' The available schemes are:
#' * `which=1` for a red-white-blue scheme.
#' * `which=2` for a red-yellow-blue scheme.
#' * `which=9.01`, `which="9A"` or `which="jet"` for [`oceColorsJet`]`(n)`.
#' * `which=9.02` or `which="9B"` for [`oceColors9B`]`(n)`.
#'
#' @param n number of colors to create
#'
#' @param which integer or character string indicating the palette
#' to use; see \dQuote{Details}.
#'
#' @aliases oce.colorsPalette oceColorsPalette
#'
#' @template colourBlindnessTemplate
#'
#' @family things related to colors
oceColorsPalette <- function(n, which=1)
{
    if ( (n <- as.integer(n[1])) > 0 ) {
        if (which == 1) {
            # Started with http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer.html
            # RdBu 11 divisions
            # and then smoothed the components with smooth.spline(...,df=6)
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
            # http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer.html
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
            # jet, also known as 9A or 9.01
            oceColorsJet(n)
        } else if (which == 9.02 || which == "9B") {
            oceColors9B(n)
        } else stop("unknown which")
    }
    else character(0)
}
oce.colorsPalette <- oceColorsPalette

#' Oce Version of axis.POSIXct
#'
#' A specialized variant of [axis.POSIXct()] that produces
#' results with less ambiguity in axis labels.
#'
#' The tick marks are set automatically based on examination of the time range on
#' the axis. The scheme was devised by constructing test cases with a typical plot
#' size and font size, and over a wide range of time scales. In some categories,
#' both small tick marks are interspersed between large ones.
#'
#' The user may set the format of axis numbers with the `tformat` argument.
#' If this is not supplied, the format is set based on the time span of the axis:
#'
#' * If this time span is less than a minute, the time axis labels are in
#' seconds (fractional seconds, if the interval is less than 2 seconds), with
#' leading zeros on small integers. (Fractional seconds are enabled with a trick:
#' the usual R format `"\%S"` is supplemented with a new format e.g.
#' `"\%.2S"`, meaning to use two digits after the decimal.)
#'
#' * If the time span exceeds a minute but is less than 1.5 days, the label
#' format is `"\%H:\%M:\%S"`.
#'
#' * If the time span exceeds 1.5 days but is less than 1 year, the format is
#' `"\%b \%d"` (e.g. Jul 15) and, again, the tick marks are set up for several
#' subcategories.
#'
#' * If the time span exceeds a year, the format is `"\%Y"`, i.e. the year
#' is displayed with 4 digits.
#'
#' It should be noted that this scheme differs from the R approach in several
#' ways. First, R writes day names for some time ranges, in a convention that is
#' seldom seen in the literature. Second, R will write nn:mm for both HH:MM and
#' MM:SS, an ambiguity that might confuse readers. Third, the use of both large
#' and small tick marks is not something that R does.
#'
#' Bear in mind that `tformat` may be set to alter the number format, but
#' that the tick mark scheme cannot (presently) be controlled.
#'
#' @param side as for [axis.POSIXct()].
#'
#' @param x as for [axis.POSIXct()].
#'
#' @param at as for [axis.POSIXct()].
#'
#' @param tformat as `format` for [axis.POSIXct()] for now, but
#' may eventually have new features for multiline labels, e.g. day on one line
#' and month on another.
#'
#' @param labels as for [axis.POSIXct()].
#'
#' @param drawTimeRange Optional indication of whether/how to draw the time range
#' in the margin on the side of the the plot opposite the time axis. If this is
#' not supplied, it defaults to the value returned by
#' [`getOption`]`("oceDrawTimeRange")`, and if that option is not set,
#' it defaults to `TRUE`. No time range is drawn if `drawTimeRange` is `FALSE`.
#' If it is `TRUE`, the range will be shown. This range refers to
#' range of the x axis (not the data). The format of the elements of that range is set by
#' [`getOption`]`("oceTimeFormat")` (or with the default value
#' of an empty string, if this option has not been set). The timezone will
#' be indicated if the time range is under a week.  For preliminary work, it makes
#' sense to use `drawTimeRange=TRUE`, but for published work it can be better
#' to drop this label and indicate something about the time in the figure caption.
#'
#' @param drawFrequency boolean, `TRUE` to show the frequency of sampling
#' in the data
#'
#' @param abbreviateTimeRange boolean, `TRUE` to abbreviate the second
#' number in the time range, e.g. dropping the year if it is the same in the
#' first number.
#'
#' @param cex.axis,cex.lab,cex.main character expansion factors for axis numbers, axis names and plot titles; see [par()].
#'
#' @param mar value for `par(mar)` for axis
#'
#' @param mgp value for `par(mgp)` for axis
#'
#' @param main title of plot
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots as for [axis.POSIXct()].
#'
#' @return A vector of times corresponding to axis ticks is returned silently.
#'
#' @author Dan Kelley
#'
#' @seealso This is used mainly by [oce.plot.ts()].
oce.axis.POSIXct <- function (side, x, at, tformat, labels = TRUE,
                              drawTimeRange,
                              abbreviateTimeRange=FALSE, drawFrequency=FALSE,
                              cex.axis=par("cex.axis"), cex.lab=par("cex.lab"), cex.main=par("cex.main"),
                              mar=par("mar"),
                              mgp=par("mgp"),
                              main="",
                              debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "oce.axis.POSIXct(..., debug=", debug, ",...) {\n", sep="", unindent=1, style="bold")
    oceDebug(debug, argShow(mar), "\n")
    oceDebug(debug, argShow(mgp), "\n")
    oceDebug(debug, "cex.axis=", cex.axis, ", cex.lab=", cex.lab, ", cex.main=", cex.main, "\n")
    oceDebug(debug, vectorShow(x, "x"))
    tformatGiven <- !missing(tformat)
    if (missing(drawTimeRange))
        drawTimeRange <- getOption("oceDrawTimeRange")
    # This was written because axis.POSIXt in R version 2.8.x did not obey the
    # time zone in the data.  (Version 2.9.0 obeys the time zone.)
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
        # The time rounding will fail for very small time intervals;
        # a wider range can be added easily.
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
        # BOOKMARK 1A
        if (missing(tformat)) {
            tformat <- "%.1S" # NOTE: this .1 is interpreted at BOOKMARK 1B
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
    } else if (d <= 20) {
        oceDebug(debug, "Time range is between 2 sec and 20 sec\n")
        t.start <- trunc(rr[1]-1, "secs")
        t.end <- trunc(rr[2]+1, "secs")
        z <- seq(t.start, t.end, by="1 sec")
        oceDebug(debug, vectorShow(z))
        if (missing(tformat)) {
            tformat <- "%S"
            oceDebug(debug, "automatic tformat='", tformat, "'\n")
        }
     } else if (d <= 60) {
        oceDebug(debug, "Time range is between 20 sec and 1 min\n")
        t.start <- trunc(rr[1]-1, "secs")
        t.end <- trunc(rr[2]+1, "secs")
        z <- seq(t.start, t.end, by="2 sec")
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
    #
    # FIXME: I was twiddling the numbers, to get more labels, but xaxs="r" fixes that.
    twiddle <- 0.04 * diff(as.numeric(range))  # FIXME: do I need this anymore?
    oceDebug(debug, "range=",
              format.POSIXct(rr[1], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC to ",
              format.POSIXct(rr[2], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC\n")
    keep <- range[1] <= (z + twiddle) & (z - twiddle) <= range[2]
    #oceDebug(debug, vectorShow(keep, "keep"))
    oceDebug(debug>1, vectorShow(z, "z before keep"))
    z <- z[keep]
    oceDebug(debug>1, vectorShow(z, "z after keep"))
    if (!is.logical(labels)) {
        labels <- labels[keep]
    } else if (labels[1]) {
        if (length(grep("[0-9]+S.*", tformat))) {
            # BOOKMARK 1B a special trick to get fractional seconds (cf BOOKMARK 1A)
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
        # time.range.data <- range(x, na.rm=TRUE)
        # what was this for?# time.range[1] <- max(time.range[1], time.range.data[1], na.rm=TRUE)
        # what was this for?# time.range[2] <- min(time.range[2], time.range.data[2], na.rm=TRUE)
        if (!is.null(getOption("oceTimeFormat"))) {
            tr1 <- format(time.range[1], getOption("oceTimeFormat"))
            tr2 <- format(time.range[2], getOption("oceTimeFormat"))
        } else {
            tr1 <- format(time.range[1])
            tr2 <- format(time.range[2])
        }
        if (abbreviateTimeRange) {
            oceDebug(debug, "abbreviating time format\n")
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
        # only show timezone if hours are shown
        oceDebug(debug, "time.range[1]:", format(time.range[1]), "\n")
        oceDebug(debug, "round(time.range[1], 'days'):", format(round(time.range[1], 'days')), "\n")
        oceDebug(debug, "time.range[2]:", format(time.range[2]), "\n")
        oceDebug(debug, "round(time.range[2], 'days'):", format(round(time.range[2], 'days')), "\n")
        tzone <- c(attr(time.range[1], "tzone"), attr(time.range[2], "tzone"))
        # Only show tzone if it is UTC (https://github.com/dankelley/oce/issues/1811)
        tzone <- if (all(tzone == "UTC")) c("", "UTC") else rep("", 2)
        # The below is not fool-proof, depending on how xlim might have been supplied; see
        #    https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14449
        if (diff(as.numeric(time.range)) > 7*86400) {
            label <- paste(tr1, tr2, sep=" to ")
        } else {
            label <- paste(tr1, tzone[1], " to ", tr2,  tzone[2], sep="")
        }
        if (drawFrequency && is.finite(1/deltat))
            label <- paste(label, "@", sprintf("%.4g Hz", 1/deltat), sep=" ")
        oceDebug(debug, "label=\"", label, "\" at cex.lab=", cex.lab, "\n", sep="")
        mtext(label, side=if (side==1) 3 else 1, cex=cex.lab*par('cex'), adj=0)
        oceDebug(debug, "cex.axis=", cex.axis, "; par('cex')=", par('cex'), "\n")
    }
    if (nchar(main) > 0) {
        mtext(main, side=if (side==1) 3 else 1, cex=cex.lab*par('cex'), adj=1)
    }
    oceDebug(debug, vectorShow(z, "z="))
    if (length(z.sub) > 0) {
        axis(side, at=z.sub, line=0, labels=FALSE, tcl=-0.25)
        oceDebug(debug, vectorShow(z.sub, "z.sub="))
    }
    oceDebug(debug, vectorShow(labels, "labels="))
    #ocex <- par('cex')
    ocex.axis <- par('cex.axis')
    ocex.lab <- par('cex.lab')
    ocex.main <- par('cex.main')
    omgp <- par('mgp')
    par(cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, mgp=mgp, tcl=-0.5)
    #axis(side, at=z, line=0, labels=labels, cex=cex, cex.axis=cex.axis, cex.main=cex.main, mar=mar, mgp=mgp)

    # If the user did gave tformat, shorten the strings for aesthetic reasons.
    if (!tformatGiven) {
        oceDebug(debug, "axis labels before shortenTimeString(): '", paste(labels, "', '"), "'\n")
        labels <- shortenTimeString(labels, debug=debug-1)
        oceDebug(debug, "axis labels after shortenTimeString(): '", paste(labels, "', '"), "'\n")
    }
    axis(side, at=z, line=0, labels=labels, mgp=mgp, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, ...)
    par(cex.axis=ocex.axis, cex.lab=ocex.lab, cex.main=ocex.main, mgp=omgp)
    oceDebug(debug, "} # oce.axis.POSIXct()\n", unindent=1, style="bold")
    zzz <- as.numeric(z)
    if (1 < length(zzz)) {
        xaxp <- c(min(zzz, na.rm=TRUE), max(zzz, na.rm=TRUE), -1+length(zzz))
        par(xaxp=xaxp)
    }
    invisible(z)                       # FIXME: or z.sub?
}                                      # oce.axis.POSIXct()

#' Convert a Numeric Time to Hour, Minute, and Second
#'
#' @param t a vector of factors or character strings, in the format 1200 for
#' 12:00, 0900 for 09:00, etc.
#'
#' @param default value to be used for the returned hour, minute and second if
#' there is something wrong with the input value (e.g. its length exceeds 4
#' characters, or it contains non-numeric characters)
#'
#' @return A list containing `hour`, `minute`, and `second`, the
#' last of which is always zero.
#'
#' @author Dan Kelley
#'
#' @examples
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
#' There are many varieties, according to the value of `type` as defined
#' in \sQuote{Details}.
#'
#' * `"unix"` handles Unix times, measured in seconds since the start
#' of the year 1970.
#'
#' * `"matlab"` handles Matlab times, measured in days since what
#' MathWorks (reference 1) calls ``January 0, 0000'' (i.e.  \code{ISOdatetime(0, 1, 1, 0,
#' 0, 0)} in R notation).
#'
#' * `"gps"` handles the GPS convention. For this, `t` is a
#' two-column matrix, with the first column being the the GPS "week"
#' (referenced to 1999-08-22) and the second being the GPS "second" (i.e. the
#' second within the week). Since the GPS satellites do not handle leap
#' seconds, the R-defined `.leap.seconds` is used for corrections.
#'
#' * `"argo"` handles Argo times, measured in days since the start of
#' the year 1900.
#'
#' * `"excel"` handles Excel times, measured in days since the start of
#' the year 1900. (Note that excel incorrectly regards 1900 as a leap year,
#' so 1 day is subtracted from `t` unless the time is less than or equal
#' to 1900 Feb 28.  Note that NA is returned for the day 60, which is
#' what excel codes for "Feb 29, 1900", the non-existing day that excel
#' accepts.
#'
#' * `"ncep1"` handles NCEP times, measured in hours since the start
#' of the year 1800.
#'
#' * `"ncep2"` handles NCEP times, measured in days since the start of
#' the year 1. (Note that, for reasons that are unknown at this time, a simple
#' R expression of this definition is out by two days compared with the UDUNITS
#' library, which is used by NCEP. Therefore, a two-day offset is applied. See
#' references 2 and 3.)
#'
#' * `"sas"` handles SAS times, indicated by `type="sas"`, have
#' origin at the start of 1960.
#'
#' * `"spss"` handles SPSS times, in seconds after 1582-10-14.
#'
#' * `"yearday"` handles a convention in which `t` is a
#' two-column matrix, with the first column being the year, and the second the
#' yearday (starting at 1 for the first second of January 1, to match the
#' convention used by Sea-Bird CTD software).
#'
#' * `"epic"` handles a convention used in the EPIC software library,
#' from the Pacific Marine Environmental Laboratory, in which `t` is a
#' two-column matrix, with the first column being the julian Day (as defined in
#' [julianDay()], for example), and with the second column being the
#' millisecond within that day. See reference 4.
#'
#' `"vms"` handles a convention used in the VMS operating system and
#' for Modified Julian Day, in which `t` is the number of seconds
#' past 1859-11-17T00:00:00 UTC. See reference 5.
#'
#' @param t an integer corresponding to a time, in a way that depends on
#' `type`.
#'
#' @param type the type of time (see \dQuote{Details}).
#'
#' @param tz a string indicating the time zone, used only for unix and matlab
#' times, since GPS times are always referenced to the UTC timezone.
#'
#' @return A [POSIXct()] time vector.
#'
#' @author Dan Kelley
#'
#' @seealso [numberAsHMS()]
#'
#' @references
#' 1. Matlab times:
#' `https://www.mathworks.com/help/matlab/ref/datenum.html`
#'
#' 2. NCEP times: `https://psl.noaa.gov/data/gridded/faq.html`
#'
#' 3. problem with NCEP times:
#' `https://github.com/dankelley/oce/issues/738`
#'
#' 4. EPIC times: software and manuals at `https://www.pmel.noaa.gov/epic/download/index.html#epslib`;
#' see also Denbo, Donald W., and Nancy N. Soreide. \dQuote{EPIC.} Oceanography 9 (1996).
#' \doi{10.5670/oceanog.1996.10}
#'
#' 5. VMS times: https://en.wikipedia.org/wiki/Epoch_(computing)
#'
#' @examples
#' numberAsPOSIXct(0)                     # unix time 0
#' numberAsPOSIXct(1, type="matlab")      # matlab time 1
#' numberAsPOSIXct(cbind(566, 345615), type="gps") # Canada Day, zero hour UTC
#' numberAsPOSIXct(cbind(2013, 1), type="yearday") # start of 2013
#'
#' # Epic time, one hour into Canada Day of year 2018. In computing the
#' # Julian day, note that this starts at noon.
#' jd <- julianDay(as.POSIXct("2018-07-01 12:00:00", tz="UTC"))
#' numberAsPOSIXct(cbind(jd, 1e3 * 1 * 3600), type="epic", tz="UTC")
#'
#' @family things related to time
numberAsPOSIXct <- function(t, type, tz="UTC")
{
    if (missing(type)) {
        type <- "unix"
    } else {
        typeAllowed <- c("unix", "matlab", "gps", "argo", "excel", "ncep1", "ncep2", "sas", "spss", "yearday", "epic", "vms")
        type <- pmatch(type, typeAllowed, nomatch=NA)
        if (is.na(type))
            stop("only permitted type values are: \"", paste(typeAllowed, collapse="\", \""), "\".", sep="")
        type <- typeAllowed[type]
    }
    if (type == "unix") {
        # We add something with a timezone, and then subtract it, as a trick to inherit the timezone
        tref <- if (!is.null(tz)) as.POSIXct("2000-01-01", tz=tz) else as.POSIXct("2000-01-01")
        return(tref + as.numeric(t) - as.numeric(tref))
    } else if (type == "matlab") {
        # R won't take a day "0", so subtract one
        return(as.POSIXct(ISOdatetime(0000, 01, 01, 0, 0, 0, tz=tz) + 86400 * (t - 1)))
    } else if (type == "yearday") {
        if (2 != ncol(t))
            stop("'t' must have two columns, one for year, the other for yearday")
        return(ISOdatetime(t[, 1], 1, 1, 0, 0, 0, tz=tz) + (t[, 2] - 1) * 24 * 3600)
    } else if (type == "argo") {
        return(t * 86400 + as.POSIXct("1900-01-01 00:00:00", tz="UTC"))
    } else if (type == "excel") {
        # We need a one-day offset if time is after Feb 28, 1900,
        # because Excel thinks 1900 was a leap year. We can check for
        # this by day count. See https://github.com/dankelley/oce/issues/1591
        # for a discussion. Note that we return NA for day 60, because
        # that is what excel produces for "Feb 29, 1900", which is in
        # fact a non-existent day so I think we ought to inform oce users
        # of that fact, with a NA.
        offset <- ifelse(t < 61, 0, 1) # excel thinks 1900 is a leap year
        rval <- 86400 * (t - 1 - offset) + as.POSIXct("1900-01-01 00:00:00", tz="UTC")
        rval[t == 60] <- NA
        return(rval)
    } else if (type == "ncep1") {
        # hours since the start of 1800
        return(t * 3600 + as.POSIXct("1800-01-01 00:00:00", tz="UTC"))
    } else if (type == "ncep2") {
        # days since 1-1-1 00:00:0.0 (supposedly, but offset to match a test case; see
        resOriginal <- t * 86400 + as.POSIXct("0001-01-01 00:00:00", tz="UTC")
        return(resOriginal - 2 * 86400) # kludge for ht of https://github.com/dankelley/oce/issues/738
    } else if (type == "gps") {
        if (!is.matrix(t) || dim(t)[2] != 2)
            stop("for GPS times, 't' must be a two-column matrix, with first col the week, second the second")

        # Account for leap seconds since the GPS start time in 1980 (for the present week wraparound grouping).
        #20171014 See http://en.wikipedia.org/wiki/Leap_second and other sources for a list.  Updates can happen
        #20171014 # on June 30 and December 31 of any given year.  The information below was last updated
        #20171014 # in January, 2017.
        #20171014 # leapsOLD <- as.POSIXct(strptime(c("1981-07-01", "1982-07-01", "1983-07-01", "1985-07-01", "1987-12-31",
        #20171014 #                                   "1989-12-31", "1990-12-31", "1992-07-01", "1993-07-01", "1994-07-01",
        #20171014 #                                   "1995-12-31", "1997-07-01", "1998-12-31", "2005-12-31", "2008-12-31",
        #20171014 #                                   "2012-07-01", "2015-07-01", "2016-12-31"),
        #20171014 #                                 format="%Y-%m-%d", tz="UTC"))
        #20171014 message("leapsOLD ", paste(leapsOLD, collapse=" "))
        leaps <- as.POSIXlt(.leap.seconds, tz="UTC")
        #20171014 message("leaps A ", paste(leaps, collapse=" "))
        leaps <- leaps[leaps > as.POSIXlt("1980-01-01 00:00:00", tz="UTC")]
        #20171014 message("leaps B ", paste(leaps, collapse=" "))
        leaps <- leaps[leaps > as.POSIXlt("1980-01-01 00:00:00", tz="UTC")]
        #20171014 message("leaps C ", paste(leaps, collapse=" "))
        t <- as.POSIXct("1999-08-22 00:00:00", tz="UTC") + 86400*7*t[, 1] + t[, 2]
        #>message("initially, t=", paste(t, collapse=" "))
        for (l in seq_along(leaps)) {
            t <- t - ifelse(t >= leaps[l], 1, 0)
            #20171014 message("l=", l, ", leaps[l]=", leaps[l],
            #20171014         ", t=", paste(t, collapse=" "), ", t>=leaps[l] ", t>=leaps[l])
        }
        #20171014 print(leapsOLD - leaps) # mostly 0 but a few one-day shifts; I trust .leap.seconds more
    } else if (type == "spss") {
        t <- as.POSIXct(t, origin="1582-10-14", tz=tz)
    } else if (type == "sas") {
        t <- as.POSIXct(t, origin="1960-01-01", tz=tz)
    } else if (type == "epic") {
        if (!is.matrix(t) || dim(t)[2] != 2)
            stop("for epic times, 't' must be a two-column matrix, with first column the julian day, and second the millisecond within that day")
        r <- do_epic_time_to_ymdhms(t[,1], t[,2])
        t <- ISOdatetime(r$year, r$month, r$day, r$hour, r$minute, r$second, tz=tz)
    } else if (type == "vms") {
        t <- as.POSIXct(t, origin="1858-11-17", tz=tz)
    } else {
        stop("unknown type '", type, "'")
    }
    t
}


#' Plot an Inset Diagram
#'
#' Adds an inset diagram to an existing plot.  Note that if the inset is a map
#' or coastline, it will be necessary to supply `inset=TRUE` to prevent
#' the inset diagram from occupying the whole device width.  After
#' `plotInset()` has been called, any further plotting will take place
#' within the inset, so it is essential to finish a plot before drawing an
#' inset.
#'
#' @param xleft location of left-hand of the inset diagram, in the existing
#' plot units.  (PROVISIONAL FEATURE: this may also be `"bottomleft"`, to
#' put the inset there.  Eventually, other positions may be added.)
#'
#' @param ybottom location of bottom side of the inset diagram, in the existing
#' plot units.
#'
#' @param xright location of right-hand side of the inset diagram, in the
#' existing plot units.
#'
#' @param ytop location of top side of the inset diagram, in the existing plot
#' units.
#'
#' @param expr An expression that draws the inset plot.  This may be a single
#' plot command, or a sequence of commands enclosed in curly braces.
#'
#' @param mar margins, in line heights, to be used at the four sides of the
#' inset diagram.  (This is often helpful to save space.)
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' # power law in linear and log form
#' x <- 1:10
#' y <- x^2
#' plot(x, y, log='xy',type='l')
#' plotInset(3, 1, 10, 8,
#'           expr=plot(x,y,type='l',cex.axis=3/4,mgp=c(3/2, 1/2, 0)),
#'           mar=c(2.5, 2.5, 1, 1))
#'
#' # CTD data with location
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
    #omfg <- par('mfg')                 # original mfg
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
    #mfg2 <- par('mfg')
    par(new=TRUE, mai=nmai)
    thismar <- par('mar')
    par(mar=thismar+mar)
    if (debug > 1) {
        cat("\n\nBEFORE expr, PAR IS:\n")
        str(par())
    }
    mfg <- par('mfg')
    oceDebug(debug, "BEFORE expr, mfg=", mfg, "\n")
    # Draw the inset plot (or perform any action, actually)
    expr
    if (mfg[1] == mfg[3] && mfg[2] == mfg[4]) {
        oceDebug(debug, "setting new=FALSE; mfg=", mfg, "\n")
        par(new=FALSE)
    } else {
        oceDebug(debug, "setting new=TRUE; mfg=", mfg, "\n")
        par(new=TRUE)
    }
    # Reset some things that could have been set in the inset, and
    # then adjust 'new' appropriately.
    par(usr=opar$usr, mai=opar$mai, cex=opar$cex, lwd=opar$lwd, lty=opar$lty, bg=opar$bg)
    oceDebug(debug, "} # plotInset()\n", unindent=1)
    invisible(NULL)
}


#' Oce Version of as.POSIXct
#'
#' Each format in `timeFormats` is used in turn as the `format`
#' argument to [as.POSIXct()], and the first that produces a
#' non-`NA` result is used.  If `timeFormats` is missing, the
#' following formats are tried, in the stated order:
#'
#' * `"\%b \%d \%Y \%H:\%M:\%S"` (e.g. `"Jul 1 2013 01:02:03"`)
#'
#' * `"\%b \%d \%Y"` (e.g. `"Jul 1 2013"`)
#'
#' * `"\%B \%d \%Y \%H:\%M:\%S"` (e.g. `"July 1 2013 01:02:03"`)
#'
#' * `"\%B \%d \%Y"` (e.g. `"July 1 2013"`)
#'
#' * `"\%d \%b \%Y \%H:\%M:\%S"` (e.g. `"1 Jul 2013 01:02:03"`)
#'
#' * `"\%d \%b \%Y"` (e.g. `"1 Jul 2013"`)
#'
#' * `"\%d \%B \%Y \%H:\%M:\%S"` (e.g. `"1 July 2013 01:02:03"`)
#'
#' * `"\%d \%B \%Y"` (e.g. `"1 July 2013"`)
#'
#' * `"\%Y-\%m-\%d \%H:\%M:\%S"` (e.g.  `"2013-07-01 01:02:03"`)
#'
#' * `"\%Y-\%m-\%d"` (e.g. `"2013-07-01"`)
#'
#' * `"\%Y-\%b-\%d \%H:\%M:\%S"` (e.g.  `"2013-July-01 01:02:03"`)
#'
#' * `"\%Y-\%b-\%d"` (e.g.  `"2013-Jul-01"`)
#'
#' * `"\%Y-\%B-\%d \%H:\%M:\%S"` (e.g. `"2013-July-01 01:02:03"`)
#'
#' * `"\%Y-\%B-\%d"` (e.g. `"2013-July-01"`)
#'
#' * `"\%d-\%b-\%Y \%H:\%M:\%S"` (e.g.  `"01-Jul-2013 01:02:03"`)
#'
#' * `"\%d-\%b-\%Y"` (e.g. `"01-Jul-2013"`)
#'
#' * `"\%d-\%B-\%Y \%H:\%M:\%S"` (e.g. `"01-July-2013 01:02:03"`)
#'
#' * `"\%d-\%B-\%Y"` (e.g. `"01-July-2013"`)
#'
#' * `"\%Y/\%b/\%d \%H:\%M:\%S"` (e.g. `"2013/Jul/01 01:02:03"`)
#'
#' * `"\%Y/\%b/\%d"` (e.g. `"2013/Jul/01"`)
#'
#' * `"\%Y/\%B/\%d \%H:\%M:\%S"` (e.g. `"2013/July/01 01:02:03"`)
#'
#' * `"\%Y/\%B/\%d"` (e.g. `"2013/July/01"`)
#'
#' * `"\%Y/\%m/\%d \%H:\%M:\%S"` (e.g. `"2013/07/01 01:02:03"`)
#'
#' * `"\%Y/\%m/\%d"` (e.g. `"2013/07/01"`)
#'
#' @param time Character string with an indication of the time.
#'
#' @param timeFormats Optional vector of time formats to use, as for [as.POSIXct()].
#'
#' @param tz Time zone.
#'
#' @return A time as returned by [as.POSIXct()].
#'
#' @author Dan Kelley
#'
#' @examples
#' decodeTime("July 1 2013 01:02:03")
#' decodeTime("Jul 1 2013 01:02:03")
#' decodeTime("1 July 2013 01:02:03")
#' decodeTime("1 Jul 2013 01:02:03")
#' decodeTime("2013-07-01 01:02:03")
#' decodeTime("2013/07/01 01:02:03")
#' decodeTime("2013/07/01")
#'
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
    # FIXME: permit time to be a vector
    res <- NA
    for (format in timeFormats) {
        #cat("TRYING FORMAT:", format, "\n")
        res <- as.POSIXct(time, format=format, tz=tz)
        if (!is.na(res)) {
            #cat("YES!\n")
            break
        }
    }
    res
}


#' Draw a Direction Field
#'
#' The direction field is indicated variously, depending on the value of
#' `type`:
#'
#' * For `type=1`, each indicator is drawn with a symbol, according to the
#' value of `pch` (either supplied globally, or as an element of the
#' `...` list) and of size `cex`, and color `col`.   Then, a
#' line segment is drawn for each, and for this `lwd` and `col` may
#' be set globally or in the `...` list.
#'
#' * For `type=2`, the points are not drawn, but arrows are drawn instead
#' of the line segments.  Again, `lwd` and `col` control the type of
#' the line.
#'
#' @param x,y coordinates at which velocities are specified. The
#'     length of `x` and `y` depends on the form of `u`
#'     and `v` (vectors or matrices).
#'
#' @param u,v velocity components in the x and y directions. Can be
#'     either vectors with the same length as `x, y`, or
#'     matrices, of dimension `length(x)` by `length(y)`.
#'
#' @param scalex,scaley scale to be used for the velocity arrows.
#'     Exactly one of these must be specified.  Arrows that have
#'     `u^2+v^2=1` will have length `scalex` along the x
#'     axis, or `scaley` along the y axis, according to which
#'     argument is given.
#'
#' @param skip either an integer, or a two-element vector indicating
#'     the number of points to skip when plotting arrows (for the
#'     matrix `u, v` case). If a single value, the same
#'     `skip` is applied to both the `x` and `y`
#'     directions. If a two-element vector, specifies different values
#'     for the `x` and `y` directions.
#'
#' @param length indication of *width* of arrowheads. The
#'     somewhat confusing name of this argument is a consequence of
#'     the fact that it is passed to [arrows()] for drawing
#'     arrows.  Note that the present default is smaller than the
#'     default used by [arrows()].
#'
#' @param add if `TRUE`, the arrows are added to an existing
#'     plot; otherwise, a new plot is started by calling
#'     [plot()] with `x`, `y` and `type="n"`.
#'     In other words, the plot will be very basic. In most cases, the
#'     user will probably want to draw a diagram first, and `add`
#'     the direction field later.
#'
#' @param type indication of the style of arrow-like indication of the
#'     direction.
#'
#' @param col color of line segments or arrows; see [par()] for meaning
#'
#' @param pch,cex plot character and expansion factor, used for
#' `type=1`; see [par()] for meanings
#'
#' @param lwd,lty line width and type, used for `type=2`; see [par()] for meaning
#'
#' @param xlab,ylab `x` and `y` axis labels
#'
#' @param debug debugging value; set to a positive integer to get
#'     debugging information.
#'
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
#' # 2D example
#' x <- seq(-2, 2, 0.1)
#' y <- x
#' xx <- expand.grid(x, y)[,1]
#' yy <- expand.grid(x, y)[,2]
#' z <- matrix(xx*exp(-xx^2 -yy^2), nrow=length(x))
#' gz <- grad(z, x, y)
#' drawDirectionField(x, y, gz$gx, gz$gy, scalex=0.5, type=2, len=0.02)
#' oceContour(x, y, z, add=TRUE)
#'
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
    #mai <- par('mai')
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
#' This provides something analogous to [contour()], but with the
#' ability to flip x and y.
#' Setting `revy=TRUE` can be helpful if the `y` data represent
#' pressure or depth below the surface.
#'
#' @aliases oce.contour oceContour
#'
#' @param x values for x grid.
#'
#' @param y values for y grid.
#'
#' @param z matrix for values to be contoured.  The first dimension of `z`
#' must equal the number of items in `x`, etc.
#'
#' @param revx set to `TRUE` to reverse the order in which the labels on
#' the x axis are drawn
#'
#' @param revy set to `TRUE` to reverse the order in which the labels on
#' the y axis are drawn
#'
#' @param add logical value indicating whether the contours should be added to
#' a pre-existing plot.
#'
#' @param tformat time format; if not supplied, a reasonable choice will be
#' made by [oce.axis.POSIXct()], which draws time axes.
#'
#' @param drawTimeRange logical, only used if the `x` axis is a time.  If
#' `TRUE`, then an indication of the time range of the data (not the axis)
#' is indicated at the top-left margin of the graph.  This is useful because
#' the labels on time axes only indicate hours if the range is less than a day,
#' etc.
#'
#' @param debug a flag that turns on debugging; set to 1 to information about
#' the processing.
#'
#' @param \dots optional arguments passed to plotting functions.
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' data(topoWorld)
#' # coastline now, and in last glacial maximum
#' lon <- topoWorld[["longitude"]]
#' lat <- topoWorld[["latitude"]]
#' z <- topoWorld[["z"]]
#' oce.contour(lon, lat, z, levels=0, drawlabels=FALSE)
#' oce.contour(lon, lat, z, levels=-130, drawlabels=FALSE, col='blue', add=TRUE)
oce.contour <- function(x, y, z, revx=FALSE, revy=FALSE, add=FALSE,
                       tformat, drawTimeRange=getOption("oceDrawTimeRange"),
                       debug=getOption("oceDebug"), ...)
{
    #dots <- list(...)
    #dotsNames <- names(dots)
    mustReverseX <- any(0 > diff(order(x)))
    mustReverseY <- any(0 > diff(order(y)))
    oceDebug(debug, "mustReverseX:", mustReverseX, '\n')
    oceDebug(debug, "mustReverseY:", mustReverseY, '\n')

    # perhaps get (x,y,z) from x, etc., trying to emulate contour()
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


    # store local values for the tricky cases of reversing axes etc
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
        # see src/library/graphics/R/contour.R
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
