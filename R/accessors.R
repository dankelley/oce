## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get data from an \code{oce} object
#'
#' In contrast to the various \code{[[} functions, this is
#' guaranteed to look only within the \code{data} slot. If
#' the named item is not found, \code{NULL} is returned.
#'
#' @param object an \code{oce} object
#' @param name String indicating the name of the item to be found.
oceGetData <- function(object, name)
{
    if (!inherits(object, "oce"))
        stop("oceGetData() only works for oce objects")
    if (missing(name))
        stop("'name' must be supplied")
    object@data[[name]]
}


#' Delete data from an \code{oce} object
#'
#' Return a copy of the supplied object that lacks the named
#' element in its \code{data} slot, and that has a note
#' about the deletion in its processing log.
#'
#' @param object an \code{oce} object
#' @param name String indicating the name of the item to be deleted.
oceDeleteData <- function(object, name)
{
    if (!inherits(object, "oce"))
        stop("oceDeleteData() only works for oce objects")
    if (name %in% names(object@data))
        object@data[[name]] <- NULL
    object@processingLog <- processingLogAppend(object@processingLog, paste("oceDeleteData() removed data$", name, sep="", collapse=""))
    object
}

#' Set something in the \code{data} slot of an \code{oce} object
#'
#' @details
#' There are three possibilities for \code{unit}:
#' \itemize{
#' \item \emph{Case 1.} \code{unit} is a \code{\link{list}} containing two items.
#' This may be a named or unnamed list. If named, then the names must be
#' \code{unit} and \code{scale}; if unnamed, then those names are assigned
#' to the items, in order. Either way, the \code{unit}
#' item must be an \code{\link{expression}} that specifies the unit,
#' and the \code{scale} item must be a string that describes the scale. For
#' example, modern temperatures have
#' \code{unit=list(unit=expression(degree*C), scale="ITS-90")}.
#' \item \emph{Case 2.} \code{unit} is an \code{\link{expression}} giving the unit as above. In this
#' case, the scale will be set to \code{""}.
#' \item \emph{Case 3.} \code{unit} is a character string that indicates the unit.
#' This is converted to an expression with 
#' \code{\link{as.expression}}, and the scale set to \code{""}.
#' }
#'
#' @param object an \code{oce} object
#' @param name String indicating the name of the item to be set.
#' @param value Value for the item.
#' @param unit An optional indication of the units for the item. This has 
#' three possible forms (see \dQuote{Details}).
#' @param originalName Optional character string giving an 'original' name (e.g.
#' as stored in the header of a data file).
#' @param note A note to be stored in the processing log. If an empty string
#' (the default) then an entry will be constructed from the function call. If
#' \code{NULL}, then no entry will be added to the processing log.
#'
#' @examples
#' data(ctd)
#' Tf <- swTFreeze(ctd)
#' ctd <- oceSetData(ctd, "freezing", Tf, list(unit=expression(degree*C), scale="ITS-90"))
#' feet <- swDepth(ctd) / 0.3048
#' ctd <- oceSetData(ctd, name="depthInFeet", value=feet, expression("feet"))
#' fathoms <- feet / 6
#' ctd <- oceSetData(ctd, "depthInFathoms", fathoms, "fathoms")
oceSetData <- function(object, name, value, unit, originalName, note="")
{
    if (!inherits(object, "oce"))
        stop("oceSetData() only works for oce objects")
    object@data[[name]] <- value
    if (nchar(note) > 0)
        object@processingLog <- processingLogAppend(object@processingLog, note)
    else if (!is.null(note))
        object@processingLog <- processingLogAppend(object@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    if (!missing(unit)) {
       if  (!("units" %in% names(object@metadata))) # some objects might not have units yet
           object@metadata$units <- list()
        if (is.list(unit)) {
            ## message("case 1")
            if (is.null(names(unit)))
                names(unit) <- c("unit", "scale")
            if (2 != sum(c("unit", "scale") %in% names(unit)))
                stop("'unit' must contain 'unit' and 'scale'")
            if (!is.expression(unit$unit))
                stop("'unit$unit' must be an expression")
            if (!is.character(unit$scale))
                stop("'unit$scale' must be a character string")
            object@metadata$units[[name]] <- unit
        } else if (is.expression(unit)) {
            ## message("case 2")
            object@metadata$units[[name]] <- list(unit=unit, scale="")
        } else if (is.character(unit)) {
            ## message("case 3")
            object@metadata$units[[name]] <- list(unit=as.expression(unit), scale="")
        } else {
            stop("'unit' must be a list, an expression, or a character string")
        }
        ## if (!is.list(unit)||2!=length(unit)) stop("'unit' must be a list of length 2")
        ## if (2 != sum(c("unit", "scale") %in% names(unit))) stop("'unit' must contain 'unit' and 'scale'")
        ## if (!is.expression(unit$unit)) stop("'unit$unit' must be an expression")
        ## if (!is.character(unit$scale)) stop("'unit$scale' must be a character string")
        ## object@metadata$units[[name]] <- unit
    }
    ## Handle originalName, if provided. Note that we have some code
    ## here to cover two types of storage.
    if (!missing(originalName)) {
        if ("dataNamesOriginal" %in% names(object@metadata)) {
            if (is.list(object@metadata$dataNamesOriginal)) { 
                ## After 2016-07-24 (issue 1017) we use a list.
                object@metadata$dataNamesOriginal[[name]] <- originalName
            } else {
                ## Before 2016-07-24 (issue 1017) we used a character vector.
                object@metadata$dataNamesOriginal <- as.list(object@metadata$dataNamesOriginal)
                object@metadata$dataNamesOriginal[[name]] <- originalName
            }
        } else {
            object@metadata$dataNamesOriginal <- list()
            object@metadata$dataNamesOriginal[[name]] <- originalName
        }
    }
    object
}

#' Get metadata element from an \code{oce} object
#'
#' In contrast to the various \code{[[} functions, this is
#' guaranteed to look only within the \code{metadata} slot. If
#' the named item is not found, \code{NULL} is returned.
#'
#' @param object an \code{oce} object
#' @param name String indicating the name of the item to be found.
oceGetMetadata <- function(object, name)
{
    if (!inherits(object, "oce"))
        stop("oceGetData() only works for oce objects")
    if (missing(name))
        stop("'name' must be supplied")
    object@metadata[[name]]
}


#' Delete metadata from an \code{oce} object
#'
#' Return a copy of the supplied object that lacks the named
#' element in its \code{metadata} slot, and that has a note
#' about the deletion in its processing log.
#'
#' @param object an \code{oce} object
#' @param name String indicating the name of the item to be deleted.
oceDeleteMetadata <- function(object, name)
{
    if (!inherits(object, "oce"))
        stop("oceDeleteData() only works for oce objects")
    if (name %in% names(object@metadata))
        object@metadata[[name]] <- NULL
    object@processingLog <- processingLogAppend(object@processingLog, paste("oceDeleteMetadata() removed metadadata$", name, sep="", collapse=""))
    object
}

#' Set something in the \code{metadata} slot of an \code{oce} object
#' @param object an \code{oce} object
#' @param name String indicating the name of the item to be set.
#' @param value Value for the item.
#' @param note A note to be stored in the processing log.
oceSetMetadata <- function(object, name, value, note="")
{
    if (!inherits(object, "oce"))
        stop("oceSetData() only works for oce objects")
    object@metadata[[name]] <- value
    object@processingLog <- processingLogAppend(object@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    if (nchar(note) > 0)
        object@processingLog <- processingLogAppend(object@processingLog, note)
    object
}

