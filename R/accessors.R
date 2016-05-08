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
#' @param object an \code{oce} object
#' @param name String indicating the name of the item to be set.
#' @param value Value for the item.
#' @param units An optional list specifying units for the item, containing an element
#' named \code{units} that is an \code{\link{expression}}, and a string named
#' \code{scale} that describes the scale used. For example, modern temperatures
#' have \code{unit=list(unit=expression(degree*C), scale="ITS-90")}.
#' @param note A note to be stored in the processing log.
oceSetData <- function(object, name, value, units, note="")
{
    if (!inherits(object, "oce"))
        stop("oceSetData() only works for oce objects")
    object@data[[name]] <- value
    object@processingLog <- processingLogAppend(object@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    if (nchar(note) > 0)
        object@processingLog <- processingLogAppend(object@processingLog, note)
    if (!missing(units) && "units" %in% names(object@metadata)) {
        if (!is.list(units)||2!=length(units)) stop("'units' must be a list of length 2")
        if (2 != sum(c("unit", "scale") %in% names(units))) stop("'units' must contain 'unit' and 'scale'")
        if (!is.expression(units$unit)) stop("'units$unit' must be an expression")
        if (!is.character(units$scale)) stop("'units$scale' must be a character string")
        object@metadata$units[[name]] <- units
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


time <- function(x)
{
    if (!("time" %in% names(x@data)))
        stop("no 'time' in names(x@data)")
    x@data$time
}

