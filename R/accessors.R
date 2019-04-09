## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get Something from the data Slot of an oce Object
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

#' Delete Something in the data Slot of an oce Object
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

#' Set Something in the data Slot of an oce Object
#'
#' @details
#' There are three possibilities for \code{unit}:
#' \itemize{
#' \item \emph{Case 1.} \code{unit} is a named or unnamed \code{\link{list}}
#' that contains two items.
#' If the list is named, the names must be
#' \code{unit} and \code{scale}. If the list is unnamed, the stated names are assigned
#' to the items, in the stated order. Either way, the \code{unit}
#' item must be an \code{\link{expression}} that specifies the unit,
#' and the \code{scale} item must be a string that describes the scale. For
#' example, modern temperatures have
#' \code{unit=list(unit=expression(degree*C), scale="ITS-90")}.
#' \item \emph{Case 2.} \code{unit} is an \code{\link{expression}} giving the unit as above. In this
#' case, the scale will be set to \code{""}.
#' \item \emph{Case 3.} \code{unit} is a character string that is converted
#' into an expression with \code{\link{parse}(text=unit)},
#' and the scale set to \code{""}.
#' }
#'
#' @param object an \code{oce} object
#' @param name String indicating the name of the item to be set.
#' @param value Value for the item.
#' @param unit An optional indication of the units for the item. This has
#' three possible forms (see \dQuote{Details}).
#' @param originalName Optional character string giving an 'original' name (e.g.
#' as stored in the header of a data file).
#' @param note Either empty (the default), a character string, or \code{NULL},
#' to control additions made to the processing log of the return value. If
#' \code{note=""} then the an entry is created based on deparsing the function call.
#' If \code{note} is a non-empty string, then that string gets added added
#' to the processing log. Finally, if \code{note=NULL}, then nothing is
#' added to the processing log. This last form is useful in cases where
#' \code{oceSetData} is to be called many times in succession, resulting
#' in an overly verbose processing log; in such cases, it might help
#' to add a note by e.g. \code{processingLog(a) <- "QC (memo dek-2018-01/31)"}
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
    if (!missing(unit) && !is.null(unit)) {
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
            ## browser()
            l <- list(unit=parse(text=unit), scale="")
            attributes(l[[1]]) <- NULL # the parse() adds unwanted attributes
            object@metadata$units[[name]] <- l
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
    if (!is.null(note)) {
        if (nchar(note) > 0)
            object@processingLog <- processingLogAppend(object@processingLog, note)
        else
            object@processingLog <- processingLogAppend(object@processingLog,
                                                        paste(deparse(match.call()),
                                                              sep="", collapse=""))
    }
    object
}

#' Get Something From the metadata Slot in an oce Object
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

#' Delete Something in the metadata Slot of an oce Object
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

#' Set Something in the metadata Slot of an oce Object
#' @param object an \code{oce} object
#' @param name String indicating the name of the item to be set.
#' @param value Value for the item.
#' @param note Either empty (the default), a character string, or \code{NULL},
#' to control additions made to the processing log of the return value. If
#' \code{note=""} then the an entry is created based on deparsing the function call.
#' If \code{note} is a non-empty string, then that string gets added added
#' to the processing log. Finally, if \code{note=NULL}, then nothing is
#' added to the processing log.  This last form is useful in cases where
#' \code{oceSetData} is to be called many times in succession, resulting
#' in an overly verbose processing log; in such cases, it might help
#' to add a note by e.g. \code{processingLog(a) <- "QC (memo dek-2018-01/31)"}
#'
oceSetMetadata <- function(object, name, value, note="")
{
    if (!inherits(object, "oce"))
        stop("oceSetData() only works for oce objects")
    object@metadata[[name]] <- value
    if (!is.null(note)) {
        if (nchar(note) > 0)
            object@processingLog <- processingLogAppend(object@processingLog, note)
        else
            object@processingLog <- processingLogAppend(object@processingLog,
                                                        paste(deparse(match.call()),
                                                              sep="", collapse=""))
    }
    object
}
