## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get Something from the data Slot of an oce Object
#'
#' In contrast to the various `[[` functions, this is
#' guaranteed to look only within the `data` slot. If
#' the named item is not found, `NULL` is returned.
#'
#' @param object an [oce-class] object.
#'
#' @param name String indicating the name of the item to be found.
#'
#' @author Dan Kelley
#'
#' @family things related to the data slot
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
#' element in its `data` slot, and that has a note
#' about the deletion in its processing log.
#'
#' @param object an `oce` object
#'
#' @param name String indicating the name of the item to be deleted.
#'
#' @author Dan Kelley
#'
#' @family things related to the data slot
oceDeleteData <- function(object, name)
{
    if (!inherits(object, "oce"))
        stop("oceDeleteData() only works for oce objects")
    if (name %in% names(object@data))
        object@data[[name]] <- NULL
    if (name %in% names(object@metadata$units))
        object@metadata$units[[name]] <- NULL
    if (name %in% names(object@metadata$flags))
        object@metadata$flags[[name]] <- NULL
    object@processingLog <- processingLogAppend(object@processingLog, paste("oceDeleteData() removed data$", name, sep="", collapse=""))
    object
}

#' Set Something in the data Slot of an oce Object
#'
#' @details
#' There are three possibilities for `unit`:
#' 1. `unit` is a named or unnamed [list()] that contains two items.
#' If the list is named, the names must be
#' `unit` and `scale`. If the list is unnamed, the stated names are assigned
#' to the items, in the stated order. Either way, the `unit`
#' item must be an [expression()] that specifies the unit,
#' and the `scale` item must be a string that describes the scale. For
#' example, modern temperatures have
#' `unit=list(unit=expression(degree*C), scale="ITS-90")`.
#' 2. `unit` is an [expression()] giving the unit as above. In this
#' case, the scale will be set to `""`.
#' 3. `unit` is a character string that is converted
#' into an expression with [parse]`(text=unit)`,
#' and the scale set to `""`.
#'
#' @param object an [oce-class] object.
#'
#' @param name String indicating the name of the item to be set.
#'
#' @param value Value for the item.
#'
#' @param unit An optional indication of the units for the item. This has
#' three possible forms (see \dQuote{Details}).
#'
#' @param originalName Optional character string giving an 'original' name (e.g.
#' as stored in the header of a data file).
#'
#' @param note Either empty (the default), a character string, or `NULL`,
#' to control additions made to the processing log of the return value. If
#' `note=""` then the an entry is created based on deparsing the function call.
#' If `note` is a non-empty string, then that string gets added added
#' to the processing log. Finally, if `note=NULL`, then nothing is
#' added to the processing log. This last form is useful in cases where
#' `oceSetData` is to be called many times in succession, resulting
#' in an overly verbose processing log; in such cases, it might help
#' to add a note by e.g. `processingLog(a) <- "QC (memo dek-2018-01/31)"`
#'
#' @examples
#' data(ctd)
#' Tf <- swTFreeze(ctd)
#' ctd <- oceSetData(ctd, "freezing", Tf, list(unit=expression(degree*C), scale="ITS-90"))
#' feet <- swDepth(ctd) / 0.3048
#' ctd <- oceSetData(ctd, name="depthInFeet", value=feet, expression("feet"))
#' fathoms <- feet / 6
#' ctd <- oceSetData(ctd, "depthInFathoms", fathoms, "fathoms")
#'
#' @author Dan Kelley
#'
#' @family things related to the data slot
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


#' Rename Something in an object's data Slot
#'
#' Rename an item within the `data` slot of an [oce-class] object.
#'
#' @param object an [oce-class] object.
#'
#' @param old character value that matches the name of an item in `object`'s `data` slot.
#'
#' @param new character value to be used as the new name that matches the name of an item in
#' `object`'s `data` slot. Thus must not be the name of something that is already in the `data`
#' slot. If `new` is the  same as `old`, then the object is returned unaltered.
#'
#' @param note character value that holds an explanation of the reason for the change. If this
#' is a string of non-zero length, then this is inserted in the processing log of the returned
#' value. If it is `NULL`, then no entry is added to the processing log.  Otherwise, the processing
#' log gets a new item that is constructed from the function call.
#'
#' @author Dan Kelley
#'
#' @family things related to the data slot
oceRenameData <- function(object, old, new, note="")
{
    if (missing(object)) stop("must provide 'object'")
    if (!inherits(object, "oce")) stop("'object' must inherit from the \"oce\" class")
    if (missing(old)) stop("must provide 'old'")
    if (!is.character(old)) stop("'old' must be a character value")
    if (missing(new)) stop("must provide 'new'")
    if (!is.character(new)) stop("'new' must be a character value")
    if (!(old %in% names(object@data)))
        stop("object's data slot does not contain an item named '", old, "'")
    if (new %in% names(object@data))
        stop("cannot rename to '", new, "' because the object's data slot already contains something with that name")
    if (new != old) {
        names <- names(object@data)
        names[names == old] <- new
        names(object@data) <- names
        if (!is.null(note)) {
            object@processingLog <- if (nchar(note) > 0) processingLogAppend(object@processingLog, note) else
                processingLogAppend(object@processingLog, paste(deparse(match.call())))
        }
    }
    object
}

#' Rename Something in an object's metadata Slot
#'
#' Rename an item within the `metadata` slot of an [oce-class] object.
#'
#' @param object an [oce-class] object.
#'
#' @param old character value that matches the name of an item in `object`'s `metadata` slot.
#'
#' @param new character value to be used as the new name that matches the name of an item in
#' `object`'s `metadata` slot. Thus must not be the name of something that is already in the `metadata`
#' slot. If `new` is the  same as `old`, then the object is returned unaltered.
#'
#' @param note character value that holds an explanation of the reason for the change. If this
#' is a string of non-zero length, then this is inserted in the processing log of the returned
#' value. If it is `NULL`, then no entry is added to the processing log.  Otherwise, the processing
#' log gets a new item that is constructed from the function call.
#'
#' @author Dan Kelley
#'
#' @family things related to the metadata slot
oceRenameMetadata <- function(object, old, new, note="")
{
    if (missing(object)) stop("must provide 'object'")
    if (!inherits(object, "oce")) stop("'object' must inherit from the \"oce\" class")
    if (missing(old)) stop("must provide 'old'")
    if (!is.character(old)) stop("'old' must be a character value")
    if (missing(new)) stop("must provide 'new'")
    if (!is.character(new)) stop("'new' must be a character value")
    if (!(old %in% names(object@metadata)))
        stop("object's data slot does not contain an item named '", old, "'")
    if (new %in% names(object@metadata))
        stop("cannot rename to '", new, "' because the object's metadata slot already contains something with that name")
    if (new != old) {
        names <- names(object@metadata)
        names[names == old] <- new
        names(object@metadata) <- names
        if (!is.null(note)) {
            object@processingLog <- if (nchar(note) > 0) processingLogAppend(object@processingLog, note) else
                processingLogAppend(object@processingLog, paste(deparse(match.call())))
        }
    }
    object
}

#' Get Something From the metadata Slot in an oce Object
#'
#' In contrast to the various `[[` functions, this is
#' guaranteed to look only within the `metadata` slot. If
#' the named item is not found, `NULL` is returned.
#'
#' @param object an `oce` object
#'
#' @param name String indicating the name of the item to be found.
#'
#' @author Dan Kelley
#'
#' @family things related to the metadata slot
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
#' element in its `metadata` slot, and that has a note
#' about the deletion in its processing log.
#'
#' @param object an `oce` object
#'
#' @param name String indicating the name of the item to be deleted.
#'
#' @author Dan Kelley
#'
#' @family things related to the metadata slot
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
#'
#' @param object an `oce` object
#'
#' @param name String indicating the name of the item to be set.
#'
#' @param value Value for the item.
#'
#' @param note Either empty (the default), a character string, or `NULL`,
#' to control additions made to the processing log of the return value. If
#' `note=""` then the an entry is created based on deparsing the function call.
#' If `note` is a non-empty string, then that string gets added added
#' to the processing log. Finally, if `note=NULL`, then nothing is
#' added to the processing log.  This last form is useful in cases where
#' `oceSetData` is to be called many times in succession, resulting
#' in an overly verbose processing log; in such cases, it might help
#' to add a note by e.g. `processingLog(a) <- "QC (memo dek-2018-01/31)"`
#'
#' @author Dan Kelley
#'
#' @family things related to the metadata slot
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
