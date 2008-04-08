ctd.edit <- function(x, item, value, reason="not specified", person="not specified")
{
    if (!inherits(x, "ctd"))
    	stop("method is only for ctd objects")
    if (missing(item)) stop("must supply an 'item' to modify")
    if (missing(value)) stop("must supply a new 'value' to for item '", item, "'")
    allowed.items <- names(x$metadata)
    if (!any(item == allowed.items)) stop("There is no item named '", item, "' in the metadata for this ctd object")
    ## OK, now do it
    res <- x
    res$metadata[item] <- value
    log.item <- paste("modified by ctd.edit(x, item=\"", item, "\", value=\"", value, "\", reason=\"", reason, "\", person=\"", person, "\")",sep="")
    res <- processing.log.append(res, log.item)
    return(res)
}
