oce.edit <- function(x, item, value, reason="not specified", person="not specified")
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (missing(item))       stop("must supply an 'item' to modify")
    if (missing(value))      stop("must supply a new 'value' to for item '", item, "'")
    if (!any(item == names(x$metadata))) stop("There is no item named '", item, "' in the metadata for this oce object")
    res <- x
    res$metadata[item] <- value
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    res <- processing.log.append(res, log.action)
    return(res)
}
