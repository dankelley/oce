oce.edit <- function(x, item, value, action, reason="not specified", person="not specified")
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (!missing(item)) {
        if (missing(value)) stop("must supply a 'value' for this 'item'")
        if (!(item %in% names(x$metadata))) stop("no item named '", item, "' in object's  metadata")
        x$metadata[item] <- value
    } else if (!missing(action)) {
        eval(parse(text=action))        # FIXME: should check if it worked
        rval <- x
    } else {
        stop("must supply either an 'item' plus a 'value', or an 'action'")
    }
    processing.log.append(x, paste(deparse(match.call()), sep="", collapse=""))
}
