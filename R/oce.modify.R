oce.modify <- function(x, action, reason="", person="")
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (missing(action)) stop("must supply an action")
    eval(parse(text=action))
    processing.log.append(x, paste(deparse(match.call()), sep="", collapse=""))
}
