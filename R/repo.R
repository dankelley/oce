#' Calculate a web link to download Oceanographic data
#'
#' This function is able to construct universal resource identifiers (URIs)
#' that can be used with \code{\link{download.file}} to download data from
#' several common Oceanographic repositories.
#'
#' The goal is to spare users the task of finding such URIs for casual use
#' or one-off applications.  There is no intention of trying to reproduce
#' the web tools provided with many repositories.
#'
#' This function will be most useful if users contribute to it, both by 
#' pointing out changes to URLs and by suggesting additions for useful
#' datasets.
#'
#' @param domain The base-level domain (more information to be added later).
#' @param subdomain The second-level domain (more information to be added later).
#' @param item A string indicating the item sought; at present only \code{"argo"} is allowed.
#' @param window An indication of a window to select data (ignored at present).
#' @param id A string indicating the ID of the sought item.
#'
#' @author Dan Kelley
#' @seealso \code{\link{download.file}} for downloading the datafiles found by \code{repositoryURI}
#' @examples
#' file <- repositoryURL(item="argo", id=list(id="6900388"))
#' \dontrun{
#' download.file(file, "argo.nc")
#' argo <- read.oce("argo.nc")
#' summary(argo)
#' plot(argo)
#' }
repositoryURL <- function(domain, subdomain, item, window, id)
{
    if (!missing(window)) warning("'window' is ignored at the moment")
    if (missing(item)) stop("'item' is missing")
    if (missing(id)) stop("'id' is missing")
    if (item == "argo") {
        if (!missing(window)) warning("'window' is ignored in this version\n")
        if (missing(domain)) domain <- "ftp://usgodae.org"
        if (missing(subdomain)) subdomain <- "bodc"
        # ftp://usgodae.org/pub/outgoing/argo//dac/coriolis/6900338/6900338_prof.nc
        if (as.numeric(id) < 0) stop("an argo id must not be negative")
        res <- paste(domain, "/pub/outgoing/argo//dac/", subdomain, "/", id, "/", id, "_prof.nc", sep="")
    } else {
        stop("unknown 'item='", item, "'")
    }
    res
}

