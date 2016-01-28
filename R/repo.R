#' Calculate a web link to download Oceanographic data
#'
#' This function is able to construct universal resource identifiers (URIs)
#' that can be used with \code{\link{download.file}} to download data from
#' several common Oceanographic repositories.   The goal of this function is to
#' spare users the task of finding such URLs for casual use or one-off
#' applications.  There is no intention of trying  to reproduce the web tools
#' provided with many repositories, nor to provide an exhaustive interface to
#' a wide suite of data types (see \dQuote{Contributing}).
#'
#' @details
#' The following values for \code{item} are handled. \itemize{
#' \item \code{item="argo"}. The data are by default sought from domain
#' \code{ftp://usgodae.org}, and the procedure is to construct
#' a test URL as \code{ftp://usgodae.org/pub/outgoing/argo//dac/} and
#' then to probe that to find the list of available 'dac' (data 
#' archiving centre) subdirectories. Then each of these is searched
#' to try to find a subdirectory with name equal to \code{id}. The
#' first matching subdirectory is then opened and a test is made
#' for the existence of a file with name constructed by pasting 
#' \code{"_prof.nc"} onto \code{id}. If this works, then the URL
#' is returned; otherwise an error message is given.
#' }
#'
#' @section History:
#' This function was drafted in late January, 2016, and it is likely to
#' change, perhaps in incompatible ways, through February.
#'
#' @section Contributing:
#' This function will be most useful if users contribute to it, both by 
#' pointing out changes to URLs and by suggesting additions for useful
#' datasets. The most useful comments will be concrete; merely suggesting
#' that an interface be provided to such-and-such server is too vague
#' to help much.
#'
#' @param item A string indicating the item sought; see \dQuote{Details}.
#' @param id A string indicating the ID of the sought item.
#' @param window An indication of a window to select data (ignored at present).
#' @param domain The base-level domain (more information to be added later).
#' @param subdomain The second-level domain (more information to be added later).
#'
#' @author Dan Kelley
#' @seealso \code{\link{download.file}} for downloading the datafiles found by \code{repositoryURI}
#' @examples
#' file <- repositoryURL(item="argo", id="6900388")
#' \dontrun{
#' download.file(file, "argo.nc")
#' argo <- read.oce("argo.nc")
#' summary(argo)
#' plot(argo)
#' }
repositoryURL <- function(item, id, window, domain, subdomain)
{
    if (missing(item)) stop("'item' is missing")
    if (item == "argo") {
        if (!missing(window)) warning("'window' is ignored at the moment")
        if (missing(id)) stop("'id' is missing")
        if (missing(domain)) domain <- "ftp://usgodae.org"
        if (missing(subdomain)) subdomain <- "bodc"
        # ftp://usgodae.org/pub/outgoing/argo//dac/coriolis/6900338/6900338_prof.nc
        if (as.numeric(id) < 0) stop("negative argo id values are not allowed")
        res <- paste(domain, "/pub/outgoing/argo//dac/", subdomain, "/", id, "/", id, "_prof.nc", sep="")
    } else {
        stop("unknown 'item='", item, "'")
    }
    res
}

