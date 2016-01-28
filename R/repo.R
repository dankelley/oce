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
#' is returned; otherwise an error message is given. Note: early
#' tests with \code{http://data.nodc.noaa.gov} showed that it stores 
#' its argo files with \emph{lower-case} names, which is inconsistent
#' with argo documentation [1] and incompatible with \code{\link{read.argo}}.
#' So, don't use that server!
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
#' @seealso \code{\link{download.file}} for downloading the files found by \code{repositoryURI}
#' @examples
#' file <- repositoryURL(item="argo", id="6900388")
#' \dontrun{
#' download.file(file, "argo.nc")
#' argo <- read.oce("argo.nc")
#' summary(argo)
#' plot(argo)
#' }
#'
#' @references
#' \itemize{
#' \item \strong{1.} The \code{argo} netCDF format is described at
#' \code{http://archimer.ifremer.fr/doc/00187/29825/40575.pdf}; 
#' as of January 28, 2016, Section 2.2.4 of this document
#' indicates that variable names are to be in upper case,
#' which is true for the \code{nodc.noaa.gov} server, so data provided
#' by this server cannot be read with \code{\link{read.argo}}.
#'}
repositoryURL <- function(item, id, window, domain, subdomain)
{
    if (missing(item)) stop("'item' is missing")
    if (item == "argo") {
        if (!missing(window)) warning("'window' is ignored at the moment")
        if (missing(id)) stop("'id' is missing")
        if (missing(domain)) domain <- "ftp://usgodae.org"
        filename <- paste(id, "_profile.nc", sep="")
        ## The file could be in any of the "dac" subdirectories, so we should
        ## try looking in each ... FIXME: code this or delete it here and in doc.
        if (missing(subdomain)) {
            subdomain <- "bodc"
            if (as.numeric(id) < 0) stop("negative argo id values are not allowed")
            res <- paste(domain, "/pub/outgoing/argo/dac/", subdomain, "/", id, "/", filename, sep="")
            ##> tf <- tempfile('oce')
            ##> message("tf: ", tf)
            ##> download.file(paste(domain, "/pub/outgoing/argo/dac/", sep=""), tf)
            ##> dacs <- gsub(".* ", "", readLines(tf))
            ##> message("dacs: ", paste(dacs, collapse=" "))
            ##> res <- NULL
            ##> for (dac in dacs) {
            ##>     message("dac: ", dac)
            ##>     res <- c(res, paste(domain, "/pub/outgoing/argo/dac/", dac, "/", id, "/",  filename, sep=""))
            ##> }
            ##> unlink(tf)
        } else {
            subdomain <- "bodc"
            if (as.numeric(id) < 0) stop("negative argo id values are not allowed")
            res <- paste(domain, "/pub/outgoing/argo/dac/", subdomain, "/", id, "/", filename, sep="")
        }
    } else {
        stop("unknown 'item='", item, "'")
    }
    res
}

