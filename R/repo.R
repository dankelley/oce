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
#' The following values for \code{item} are handled.
#'
#' \strong{argo} data. These may be looked up by float number or 
#' by ocean/day window.\enumerate{
#' \item \emph{Per-float mode.} The return value is constructed
#' as \code{[domain]/pub/outgoing/argo/dac/[subdomain]/[id]/[id]_prof.nc},
#' where square brackets indicate the values of the named arguments
#' to \code{repositoryURL}. This has only been checked for domain
#' \code{domain="ftp://usgodae.org"} and \code{subdomain="bodc"},
#' so these are the defaults. The value of the relevant subdomain may
#' be known to users familiar with argo data; others can discover the
#' data archiving center for a given float by inspecting the subdirectories
#' of the FTP site \code{ftp://usgodae.org/pub/outgoing/argo/dac}
#' or by accessing
#' \code{ftp://usgodae.org/pub/outgoing/argo/ar_index_global_meta.txt}
#' and searching for the float identifer.
#' \item \emph{Per-ocean/day mode.}  See example 2.
#' }
#' Caution: tests made in January 2016 revealed that 
#' \code{http://data.nodc.noaa.gov} stores argo files with
#' \emph{lower-case} names, which is inconsistent
#' with argo documentation [1] and incompatible with \code{\link{read.argo}}.
#'
#' @section History:
#' This function was drafted in late January, 2016, and its argument
#' list is likely to change through the early months of 2016. Some
#' of this will be to accommodate new data types, but changes to 
#' website structures will also precipitate changes to how this
#' function works.
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
#' @param window A list specifying a data window for a snapshot; the contents are
#' \code{ocean}, a string naming the ocean (must be \code{"Atlantic"},
#' \code{"Pacific"} or \code{"Indian"}) and a string named \code{time},
#' specifying a date in the format \code{yyyymmdd}.
#' @param domain The base-level domain (more information to be added later).
#' @param subdomain The second-level domain (more information to be added later).
#'
#' @author Dan Kelley
#' @seealso \code{\link{download.file}} for downloading the files found by \code{repositoryURI}
#' @examples
#' # 1. Historical data from individual float
#' file <- repositoryURL(item="argo", id="6900388")
#' \dontrun{
#' download.file(file, "argo.nc")
#' argo <- read.oce("argo.nc")
#' summary(argo)
#' plot(argo)
#' mtext(file, side=3, line=0)
#' }
#'
#' # 2. Snapshot of Atlantic floats on New Year's day, 2016.
#' file <- repositoryURL(item="argo",
#'                       window=list(ocean="atlantic", time="20130101"))
#' \dontrun{
#' download.file(file, "ny.nc")
#' ny <- read.oce("ny.nc")
#' summary(ny)
#' plot(ny)
#' mtext(file, side=3, line=0)
#'}
#'
#' @references
#' 1. The \code{argo} netCDF format is described at
#' \code{http://archimer.ifremer.fr/doc/00187/29825/40575.pdf}; 
#' as of January 28, 2016, Section 2.2.4 of this document
#' indicates that variable names are to be in upper case,
#' which is true for the \code{nodc.noaa.gov} server, so data provided
#' by this server cannot be read with \code{\link{read.argo}}.
repositoryURL <- function(item, id, window, domain, subdomain)
{
    if (missing(item)) stop("'item' is missing")
    if (item == "argo") {
        if (missing(window)) {
            if (missing(id)) stop("'id' is missing")
            if (missing(domain)) domain <- "ftp://usgodae.org"
            filename <- paste(id, "_prof.nc", sep="")
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
            if (missing(domain)) domain <- "ftp://usgodae.org"
            if (!is.list(window) || !("ocean" %in% names(window)) || !("time" %in% names(window)))
                stop("'window' must be a list containing 'ocean' and 'time'")
            window$ocean <- tolower(window$ocean)
            if (!(window$ocean %in% c("atlantic", "pacific", "indian")))
                stop("window$ocean must be one of: 'Atlantic', 'Pacific' or 'Indian'")
            ocean <- paste(window$ocean, "_ocean", sep="")
            year <- substr(window$time, 1, 4)
            month <- substr(window$time, 5, 6)
            res <- paste(domain, "/pub/outgoing/argo/geo/", ocean, "/", year, "/", month, "/", window$time, "_prof.nc", sep="")
        }
    } else {
        stop("unknown 'item='", item, "'")
    }
    res
}

