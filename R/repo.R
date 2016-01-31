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
#' \enumerate{
#' \item \code{item="argo"} looks up argo data, referenced either by 
#' float id or by basin plus time.
#'
#' \itemize{
#'
#' \item \emph{id mode.} The return value is constructed
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
#' \item \emph{basin/time mode.}  See example 2.
#'
#' }
#'
#' Caution: tests made in January 2016 revealed that 
#' \code{http://data.nodc.noaa.gov} stores argo files with
#' \emph{lower-case} names, which is inconsistent
#' with argo documentation [1] and incompatible with \code{\link{read.argo}}.
#'
#' \item \code{item="hydrography"} looks up hydrographic cruise data.
#' At present, the only option is \code{window=list(cruise=EXPOCODE)},
#' where \code{EXPOCODE} is a string holding an expedition code.  See example 3.
#'
#' }
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
#' \code{basin}, a string naming the ocean basin (must be \code{"Atlantic"},
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
#' # 2. Plot map of Atlantic floats on New Year's Day, 2016.
#' file <- repositoryURL(item="argo",
#'                       window=list(basin="atlantic", time="20130101"))
#' \dontrun{
#' download.file(file, "nyd.nc")
#' nyd <- read.oce("nyd.nc")
#' summary(nydj)
#' plot(nyd)
#' mtext(file, side=3, line=0)
#'}
#'
#' # 3. Get a URL to a zipfile that expands to a directory holding files
#' #    for data from cruise 74JC20150110.
#' file <- repositoryURL(item="hydrography", window=list(cruise="74JC20150110"))
#' download.file(file, "cchdo_74JC20150110.zip")
#' # expect  http://cchdo.ucsd.edu/cruise/74JC20150110?download=dataset
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
            if (!is.list(window) || !("basin" %in% names(window)) || !("time" %in% names(window)))
                stop("'window' must be a list containing 'basin' and 'time'")
            window$ocean <- tolower(window$basin)
            if (!(window$basin %in% c("atlantic", "pacific", "indian")))
                stop("window$basin must be one of: 'Atlantic', 'Pacific' or 'Indian'")
            basin <- paste(window$basin, "_ocean", sep="")
            year <- substr(window$time, 1, 4)
            month <- substr(window$time, 5, 6)
            res <- paste(domain, "/pub/outgoing/argo/geo/", basin, "/", year, "/", month, "/", window$time, "_prof.nc", sep="")
        }
    } else if (item == "hydrography") {
        message('use window=list(cruise="")')
        if (missing(domain)) domain="http://cchdo.ucsd.edu"
        if (missing(window)) stop("must provide 'window'")
        if (!("cruise" %in% names(window))) stop("window must contain 'cruise'")
        res <- paste(domain, "/cruise/", window$cruise, "?download=dataset", sep="")
        ## Oh, this doesn't look good. The website
        ##     http://cchdo.ucsd.edu/cruise/74JC20150110
        ## has the following links. How to guess the subdir after 'data'??
        ##     http://cchdo.ucsd.edu/data/542/74JC20150110_hy1.csv
        ##     http://cchdo.ucsd.edu/data/336/74JC20150110_nc_hyd.zip
        ##     http://cchdo.ucsd.edu/data/11995/74JC20150110_ct1.zip
        ##     http://cchdo.ucsd.edu/data/11996/74JC20150110_nc_ctd.zip
        ## Maybe use the following
        ##     http://cchdo.ucsd.edu/cruise/74JC20150110?download=dataset
        ## which yields a zip file with contents:
        ##    0_74JC20150110_nc_hyd.zip
        ##    1_74JC20150110_hy1.csv
        ##    4_74JC20150110_ct1.zip
        ##    5_74JC20150110_nc_ctd.zip
        ## This looks more promising than the direct links, since we should be 
        ## able to rely on the user pick whatever they want.
    } else {
        stop("unknown 'item='", item, "'")
    }
    res
}

