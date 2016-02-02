#' Calculate a web link to download Oceanographic data
#'
#' This function is able to construct universal resource locators (URLs)
#' that can be used with \code{\link{download.file}} to download data from
#' several common Oceanographic repositories. The goal of this function is to
#' spare users the task of finding such URLs for casual use or one-off
#' applications. There is no intention of trying to reproduce the web tools
#' provided with many data repositories, nor of providing an exhaustive
#' list of data types. Readers are advised strongly to become familiar with
#' the data sites that are relevant to their own work, and also to the
#' wide-reaching NOAA data-accumulator system known as ERDDAP [1], which
#' provides powerful graphical tools for finding data. An R interface to
#' ERDDAP is provided by the \CRANpkg{rerddap} package, of which 
#' the present function is the faintest of faint echoes.
#'
#' @details
#' The following values for \code{item} are handled. In each example, square
#' brackets are used to indicate that the contents are to be replaced by
#' the named item from the argument list.
#'
#' \enumerate{
#' \item \code{item="argo"} looks up argo data, referenced either by 
#' float id or by basin plus time.
#'
#' \itemize{
#'
#' \item \emph{id mode.} The return value is constructed
#' as \preformatted{"[domain]/pub/outgoing/argo/dac/[subdomain]/[id]/[id]_prof.nc"}
#' where if \code{domain} is missing a default of 
#' \code{"ftp://usgodae.org"} will be used, and similarly \code{"bodc"}
#' will be used for \code{subdomain} if it is missing. The proper subdomain
#' for a given float \code{id} will be known to users familiar with the data,
#' while other users may infer it by inspecting the subdirectories
#' of the FTP site \code{ftp://usgodae.org/pub/outgoing/argo/dac}
#' or by accessing
#' \code{ftp://usgodae.org/pub/outgoing/argo/ar_index_global_meta.txt}
#' and searching for the float identifier, \code{id}. See example 1.
#'
#' \item \emph{basin/time mode.} The return value is constructed as
#' \preformatted{"[domain]/pub/outgoing/argo/geo/[B]_ocea/[yyyy]/[mm]/[dd]/[yyy][mm][dd]_prof.nc"}
#' where \code{B} is the value of \code{window$basin}, and \code{yyyy},
#' \code{mm} and \code{dd} are extracted from the characters of \code{window$time}.
#' See example 2.
#'
#' }
#'
#' Caution: tests made in January 2016 revealed that 
#' \code{http://data.nodc.noaa.gov} stores argo files with
#' \emph{lower-case} names, which is inconsistent
#' with argo documentation [2] and incompatible with \code{\link{read.argo}}.
#'
#' \item \code{item="hydrography"} looks up hydrographic data, at present
#' only by specified cruise (or EXPOCODE, in the notation used by archiving 
#' agencies). The return value is constructed as
#' \preformatted{"[domain]/cruise/[window$cruise]?download=dataset"} where
#' the \code{domain} argument defaults to \code{"http://cchdo.ucsd.edu"}
#' if it is not provided and \code{window$cruise} is a string containing 
#' the cruise EXPOCODE.  (It is assumed that readers will know the identifier,
#' or look it up with the searching tools provided at \url{http://cchdo.ucsd.edu}).
#' The returned URL points to a zip file
#' that can be expanded to get including bottle and CTD data. See example 3.
#'
#' }
#'
#' @section Contributing:
#' This function was drafted in late January, 2016, and its argument
#' list is likely to change through the early months of 2016. Some
#' of this will be to accommodate new data types, but changes to 
#' website structures will also precipitate changes to how this
#' function works.
#'
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
#' @seealso The \CRANpkg{rerddap} package provides excellent capabilities for searching for datasets, and the related website \url{http://coastwatch.pfeg.noaa.gov/erddap/index.html} provides excellent graphical interfaces to this archive. \code{\link{download.file}} for downloading the files found by \code{repositoryURI}
#' @examples
#' # Example 1.
#' # Trajectory of the argo float used for data("argo").
#' file <- repositoryURL(item="argo", id="6900388")
#' \dontrun{
#' download.file(file, "argo.nc")
#' argo <- read.oce("argo.nc")
#' summary(argo)
#' plot(argo)
#' mtext(file, side=3, line=0)}
#'
#' # Example 2.
#' # Map of Atlantic float SST on New Year's Day, 2016, 
#' # with faded colours if the top datum is not close to 
#' # the surface.
#' file <- repositoryURL(item="argo",
#'                       window=list(basin="Atlantic", time="20130101"))
#' \dontrun{
#' download.file(file, "nyd.nc")
#' nyd <- read.oce("nyd.nc")
#' ## Colour-code
#' Tlim <- c(-2, 30)
#' temp <- nyd[["temperature"]][1,]
#' cm <- colormap(temp, zlim=Tlim)
#' pres <- nyd[['pressure']][1,]
#' col <- cm$zcol
#' col2 <- unlist(lapply(seq_along(temp),
#'                       function(i) adjustcolor(col[i], pres[i]/10)))
#' par(mar=c(4, 4, 1, 4))
#' drawPalette(colormap=cm)
#' plot(nyd, pch=21, bg=col2, cex=2, mar=c(4, 4, 1, 4))
#' mtext(file, side=3, line=0)}
#'
#' # Example 3.
#' # Zipfile holds data from cruise 74JC20150110.
#' file <- repositoryURL(item="hydrography", window=list(cruise="74JC20150110"))
#' \dontrun{
#' download.file(file, "cchdo_74JC20150110.zip")}
#'
#' @references
#' \itemize{
#' \item 1. The ERDDAP site is at \url{http://coastwatch.pfeg.noaa.gov/erddap/index.html} and
#' \url{http://coastwatch.pfeg.noaa.gov/erddap/information.html} provides an itemized summary
#' of some key issues relating to it.  Rich Signell's video tutorial on ERDDAP
#' (\url{https://www.youtube.com/watch?v=18xZoXu1USM}) is a good way to get an idea for
#' the scope of ERDDAP.
#' \item 2. The \code{argo} netCDF format is described at
#' \code{http://archimer.ifremer.fr/doc/00187/29825/40575.pdf}; 
#' as of January 28, 2016, Section 2.2.4 of this document
#' indicates that variable names are to be in upper case,
#' which is true for the \code{nodc.noaa.gov} server, so data provided
#' by this server cannot be read with \code{\link{read.argo}}.
#' }
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
            window$basin <- tolower(window$basin)
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

