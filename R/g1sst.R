# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

setClass("satellite", contains="oce")

#' Class to Store G1SST Satellite-model Data
#'
#' This class stores G1SST model-satellite products.
#'
#' G1SST is an acronym for global 1-km sea surface temperature, a product
#' that combines satellite data with the model output. It is provided by
#' the JPO ROMS (Regional Ocean Modelling System) modelling group.
#' See the JPL website (reference 1) to learn more about the data, and see
#' the [read.g1sst()] documentation for an example
#' of downloading and plotting.
#'
#' It is important not to regard G1SST data in the same category as,
#' say, [amsr-class] data, because the two products
#' differ greatly with respect to cloud cover. The satellite used by
#' [amsr-class] has the ability to sense water temperature
#' even if there is cloud cover, whereas `g1sst` fills in cloud
#' gaps with model simulations.  It can be helpful to consult
#' reference 1 for a given time, clicking and then unclicking the radio button
#' that turns off the model-based filling of cloud gaps.
#'
#' @templateVar class g1sst
#'
#' @templateVar dataExample {}
#'
#' @templateVar metadataExample {}
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @references
#' 1. JPO OurOcean Portal `https://ourocean.jpl.nasa.gov/SST/`
#' (link worked in 2016 but was seen to fail 2017 Feb 2).
#'
#' @author Dan Kelley
#'
#' @family classes holding satellite data
#' @family things related to g1sst data
setClass("g1sst", contains="satellite")


#' @title Extract Something From a G1SST Object
#'
#' @param x a [g1sst-class] object.
#'
#' @section Details of the Specialized Method:
#'
#' * If `i` is `"?"`, then the return value is a list
#' containing four items, each of which is a character vector
#' holding the names of things that can be accessed with `[[`.
#' The `data` and `metadata` items hold the names of
#' entries in the object's data and metadata
#' slots, respectively. The `dataDerived`
#' and `metadataDerived` items are each NULL, because
#' no derived values are defined by [g1sst-class] objects.
#'
#' @template sub_subTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to g1sst data
setMethod(f="[[",
          signature(x="g1sst", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "?")
                  return(list(metadata=sort(names(x@metadata)),
                          metadataDerived=NULL,
                          data=sort(names(x@data)),
                          dataDerived=NULL))
              callNextMethod()         # [[
          })

#' @title Replace Parts of a G1SST Object
#'
#' @param x a [g1sst-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to g1sst data
setMethod(f="[[<-",
          signature(x="g1sst", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })




#' @title Read a G1SST file
#'
#' @description
#' Read a G1SST file in the netcdf format provided by the ERDDAP server
#' (see reference 1).
#'
#' @details
#' As noted in the documentation for the [g1sst-class] class, one
#' must be aware of the incorporation of model simulations in the
#' `g1sst` product. For example, the code presented below
#' might lead one to believe that the mapped field represents
#' observations, whereas in fact it can be verified by
#' consulting reference 2 (clicking and unclicking the radio button to
#' show just the data) that the field mostly derives from simulation.
#'
#' @param file character value containing the name of a netcdf file containing G1SST data.
#'
#' @template encodingIgnoredTemplate
#'
#' @return A [g1sst-class] object.
#'
#' @examples
#'\dontrun{
#' # Construct query, making it easier to understand and modify.
#' day <- "2016-01-02"
#' lon0 <- -66.5
#' lon1 <- -64.0
#' lat0 <- 44
#' lat1 <- 46
#' source <- paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/",
#'                 "jplG1SST.nc?",
#'                 "SST%5B(", day, "T12:00:00Z)",
#'                 "%5D%5B(", lat0, "):(", lat1, ")",
#'                 "%5D%5B(", lon0, "):(", lon1, ")",
#'                 "%5D", sep="")
#' if (!length(list.files(pattern="^a.nc$")))
#'     download.file(source, "a.nc")
#' d <- read.g1sst("a.nc")
#' plot(d, "SST", col=oceColorsTemperature)
#' if (requireNamespace("ocedata", quietly=TRUE)) {
#'     data(coastlineWorldFine, package="ocedata")
#'     lines(coastlineWorldFine[['longitude']],coastlineWorldFine[['latitude']])
#' }
#'}
#'
#' @author Dan Kelley
#'
#' @references
#' 1. ERDDAP Portal `https://coastwatch.pfeg.noaa.gov/erddap/`
#' 2. JPO OurOcean Portal `https://ourocean.jpl.nasa.gov/SST/`
#' (link worked in 2016 but was seen to fail 2017 Feb 2).
#'
#' @family things related to g1sst data
read.g1sst <- function(file, encoding=NA)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop('must install.packages("ncdf4") to read g1sst data')
    if (!is.character(file))
        stop("file must be a character string")
    f <- ncdf4::nc_open(file)
    res <- new("g1sst", file=file)
    ## Change the 1-col ncdf4 output to a vector
    res@metadata$longitude <- as.vector(ncdf4::ncvar_get(f, "longitude"))
    res@metadata$latitude <- as.vector(ncdf4::ncvar_get(f, "latitude"))
    res@metadata$time <- numberAsPOSIXct(ncdf4::ncvar_get(f, "time"))
    res@metadata$units$SST <- list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$satellite <- "g1sst"
    res@data$SST <- ncdf4::ncvar_get(f, "SST")
    res
}
