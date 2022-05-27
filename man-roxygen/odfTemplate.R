#' @section Caution:
#' 
#' Lacking detailed documentation of the ODF file format, the [read.odf()] and
#' [read.ctd.odf()] functions were crafted based on inspection of data files, and
#' so some guesses had to be made.
#' 
#' The \code{PARAMETER_HEADER} chunks describing quality-control flags are
#' a case in point.  These contain \code{NAME} components that
#' refer to other \code{PARAMETER_HEADER} chunks that hold measured data.  However,
#' those references are not always matched well with the data names, and
#' even if they do match, the cross-reference syntax used by
#' the Bedford Institute of Oceanography differs from that used by
#' l’Institut Maurice-Lamontagne. To simplify coding, it was assumed that
#' each quality-control sequence applies to the data sequence
#' immediately preceding it.  (This assumption is made in other
#' analysis systems.)
#' 
#' It is also prudent to pay attention to the units decoding,
#' which [read.odf()] handles by calling [unitFromString()].
#' Be on the lookout for incorrect temperature scales, which
#' are sometimes reported with nonstandard strings in ODF files.
#' Also, note that you may see warnings about conductivity ratios,
#' which some ODF files incorrectly suggest have dimensions.

