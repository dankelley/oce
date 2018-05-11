#' @title Establish a data-quality scheme for a <%=class%> object
#'
#' @description
#' This function stores add an item named \code{flagScheme}
#' to the \code{metadata} slot. This is a list containing two
#' items: \code{name} and \code{mapping}, as provided in the
#' function arguments.
#' The purpose is both to document a flag scheme
#' and to make it so that \code{\link{initializeFlags}},
#' \code{\link{setFlags}} and \code{\link{handleFlags}}
#' can specify flags by
#' name, as opposed to number. This is a generic function,
#' that may be specialized to the class of \code{object}
#' (see \dQuote{Details}.
#'
#' @details
#' \strong{General:}
#' The following pre-defined schemes are available (note that the
#' names are simplified from the phrases used in defining
#' documentation):
#' \itemize{
#' \item \code{name="WHP CTD"} defaults \code{mapping} to
#' \code{list(not_calibrated=1, acceptable=2, questionable=3,
#'     bad=4, not_reported=5, interpolated=6,
#'     despiked=7, missing=9)}
#' \item \code{name="WHP bottle"} defaults \code{mapping} to
#' \code{list(no_information=1, no_problems_noted=2, leaking=3,
#'     did_not_trip=4, not_reported=5, discrepency=6,
#'     unknown_problem=7, did_not_trip=8, missing=9)}
#' \item \code{name="argo"} defaults \code{mapping} to
#' \code{list(not_assessed=0, passed_all_tests=1, probably_good=2,
#'     probably_bad=3, bad=4, averaged=7,
#'     interpolated=8, missing=9)}
#'}
#'
#' \strong{Specific to the <%=class%> objects:}
#' <%=details%>
#'
#' @section Caution:
#' This function was added in early May, 2018, and is likely to undergo
#' changes until the mid-summer of that year.  Use with caution.
#'
#' @param object An oce object.
#'
#' @param name Character value naming the scheme. If this refers
#' to a pre-defined scheme, then \code{mapping} must not be provided.
#'
#' @param mapping A list of named items describing the mapping from
#' flag meaning to flag numerical value, e.g \code{list(good=1, bad=2)}
#' might be used for a hypothetical class.
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with the \code{metadata} slot containing \code{flagScheme}.
#'
#' @references
#' 1. The codes for \code{"WHP CTD"} and \code{"WHP bottle"} are defined at
#' \url{https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm}
#'
#' 2. The codes for \code{"Argo"} are defined at
#' \url{http://www.oceannetworks.ca/data-tools/data-quality}
#'
#' @family functions relating to data-quality flags

