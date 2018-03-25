#' This function handles the following object classes directly:
#' \code{\link{adp-class}},
#' \code{\link{adv-class}},
#' \code{\link{argo-class}} (selection by profile),
#' \code{\link{ctd-class}},
#' \code{\link{coastline-class}},
#' \code{\link{section-class}} (selection by station)
#' and
#' \code{\link{topo-class}} (selection by longitude and latitude).
#' It does not handle
#' \code{\link{amsr-class}} or
#' \code{\link{landsat-class}}
#' at all, and simply returns \code{x} in those cases.
#' For all other classes, it calls \code{\link{<%=headOrTail%>}}
#' with \code{n} as provided, for each item in the \code{data}
#' slot, issuing a warning if that item is not a vector.
#'
#' The plan is that most classes should be covered by July 2018.
#' Please contact the author if there is a class you need handled
#' before that date.

