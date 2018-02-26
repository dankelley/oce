#' NOTE: this is a preliminary function, subject to change.
#' For example, it is an S3 generic, although for uniformity
#' with the rest of oce, it ought to be an S4 generic.
#'
#' This function handles the following object classes directly:
#' \code{\link{adp-class}},
#' \code{\link{adv-class}},
#' \code{\link{ctd-class}},
#' \code{\link{section-class}}.
#' and
#' \code{\link{topo-class}}. It does not handle
#' \code{\link{amsr-class}} or
#' \code{\link{landsat-class}}
#' at all, and simply returns \code{x} in those cases.
#' For all other classes, it simply calls \code{\link{<%=headOrTail%>}}
#' with \code{n} as provided, for each item in the \code{data}
#' slot, issuing a warning if that item is not a vector.
#'
#' Please contact the author if there is a class you need handled.

