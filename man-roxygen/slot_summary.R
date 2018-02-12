#' @slot data In this and all \code{oce} objects, the \code{data} slot is a
#' \code{\link{list}} containing the main data for the object.
#'
#' @slot metadata In this and all \code{oce} objects, the \code{metadata} slot is
#' a \code{\link{list}} containing information about the \code{data} or about the
#' object itself.  An example of the former might be the location at which a
#' measurement was made, and of the latter might be the name of the experiment in
#' which the data were collected.
#'
#' @slot processingLog In this and all \code{oce} objects, the \code{processingLog}
#' slot is a \code{\link{list}} with entries describing the creation and evolution
#' of the object. This slot is updated by various \code{oce} functions.  The
#' contents are listed in object summaries and with
#' \code{\link{processingLogShow}}.

