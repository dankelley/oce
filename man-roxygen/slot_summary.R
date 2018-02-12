#' @slot data As with all \code{oce} objects, the \code{data} slot is a
#' \code{\link{list}} containing the main data for the object.
#'
#' @slot metadata As with all \code{oce} objects, the \code{metadata} slot is
#' a \code{\link{list}} containing information about the \code{data} or about the
#' object itself.  An example of the former might be the location at which a
#' measurement was made, and of the latter might be the name of the experiment in
#' which the data were collected.
#'
#' @slot processingLog As with all \code{oce} objects, the \code{processingLog}
#' slot is a \code{\link{list}} with entries describing the creation and evolution
#' of the object. The contents are updated by various \code{oce} functions to
#' keep a record of processing steps.  Object summaries and
#' \code{\link{processingLogShow}} both display the log.

