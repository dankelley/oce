#' @section A note about flags:
#' Data-quality flags contained within the original object are ignored by this
#' function, and the returned value contains no such flags.  This is because such
#' flags represent an assessment of the original data, not of quantities derived
#' from those data.  This function produces a warning to this effect. The
#' recommended practice is to use \code{\link{handleFlags}} or some other means to
#' deal with flags before calling the present function.
