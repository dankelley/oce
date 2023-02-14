#' @details
#'
#' The returned value is a copy of `object` that has been modified in 4 ways.
#' (1) the horizontal components of velocity are rotated clockwise by
#' `declination` degrees.  (2) If the object holds heading values, then
#' `declination` is added to them. (3) The `north` item in the `metadata` slot
#' is set to `"geographic"`, and a warning is issued if this was also the value
#' in `object`.  (4) The `declination` item in the `metadata` slot is set to
#' the value supplied to this function.
