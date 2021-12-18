#' @param i Character string indicating the name of an item to extract.
#'
#' @param j Optional additional information on the `i` item.
#'
#' @param ... Optional additional information (ignored).
#'
#' @description
#' \code{[[} provides a way to look up information
#' within `oce` objects, without knowing
#' the details of how that information is stored
#' within the object. For some object classes,
#' `[[` can also return quantities are not actually stored
#' within the object, but that can be computed from
#' such information.
#'
#' @details
#'
#' A two-step process is used to try to find the
#' requested information. First, a class-specific
#' function is used, as described in
#' \dQuote{Details of the Specialized Method}.
#' If this method cannot locate the information,
#' then a general method is used, as described in
#' \dQuote{Details of the General Method}.
#' If neither of these methods finds the requested
#' information, then `NULL` is returned.
#'
#' @section Details of the General Method:
#'
#' If the specialized method
#' produces no matches, the following generalized
#' method is applied. As with the specialized method, the procedure hinges
#' first on the values of `i` and, optionally, `j`. The work
#' proceeds in steps, by testing a sequence of possible conditions
#' in sequence.
#'
#' 1. A check is made as to whether `i` names one of the standard
#' `oce` slots. If so, `[[` returns the slot contents of that slot.
#' Thus, `x[["metadata"]]` will retrieve the `metadata` slot,
#' while `x[["data"]]` and `x[["processingLog"]]` return
#' those slots.
#'
#' 2. If `i` is a string ending in the
#' `"Unit"`, then the characters preceding that string
#' are taken to be the name of an item in the data object, and a list
#' containing the unit is returned (or `NULL` if
#' there is no such unit). This list consists of an item
#' named `unit`, which is an [expression()], and
#' an item named `scale`, which is a string describing the
#' measurement scale.  If the string ends in `" unit"`, e.g.
#' `x[["temperature unit"]]` (note the space),
#' then just the expression is returned, and if it ends in
#' `" scale"`, then just the scale is returned.
#'
#' 3. If `i` is a string ending in `"Flag"`, then the corresponding
#' data-quality flag is returned (or `NULL` if there is no such flag).
#'
#' 4. If the object holds hydrographic information (pressure,
#' salinity, temperature, longitude and latitude) then another
#' set of possibilities arises.  If `i` is `"sigmaTheta"`, then the value of
#' [swSigmaTheta()] is called with \code{x} as the sole
#' argument, and the results are returned. Similarly,
#' [swSigma0()] is used if `i="sigma0"`, and
#' [swSpice()] is used if `i="spice"`. Of course, these
#' actions only make sense for objects that contain
#' the relevant items within their `data` slot.
#'
#' 5. After these possibilities are eliminated,
#' the action depends on whether `j` has been provided.
#' If `j` is not provided, or is the string `""`,
#' then `i` is sought
#' in the `metadata` slot, and then in the `data` slot,
#' returning whichever is found first.  In other words, if `j`
#' is not provided, the `metadata` slot takes preference over
#' the `data` slot. However, if `j` is provided, then
#' it must be either the string `"metadata"` or `"data"`,
#' and it directs where to look.
#'
#' If none of the above-listed conditions holds, then `NULL` is returned.
#'
#' @family functions that extract parts of oce objects

