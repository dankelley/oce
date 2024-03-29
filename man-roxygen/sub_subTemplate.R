# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
 
#' @param i character value indicating the name of an item to extract.
#'
#' @param j optional additional information on the `i` item.
#'
#' @param ... ignored.
#'
#' @description
#'
#' Generally, the \code{[[} method lets users extract information from `oce`
#' objects, without having to know the details of the internal storage.  For
#' many `oce` sub-classes, `[[` can also return quantities that are computed
#' from the object's contents.
#'
#' @details
#'
#' A two-step process is used to try to find the requested information. First, a
#' class-specific function is used (see \dQuote{Details of the Specialized
#' Method}).  If this yields nothing, then a general method is used (see
#' \dQuote{Details of the General Method}).  If both methods fail, then `[[`
#' returns NULL.
#'
#' Some understanding of the subclass is required to know what can be retrieved
#' with `[[`. When dealing with an unfamiliar subclass, it can be useful to
#' first use `x[["?"]]` to get a listing of the retrievable items. See
#' \dQuote{Details of the Specialized Method} for more information.
#'
## NOTE: Roxygen2 puts this after the Specialized method, which is where I want
## it.  I don't understand why, though and therefore will be on the lookout for
## changes that might occur in either R itself, or in Roxygen2.
#'
#' @section Details of the General Method:
#'
#' Note: the text of this section is identical for all `oce` subclasses, and so
#' some of what you read here may not be relevant to the class being described
#' in this help page.
#'
#' If the specialized method produces no matches, the following generalized
#' method is applied. As with the specialized method, the procedure hinges first
#' on the values of `i` and, optionally, `j`. The work proceeds in steps, by
#' testing a sequence of possible conditions in sequence.
#'
#' 1. A check is made as to whether `i` names one of the standard `oce` slots.
#' If so, `[[` returns the slot contents of that slot.  Thus, `x[["metadata"]]`
#' will retrieve the `metadata` slot, while `x[["data"]]` and
#' `x[["processingLog"]]` return those slots.
#'
#' 2. If `i` is a string ending in the `"Unit"`, then the characters preceding
#' that string are taken to be the name of an item in the data object, and a
#' list containing the unit is returned (or `NULL` if there is no such unit).
#' This list consists of an item named `unit`, which is an [expression()], and
#' an item named `scale`, which is a string describing the measurement scale.
#' If the string ends in `" unit"`, e.g.  `x[["temperature unit"]]` (note the
#' space), then just the expression is returned, and if it ends in `" scale"`,
#' then just the scale is returned.
#'
#' 3. If `i` is a string ending in `"Flag"`, then the corresponding data-quality
#' flag is returned (or `NULL` if there is no such flag).
#'
#' 4. If the object holds hydrographic information (pressure, salinity,
#' temperature, longitude and latitude) then another set of possibilities
#' arises.  If `i` is `"sigmaTheta"`, then the value of [swSigmaTheta()] is
#' called with \code{x} as the sole argument, and the results are returned.
#' Similarly, [swSigma0()] is used if `i="sigma0"`, and [swSpice()] is used if
#' `i="spice"`. Of course, these actions only make sense for objects that
#' contain the relevant items within their `data` slot.
#'
#' 5. After these possibilities are eliminated, the action depends on whether
#' `j` has been provided.  If `j` is not provided, or is the string `""`, then
#' `i` is sought in the `metadata` slot, and then in the `data` slot, returning
#' whichever is found first.  In other words, if `j` is not provided, the
#' `metadata` slot takes preference over the `data` slot. However, if `j` is
#' provided, then it must be either the string `"metadata"` or `"data"`, and it
#' directs where to look.
#'
#' 6. If none of the above-listed conditions holds, then `NULL` is returned.
#'
#' @family functions that extract parts of oce objects

