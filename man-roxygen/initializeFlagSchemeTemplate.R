#' @title Establish a data-quality scheme for a <%=class%> object
#'
#' @description
#' This function stores adds an item called \code{flagScheme} to the
#' \code{metadata} slot. The purpose is both to document a flag scheme
#' and to make it so that \code{\link{initializeFlags}}
#' and \code{\link{setFlags}} can specify flags by
#' name, as opposed to number. A generic function, it is
#' specialized for some classes via interpretation of the
#' \code{scheme} argument (see \dQuote{Details}, for those
#' object classes that have such specializations).
#'
#' @details
#' <%=details%>
#'
#' @section Caution:
#' This function was added in early May, 2018, and is likely to undergo
#' changes until the mid-summer of that year.  Use with caution.
#'
#' @param object An oce object.
#'
#' @param scheme A list describing the flag scheme, e.g \code{list(good=1, bad=2)}
#' might be used for a hypothetical class. Some classes also permit \code{scheme}
#' to be a character string that names a built-in scheme; see \dQuote{Details}.
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with the \code{metadata} slot set to the indicated
#' \code{scheme} (expanded to a list, if the argument was a recognized
#' character string).
#'
#' @family functions relating to data-quality flags

