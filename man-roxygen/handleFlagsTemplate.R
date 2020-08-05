#' @description Data-quality flags are stored in the `metadata`
#' slot of [oce-class] objects in a
#' [list] named `flags`.
#' The present function (a generic that has specialized versions
#' for various data classes) provides a way to
#' manipulate the contents of the `data` slot, based on
#' such data-quality flags. For example, a common operation is to replace
#' erroneous data with `NA`.
#'
#' If the `flags` within `object`'s `metadata` slot is empty,
#' then `object` is returned, unaltered.
#' Otherwise, `handleFlags` examines `object@metadata$flags`
#' in the context of the `flags` argument, and then
#' carries out actions that are specified by the `actions` argument.
#' By default, this sets the returned `data` entries to `NA`,
#' wherever the corresponding `metadata$flag` values
#' signal unreliable data. To maintain a hint as to why
#' `data` were changed, `metadata$flags` in the
#' returned value is a direct copy of the corresponding
#' entry in `object`.
#'
#' @param flags A [list] specifying flag values upon which
#' actions will be taken. This can take two forms.
#'
#' * In the first form, the
#' list has named elements each containing a vector of integers. For example,
#' salinities flagged with values of 1 or 3:9 would be specified
#' by `flags=list(salinity=c(1,3:9))`. Several data items can be specified,
#' e.g. `flags=list(salinity=c(1,3:9), temperature=c(1,3:9))` indicates
#' that the actions are to take place for both salinity and temperature.
#'
#' * In the second form, `flags` is a list holding a single *unnamed* vector, and
#' this means to apply the actions to *all* the data entries.  For example,
#' `flags=list(c(1,3:9))` means to apply not just to salinity and temperature,
#' but to everything within the `data` slot.
#'
#' If `flags` is not provided, then [defaultFlags()] is called, to try to
#' determine a reasonable default.
#'
#' @param actions an optional [list] that contains items with
#' names that match those in the `flags` argument.  If `actions`
#' is not supplied, the default will be to set all values identified by
#' `flags` to `NA`; this can also be specified by
#' specifying `actions=list("NA")`. It is also possible to specify
#' functions that calculate replacement values. These are provided
#' with `object` as the single argument, and must return a
#' replacement for the data item in question.
#' See \dQuote{Details} for the default that is used if `actions` is not supplied.
#'
#' @param where an optional character value that permits the function to work with
#' objects that store flags in e.g. `object@metadata$flags$where`
#' instead of in `object@metadata$flags`, and data within
#' `object@data$where` instead of within `object@data`. The
#' default value of `NULL` means to look withing `object@metadata`
#' itself, and this is the default within `oce`.  (The purpose of `where`
#' is to make `oce` extensible by other packages, which may choose to store
#' data two levels deep in the `data` slot.)
#'
#' @param debug An optional integer specifying the degree of debugging, with
#' value 0 meaning to skip debugging and 1 or higher meaning to print some
#' information about the arguments and the data. It is usually a good idea to set
#' this to 1 for initial work with a dataset, to see which flags are being
#' handled for each data item. If not supplied, this defaults to the value of
#' [`getOption`]`("oceDebug")`.
#'
#' @family functions relating to data-quality flags

