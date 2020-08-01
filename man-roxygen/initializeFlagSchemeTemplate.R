## DEVELOPER NOTE: keep in synch with R/AllClass.R and tests/testthat/test_flags.R

#' @title Establish a data-quality scheme for a <%=class%> object
#'
#' @description
#' This function stores add an item named `flagScheme`
#' to the `metadata` slot of an object inheriting from
#' [<%=class%>-class]. This is a list containing two
#' items: `name` and `mapping`, as provided in the
#' function arguments.
#' The purpose is both to document a flag scheme
#' and to make it so that [initializeFlags()],
#' [setFlags()] and [handleFlags()]
#' can specify flags by
#' name, as opposed to number. This is a generic function,
#' that may be specialized to the class of `object`
#' (see \dQuote{Details}).
#'
#' @details
#' The following pre-defined schemes are available (note that the
#' names are simplified from the phrases used in defining
#' documentation):
#'
#' * `name="argo"` defaults `mapping` to
#' OLD (prior to June 10, 2020)
#' ```
#' list(not_assessed=0, passed_all_tests=1, probably_good=2,
#'      probably_bad=3, bad=4, averaged=7,
#'      interpolated=8, missing=9)
#'```
#' NEW (after June 10, 2020)
#'```
#' list(not_assessed=0, passed_all_tests=1, probably_good=2,
#'      probably_bad=3, bad=4, changed=5, not_used_6=6, not_used_7=7,
#'      estimated=8, missing=9)
#' ```
#' See reference 1 for a deeper explanation of the meanings of these codes.
#'
#' * `name="BODC"` defaults `mapping` to
#' ```
#' list(no_quality_control=0, good=1, probably_good=2,
#'      probably_bad=3, bad=4, changed=5,
#'      below_detection=6, in_excess=7, interpolated=8,
#'      missing=9)
#' ```
#' See reference 2 for a deeper explanation of the meanings of these codes,
#' and note that codes `A` and `Q` are not provided in
#' oce.
#'
#' * `name="DFO"` defaults `mapping` to
#' ```
#' list(no_quality_control=0, appears_correct=1, appears_inconsistent=2,
#'      doubtful=3, erroneous=4, changed=5,
#'      qc_by_originator=8, missing=9)
#' ```
#' See reference 3 for a deeper explanation of the meanings of these codes.
#'
#' * `name="WHP bottle"` defaults `mapping` to
#' ```
#' list(no_information=1, no_problems_noted=2, leaking=3,
#'      did_not_trip=4, not_reported=5, discrepency=6,
#'      unknown_problem=7, did_not_trip=8, no_sample=9)
#' ```
#' See reference 4 for a deeper explanation of the meanings of these codes.
#'
#' * `name="WHP CTD"` defaults `mapping` to
#' ```
#' list(not_calibrated=1, acceptable=2, questionable=3,
#'     bad=4, not_reported=5, interpolated=6,
#'     despiked=7, missing=9)
#' ```
#' See reference 4 for a deeper explanation of the meanings of these codes.
#'
#'
#' @section Caution:
#' This function was added in early May, 2018, and is likely to undergo
#' changes until the autumn of that year.  Use with caution.
#'
#' @param object An oce object.
#'
#' @param name a character value naming the scheme. If this refers
#' to a pre-defined scheme, then `mapping` must not be provided,
#' because doing so would contradict the pre-defined scheme, defeating
#' its purpose of providing concreteness and clarity.
#'
#' @param mapping a list of named items describing the mapping from
#' flag meaning to flag numerical value, e.g `list(good=1, bad=2)`
#' might be used for a hypothetical class.
#'
#' @param default an integer vector of flag values that are not considered
#' to be good. If this is not provided, but if `name` is `"argo"`,
#' `"BODC"`,
#' `"DFO"`,
#' `"WHP bottle"`, or
#' `"WHP CTD"`, then a conservative value will be set automatically,
#' equal to the list of flag values that designate bad or questionable data.
#' For example, for `name="WHP CTD"`, the setting will be
#' `c(1,3,4,5,6,7,9)`, leaving only value `2`, which corresponds
#' with "acceptable" in the notation used for that flag scheme.
#'
#' @param update a logical value indicating whether the scheme provided is
#' to update an existing scheme.  The default value, `FALSE`, prevents such
#' an attempt to alter an existing flag scheme, if one is already embedded
#' in `object`.
#'
#' @param debug an integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with the `metadata` slot containing `flagScheme`.
#'
#' @references
#' 1. The codes for `"argo"` are derived from information in Table 4.1
#' of Wong, Annie, Robert Keeley, Thierry Carval, and Argo Data Management Team
#' (8 January 2020), "Argo Quality Control Manual for CTD and Trajectory Data, Version 3.3,"
#' available at \url{https://archimer.ifremer.fr/doc/00228/33951/}
#' as of June 2020.
#'
#' 2. The codes for `"BODC"` are defined at
#' http://seadatanet.maris2.nl/v_bodc_vocab_v2/browse.asp?order=conceptid&formname=search&screen=0&lib=l20
#'
#' 3. The codes for `"DFO"` are defined at
#' http://www.dfo-mpo.gc.ca/science/data-donnees/code/list/014-eng.html
#'
#' 4. The codes for `"WHP CTD"` and `"WHP bottle"` are defined at
#' https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm
#'
#' @family functions relating to data-quality flags
#' @family things related to <%=class%> data

