#' @return a [ctd-class] object. The details of the contents
#' depend on the source file. The `metadata` slot is particularly
#' variable across data formats, because the meta-information provided
#' in those formats varies so widely.
#'
#' @param file a connection or a character string giving the name of the file to
#' load.  For [read.ctd.sbe()] and [read.ctd.woce()], this may be a
#' wildcard (e.g. `"*.cnv"` or `"*.csv"`) in which case the return
#' value is a vector containing CTD objects created by reading the files from
#' [list.files()] with `pattern` set to the specified wildcard
#' pattern.
#'
#' @param debug an integer specifying whether debugging information is
#' to be printed during the processing. This is a general parameter that
#' is used by many `oce` functions. Generally, setting `debug=0`
#' turns off the printing, while higher values suggest that more information
#' be printed.
#'
#' @param columns an optional [list] that can be used to convert unrecognized
#' data names to resultant variable names.  This is used only by
#' [read.ctd.sbe()] and [read.ctd.odf()]. For example,
#' if a data file named salinity as `"SAL"`, then using
#' ```
#' d <- read.ctd(f, columns=list(
#'     salinity=list(name="SAL",
#'                   unit=list(unit=expression(),
#'                   scale="PSS-78"))))
#' ```
#' would assign the `"SAL"` column to the `salinity` entry in the data
#' slot of the CTD object returned by the `read.*` function.
#'
#' @param station optional character string containing an identifying name or
#' number for the station. This can be useful if the routine cannot determine the
#' name automatically, or if another name is preferred.
#'
#' @param missingValue optional missing-value flag; data matching this value will
#' be set to `NA` upon reading. If this is provided, then it overrules any
#' missing-value flag found in the data. For Seabird (`.cnv`) files, there is
#' usually no need to set `missingValue`, because it can be inferred from the
#' header (typically as -9.990e-29). Set `missingValue=NULL` to turn off
#' missing-value detection, even in `.cnv` files that contain missing-value
#' codes in their headers. If `missingValue` is not specified,
#' then an attempt is made to infer such a value from the data, by testing
#' whether salinity and/or temperature has a minimum that is under -8 in value;
#' this should catch common values in files, without false positives. A warning
#' will be issued in this case, and a note inserted in the processing log of
#' the return value.
#'
#' @param deploymentType character string indicating the type of deployment. Use
#' `"unknown"` if this is not known, `"profile"` for a profile (in
#' which the data were acquired during a downcast, while the device was lowered
#' into the water column, perhaps also including an upcast; `"moored"` if
#' the device is installed on a fixed mooring, `"thermosalinograph"` (or
#' `"tsg"`) if the device is mounted on a moving vessel, to record
#' near-surface properties, or `"towyo"` if the device is repeatedly
#' lowered and raised.
#'
#' @param monitor boolean, set to `TRUE` to provide an indication of
#' progress.  This is useful if `filename` is a wildcard.
#'
#' @param processingLog if provided, the action item to be stored in the log.
#' This is typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.
#'
#' @param ... additional arguments, passed to called routines.
#'
#' @family things related to ctd data
#'
