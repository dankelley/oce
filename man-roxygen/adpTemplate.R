#' @param file a connection or a character string giving the name of the file
#' to load.  (For `read.adp.sontek.serial`, this is generally a list of
#' files, which will be concatenated.)
#'
#' @param from indication of the first profile to read.  This can be an
#' integer, the sequence number of the first profile to read, or a POSIXt time
#' before which profiles should be skipped, or a character string that converts
#' to a POSIXt time (assuming UTC timezone).  See \dQuote{Examples}, and make
#' careful note of the use of the `tz` argument. If `from` is not
#' supplied, it defaults to 1.
#'
#' @param to an optional indication of the last profile to read, in a
#' format as described for `from`.  As a special case, `to=0` means
#' to read the file to the end.  If `to` is not supplied, then it defaults
#' to 0.
#'
#' @param by an optional indication of the stride length to use while walking through
#' the file.  If this is an integer, then `by-1` profiles are skipped
#' between each pair of profiles that is read, e.g. the default `by=1`
#' means to read all the data.  (For RDI files *only*, there are some
#' extra features to avoid running out of memory; see \dQuote{Memory considerations}.)
#'
#' @param tz character string indicating time zone to be assumed in the data.
#'
#' @param longitude optional signed number indicating the longitude in degrees
#' East.
#'
#' @param latitude optional signed number indicating the latitude in degrees
#' North.
#'
#' @param monitor boolean, set to `TRUE` to provide an indication of progress
#' in reading the file, either by printing a dot for each profile or by writing
#' a textual progress bar with [txtProgressBar()].
#'
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots optional additional arguments that some (but not all)
#' `read.adp.*()` functions pass to lower-level functions.
#'
#' @return An [adp-class] object.
#' The contents of that object make sense for the particular instrument
#' type under study, e.g. if the data file contains
#' NMEA strings, then navigational data will be stored in an item
#' called `nmea` in the `data` slot).

