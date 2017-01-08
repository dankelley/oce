#' @param file a connection or a character string giving the name of the file
#' to load.  (For \code{read.adp.sontek.serial}, this is generally a list of
#' files, which will be concatenated.)
#'
#' @param from indication of the first profile to read.  This can be an
#' integer, the sequence number of the first profile to read, or a POSIXt time
#' before which profiles should be skipped, or a character string that converts
#' to a POSIXt time (assuming UTC timezone).  See \dQuote{Examples}, and make
#' careful note of the use of the \code{tz} argument. If \code{from} is not
#' supplied, it defaults to 1.
#'
#' @param to an optional indication of the last profile to read, in a
#' format as described for \code{from}.  As a special case, \code{to=0} means
#' to read the file to the end.  If \code{to} is not supplied, then it defaults
#' to 0.
#'
#' @param by an optional indication of the stride length to use while walking through
#' the file.  If this is an integer, then \code{by-1} profiles are skipped
#' between each pair of profiles that is read, e.g. the default \code{by=1}
#' means to read all the data.  For RDI files, there are some other
#' special features. If \code{by} is a string  representing a time interval,
#' then \code{\link{ctimeToSeconds}} is used to infer a stride interval in
#' seconds (e.g. \code{"30:00"} means a half-hour interval). For RDI files,
#' if \code{by} is not given, a default will be chosen based on the file
#' size: with file size under 1Mb, this will be \code{by=1}, meaning
#' that no profiles are to be skipped, while for larger files, \code{by}
#' will be set to progressively larger values.
#'
#' @param tz character string indicating time zone to be assumed in the data.
#'
#' @param longitude optional signed number indicating the longitude in degrees
#' East.
#'
#' @param latitude optional signed number indicating the latitude in degrees
#' North.
#'
#' @param monitor boolean, set to \code{TRUE} to provide an indication (with
#' numbers and dots) of every profile read.
#'
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots additional arguments, passed to called routines.
#'
#' @return An \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
