#' @param file a connection or a character string giving the name of the file
#' to load.  (For \code{read.adp.sontek.serial}, this is generally a list of
#' files, which will be concatenated.
#'
#' @param from indication of the first profile to read.  This can be an
#' integer, the sequence number of the first profile to read, or a POSIXt time
#' before which profiles should be skipped, or a character string that converts
#' to a POSIXt time (assuming UTC timezone).  See \dQuote{Examples}, and make
#' careful note of the use of the \code{tz} argument.
#'
#' @param to if supplied, an indication of the last profile to read, in a
#' format as described for \code{from}.  If not supplied, the whole file will
#' be read.
#'
#' @param by an indication of the stride length to use while walking through
#' the file.  If this is an integer, then \code{by-1} profiles are skipped
#' between each pair of profiles that is read; use \code{by=1} (the
#' default) to read all the data.  If this is a string
#' representing a time interval, in colon-separated format (MM:SS), then this
#' interval is divided by the sampling interval, to infer a stride length.
#' Note that this \code{by} parameter only makes sense if the data are
#' sampled at a uniform rate; for burst-mode data, the best plan
#' is to read all the data.
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
