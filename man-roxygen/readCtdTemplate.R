#' @return An object of \code{\link{ctd-class}}. The details of the contents
#' depend on the source file. The \code{metadata} slot is particularly 
#' variable across data formats, because the meta-information provided
#' in those formats varies widely.
#' 
#' @param file A connection or a character string giving the name of the file to
#' load.  For \code{read.ctd.sbe()} and \code{read.ctd.woce()}, this may be a
#' wildcard (e.g. \code{"*.cnv"} or \code{"*.csv"}) in which case the return
#' value is a vector containing CTD objects created by reading the files from
#' \code{\link{list.files}} with \code{pattern} set to the specified wildcard
#' pattern.
#' 
#' @param debug An integer specifying whether debugging information is
#' to be printed during the processing. This is a general parameter that
#' is used by many \code{oce} functions. Generally, setting \code{debug=0}
#' turns off the printing, while higher values suggest that more information
#' be printed.
#' 
#' @param columns Ignored, and slated for removal in April, 2016;
#' see \link{oce-defunct}.
#' 
#' @param station Optional character string containing an identifying name or
#' number for the station. This can be useful if the routine cannot determine the
#' name automatically, or if another name is preferred.
#' 
#' @param missing.value Optional missing-value flag; data matching this value will
#' be set to \code{NA} upon reading.
#' 
#' @param monitor Boolean, set to \code{TRUE} to provide an indication of
#' progress.  This is useful if \code{filename} is a wildcard.
#' 
#' @param processingLog If provided, the action item to be stored in the log.
#' This is typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.
#' 
#' @param ... additional arguments, passed to called routines.
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{ctd} data
