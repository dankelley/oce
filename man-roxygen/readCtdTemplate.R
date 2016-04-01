#' @description The \code{read.ctd} family of functions can read a variety of CTD
#' data formats. See \dQuote{Details}.
#'
#' @return An object of \code{\link{ctd-class}}. The details of the contents
#' depend on the source file. The \code{metadata} slot is particularly 
#' variable across data formats, because the meta-information provided
#' in those formats varies widely.
#' 
#' @param file A connection or a character string giving the name of the file to
#' load.  For \code{read.ctd.sbe()} and \code{read.ctd.woce()}, this may be a
#' wildcard (e.g.  \code{"*.cnv"} or \code{"*.csv"}) in which case the return
#' value is a vector containing CTD objects created by reading the files from
#' \code{\link{list.files}} with \code{pattern} set to the specified wildcard
#' pattern.
#' 
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#' 
#' @param columns If \code{NULL}, then \code{read.ctd} tries to infer column names
#' from the header.  For SBE files only, the \code{column} argument can control
#' the column selection.  It is a list that names data types and the columns
#' containing them, starting at 1.  The list must include \code{"pressure"},
#' \code{"temperature"} and \code{"salinity"}, with any other values being ignored
#' (in this version of the function).
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
#' @family functions that handle ctd data
#' 
#' @author Dan Kelley
#' 
