#' This function handles the following object classes directly:
#' [adp-class],
#' [adv-class],
#' [argo-class] (selection by profile),
#' [coastline-class],
#' [ctd-class],
#' [echosounder-class] (selection by ping),
#' [section-class] (selection by station)
#' and
#' [topo-class] (selection by longitude and latitude).
#' It does not handle
#' [amsr-class] or
#' [landsat-class]
#' yet, instead issuing a warning and returning `x` in those cases.
#' For all other classes, it calls [<%=headOrTail%>()]
#' with `n` as provided, for each item in the `data`
#' slot, issuing a warning if that item is not a vector; the
#' author is quite aware that this may not work well for all classes.
#' The plan is to handle all appropriate classes by July 2018.
#' Please contact the author if there is a class you need handled
#' before that date.

