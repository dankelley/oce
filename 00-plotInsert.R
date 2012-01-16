## Thoughts on this.
##  1. possibly should have (generally) an oce stack, with margins etc, so could maybe do
##             oceStackPush(mar=...) 
##     or
##             oceStackPush(plotInset(...)) # return value is things to be changed, like mai
##     after which, when plotting is done, we would do
##             oceStackPop()
library(oce)
source('~/src/R-kelley/oce/R/ctd.R')
data(ctd)
plot(ctd, which='TS')
plotInset <- function(xleft, ybottom, xright, ytop, expr,
                      bg="pink", mar=c(3.5, 3.5, 1, 1),
                      debug=getOption("oceDebug"))
{
    if (debug) {
        cat("at plotInset entry, mar=", par('mar'), '\n')
        cat("at plotInset entry, mai=", par('mai'), '\n')
    }
    rect(xleft, ybottom, xright, ytop, col=bg, border=bg)
    mai <- par('mai')                  # bottom left top right
    usr <- par('usr')                  # xmin xmax ymin ymax
    din <- dev.size(units='in')        # width height
    pin <- par('pin')
    if (debug) {
        cat("mai=", mai, "\n")
        cat("usr=", usr, "\n")
        cat("din=", din, "\n")
        cat("pin=", pin, "\n")
    }
    ## FIXME: use din or pin?
    x2in <- function(x) mai[2] + (x-usr[1]) * (din[1]-mai[2]-mai[4]) / (usr[2]-usr[1])
    y2in <- function(y) mai[1] + (y-usr[3]) * (din[2]-mai[1]-mai[3]) / (usr[4]-usr[3])
    nmai <- c(y2in(ybottom), x2in(xleft), din[2]-y2in(ytop), din[1]-x2in(xright))
    if (debug) cat("nmai:", nmai, "\n")
    par(new=TRUE, mai=nmai)
    thismar <- par('mar')
    par(mar=thismar+mar)
    expr
    if (debug) {
        cat("before plot, mar=", par('mar'), '\n')
        cat("before plot, mai=", par('mai'), '\n')
    }
    par(mai=mai, usr=usr)
    if (debug) {
        cat("after plot, mar=", par('mar'), '\n')
        cat("after plot, mai=", par('mai'), '\n')
    }
}
#plotInset(30, 4, 31, 8, expr=plot(ctd, which='temperature', mar=NULL), debug=2) # almost works (mar wrong)
plotInset(30, 4, 31, 8, expr=plot(ctd, which='map', mar=NULL, debug=3), debug=2) # fails (mar very wrong)
#plotInset(29.9, 3.1, 31, 8, expr=plot(1:10, (1:10)^2), debug=2) # works

