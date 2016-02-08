library(oce)
set.seed(441)
z <- rnorm(20)

## Part 1: test zlim in various conditions.  The docs explain those conditions,
## which are detailed, so there is little point in restating them here.  If
## the present tests are correct, those conditions are defined by lack of
## reported errors from the stopifnot() tests.
##
## There are a *lot* of possible tests.  Either z is given or not.  That's 2
## tests.  Either zlim is given or not.  Now up to 4 tests.  Either breaks is
## given or not, now up to 8 tests.  Ditto name for 16 tests.  Ditto x0 for 32
## tests.  Of course some cases do not apply, e.g. it's an error to give both
## breaks and name.  Still, we have many possible tests, and only a subset
## are here.
##
## My plan is to add new tests here, as issues get reported and fixed. That
## way, the issues should not come back to bit us later, which they have done
## with colormap(), time and time again.

## 1. z alone or with zlim
cm <- colormap(z=z)
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, rangeExtended(z)))
stopifnot(!any(is.na(cm$zcol)))
cm <- colormap(z=z, zlim=c(-10,10))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, c(-10,10)))
stopifnot(!any(is.na(cm$zcol)))

## 2. breaks alone (illegal to also give zlim)
cm <- colormap(breaks=seq(0, 3, 0.1))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, range(c(0, 3))))

## 3. name alone or with zlim
cm <- colormap(name="gmt_globe")
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, range(c(-10000, 10000))))
cm <- colormap(name="gmt_globe", zlim=c(-1,1))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, c(-1, 1)))

## 4. breaks plus name (latter ignored since former given)
cm <- colormap(breaks=0:5, name="gmt_globe")
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, range(c(0, 5))))

## 5. case D: (x0,col0,x1,col1) alone or with zlim
## without blending
cm <- colormap(x0=c(0,1), col0=c('red', 'blue'), x1=c(0.5, 1.5), col1=c("pink", "green"))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, range(c(0, 1.5))))
stopifnot(all.equal(cm$breaks, c(0.0, 1.0, 1.5)))
stopifnot(all.equal(cm$col, c("#FF0000", "#0000FF")))
## with blending
cm <- colormap(x0=c(0,1), col0=c('red', 'blue'), x1=c(0.5, 1.5), col1=c("pink", "green"),blend=0.5)
stopifnot(all.equal(cm$breaks, c(0.0, 1.0, 1.5)))
stopifnot(all.equal(cm$col, c("#FF6065", "#007F7F")))
## with zlim, which should override the default inferred from x0 and x1
cm <- colormap(zlim=c(-10,10), x0=c(0,1), col0=c('red', 'blue'), x1=c(0.5, 1.5), col1=c("pink", "green"))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, c(-10, 10)))

## 6. z plus breaks
cm <- colormap(z=z, breaks=seq(0, 3, 0.1))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, range(c(0, 3))))

## 7. z plus name, alone or with zlim
cm <- colormap(z=z, name="gmt_globe")
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, range(c(-10000, 10000))))
stopifnot(!any(is.na(cm$zcol)))
cm <- colormap(z=z, name="gmt_globe", zlim=c(-1,1))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, c(-1, 1)))
stopifnot(!any(is.na(cm$zcol)))

## 8. z plus (x0,col0,x1,col1) alone [z wins] or with zlim [zlim wins]
cm <- colormap(z=z, x0=c(0,1), col0=c('red', 'blue'), x1=c(0.5, 1.5), col1=c("pink", "green"))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, c(0, 1.5)))
stopifnot(!any(is.na(cm$zcol)))
## zlim in next call should not be ignored

cm <- colormap(breaks=c(-1e4, -1e2, 0, 1e2, 1e4), col=c("purple", "blue", "white", "tan"))
cm <- colormap(z, zlim=c(-10,10), x0=c(0,1), col0=c('red', 'blue'), x1=c(0.5, 1.5), col1=c("pink", "green"))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, c(-10, 10)))
stopifnot(!any(is.na(cm$zcol)))
stopifnot(!any(is.na(cm$zcol)))

## 9. test some actual colours, based on issue 449b.R at
##    https://github.com/dankelley/oce-issues/tree/master/449
t <- seq(0, 1, length.out=100)
plim <- c(38, 41)
p <- mean(plim) + diff(plim) / 2 * cos(2 * pi * t)
zlim <- c(38.5, 40.5)
cm <- colormap(p, breaks=seq(zlim[1], zlim[2], 0.1))
stopifnot(all.equal(class(cm), c("list","colormap")))
stopifnot(all.equal(cm$zlim, zlim))
stopifnot(all.equal(cm$breaks, seq(38.5, 40.5, 0.1)))
stopifnot(all.equal(cm$col,
                    c("#00007F","#0000B4","#0000EA","#0021FF","#0056FF","#008CFF","#00C2FF","#00F8FF","#2EFFCF","#64FF99","#99FF64",
                      "#CFFF2E","#FFF800","#FFC200","#FF8C00","#FF5600","#FF2100","#EA0000","#B40000","#7F0000")))
stopifnot(all.equal(cm$zcol,
                    c("#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000",
                      "#7F0000","#7F0000","#7F0000","#7F0000","#B40000","#EA0000","#EA0000","#FF2100","#FF5600","#FF8C00","#FFC200",
                      "#FFF800","#CFFF2E","#99FF64","#64FF99","#2EFFCF","#00F8FF","#00C2FF","#00C2FF","#008CFF","#0056FF","#0021FF",
                      "#0000EA","#0000B4","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F",
                      "#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F",
                      "#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#00007F","#0000B4",
                      "#0000EA","#0021FF","#0056FF","#008CFF","#00C2FF","#00C2FF","#00F8FF","#2EFFCF","#64FF99","#99FF64","#CFFF2E",
                      "#FFF800","#FFC200","#FF8C00","#FF5600","#FF2100","#EA0000","#EA0000","#B40000","#7F0000","#7F0000","#7F0000",
                      "#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000","#7F0000",
                      "#7F0000")))
stopifnot(all.equal(cm$missingColor, "gray"))
stopifnot(all.equal(cm$x0,
                    c(38.6,38.7,38.8,38.9,39.0,39.1,39.2,39.3,39.4,39.5,39.6,39.7,39.8,39.9,40.0,40.1,40.2,40.3,40.4,40.5)))
stopifnot(all.equal(cm$x1,
                    c(38.6,38.7,38.8,38.9,39.0,39.1,39.2,39.3,39.4,39.5,39.6,39.7,39.8,39.9,40.0,40.1,40.2,40.3,40.4,40.5)))
stopifnot(all.equal(cm$col0,
                    c("#00007F","#0000B4","#0000EA","#0021FF","#0056FF","#008CFF","#00C2FF","#00F8FF","#2EFFCF","#64FF99","#99FF64",
                      "#CFFF2E","#FFF800","#FFC200","#FF8C00","#FF5600","#FF2100","#EA0000","#B40000","#7F0000")))
stopifnot(all.equal(cm$col1,
                    c("#00007F","#0000B4","#0000EA","#0021FF","#0056FF","#008CFF","#00C2FF","#00F8FF","#2EFFCF","#64FF99","#99FF64",
                      "#CFFF2E","#FFF800","#FFC200","#FF8C00","#FF5600","#FF2100","#EA0000","#B40000","#7F0000")))

## Ex1 from help(colormap)
x <- seq(0, 1, length.out=40)
y <- sin(2 * pi * x)
c <- colormap(y)
stopifnot(all.equal(c$zlim,
                    c(-1.079124,1.079124),
                    tolerance=0.00001))
stopifnot(all.equal(c$breaks[1:3],
                    c(-1.079124,-1.068279,-1.057433),
                    tolerance=0.00001))
stopifnot(all.equal(c$col[1:4], c("#00007F","#000084","#000089","#00008E")))
stopifnot(all.equal(c$zcol[1:4], c("#7FFF7F","#CCFF32","#FFE900","#FFA000")))
stopifnot(all.equal(c$missingColor, "gray"))
stopifnot(all.equal(c$x0[1:5],
                    c(-1.068279,-1.057433,-1.046588,-1.035742,-1.024897),
                    tolerance=0.00001))
stopifnot(all.equal(c$x0[1:5],
                    c(-1.068279,-1.057433,-1.046588,-1.035742,-1.024897),
                    tolerance=0.00001))
stopifnot(all.equal(c$col0[1:4], c("#00007F","#000084","#000089","#00008E")))
stopifnot(all.equal(c$col1[1:4], c("#00007F","#000084","#000089","#00008E")))
stopifnot(all.equal(class(c), c("list","colormap")))

## Construct from (x0,x1,col0,col1)
## 20150426 (issue 637)
n <- 11 # must be odd
x0 <- seq.int(1,n-1)
x1 <- x0 + 1
col <- oceColorsJet(n)
col0 <- col[x0]
col1 <- col[x1]
cm <- colormap(x0=x0, x1=x1, col0=col0, col1=col1)
names(cm)
names(colormap(name="gmt_globe"))
stopifnot("black" == cm$zcol)
stopifnot("gray" == cm$missingColor)
stopifnot(all.equal(x0, cm$x0))
stopifnot(all.equal(x1, cm$x1))
stopifnot(n-1 == length(cm$col0))
stopifnot(n-1 == length(cm$col1))
stopifnot(all.equal(cm$col0, col0))
stopifnot(all.equal(cm$col1, col1))
## The next plot looks OK to dk, so its output is used
## in the following tests.
if (interactive()) {
    drawPalette(colormap=cm)
}
stopifnot(all.equal(cm$col, c("#00007F", "#0000E5", "#004CFF", "#00B2FF",
                              "#19FFE5", "#7FFF7F", "#E5FF19", "#FFB200",
                              "#FF4C00", "#E50000"))) 
stopifnot(all.equal(cm$breaks, 1:11))

