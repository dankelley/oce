library(oce)
set.seed(441)
z <- rnorm(20)

## Part 1: test zlim in various conditions.  The docs explain those conditions,
## which are detailed, so there is little point in restating them here.  If
## the present tests are correct, those conditions are defined by lack of
## reported errors from the stopifnot() tests.
ZLIM <- rangeExtended(z)

## There are a *lot* of possible tests.  Either z is given or not.  That's 2
## tests.  Either zlim is given or not.  Now up to 4 tests.  Either breaks is
## given or not, now up to 8 tests.  Ditto name for 16 tests.  Ditto x0 for 32
## tests.  Of course some cases do not apply, e.g. it's an error to give both
## breaks and name.  Still, a *lot* of tests, and only a subset done
## here, in hopes of catching coding errors.

## 1. z alone or with zlim
cm <- colormap(z=z)
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, ZLIM))
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
stopifnot(all.equal(cm$zlim, rangeExtended(c(-10000, 10000))))
cm <- colormap(name="gmt_globe", zlim=c(-1,1))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, c(-1, 1)))

## 4. breaks plus name (latter ignored since former given)
cm <- colormap(breaks=0:5, name="gmt_globe")
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, rangeExtended(c(0, 5))))

## 5. (x0,col0,x1,col1) alone or with zlim
cm <- colormap(x0=c(0,1), col0=c('red', 'blue'), x1=c(0.5, 1.5), col1=c("pink", "green"))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, range(c(0, 1.5))))
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
stopifnot(all.equal(cm$zlim, rangeExtended(c(-10000, 10000))))
stopifnot(!any(is.na(cm$zcol)))
cm <- colormap(z=z, name="gmt_globe", zlim=c(-1,1))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, c(-1, 1)))
stopifnot(!any(is.na(cm$zcol)))

## 8. z plus (x0,col0,x1,col1) alone [z wins] or with zlim [zlim wins]
cm <- colormap(z=z, x0=c(0,1), col0=c('red', 'blue'), x1=c(0.5, 1.5), col1=c("pink", "green"))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, ZLIM))
stopifnot(!any(is.na(cm$zcol)))
cm <- colormap(z, zlim=c(-10,10), x0=c(0,1), col0=c('red', 'blue'), x1=c(0.5, 1.5), col1=c("pink", "green"))
stopifnot(length(cm$breaks) == 1 + length(cm$col))
stopifnot(all.equal(cm$zlim, c(-10, 10)))
stopifnot(!any(is.na(cm$zcol)))

## Below should raise an error, and it does, so no need to interrupt the flow
## here with an error!
##try(str(colormap(zclip=TRUE)))


