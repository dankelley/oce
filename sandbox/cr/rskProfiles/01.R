detectProfiles <- function(x, dp=3, Cmin=0.05) {
    p <- x[['pressure']]
    t <- x[['time']]
    C <- x[['conductivity']]

    cs <- NULL
    castState <- 0 # 0 for unknown, 1 for down, 2 for up
    event <- 0 # 0 for nothing, 1 for downcast start, 2 for upcast start, 3 for out of water
    res <- read.table(text = "",
                      colClasses=c('integer', 'integer', 'POSIXct'),
                      col.names=c('event', 'index', 'time'))                      

    index <- seq_along(t)
    k <- 1
    klast <- k
    maxp <- p[1]
    minp <- p[1]
    
    for (k in index) {

        event <- 0
        if (C[k] < Cmin) {
            cat('OUT OF WATER!\n')
            event <- 3
            minp <- p[k]
        } else {
            
            if (castState == 0) {
                if (p[k] > maxp) {
                    maxp <- p[k]
                    if ( (maxp - minp) > dp ) {
                        castState <- 1
                        event <- 1
                    }
                }
                if (p[k] < minp) {
                    minp <- p[k]
                    if ( (maxp - minp) > dp ) {
                        castState <- 2
                        event <- 2
                    }
                }
            } else if (castState == 1) {
                if (p[k] > maxp)
                    maxp  <- p[k]
                if (p[k] < minp)
                    minp <- p[k]
                if ( (maxp - p[k]) > max(c(dp, 0.05*(maxp-minp))) ) {
                    castState <- 2
                    event <- 2
                    minp <- p[k]
                }
            } else if (castState == 2) {
                if (p[k] > maxp)
                    maxp <- p[k]
                if (p[k] < minp)
                    minp <- p[k]
                if ( (p[k] - minp) > max(c(dp, 0.05*(maxp-minp))) ) {
                    castState <- 1
                    event <- 1
                    maxp <- p[k]
                }
            }
            ## cs[k] <- castState
            
            if (event == 1) {
                ## downcast detected
                profileTime <- t[klast:k]
                i <- (klast:k)[which(p[klast:k] == minp)]
                res <- rbind(res, data.frame(event=event, index=i, time=t[i]))
                klast <- k
            } else if (event == 2) {
                ## upcast detected
                profileTime <- t[klast:k]
                i <- (klast:k)[which(p[klast:k] == maxp)]
                res <- rbind(res, data.frame(event=event, index=i, time=t[i]))
                klast <- k
            } else if (event == 3) {
                res <- rbind(res, data.frame(event=event, index=k, time=t[k]))
            }
        }
    }
    return(res)
}

library(oce)
d <- as.ctd(read.oce('066015_20181024_1010.rsk'))

prof <- detectProfiles(d)

## Now use ctdFindProfiles with the `breaks` argument to split the up/down casts
dd <- ctdFindProfiles(d, breaks=prof$index)[-1]

## If we want only up/down:
down <- dd[prof$event == 1]
up <- dd[prof$event == 2]
