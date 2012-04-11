library(oce)
t <- ISOdatetime(1957, 10, 4, hour=0, min=0, sec=0, tz="ET")+0.81*86400
stopifnot(all.equal(julianDay(t), 2436116.31, 0.01))

