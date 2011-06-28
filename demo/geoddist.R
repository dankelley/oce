library(oce);
cat("Originally, the kilometre was defined as 1/10000 of the\n")
cat("distance from the equator to the pole on a line running\n")
cat("through Paris. This deviates from the present definition\n")
cat("by ")
cat(round(0.01*abs(1 - geodDist(0,2,90,2)/1e4),digits=7))
cat("%, according to geodDist().\n")
