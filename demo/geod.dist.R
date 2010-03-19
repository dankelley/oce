library(oce);
cat("Originally, the kilometre was defined as 1/10000 of the distance
from the equator to the pole on a line running through Paris.  This
deviates from the modern definition by",
round(0.01*abs(1 - geod.dist(0,2,90,2)/1e4),digits=7),
"percent, according to the
geodesic distance calculated by geod.dist().\n")
