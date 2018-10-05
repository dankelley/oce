## ---- echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -------------------------------------------------------------------------
library(oce)

## -------------------------------------------------------------------------
library(oce)
data(ctdRaw)

## ----fig.cap="Figure 1. Summary plot for raw CTD profile."----------------
plot(ctdRaw)

## -------------------------------------------------------------------------
badS <- with(ctdRaw[["data"]], salinity < 25 | 40 < salinity)
badT <- with(ctdRaw[["data"]], temperature < -2 | 40 < temperature)

## -------------------------------------------------------------------------
S <- ctdRaw[["salinity"]]
T <- ctdRaw[["temperature"]]
bad <- (S < 25 | 40 < S) || (T < -2 | 40 < T)

## -------------------------------------------------------------------------
qc <- ctdRaw

## -------------------------------------------------------------------------
qc <- initializeFlagScheme(qc, "WHP CTD")

## -------------------------------------------------------------------------
qc <- initializeFlags(qc, "salinity", 2)
qc <- initializeFlags(qc, "temperature", 2)

## -------------------------------------------------------------------------
qc <- setFlags(qc, "salinity", badS, value="bad")

## -------------------------------------------------------------------------
names(qc[["flags"]])

## -------------------------------------------------------------------------
qc <- setFlags(qc, "temperature", badT, value="bad")

## -------------------------------------------------------------------------
qch <- handleFlags(qc, flags=list(c(1, 3:9)))

## ----fig.cap="Figure 2. Summary plot for range-checked CTD profile."------
plot(qch)

## ----eval=FALSE-----------------------------------------------------------
#  options(eos="gsw")
#  data(ctd)
#  qc <- ctd
#  qc <- initializeFlagScheme(qc, "WHP CTD")
#  qc <- initializeFlags(qc, "salinity", 2)
#  Sspan <- diff(range(qc[["SA"]]))
#  Tspan <- diff(range(qc[["CT"]]))
#  n <- length(qc[["SA"]])
#  par(mfrow=c(1, 1))
#  plotTS(qc, type="o")
#  message("Click on bad points; quit by clicking to right of plot")
#  for (i in seq_len(n)) {
#      xy <- locator(1)
#      if (xy$x > par("usr")[2])
#          break
#      i <- which.min(abs(qc[["SA"]] - xy$x)/Sspan + abs(qc[["CT"]] - xy$y)/Tspan)
#      qc <- setFlags(qc, "salinity", i=i, value="bad")
#      qc <- handleFlags(qc)
#      plotTS(qc, type="o")
#  }

## -------------------------------------------------------------------------
data(section)
s <- handleFlags(section, flags=list(c(1, 3:9)))
par(mfrow=c(2, 1))
plotTS(section)
plotTS(s)

## -------------------------------------------------------------------------
data(adp)
v <- adp[["v"]]
i2 <- array(FALSE, dim=dim(v)) # construct array to match 'v'
g <- adp[["g", "numeric"]]
G <- 25                  # for percent-good field, named 'g'
V4 <- 0.45               # for error velocity field, in beam 4
for (k in 1:3)
    i2[,,k] <- ((g[,,k]+g[,,4]) < G) | (v[,,4] > V4)
adpQC2 <- initializeFlags(adp, "v", 2)
adpQC2 <- setFlags(adpQC2, "v", i2, 3)
adpClean2 <- handleFlags(adpQC2, flags=list(3), actions=list("NA"))
par(mfrow=c(2,1))
plot(adp, which="u1")                  # original
plot(adpClean2, which="u1")            # altered

