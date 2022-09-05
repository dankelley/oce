# The expect_equal lines are testing against an email from Ragnar at Norek.

rm(list=ls())
options(warn=1) # show when occur
library(oce)
library(testthat)
d <- read.oce("~/git/oce/tests/testthat/local_data/ad2cp/ad2cp_01.ad2cp")
m <- function(...) cat(oce::vectorShow(...))
BD <- ad2cpHeaderValue(d, "GETECHO", "BD")
expect_equal(BD, 2)
BD <- d@data$echosounder$blankingDistance # will work even if no header
expect_equal(BD, 2)
# I cnnot see in any Nortek document where XMIT1 is stored in the data,
# except in the header.  Note that it is in ms.
XMIT1 <- 1e-3*ad2cpHeaderValue(d, "GETECHO", "XMIT1")
expect_equal(XMIT1, 1e-3)
vsound <- 1500                         # m/s, value in Ragnar's email
soundSpeed <- mean(d@data$echosounder$soundSpeed)

samplingRate <- d@data$echosounderRaw$samplingRate
expect_equal(samplingRate, 4464.285644531)
isample <- (XMIT1 + 2*BD/vsound) * samplingRate
m("(with const sound speed)", isample)
isample <- (XMIT1 + 2*BD/soundSpeed) * samplingRate
m("(with proper sound speed)", isample)

Ner <- length(d[["echosounderRaw"]]$distance)

