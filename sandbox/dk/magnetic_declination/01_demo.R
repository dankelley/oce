library(oce)
if (!interactive()) pdf("01_demo.pdf")
par(mar=c(3, 3, 1, 1),  mgp=c(2, 0.7, 0))
cex <- 0.4

# current meter
data(cm)
summary(cm)
u <- cm[["u"]]
v <- cm[["v"]]
lim <- c(-1, 1) * max(abs(c(u, v)))
plot(u, v, xlim=lim, asp=1, cex=cex)
cm2 <- applyMagneticDeclination(cm, 90) # rotate clockwise 90deg
summary(cm2)
u2 <- cm2[["u"]]
v2 <- cm2[["v"]]
points(u2, v2, col=2, cex=cex)
mtext("data(cm): black=orig, red=applyMagneticDeclination(cm, 90)")

# adp
data(adp)
summary(adp)
velo <- adp[["v"]]
u <- velo[, , 1]
v <- velo[, , 2]
lim <- c(-1, 1) * max(abs(c(u, v)), na.rm=TRUE)
plot(u, v, xlim=lim, asp=1, cex=cex)
adp2 <- applyMagneticDeclination(adp, 90) # rotate clockwise 90deg
summary(adp2)
velo2 <- adp2[["v"]]
u2 <- velo2[, , 1]
v2 <- velo2[, , 2]
points(u2, v2, col=2, cex=cex)
mtext("data(adp): black=orig, red=applyMagneticDeclination(adp, 90)")

# adv
data(adv)
summary(adv)
velo <- adv[["v"]]
u <- velo[, 1]
v <- velo[, 2]
lim <- c(-1, 1) * max(abs(c(u, v)), na.rm=TRUE)
plot(u, v, xlim=lim, asp=1, cex=cex)
adv2 <- applyMagneticDeclination(adv, 90) # rotate clockwise 90deg
summary(adv2)
velo2 <- adv2[["v"]]
u2 <- velo2[, 1]
v2 <- velo2[, 2]
points(u2, v2, col=2, cex=cex)
mtext("data(adv): black=orig, red=applyMagneticDeclination(adv, 90)")
if (!interactive()) dev.off()
