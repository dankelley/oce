set.seed(20230120)
# Test of inferring 4-hour signal from data sampled at 6-hours

tau <- 4.1 # imagined tidal constituent [hours]
deltat <- 6 # time between data [hours]
days <- 5 # time interval of simulated data
ampC <- 1
ampS <- 0.5
sd <- 0.05 # noise to avoid perfect fit

# underlying signal
t0 <- seq(0, days*24, 0.1) # hours
x0 <- ampC*cos(2*pi*t0/tau) + ampS*sin(2*pi*t0/tau) + rnorm(length(t0), sd=sd)

# sieve to get at 6 hourly data
sieve <- 0 == t0 %% deltat

# sample every 6 hours
ts <- t0[sieve]
xs <- x0[sieve]
stopifnot(all(diff(ts) == 6))

C <- cos(2 * pi * ts / tau)
S <- sin(2 * pi * ts / tau)
M <- cbind(C=C, S=S)
model <- lm(xs ~ M)
summary(model)
ci <- confint(model)
meanPM <- function(ci) {
    sprintf("%.3f±%.3f", mean(ci), 0.5*(ci[2] - ci[1]))
}

text <- sprintf("Fitted: intercept %s, cosine %s, sine %s",
    meanPM(ci[1,]), meanPM(ci[2,]), meanPM(ci[3,]))

par(mfrow=c(3, 1), mar=c(3, 3, 1.5, 1), mgp=c(1.9, 0.7, 0))
plot(t0, x0, type="l")
mtext(paste("Underlying signal: period ", tau, "with cosine amplitude ", ampC, ", sine amplitude ", ampS), cex=par("cex"))
abline(v=ts, col=2)

plot(ts, xs, type="l")
mtext("Sampled signal", cex=par("cex"))

image(ts, 1:2, M)
mtext(text, cex=par("cex"))
