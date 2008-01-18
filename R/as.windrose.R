as.windrose <- function(x, y, dtheta = 5)
{
    dt <- dtheta * pi / 180
    dt2 <- dt / 2
    R <- sqrt(x^2 + y^2)
    angle <- atan2(y, x) + pi
    L <- max(R, na.rm=TRUE)
    nt <- 2 * pi / dt
    theta <- count <- mean <- median <- vector("numeric", nt)
    for (i in 1:nt) {
        theta[i] <- i * dt
        inside <- (angle < (theta[i] + dt2)) & (angle > (theta[i] - dt2))
        count[i] <- sum(inside)
        mean[i] <- mean(R[inside], na.rm=TRUE)
        median[i] <- median(R[inside])
    }
    rval <- list(theta=theta*180/pi, count=count, mean=mean, median=median)
    class(rval) <- "windrose"
    rval
}
