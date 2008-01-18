as.windrose <- function(x, y, dtheta = 15)
{
    dt <- dtheta * pi / 180
    dt2 <- dt / 2
    R <- sqrt(x^2 + y^2)
    angle <- atan2(y, x)
    L <- max(R, na.rm=TRUE)
    nt <- 2 * pi / dt
    theta <- count <- mean <- vector("numeric", nt)
    fivenum <- matrix(0, nt, 5)
    for (i in 1:nt) {
        theta[i] <- i * dt
        if (theta[i] < pi)
            inside <- (angle < (theta[i] + dt2)) & (angle > (theta[i] - dt2))
        else {
            inside <- ((2*pi+angle) < (theta[i] + dt2)) & ((2*pi+angle) > (theta[i] - dt2))
        }
        count[i] <- sum(inside)
        mean[i] <- mean(R[inside], na.rm=TRUE)
        fivenum[i,] <- fivenum(R[inside], na.rm=TRUE)
    }
    data <- list(theta=theta*180/pi, count=count, mean=mean, fivenum=fivenum)
    metadata <- list(n=length(x), dtheta=dtheta)
    log <- list(time=c(Sys.time()), action=c("created by as.windrose()"))
    res <- list(data=data, metadata=metadata, log=log)
    class(res) <- c("windrose", "oce")
    res
}
