plot.profile <- function (x,
                          type = "S",
                          col.S = "darkgreen",
                          col.t = "red",
                          col.rho = "blue",
                          col.N2 = "brown",
                          grid = FALSE,
                          col.grid = "lightgray",
                          Slim, Tlim, densitylim, N2lim, plim,
                          lwd=par("lwd"),
                          ...)
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    old.mgp <- par("mgp")
    if (!"mgp" %in% names(list(...))) par(mgp = c(2, 2/3, 0))
    pname <- "Pressure [ dbar ]"
    if (missing(plim)) plim <- rev(range(x$data$pressure)) else plim <- rev(sort(plim))
    if (type == "S") {
        if (missing(Slim)) Slim <- range(x$data$salinity, na.rm=TRUE)
        plot(x$data$salinity, x$data$pressure,
             xlim=Slim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE, mgp=mgp)
        mtext("Salinity [ PSU ]", side = 3, line = mgp[1], col = col.S, cex=par("cex"))
        axis(2)
        axis(3, col = col.S, col.axis = col.S, col.lab = col.S, mgp=mgp)
        box()
        if (grid) grid(col=col.grid)
        lines(x$data$salinity, x$data$pressure, col = col.S, lwd=lwd)
    } else if (type == "T") {
        if (missing(Tlim)) Tlim <- range(x$data$temperature, na.rm=TRUE)
        plot(x$data$temperature, x$data$pressure,
             xlim=Tlim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE, mgp=mgp)
        mtext(expression(paste("Temperature [ ", degree, "C ]")), side = 3, line = mgp[1], col = col.t, cex=par("cex"))
        axis(2)
        axis(3, col = col.t, col.axis = col.t, col.lab = col.t, mgp=mgp)
        box()
        if (grid) grid(col=col.grid)
        lines(x$data$temperature, x$data$pressure, col = col.t, lwd=lwd)
    } else if (type == "density") {
	st <- sw.sigma.theta(x$data$salinity, x$data$temperature, x$data$pressure)
        if (missing(densitylim)) densitylim <- range(st, na.rm=TRUE)
        plot(st, x$data$pressure,
             xlim=densitylim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE, mgp=mgp)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = mgp[1], col = col.rho, cex=par("cex"))
        axis(2)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho, mgp=mgp)
        box()
        if (grid) grid(col=col.grid)
        lines(x$data$sigma.theta, x$data$pressure, col = col.rho, lwd=lwd)
    } else if (type == "density+N2") {
        if (missing(densitylim)) densitylim <- range(x$data$sigma.theta, na.rm=TRUE)
	st <- sw.sigma.theta(x$data$salinity, x$data$temperature, x$data$pressure)
        plot(st, x$data$pressure,
             xlim=densitylim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE, mgp=mgp)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho, mgp=mgp)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = mgp[1], col = col.rho, cex=par("cex"))
        axis(2)
        box()
        lines(st, x$data$pressure, col = col.rho, lwd=lwd)
        par(new = TRUE)
        N2 <- sw.N2(x$data$pressure, st, ...)
        if (missing(N2lim)) N2lim <- range(N2, na.rm=TRUE)
        plot(N2, x$data$pressure,
             xlim=N2lim, ylim=plim,
             type = "n", xlab = "", ylab = "", axes = FALSE, lwd=lwd, mgp=mgp)
        axis(1, col = col.N2, col.axis = col.N2, col.lab = col.N2, mgp=mgp)
        lines(N2, x$data$pressure, col = col.N2, lwd=lwd)
        mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 1, line = mgp[1], col = col.N2, cex=par("cex"))
        box()
        if (grid) grid(col=col.grid)
    } else if (type == "N2") {
        N2 <- N2(x$data$pressure, x$data$sigma.theta, df = length(x$data$pressure)/4)
        if (missing(N2lim)) N2lim <- range(N2, na.rm=TRUE)
        plot(N2, x$data$pressure,
             xlim=N2lim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE, mgp=mgp)
        mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 3, line = mgp[1], col = col.N2, cex=par("cex"))
        axis(2)
        axis(3, col = col.N2, col.axis = col.N2, col.lab = col.N2, mgp=mgp)
        box()
        if (grid) grid(col=col.grid)
        lines(N2, x$data$pressure, col = col.N2, lwd=lwd)
        abline(v = 0, col = col.N2)
    } else if (type == "S+T") {
        if (missing(Slim)) Slim <- range(x$data$salinity, na.rm=TRUE)
        if (missing(Tlim)) Tlim <- range(x$data$temperature, na.rm=TRUE)
        plot(x$data$temperature, x$data$pressure,
             xlim=Tlim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE, mgp=mgp)
        axis(3, col = col.t, col.axis = col.t, col.lab = col.t, mgp=mgp)
        mtext(expression(paste("Temperature [ ", degree, "C ]")), side = 3, line = mgp[1], col = col.t, cex=par("cex"))
        axis(2, mgp=mgp)
        box()
        lines(x$data$temperature, x$data$pressure, col = col.t, lwd=lwd)
        par(new = TRUE)
        plot(x$data$salinity, x$data$pressure,
             xlim=Slim, ylim=plim,
             type = "n", xlab = "", ylab = "", axes = FALSE, mgp=mgp)
        axis(1, col = col.S, col.axis = col.S, col.lab = col.S, mgp=mgp)
        lines(x$data$salinity, x$data$pressure, col = col.S, lwd=lwd)
        mtext("Salinity [ PSU ]", side = 1, line = mgp[1], col = col.S, cex=par("cex"))
        box()
        if (grid) grid(col=col.grid)
    } else {
        stop("unknown type, ", type, ", given")
    }
    par(mgp=old.mgp)
}
