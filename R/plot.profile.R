plot.profile <- function (x,
                          type = "S",
                          col.S = "darkgreen",
                          col.t = "red",
                          col.rho = "blue",
                          col.N2 = "brown",
                          grid = FALSE,
                          col.grid = "lightgray",
                          ...)
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    pname <- "Pressure [ dbar ]"
    if (type == "S") {
        plot(x$data$salinity, x$data$pressure, ylim = rev(range(x$data$pressure)),
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        mtext("Salinity [ PSU ]", side = 3, line = 3, col = col.S)
        axis(2, ...)
        axis(3, col = col.S, col.axis = col.S, col.lab = col.S, ...)
        box()
        if (grid) grid(col=col.grid)
        lines(x$data$salinity, x$data$pressure, col = col.S)
    } else if (type == "T") {
        plot(x$data$temperature, x$data$pressure, ylim = rev(range(x$data$pressure)),
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        mtext(expression(paste("Temperature [ ", degree, "C ]")),
              side = 3, line = 3, col = col.t)
        axis(2, ...)
        axis(3, col = col.t, col.axis = col.t, col.lab = col.t, ...)
        box()
        if (grid) grid(col=col.grid)
        lines(x$data$temperature, x$data$pressure, col = col.t)
    } else if (type == "density") {
        plot(x$data$sigma.theta, x$data$pressure, ylim = rev(range(x$data$pressure)),
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3,
              line = 3, col = col.rho)
        axis(2, ...)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho, ...)
        box()
        if (grid) grid(col=col.grid)
        lines(x$data$sigma.theta, x$data$pressure, col = col.rho)
    } else if (type == "density+N2") {
	st <- sw.sigma.theta(x$data$salinity, x$data$temperature, x$data$pressure)
        plot(st, x$data$pressure, ylim = rev(range(x$data$pressure)),
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho, ...)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3,
              line = 3, col = col.rho)
        axis(2, ...)
        box()
        lines(st, x$data$pressure, col = col.rho)
        par(new = TRUE)
        N2 <- sw.N2(x$data$pressure, st, ...)
        plot(N2, x$data$pressure, ylim = rev(range(x$data$pressure)),
             type = "n", xlab = "", ylab = "", axes = FALSE)
        axis(1, col = col.N2, col.axis = col.N2, col.lab = col.N2)
        lines(N2, x$data$pressure, col = col.N2)
                                        #abline(v = 0, col = col.N2)
        mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 1,
              line = 3, col = col.N2)
        box()
        if (grid) grid(col=col.grid)
    } else if (type == "N2") {
        N2 <- N2(x$data$pressure, x$data$sigma.theta, df = length(x$data$pressure)/4)
        plot(N2, x$data$pressure, ylim = rev(range(x$data$pressure)),
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 3,
              line = 3, col = col.N2)
        axis(2, ...)
        axis(3, col = col.N2, col.axis = col.N2, col.lab = col.N2, ...)
        box()
        if (grid) grid(col=col.grid)
        lines(N2, x$data$pressure, col = col.N2)
        abline(v = 0, col = col.N2)
    } else if (type == "S+T") {
        par(mfcol = c(2, 2))
        plot(x$data$temperature, x$data$pressure, ylim = rev(range(x$data$pressure)),
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        axis(3, col = col.t, col.axis = col.t, col.lab = col.t, ...)
        mtext(expression(paste("Temperature [ ", degree, "C ]")),
              side = 3, line = 3, col = col.t)
        axis(2, ...)
        box()
        lines(x$data$temperature, x$data$pressure, col = col.t)
        par(new = TRUE)
        plot(x$data$salinity, x$data$pressure, ylim = rev(range(x$data$pressure)),
             type = "n", xlab = "", ylab = "", axes = FALSE)
        axis(1, col = col.S, col.axis = col.S, col.lab = col.S, ...)
        lines(x$data$salinity, x$data$pressure, col = col.S)
        mtext("Salinity [ PSU ]", side = 1, line = 3, col = col.S)
        box()
        if (grid) grid(col=col.grid)
    } else {
        stop("unknown type, ", type, ", given")
    }
}
