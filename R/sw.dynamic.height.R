sw.dynamic.height <- function(x, pref=2000)
{
    height <- function(ctd, pref)
    {
        if (sum(!is.na(ctd$data$pressure)) < 2) return(NA) # cannot integrate then
        g <- if (is.na(ctd$metadata$latitude)) 9.8 else gravity(ctd$metadata$latitude)
        np <- length(ctd$data$pressure)
        rho <- sw.rho(ctd)
        if (sum(!is.na(rho)) < 2) return(NA)
        ## 1e4 converts decibar to Pa
        dzdp <- ((1/rho - 1/sw.rho(rep(35,np),rep(0,np),ctd$data$pressure))/g)*1e4
##        print(summary(ctd))
        integrand <- approxfun(ctd$data$pressure, dzdp, rule=2)
        integrate(integrand, 0, pref)$value
    }
    if (inherits(x, "section")) {
        lon0 <- x$data$station[[1]]$metadata$longitude
        lat0 <- x$data$station[[1]]$metadata$latitude
        ns <- length(x$data$station)
        d <- vector("numeric", ns)
        h <- vector("numeric", ns)
        for (i in 1:ns) {
##            cat("i=",i,"\n")
            d[i] <- geod.dist(x$data$station[[i]]$metadata$latitude, x$data$station[[i]]$metadata$longitude, lat0, lon0)
            h[i] <- height(x$data$station[[i]], pref)
        }
        return(list(distance=d, height=h))
    } else if (inherits(x, "ctd")) {
        return(height(x, pref))
    } else {
        stop("method only works for 'section' or 'ctd' objects")
    }
}
