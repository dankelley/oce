#>> datestr(n)
#
#ans =
#
#22-Jan-2008 18:50:24
#
#>> [astro, ader]=t_astron(n)
#
#astro =
#
#    1.2886
#    0.3339
#    0.8375
#    0.1423
#    0.0856
#    0.7863
#
#
#ader =
#
#    0.9661
#    0.0366
#    0.0027
#    0.0003
#    0.0001
#    0.0000


tidem.astron <- function(t)
{
                                        # Code mimics t_astron in t_tide
    d <- as.numeric(difftime(t, ISOdatetime(1899,12,31,12,0,0), units="days"))
    D <- d / 10000
    a <- matrix(c(1, d, D^2, D^3), 4, 1)
    sc.hc.pc.np.pp <-
        matrix(c(270.434164, 13.1763965268,-0.0000850, 0.000000039,
                 279.696678,  0.9856473354, 0.00002267,0.000000000,
                 334.329556,  0.1114040803,-0.0007739,-0.00000026 ,
                 -259.183275, 0.0529539222,-0.0001557,-0.000000050,
                 281.220844,  0.0000470684, 0.0000339, 0.000000070),
               nrow=5, ncol=4, byrow=TRUE)
    astro <- ((sc.hc.pc.np.pp %*% a) / 360) %% 1
	rem <- as.numeric(difftime(n, trunc.POSIXt(n,units="days"), units="days"))
    tau <- rem + astro[2,1] - astro[1,1]
    astro <- c(tau, astro)
	da <- matrix(c(0, 1, 2e-4*D, 3e-4*D^2), 4, 1)
	ader <- (sc.hc.pc.np.pp %*% da) / 360
	dtau <- 1 + ader[2,1] - ader[1,1]
	ader <- c(dtau, ader)
    data.frame(astro=astro, ader=ader)
}

# Test against matlab t_astron
a <- astron(as.POSIXct("2008-01-22 18:50:24"))
stopifnot(all.equal(a$astro, c(1.2886, 0.3339, 0.8375, 0.1423, 0.0856, 0.7863), 0.001))
stopifnot(all.equal(a$ader,  c(0.9661, 0.0366, 0.0027, 0.0003, 0.0001, 0.0000), 0.001))
